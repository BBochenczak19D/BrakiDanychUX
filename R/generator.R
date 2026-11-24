#' Generowanie danych z brakami nielosowymi o znanym efekcie
#'
#' Procedura tworzy populacje, w ktorej czynnik zaklocajacy \eqn{U} o trzech
#' poziomach wplywa na zabieg \eqn{E}, wynik \eqn{D} oraz na
#' prawdopodobienstwo braku wartosci \eqn{U} (\eqn{R = 1}). Braki sa
#' nielosowe: zaleza od nieobserwowanej wartosci \eqn{U}. Model:
#' \deqn{P(U = u) = (0{,}4;\ 0{,}35;\ 0{,}25), \quad u \in \{0, 1, 2\},}
#' \deqn{P(E = 1 \mid U = u) = \mathrm{logit}^{-1}(-0{,}5 + 0{,}5u),}
#' \deqn{P(D = 1 \mid E, U) = \mathrm{logit}^{-1}(-1 + 0{,}7E + 0{,}6U),}
#' \deqn{P(R = 1 \mid E, U, D) = \mathrm{logit}^{-1}(-1{,}5 + 0{,}5E +
#'       0{,}9U + \gamma D).}
#' Przy \eqn{\gamma = 0} zachodzi \eqn{R \perp D \mid E, U}, czyli zalozenie
#' metody. Wartosc \eqn{\gamma \ne 0} (argument \code{naruszenie}) tworzy
#' wariant, w ktorym brak zalezy takze od wyniku.
#'
#' Prawdziwe ryzyko po interwencji wynika z parametrow:
#' \eqn{P(D_e = 1) = \sum_u P(D = 1 \mid E = e, U = u) P(U = u)}.
#' Funkcja zwraca je razem z dokladna tablica prawdopodobienstw obserwowanych
#' (bez losowania) i proba jednostek.
#'
#' Kolejnosc losowania dla proby: \eqn{U}, \eqn{E}, \eqn{D}, \eqn{R}, kazda
#' zmienna wektorowo dla wszystkich jednostek.
#'
#' @param n Liczba jednostek w probie.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu losowania.
#' @param naruszenie Wspolczynnik \eqn{\gamma} wyniku w modelu braku;
#'   0 oznacza spelnione zalozenie \eqn{R \perp D \mid E, U}.
#'
#' @return Lista ze skladnikami:
#'   \describe{
#'     \item{dane}{proba jednostek: \code{zabieg}, \code{wynik},
#'       \code{zaklocenie} (\code{NA} przy braku), \code{brak}}
#'     \item{pelne}{ta sama proba z zachowana wartoscia \code{zaklocenie}
#'       przed maskowaniem}
#'     \item{tablica}{dokladna tablica obserwowanych prawdopodobienstw
#'       w formacie \code{\link{przygotuj_mnar}}, z kolumna \code{p}}
#'     \item{prawda}{wektor: \code{ryzyko0}, \code{ryzyko1}, \code{RD},
#'       \code{RR}}
#'     \item{rozklad_w_brakach}{macierz \eqn{P(U = u \mid E = e, R = 1)},
#'       wiersze: zabieg}
#'     \item{parametry}{lista parametrow modelu i wywolania}
#'   }
#'
#' @examples
#' g <- generuj_mnar(n = 500)
#' head(g$dane)
#' g$prawda
#' granice_mnar(przygotuj_mnar(g$tablica, wagi = g$tablica$p))$ryzyka[, 1:3]
#'
#' @export
generuj_mnar <- function(n = 5000, ziarno = 202706, naruszenie = 0) {
  # TODO: kontrola argumentow (n dodatnia liczba calkowita, ziarno
  #   pojedyncza liczba calkowita, naruszenie skonczone).
  odwrotny_logit <- stats::plogis
  p_u <- c(0.4, 0.35, 0.25)
  u <- 0:2
  p_e1 <- odwrotny_logit(-0.5 + 0.5 * u)
  p_d1 <- function(e, u) odwrotny_logit(-1 + 0.7 * e + 0.6 * u)
  p_r1 <- function(e, u, d) {
    odwrotny_logit(-1.5 + 0.5 * e + 0.9 * u + naruszenie * d)
  }

  # Dokladny rozklad lacznych (U, E, D, R)
  siatka <- expand.grid(u = u, e = 0:1, d = 0:1, r = 0:1)
  siatka$p <- p_u[siatka$u + 1] *
    ifelse(siatka$e == 1, p_e1[siatka$u + 1], 1 - p_e1[siatka$u + 1]) *
    ifelse(siatka$d == 1, p_d1(siatka$e, siatka$u),
           1 - p_d1(siatka$e, siatka$u)) *
    ifelse(siatka$r == 1, p_r1(siatka$e, siatka$u, siatka$d),
           1 - p_r1(siatka$e, siatka$u, siatka$d))

  obserwowane <- siatka[siatka$r == 0, ]
  w_brakach <- stats::aggregate(p ~ e + d, data = siatka[siatka$r == 1, ],
                                FUN = sum)
  tablica <- rbind(
    data.frame(zabieg = obserwowane$e, wynik = obserwowane$d,
               zaklocenie = obserwowane$u, brak = 0L, p = obserwowane$p),
    data.frame(zabieg = w_brakach$e, wynik = w_brakach$d,
               zaklocenie = NA_integer_, brak = 1L, p = w_brakach$p)
  )
  rownames(tablica) <- NULL

  ryzyko <- c(ryzyko0 = sum(p_d1(0, u) * p_u), ryzyko1 = sum(p_d1(1, u) * p_u))
  prawda <- c(ryzyko, RD = unname(ryzyko[2] - ryzyko[1]),
              RR = unname(ryzyko[2] / ryzyko[1]))

  braki <- siatka[siatka$r == 1, ]
  rozklad_w_brakach <- t(sapply(0:1, function(e) {
    masa <- tapply(braki$p[braki$e == e], braki$u[braki$e == e], sum)
    masa / sum(masa)
  }))
  dimnames(rozklad_w_brakach) <- list(zabieg = c("0", "1"),
                                      zaklocenie = as.character(u))

  proba <- withr::with_seed(ziarno, {
    uu <- sample(u, n, replace = TRUE, prob = p_u)
    ee <- stats::rbinom(n, 1, p_e1[uu + 1])
    dd <- stats::rbinom(n, 1, p_d1(ee, uu))
    rr <- stats::rbinom(n, 1, p_r1(ee, uu, dd))
    data.frame(zabieg = ee, wynik = dd, zaklocenie = uu, brak = rr)
  })
  dane <- proba
  dane$zaklocenie[dane$brak == 1] <- NA_integer_

  list(
    dane = dane,
    pelne = proba,
    tablica = tablica,
    prawda = prawda,
    rozklad_w_brakach = rozklad_w_brakach,
    parametry = list(p_u = p_u, n = n, ziarno = ziarno,
                     naruszenie = naruszenie)
  )
}
