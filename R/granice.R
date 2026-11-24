#' Granice ryzyk po interwencji i kontrastu przy brakach nielosowych
#'
#' Funkcja wyznacza granice podstawowe ryzyka \eqn{P(D_e = 1)} dla obu
#' wartosci zabiegu wedlug rownania (3) artykulu Pena (2024) oraz granice
#' roznicy ryzyk (RD) albo ilorazu ryzyk (RR).
#'
#' Dla ramienia \eqn{e} niech \eqn{r_{eu} = P(D = 1 \mid E = e, U = u, R = 0)},
#' \eqn{m_e = \min_u r_{eu}}, \eqn{M_e = \max_u r_{eu}},
#' \eqn{c_e = \sum_u r_{eu} P(U = u \mid E = 1 - e, R = 0)},
#' \eqn{a_e = P(D = 1, E = e)}, \eqn{b_e = P(R = 0, E = 1 - e)} oraz
#' \eqn{h_e = P(R = 1, E = 1 - e)}. Granice wynosza
#' \deqn{L_e = a_e + c_e b_e + m_e h_e, \qquad
#'       U_e = a_e + c_e b_e + M_e h_e.}
#' Kontrasty: \eqn{RD \in [L_1 - U_0, U_1 - L_0]},
#' \eqn{RR \in [L_1 / U_0, U_1 / L_0]}.
#'
#' Granice wymagaja zalozen przyczynowych artykulu: czynnik \eqn{U} kontroluje
#' zaklocenie zaleznosci miedzy zabiegiem a wynikiem, a brak zalezy od
#' zabiegu i czynnika, lecz nie od wyniku
#' (\eqn{R \perp D \mid E, U}).
#'
#' Iloraz ryzyk ma osobne reguly dla zer: \eqn{L_0 = 0} zachodzi dokladnie
#' wtedy, gdy \eqn{U_0 = 0}. Dolna granica RR wynosi \eqn{L_1 / U_0} przy
#' \eqn{U_0 > 0}, \code{Inf} przy \eqn{U_0 = 0} i \eqn{L_1 > 0}, a w
#' pozostalych przypadkach \code{NA}. Gorna granica wynosi \eqn{U_1 / L_0}
#' przy \eqn{L_0 > 0}, \code{Inf} przy \eqn{L_0 = 0} i \eqn{U_1 > 0},
#' a w pozostalych przypadkach \code{NA}. Status kontrastu opisuje te
#' przypadki.
#'
#' @param dane Obiekt klasy \code{tablice_mnar} z \code{\link{przygotuj_mnar}}
#'   albo ramka danych jednostkowych przekazywana do tej funkcji.
#' @param kontrast \code{"roznica"} (RD) albo \code{"iloraz"} (RR).
#'
#' @return Obiekt klasy \code{granice_mnar}, lista ze skladnikami:
#'   \describe{
#'     \item{ryzyka}{ramka danych z kolumnami \code{zabieg}, \code{dolna},
#'       \code{gorna} oraz skladnikami wzoru \code{a}, \code{b}, \code{h},
#'       \code{c}, \code{m}, \code{M}, \code{suma_r},
#'       \code{ryzyko_w_brakach}}
#'     \item{kontrasty}{ramka danych z kolumnami \code{kontrast},
#'       \code{dolna}, \code{gorna}, \code{status}}
#'     \item{wrazliwosc}{\code{NULL}; skladnik wypelnia
#'       \code{\link{wrazliwosc_mnar}}}
#'     \item{liczebnosci}{suma wag komorek z \code{\link{przygotuj_mnar}}}
#'     \item{parametry}{lista: kontrast, poziomy czynnika, typ danych,
#'       macierz ryzyk \eqn{r_{eu}}}
#'     \item{status}{\code{"granice_podstawowe"}}
#'   }
#'
#' @references Pena, J. M. (2024). Bounds and sensitivity analysis of the
#'   causal effect under outcome-independent MNAR confounding. arXiv
#'   2410.06726, wersja 2. \doi{10.48550/arXiv.2410.06726}
#'
#' @seealso \code{\link{wrazliwosc_mnar}}, \code{\link{siatka_wrazliwosci}}
#'
#' @examples
#' wynik <- granice_mnar(przyklad_mnar)
#' wynik
#' granice_mnar(przyklad_mnar, kontrast = "iloraz")$kontrasty
#'
#' @export
granice_mnar <- function(dane, kontrast = c("roznica", "iloraz")) {
  kontrast <- match.arg(kontrast)
  tab <- jako_tablice(dane)
  sk <- skladniki_mnar(tab)
  t <- sk$tabela

  # Rownanie (3). Artykul zapisuje gorna granice jako min(1, .); suma nie
  # przekracza 1, poniewaz a_e + b_e + h_e <= P(E = e) + P(E = 1 - e).
  baza <- t$a + t$c * t$b
  ryzyka <- data.frame(zabieg = t$zabieg,
                       dolna = baza + t$m * t$h,
                       gorna = baza + t$M * t$h)
  ryzyka <- cbind(ryzyka, t[, -1])

  nowe_granice_mnar(ryzyka, kontrast, tab, sk)
}


# Konstruktor obiektu wyniku wspolny dla granic podstawowych i wrazliwosci.
nowe_granice_mnar <- function(ryzyka, kontrast, tab, sk, wrazliwosc = NULL,
                              status = "granice_podstawowe") {
  granice <- if (is.null(wrazliwosc)) ryzyka else
    data.frame(dolna = wrazliwosc$dolna_przeciecie,
               gorna = wrazliwosc$gorna_przeciecie)
  kontrasty <- przedzial_kontrastu(granice$dolna[1], granice$gorna[1],
                                   granice$dolna[2], granice$gorna[2],
                                   kontrast)
  structure(
    list(
      ryzyka = ryzyka,
      kontrasty = kontrasty,
      wrazliwosc = wrazliwosc,
      liczebnosci = tab$liczebnosci,
      parametry = list(kontrast = kontrast, poziomy = tab$poziomy,
                       typ = tab$typ, r = sk$r),
      status = status
    ),
    class = "granice_mnar"
  )
}


# Granice RD albo RR z granic ryzyk (L0, U0) i (L1, U1).
przedzial_kontrastu <- function(l0, u0, l1, u1, kontrast) {
  if (kontrast == "roznica") {
    return(data.frame(kontrast = "RD", dolna = l1 - u0, gorna = u1 - l0,
                      status = "zdefiniowany", stringsAsFactors = FALSE))
  }
  dolna <- if (u0 > 0) l1 / u0 else if (l1 > 0) Inf else NA_real_
  gorna <- if (l0 > 0) u1 / l0 else if (u1 > 0) Inf else NA_real_
  status <- if (anyNA(c(dolna, gorna))) {
    "iloraz_niezdefiniowany"
  } else if (any(is.infinite(c(dolna, gorna)))) {
    "iloraz_nieograniczony"
  } else {
    "zdefiniowany"
  }
  data.frame(kontrast = "RR", dolna = dolna, gorna = gorna, status = status,
             stringsAsFactors = FALSE)
}
