#' Analiza wrazliwosci granic przy brakach nielosowych
#'
#' Funkcja zaweza granice podstawowe, korzystajac z dodatkowych zalozen
#' o rozkladzie czynnika zaklocajacego wsrod jednostek z brakiem. Parametry
#' \eqn{\alpha_e} i \eqn{\beta_e} ograniczaja kazde prawdopodobienstwo
#' \eqn{P(U = u \mid E = e, R = 1)}:
#' \eqn{\alpha_e \le P(U = u \mid E = e, R = 1) \le \beta_e} dla wszystkich
#' poziomow \eqn{u}. Granice z rownania (6) artykulu Pena (2024) wynosza
#' \deqn{L^s_e = a_e + c_e b_e + \alpha_{1-e} h_e \sum_u r_{eu}, \qquad
#'       U^s_e = a_e + c_e b_e + \beta_{1-e} h_e \sum_u r_{eu},}
#' gdzie suma \eqn{\sum_u r_{eu}} jest zwykla suma ryzyk po poziomach
#' czynnika. Wynik koncowy jest przecieciem granic z rownania (6),
#' granic podstawowych z rownania (3) i przedzialu \eqn{[0, 1]}; wszystkie
#' konce przed przecieciem sa zwracane.
#'
#' Parametry przechodza dwie kontrole wykonalnosci, obie konieczne: istnienie
#' rozkladu o sumie 1 w zadanych granicach, czyli
#' \eqn{0 \le \alpha_e \le 1/K \le \beta_e \le 1}, oraz warunek artykulu
#' \eqn{\alpha_e \le \min(1, p_e / \sum_u r_{eu}) \le \beta_e}, gdzie
#' \eqn{p_e = P(D = 1 \mid E = e, R = 1)} jest ryzykiem obserwowanym
#' w brakach. Naruszenie ktorejkolwiek zatrzymuje obliczenie. Dodatkowo
#' funkcja wyznacza dokladny zakres wartosci \eqn{\sum_u r_{eu} q_u} dla
#' rozkladow \eqn{q} spelniajacych ograniczenia i raportuje, czy \eqn{p_e}
#' miesci sie w tym zakresie.
#'
#' @inheritParams granice_mnar
#' @param alfa,beta Wektory dlugosci 2 z dolnym i gornym ograniczeniem
#'   prawdopodobienstw \eqn{P(U = u \mid E = e, R = 1)} dla \eqn{e = 0} i
#'   \eqn{e = 1}, nazwane \code{"0"} i \code{"1"} albo podane w tej
#'   kolejnosci.
#'
#' @return Obiekt klasy \code{granice_mnar} (opis w
#'   \code{\link{granice_mnar}}) ze skladnikiem \code{wrazliwosc}: ramka
#'   danych z koncami granic podstawowych, granic z rownania (6) i ich
#'   przeciecia dla obu ramion. Kontrast jest liczony z przeciecia. Skladnik
#'   \code{parametry$wykonalnosc} zawiera wyniki kontroli wykonalnosci.
#'
#' @references Pena, J. M. (2024). Bounds and sensitivity analysis of the
#'   causal effect under outcome-independent MNAR confounding. arXiv
#'   2410.06726, wersja 2. \doi{10.48550/arXiv.2410.06726}
#'
#' @examples
#' wynik <- wrazliwosc_mnar(przyklad_mnar, alfa = c(0.2, 0.2),
#'                          beta = c(0.5, 0.5))
#' wynik$wrazliwosc
#' wynik$kontrasty
#'
#' @export
wrazliwosc_mnar <- function(dane, alfa, beta,
                            kontrast = c("roznica", "iloraz")) {
  kontrast <- match.arg(kontrast)
  tab <- jako_tablice(dane)
  sk <- skladniki_mnar(tab)
  podstawowe <- granice_mnar(tab, kontrast)$ryzyka
  alfa <- jako_parametr_ramion(alfa)
  beta <- jako_parametr_ramion(beta)

  wykonalnosc <- sprawdz_wykonalnosc(sk, alfa, beta)
  if (!all(wykonalnosc$warunek_sumy)) {
    stop("Parametry wrazliwosci nie dopuszczaja rozkladu o sumie 1.",
         call. = FALSE)
  }
  if (!all(wykonalnosc$warunek_artykulu, na.rm = TRUE)) {
    stop("Parametry wrazliwosci sa niezgodne z ryzykiem obserwowanym ",
         "w brakach (warunek artykulu).", call. = FALSE)
  }

  wrazliwosc <- granice_wrazliwosci(sk, podstawowe, alfa, beta)
  # Przy spelnionym warunku sumy przeciecie nie moze byc puste:
  # alfa * sum(r) <= sum(r) / K <= M oraz beta * sum(r) >= m.
  # Kontrola chroni przed bledem numerycznym albo zmiana wzorow.
  if (any(wrazliwosc$dolna_przeciecie > wrazliwosc$gorna_przeciecie + 1e-12)) {
    stop("Przeciecie granic jest puste.", call. = FALSE)
  }

  status <- if (all(wykonalnosc$warunek_dokladny, na.rm = TRUE)) {
    "wrazliwosc"
  } else {
    "wrazliwosc_warunek_dokladny_niespelniony"
  }
  wynik <- nowe_granice_mnar(podstawowe, kontrast, tab, sk,
                             wrazliwosc = wrazliwosc, status = status)
  wynik$parametry$alfa <- alfa
  wynik$parametry$beta <- beta
  wynik$parametry$wykonalnosc <- wykonalnosc
  wynik
}


#' Siatka wariantow analizy wrazliwosci
#'
#' Funkcja powtarza \code{\link{wrazliwosc_mnar}} dla kazdej kombinacji
#' czterech parametrow \eqn{(\alpha_0, \alpha_1, \beta_0, \beta_1)}.
#' Kombinacje niewykonalne nie zatrzymuja obliczenia: zostaja oznaczone
#' w kolumnie \code{wykonalna} wraz z przyczyna. Siatka jest jawna sciezka
#' w czterowymiarowej przestrzeni parametrow; kazdy wiersz podaje komplet
#' wartosci.
#'
#' @inheritParams granice_mnar
#' @param siatka Ramka danych z kolumnami \code{alfa0}, \code{alfa1},
#'   \code{beta0}, \code{beta1} i opcjonalnie \code{nazwa}.
#'
#' @return Obiekt klasy \code{siatka_mnar}, lista ze skladnikami:
#'   \describe{
#'     \item{wyniki}{ramka danych: parametry, \code{wykonalna},
#'       \code{przyczyna}, granice obu ryzyk i kontrastu po przecieciu oraz
#'       \code{zaweza} (czy ktores ryzyko ma granice wezsze od
#'       podstawowych)}
#'     \item{podstawowe}{obiekt \code{granice_mnar} z granicami
#'       podstawowymi}
#'     \item{parametry}{lista z rodzajem kontrastu}
#'   }
#'
#' @examples
#' siatka <- expand.grid(alfa0 = c(0, 0.2), alfa1 = c(0, 0.2),
#'                       beta0 = c(0.5, 1), beta1 = c(0.5, 1))
#' wynik <- siatka_wrazliwosci(przyklad_mnar, siatka)
#' wynik
#'
#' @export
siatka_wrazliwosci <- function(dane, siatka,
                               kontrast = c("roznica", "iloraz")) {
  # TODO: kontrola struktury siatki (wymagane kolumny, wartosci liczbowe
  #   z przedzialu [0, 1], unikalne nazwy wierszy).
  kontrast <- match.arg(kontrast)
  tab <- jako_tablice(dane)
  podstawowe <- granice_mnar(tab, kontrast)
  if (is.null(siatka$nazwa)) {
    siatka$nazwa <- sprintf("wariant_%02d", seq_len(nrow(siatka)))
  }

  wiersze <- lapply(seq_len(nrow(siatka)), function(i) {
    alfa <- c(`0` = siatka$alfa0[i], `1` = siatka$alfa1[i])
    beta <- c(`0` = siatka$beta0[i], `1` = siatka$beta1[i])
    wynik <- tryCatch(wrazliwosc_mnar(tab, alfa, beta, kontrast),
                      error = function(blad) conditionMessage(blad))
    if (is.character(wynik)) {
      return(data.frame(wykonalna = FALSE, przyczyna = wynik,
                        ryzyko0_dolna = NA_real_, ryzyko0_gorna = NA_real_,
                        ryzyko1_dolna = NA_real_, ryzyko1_gorna = NA_real_,
                        kontrast_dolna = NA_real_, kontrast_gorna = NA_real_,
                        zaweza = NA))
    }
    w <- wynik$wrazliwosc
    zaweza <- any(w$dolna_przeciecie > w$dolna_podstawowa + 1e-12 |
                    w$gorna_przeciecie < w$gorna_podstawowa - 1e-12)
    data.frame(wykonalna = TRUE, przyczyna = "",
               ryzyko0_dolna = w$dolna_przeciecie[1],
               ryzyko0_gorna = w$gorna_przeciecie[1],
               ryzyko1_dolna = w$dolna_przeciecie[2],
               ryzyko1_gorna = w$gorna_przeciecie[2],
               kontrast_dolna = wynik$kontrasty$dolna,
               kontrast_gorna = wynik$kontrasty$gorna,
               zaweza = zaweza)
  })
  wyniki <- cbind(siatka[c("nazwa", "alfa0", "alfa1", "beta0", "beta1")],
                  do.call(rbind, wiersze))
  rownames(wyniki) <- NULL

  structure(
    list(wyniki = wyniki, podstawowe = podstawowe,
         parametry = list(kontrast = kontrast)),
    class = "siatka_mnar"
  )
}


# Parametr dla dwoch ramion jako wektor nazwany "0", "1".
jako_parametr_ramion <- function(x) {
  # TODO: kontrola dlugosci, typu, zakresu [0, 1] i nazw parametru.
  if (is.null(names(x))) {
    names(x) <- c("0", "1")
  }
  x[c("0", "1")]
}


# Kontrole wykonalnosci parametrow dla obu ramion.
sprawdz_wykonalnosc <- function(sk, alfa, beta) {
  k <- sk$k
  t <- sk$tabela
  wiersze <- lapply(1:2, function(i) {
    r <- sk$r[i, ]
    p <- t$ryzyko_w_brakach[i]
    a <- alfa[[i]]
    b <- beta[[i]]
    warunek_sumy <- a >= 0 && b <= 1 && a <= 1 / k && b >= 1 / k
    iloraz <- if (is.na(p) || sum(r) == 0) NA_real_ else min(1, p / sum(r))
    warunek_artykulu <- if (is.na(iloraz)) NA else
      a <= iloraz + 1e-12 && iloraz <= b + 1e-12
    zakres <- if (warunek_sumy) zakres_ryzyka(r, a, b) else c(NA_real_, NA_real_)
    warunek_dokladny <- if (is.na(p) || !warunek_sumy) NA else
      p >= zakres[1] - 1e-12 && p <= zakres[2] + 1e-12
    data.frame(zabieg = i - 1L, alfa = a, beta = b,
               warunek_sumy = warunek_sumy,
               iloraz_artykulu = iloraz,
               warunek_artykulu = warunek_artykulu,
               ryzyko_w_brakach = p,
               zakres_dolny = zakres[1], zakres_gorny = zakres[2],
               warunek_dokladny = warunek_dokladny)
  })
  do.call(rbind, wiersze)
}


# Najmniejsza i najwieksza wartosc sum_u r_u q_u dla rozkladow q
# z ograniczeniami alfa <= q_u <= beta, sum q_u = 1. Masa ponad alfa trafia
# kolejno do poziomow o najmniejszym (minimum) albo najwiekszym (maksimum)
# ryzyku.
zakres_ryzyka <- function(r, alfa, beta) {
  rozdziel <- function(porzadek) {
    q <- rep(alfa, length(r))
    reszta <- 1 - sum(q)
    for (j in porzadek) {
      dodatek <- min(beta - alfa, reszta)
      q[j] <- q[j] + dodatek
      reszta <- reszta - dodatek
    }
    sum(r * q)
  }
  c(rozdziel(order(r)), rozdziel(order(r, decreasing = TRUE)))
}


# Konce granic z rownania (6) i ich przeciecie z granicami podstawowymi
# oraz przedzialem [0, 1]. Ramie e korzysta z parametrow ramienia 1 - e,
# poniewaz h_e dotyczy brakow w ramieniu przeciwnym.
granice_wrazliwosci <- function(sk, podstawowe, alfa, beta) {
  t <- sk$tabela
  baza <- t$a + t$c * t$b
  alfa_uzyte <- unname(alfa[c("1", "0")])
  beta_uzyte <- unname(beta[c("1", "0")])
  dolna_s <- baza + alfa_uzyte * t$h * t$suma_r
  gorna_s <- baza + beta_uzyte * t$h * t$suma_r
  data.frame(
    zabieg = t$zabieg,
    alfa_uzyte = alfa_uzyte,
    beta_uzyte = beta_uzyte,
    dolna_podstawowa = podstawowe$dolna,
    gorna_podstawowa = podstawowe$gorna,
    dolna_wrazliwosc = dolna_s,
    gorna_wrazliwosc = gorna_s,
    dolna_przeciecie = pmax(podstawowe$dolna, dolna_s, 0),
    gorna_przeciecie = pmin(podstawowe$gorna, gorna_s, 1)
  )
}
