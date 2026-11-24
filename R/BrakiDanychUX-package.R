#' BrakiDanychUX: granice efektu przyczynowego przy brakach nielosowych
#'
#' Pakiet wyznacza nieparametryczne granice efektu binarnego zabiegu na
#' binarny wynik, gdy dyskretny czynnik zaklocajacy jest czesciowo
#' nieobserwowany, a brak jego wartosci zalezy od niej samej (braki
#' nielosowe). Metoda pochodzi z pracy Pena (2024): granice podstawowe
#' (rownanie 3) oraz analiza wrazliwosci sterowana ograniczeniami rozkladu
#' czynnika zaklocajacego w brakach (rownanie 6).
#'
#' Sciezka analizy:
#' \itemize{
#'   \item \code{\link{przygotuj_mnar}} zamienia dane na tablice
#'     prawdopodobienstw,
#'   \item \code{\link{granice_mnar}} wyznacza granice ryzyk i kontrastu,
#'   \item \code{\link{wrazliwosc_mnar}} i \code{\link{siatka_wrazliwosci}}
#'     zaweza granice przy jawnych zalozeniach o brakach,
#'   \item \code{\link{generuj_mnar}} tworzy dane o znanym efekcie.
#' }
#'
#' @references Pena, J. M. (2024). Bounds and sensitivity analysis of the
#'   causal effect under outcome-independent MNAR confounding. arXiv
#'   2410.06726, wersja 2. \doi{10.48550/arXiv.2410.06726}
#'
#' @keywords internal
"_PACKAGE"
