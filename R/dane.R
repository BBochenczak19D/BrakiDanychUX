#' Przykladowe dane z brakami nielosowymi
#'
#' Proba 2000 jednostek wygenerowana funkcja \code{\link{generuj_mnar}}
#' (ziarno 202706) w skrypcie \code{data-raw/generuj_dane.R}. Czynnik
#' zaklocajacy ma trzy poziomy, a brak jego wartosci zalezy od zabiegu i od
#' samej wartosci czynnika. Prawdziwe ryzyka po interwencji wynosza 0,3855
#' dla \eqn{E = 0} i 0,5490 dla \eqn{E = 1}.
#'
#' @format Ramka danych o 2000 wierszach i 4 zmiennych:
#' \describe{
#'   \item{zabieg}{zabieg \eqn{E}, 0 albo 1}
#'   \item{wynik}{wynik \eqn{D}, 0 albo 1}
#'   \item{zaklocenie}{czynnik zaklocajacy \eqn{U}, poziomy 0, 1, 2;
#'     \code{NA} przy braku}
#'   \item{brak}{wskaznik braku \eqn{R}: 1, gdy wartosc \code{zaklocenie}
#'     jest nieobserwowana}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' table(przyklad_mnar$brak, przyklad_mnar$zabieg)
"przyklad_mnar"
