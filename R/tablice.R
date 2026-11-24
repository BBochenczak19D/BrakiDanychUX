#' Przygotowanie tablic prawdopodobienstw przy brakach w czynniku zaklocajacym
#'
#' Funkcja zamienia dane jednostkowe albo dokladna tablice prawdopodobienstw
#' na dwie tablice, z ktorych korzystaja granice: rozklad
#' \eqn{P(D, E, U, R = 0)} dla jednostek z obserwowanym czynnikiem
#' zaklocajacym oraz rozklad \eqn{P(D, E, R = 1)} dla jednostek z brakiem.
#' Oznaczenia: \eqn{E} zabieg, \eqn{D} wynik, \eqn{U} czynnik zaklocajacy,
#' \eqn{R} wskaznik braku (\eqn{R = 1} oznacza brak wartosci \eqn{U}).
#'
#' Wejscie ma postac ramki danych z kolumnami \code{zabieg}, \code{wynik},
#' \code{zaklocenie} i \code{brak}. Kazdy wiersz jest jednostka (dane
#' z badania) albo komorka tablicy; w drugim przypadku argument \code{wagi}
#' podaje mase prawdopodobienstwa komorki. Wagi sa normalizowane do sumy 1,
#' wiec moga to byc takze licznosci. Dla wierszy z \code{brak = 1} kolumna
#' \code{zaklocenie} ma wartosc \code{NA}.
#'
#' @param dane Ramka danych z kolumnami \code{zabieg} (0/1), \code{wynik}
#'   (0/1), \code{zaklocenie} (poziom czynnika albo \code{NA} przy braku)
#'   i \code{brak} (0/1).
#' @param poziomy Wektor poziomow czynnika zaklocajacego w ustalonej
#'   kolejnosci. Domyslnie posortowane wartosci obserwowane.
#' @param wagi Wektor nieujemnych wag wierszy albo \code{NULL} (kazdy wiersz
#'   ma wage 1).
#'
#' @return Obiekt klasy \code{tablice_mnar}, lista ze skladnikami:
#'   \describe{
#'     \item{obserwowane}{tablica \eqn{2 \times 2 \times K} z
#'       \eqn{P(D = d, E = e, U = u, R = 0)}; wymiary: wynik, zabieg,
#'       zaklocenie}
#'     \item{braki}{macierz \eqn{2 \times 2} z \eqn{P(D = d, E = e, R = 1)};
#'       wymiary: wynik, zabieg}
#'     \item{poziomy}{poziomy czynnika zaklocajacego}
#'     \item{wsparcie}{macierz logiczna zabieg x zaklocenie: czy komorka
#'       \eqn{(E = e, U = u, R = 0)} ma dodatnia mase}
#'     \item{liczebnosci}{ramka danych z suma wag i suma wag jednostek
#'       z wynikiem 1 w kazdej komorce zabieg, zaklocenie, brak}
#'     \item{typ}{\code{"proba"} dla danych jednostkowych bez wag albo
#'       \code{"tablica"} dla danych z wagami}
#'     \item{suma_wag}{suma wag przed normalizacja}
#'   }
#'
#' @examples
#' tab <- przygotuj_mnar(przyklad_mnar)
#' tab$wsparcie
#' tab$liczebnosci
#'
#' @export
przygotuj_mnar <- function(dane, poziomy = NULL, wagi = NULL) {
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - `zabieg` i `wynik` wylacznie 0/1, bez brakow, komunikat
  #     "Zabieg i wynik musza byc binarne i kompletne",
  #   - `brak` wylacznie 0/1, `zaklocenie` rowne NA dokladnie tam, gdzie
  #     brak = 1, komunikat "Wskaznik braku musi byc zgodny z zakloceniem",
  #   - kazda komorka (zabieg, zaklocenie) wsrod obserwowanych ma dodatnia
  #     mase, komunikat "Brak obserwowanego wsparcia w komorce
  #     zabieg-zaklocenie" (wsparcie jest juz liczone w skladniku `wsparcie`),
  #   - `poziomy` bez powtorzen i obejmujace wszystkie obserwowane wartosci,
  #   - `wagi` skonczone, nieujemne, o dlugosci rownej liczbie wierszy.
  #   Komunikaty bez znakow diakrytycznych, stop(..., call. = FALSE).
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.data.frame(dane)) {
    stop("Argument `dane` musi byc ramka danych.", call. = FALSE)
  }
  wymagane <- c("zabieg", "wynik", "zaklocenie", "brak")
  brakujace <- setdiff(wymagane, names(dane))
  if (length(brakujace)) {
    stop("Brak kolumn: ", paste(brakujace, collapse = ", "), ".",
         call. = FALSE)
  }

  typ <- if (is.null(wagi)) "proba" else "tablica"
  if (is.null(wagi)) {
    wagi <- rep(1, nrow(dane))
  }
  if (is.null(poziomy)) {
    poziomy <- sort(unique(dane$zaklocenie[!is.na(dane$zaklocenie)]))
  }
  k <- length(poziomy)

  e <- factor(dane$zabieg, levels = 0:1)
  d <- factor(dane$wynik, levels = 0:1)
  u <- factor(match(dane$zaklocenie, poziomy), levels = seq_len(k))
  obserwowany <- dane$brak == 0

  suma_wag <- sum(wagi)
  zero_dla_na <- function(x) {
    x[is.na(x)] <- 0
    x
  }
  obserwowane <- zero_dla_na(tapply(
    wagi[obserwowany],
    list(d[obserwowany], e[obserwowany], u[obserwowany]),
    sum
  )) / suma_wag
  braki <- zero_dla_na(tapply(
    wagi[!obserwowany],
    list(d[!obserwowany], e[!obserwowany]),
    sum
  )) / suma_wag
  dimnames(obserwowane) <- list(wynik = c("0", "1"), zabieg = c("0", "1"),
                                zaklocenie = as.character(poziomy))
  dimnames(braki) <- list(wynik = c("0", "1"), zabieg = c("0", "1"))

  wsparcie <- apply(obserwowane, c(2, 3), sum) > 0

  komorki <- expand.grid(zaklocenie = poziomy, zabieg = 0:1,
                         stringsAsFactors = FALSE)
  komorki <- data.frame(zabieg = komorki$zabieg,
                        zaklocenie = komorki$zaklocenie, brak = 0L)
  wiersz_u <- match(komorki$zaklocenie, poziomy)
  komorki$waga <- vapply(seq_len(nrow(komorki)), function(i) {
    sum(obserwowane[, komorki$zabieg[i] + 1, wiersz_u[i]])
  }, numeric(1)) * suma_wag
  komorki$waga_wynik_1 <- vapply(seq_len(nrow(komorki)), function(i) {
    obserwowane[2, komorki$zabieg[i] + 1, wiersz_u[i]]
  }, numeric(1)) * suma_wag
  w_brakach <- data.frame(
    zabieg = 0:1,
    zaklocenie = rep(poziomy[NA_integer_], 2),
    brak = 1L,
    waga = colSums(braki) * suma_wag,
    waga_wynik_1 = braki[2, ] * suma_wag
  )
  liczebnosci <- rbind(komorki, w_brakach)
  rownames(liczebnosci) <- NULL

  structure(
    list(
      obserwowane = obserwowane,
      braki = braki,
      poziomy = poziomy,
      wsparcie = wsparcie,
      liczebnosci = liczebnosci,
      typ = typ,
      suma_wag = suma_wag
    ),
    class = "tablice_mnar"
  )
}


# Skladniki wzorow dla obu ramion zabiegu, z oznaczeniami artykulu:
#   r_eu = P(D = 1 | E = e, U = u, R = 0), m_e = min_u r_eu, M_e = max_u r_eu,
#   c_e = sum_u r_eu P(U = u | E = 1 - e, R = 0),
#   a_e = P(D = 1, E = e), b_e = P(R = 0, E = 1 - e), h_e = P(R = 1, E = 1 - e),
#   p_e = P(D = 1 | E = e, R = 1) (ryzyko obserwowane w brakach).
skladniki_mnar <- function(tab) {
  o <- tab$obserwowane
  b <- tab$braki
  wiersze <- lapply(0:1, function(e) {
    przeciwne <- 1 - e
    masa_eu <- apply(o[, e + 1, , drop = FALSE], 3, sum)
    r <- o[2, e + 1, ] / masa_eu
    masa_przeciwne <- apply(o[, przeciwne + 1, , drop = FALSE], 3, sum)
    rozklad_przeciwne <- masa_przeciwne / sum(masa_przeciwne)
    braki_e <- sum(b[, e + 1])
    list(
      r = r,
      wiersz = data.frame(
        zabieg = e,
        a = sum(o[2, e + 1, ]) + b[2, e + 1],
        b = sum(masa_przeciwne),
        h = sum(b[, przeciwne + 1]),
        c = sum(r * rozklad_przeciwne),
        m = min(r),
        M = max(r),
        suma_r = sum(r),
        ryzyko_w_brakach = if (braki_e > 0) b[2, e + 1] / braki_e else NA_real_
      )
    )
  })
  wynik <- do.call(rbind, lapply(wiersze, `[[`, "wiersz"))
  rownames(wynik) <- NULL
  r <- rbind(wiersze[[1]]$r, wiersze[[2]]$r)
  dimnames(r) <- list(zabieg = c("0", "1"),
                      zaklocenie = as.character(tab$poziomy))
  list(tabela = wynik, r = r, k = length(tab$poziomy))
}


# Dane wejsciowe funkcji obliczeniowych: obiekt `tablice_mnar` albo ramka
# danych jednostkowych.
jako_tablice <- function(dane) {
  if (inherits(dane, "tablice_mnar")) dane else przygotuj_mnar(dane)
}
