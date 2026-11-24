# Warstwa trzecia: dane, przy ktorych metoda nie ma sensu.
#
# Testy opisuja kontrakt bledow z briefu tematu. Oczekiwanym wynikiem jest
# zatrzymanie wykonania z komunikatem, a nie liczba. Walidacja wejscia nie
# jest jeszcze zaimplementowana, wiec testy sa pomijane; po implementacji
# kontroli w R/tablice.R (przygotuj_mnar) i w R/wrazliwosc.R
# (jako_parametr_ramion, siatka_wrazliwosci) nalezy usunac wywolania pomin().

pomin <- function() {
  skip("Walidacja wejscia do implementacji (kontrakt bledow, SPEC.md).")
}

test_that("zabieg albo wynik spoza {0, 1} lub z brakiem zatrzymuje wykonanie", {
  pomin()
  d <- przyklad_mnar
  d$zabieg[1] <- 2
  expect_error(przygotuj_mnar(d), "Zabieg i wynik musza byc binarne i kompletne")
  d <- przyklad_mnar
  d$wynik[1] <- NA
  expect_error(przygotuj_mnar(d), "Zabieg i wynik musza byc binarne i kompletne")
})

test_that("wskaznik braku niezgodny z zakloceniem zatrzymuje wykonanie", {
  pomin()
  d <- przyklad_mnar
  i <- which(d$brak == 0)[1]
  d$brak[i] <- 1L
  expect_error(przygotuj_mnar(d), "Wskaznik braku musi byc zgodny z zakloceniem")
  d <- przyklad_mnar
  i <- which(d$brak == 1)[1]
  d$zaklocenie[i] <- 1
  expect_error(przygotuj_mnar(d), "Wskaznik braku musi byc zgodny z zakloceniem")
})

test_that("brak obserwowanego wsparcia zatrzymuje wykonanie", {
  pomin()
  d <- przyklad_mnar
  usun <- d$brak == 0 & d$zabieg == 1 & d$zaklocenie %in% 2
  d <- d[!usun, ]
  expect_error(przygotuj_mnar(d),
               "Brak obserwowanego wsparcia w komorce zabieg-zaklocenie")
})

test_that("calkowicie nieobserwowany czynnik zatrzymuje wykonanie", {
  pomin()
  d <- przyklad_mnar
  d$zaklocenie <- NA
  d$brak <- 1L
  expect_error(przygotuj_mnar(d),
               "Brak obserwowanego wsparcia w komorce zabieg-zaklocenie")
})

test_that("niepoprawne parametry wrazliwosci zatrzymuja wykonanie", {
  pomin()
  expect_error(wrazliwosc_mnar(przyklad_mnar, c(-0.1, 0), c(1, 1)), "alfa")
  expect_error(wrazliwosc_mnar(przyklad_mnar, c(0, 0, 0), c(1, 1)), "alfa")
  expect_error(wrazliwosc_mnar(przyklad_mnar, c(0, 0), c(1.2, 1)), "beta")
  expect_error(siatka_wrazliwosci(przyklad_mnar, data.frame(alfa0 = 0)),
               "beta0")
})

test_that("niepoprawne wagi zatrzymuja wykonanie", {
  pomin()
  expect_error(przygotuj_mnar(przyklad_mnar, wagi = -1), "wagi")
  expect_error(przygotuj_mnar(przyklad_mnar,
                              wagi = rep(-1, nrow(przyklad_mnar))), "wagi")
})
