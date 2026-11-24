# Warstwa druga: wlasnosci niezalezne od konkretnych danych.

# Niezalezna, najprostsza wersja wzoru (3) liczona bezposrednio z tablicy
# lacznych prawdopodobienstw, z jawnymi petlami.
granice_wprost <- function(tab) {
  wynik <- matrix(NA_real_, 2, 2)
  for (e in 0:1) {
    o <- 1 - e
    obs <- tab[tab$brak == 0, ]
    bra <- tab[tab$brak == 1, ]
    a <- sum(tab$p[tab$wynik == 1 & tab$zabieg == e])
    poziomy <- sort(unique(obs$zaklocenie))
    r <- numeric(length(poziomy))
    pu_o <- numeric(length(poziomy))
    for (j in seq_along(poziomy)) {
      komorka <- obs$zabieg == e & obs$zaklocenie == poziomy[j]
      r[j] <- sum(obs$p[komorka & obs$wynik == 1]) / sum(obs$p[komorka])
      pu_o[j] <- sum(obs$p[obs$zabieg == o & obs$zaklocenie == poziomy[j]])
    }
    b <- sum(pu_o)
    cc <- sum(r * pu_o / b)
    h <- sum(bra$p[bra$zabieg == o])
    wynik[e + 1, ] <- c(a + cc * b + min(r) * h, a + cc * b + max(r) * h)
  }
  wynik
}

losowy_model <- function(k) {
  list(p_u = prop.table(stats::runif(k, 0.2, 1)),
       p_e1 = stats::runif(k, 0.1, 0.9),
       p_d1 = matrix(stats::runif(2 * k, 0.05, 0.95), 2),
       p_r1 = matrix(stats::runif(2 * k, 0.05, 0.6), 2))
}

test_that("wynik zgadza sie z niezaleznym obliczeniem wzoru (3)", {
  withr::local_seed(1)
  for (powtorzenie in 1:30) {
    m <- losowy_model(sample(2:4, 1))
    tab <- do.call(tablica_z_modelu, m)
    w <- granice_mnar(przygotuj_mnar(tab, wagi = tab$p))
    ref <- granice_wprost(tab)
    expect_equal(w$ryzyka$dolna, ref[, 1], tolerance = 1e-12)
    expect_equal(w$ryzyka$gorna, ref[, 2], tolerance = 1e-12)
  }
})

test_that("granice zawieraja prawdziwe ryzyko, gdy zalozenia sa spelnione", {
  withr::local_seed(2)
  for (powtorzenie in 1:50) {
    m <- losowy_model(sample(2:4, 1))
    tab <- do.call(tablica_z_modelu, m)
    prawda <- c(sum(m$p_d1[1, ] * m$p_u), sum(m$p_d1[2, ] * m$p_u))
    w <- granice_mnar(przygotuj_mnar(tab, wagi = tab$p))
    expect_true(all(w$ryzyka$dolna <= prawda + 1e-12))
    expect_true(all(prawda <= w$ryzyka$gorna + 1e-12))
    expect_true(all(w$ryzyka$dolna >= 0 & w$ryzyka$gorna <= 1))
    rd <- w$kontrasty
    expect_true(rd$dolna >= -1 && rd$gorna <= 1)
  }
})

test_that("przeciecie wrazliwosci miesci sie w granicach podstawowych", {
  withr::local_seed(3)
  for (powtorzenie in 1:50) {
    k <- sample(2:4, 1)
    m <- losowy_model(k)
    tablica <- do.call(tablica_z_modelu, m)
    tab <- przygotuj_mnar(tablica, wagi = tablica$p)
    alfa <- c(`0` = 0, `1` = 0)
    beta <- c(`0` = 1, `1` = 1)
    w <- wrazliwosc_mnar(tab, alfa, beta)
    s <- w$wrazliwosc
    expect_true(all(s$dolna_przeciecie >= s$dolna_podstawowa - 1e-12))
    expect_true(all(s$gorna_przeciecie <= s$gorna_podstawowa + 1e-12))
    expect_true(all(s$dolna_przeciecie <= s$gorna_przeciecie))
  }
})

test_that("wezszy zbior dopuszczalnych rozkladow nie poszerza granic", {
  g <- generuj_mnar()
  tab <- przygotuj_mnar(g$tablica, wagi = g$tablica$p)
  szerokie <- wrazliwosc_mnar(tab, c(0.1, 0.1), c(0.6, 0.6))$wrazliwosc
  waskie <- wrazliwosc_mnar(tab, c(0.25, 0.2), c(0.4, 0.5))$wrazliwosc
  expect_true(all(waskie$dolna_przeciecie >= szerokie$dolna_przeciecie - 1e-12))
  expect_true(all(waskie$gorna_przeciecie <= szerokie$gorna_przeciecie + 1e-12))
})

test_that("wynik nie zalezy od kolejnosci wierszy ani kolejnosci poziomow", {
  withr::local_seed(4)
  a <- granice_mnar(przyklad_mnar)
  b <- granice_mnar(przyklad_mnar[sample(nrow(przyklad_mnar)), ])
  expect_equal(a$ryzyka, b$ryzyka)
  c <- granice_mnar(przygotuj_mnar(przyklad_mnar, poziomy = c(2, 0, 1)))
  expect_equal(a$ryzyka$dolna, c$ryzyka$dolna)
  expect_equal(a$ryzyka$gorna, c$ryzyka$gorna)
})

test_that("kompletny czynnik zaklocajacy daje punktowa identyfikacje", {
  g <- generuj_mnar(n = 1000)
  pelne <- g$pelne
  pelne$brak <- 0L
  w <- granice_mnar(pelne)
  expect_equal(w$ryzyka$dolna, w$ryzyka$gorna)
})
