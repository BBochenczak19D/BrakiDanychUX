test_that("parametry niedopuszczajace rozkladu o sumie 1 zatrzymuja obliczenie", {
  # Dla K = 3 poziomow musi zachodzic alfa <= 1/3 <= beta.
  expect_error(wrazliwosc_mnar(przyklad_mnar, c(0.4, 0), c(1, 1)),
               "nie dopuszczaja rozkladu o sumie 1")
  expect_error(wrazliwosc_mnar(przyklad_mnar, c(0, 0), c(0.3, 1)),
               "nie dopuszczaja rozkladu o sumie 1")
  # Dla K = 2 wartosc alfa = 0,6 jest niewykonalna
  tk <- tablica_kontrolna()
  expect_error(wrazliwosc_mnar(przygotuj_mnar(tk, wagi = tk$p),
                               c(0.6, 0), c(1, 1)),
               "nie dopuszczaja rozkladu o sumie 1")
})

test_that("warunek artykulu odrzuca parametry niezgodne z ryzykiem w brakach", {
  # Warunek artykulu: alfa_e <= min(1, p_e / sum_u r_eu) <= beta_e, gdzie
  # p_e = P(D = 1 | E = e, R = 1). Warunek sumy (alfa <= 1/K <= beta)
  # go nie zastepuje.
  g <- generuj_mnar()
  tab <- przygotuj_mnar(g$tablica, wagi = g$tablica$p)
  w <- granice_mnar(tab)
  iloraz <- w$ryzyka$ryzyko_w_brakach / w$ryzyka$suma_r
  # Ramie 1: iloraz okolo 0,3575; beta_1 = 0,34 spelnia warunek sumy
  # (0,34 >= 1/3), ale lamie warunek artykulu.
  expect_gt(iloraz[2], 0.34)
  expect_error(wrazliwosc_mnar(tab, c(0, 0), c(1, 0.34)),
               "warunek artykulu")
})

test_that("raport wykonalnosci zawiera dokladny zakres ryzyka w brakach", {
  g <- generuj_mnar()
  tab <- przygotuj_mnar(g$tablica, wagi = g$tablica$p)
  prawdziwe_alfa <- apply(g$rozklad_w_brakach, 1, min)
  prawdziwe_beta <- apply(g$rozklad_w_brakach, 1, max)
  w <- wrazliwosc_mnar(tab, prawdziwe_alfa, prawdziwe_beta)
  wyk <- w$parametry$wykonalnosc
  expect_true(all(wyk$warunek_sumy))
  expect_true(all(wyk$warunek_artykulu))
  expect_true(all(wyk$warunek_dokladny))
  expect_true(all(wyk$zakres_dolny <= wyk$ryzyko_w_brakach + 1e-12))
  expect_identical(w$status, "wrazliwosc")
})

test_that("prawdziwe parametry wrazliwosci daja granice zawierajace prawde", {
  g <- generuj_mnar()
  tab <- przygotuj_mnar(g$tablica, wagi = g$tablica$p)
  w <- wrazliwosc_mnar(tab, apply(g$rozklad_w_brakach, 1, min),
                       apply(g$rozklad_w_brakach, 1, max))
  prawda <- g$prawda[c("ryzyko0", "ryzyko1")]
  s <- w$wrazliwosc
  expect_true(all(s$dolna_przeciecie <= prawda + 1e-12))
  expect_true(all(prawda <= s$gorna_przeciecie + 1e-12))
  # Ramie 1 zostaje zawezone wzgledem granic podstawowych
  expect_gt(s$dolna_przeciecie[2], s$dolna_podstawowa[2])
  expect_lt(s$gorna_przeciecie[2], s$gorna_podstawowa[2])
})

test_that("siatka oznacza warianty niewykonalne i zawezajace", {
  siatka <- expand.grid(alfa0 = c(0, 0.3), alfa1 = c(0, 0.3),
                        beta0 = c(0.4, 1), beta1 = c(0.3, 1))
  w <- siatka_wrazliwosci(przyklad_mnar, siatka)
  expect_equal(nrow(w$wyniki), 16)
  # beta1 = 0,3 < 1/3 jest niewykonalne dla K = 3
  expect_true(all(!w$wyniki$wykonalna[w$wyniki$beta1 == 0.3]))
  expect_true(all(w$wyniki$wykonalna[w$wyniki$beta1 == 1]))
  expect_true(any(w$wyniki$zaweza, na.rm = TRUE))
  expect_true(all(w$wyniki$przyczyna[!w$wyniki$wykonalna] != ""))
})

test_that("dla K = 2 rozklad U w brakach jest identyfikowalny z danych", {
  # Przy dwoch poziomach P(D = 1 | E = e, R = 1) = r_e0 q_0 + r_e1 (1 - q_0),
  # wiec q_0 = (r_e1 - p_e) / (r_e1 - r_e0). Model z briefu (U binarne).
  odwrotny <- stats::plogis
  u <- 0:1
  p_u <- c(0.6, 0.4)
  tab <- tablica_z_modelu(
    p_u = p_u,
    p_e1 = odwrotny(-0.5 + u),
    p_d1 = rbind(odwrotny(-1 + 0.8 * u), odwrotny(-1 + 0.7 + 0.8 * u)),
    p_r1 = rbind(odwrotny(-1 + 1.2 * u), odwrotny(-1 + 0.5 + 1.2 * u))
  )
  w <- granice_mnar(przygotuj_mnar(tab, wagi = tab$p))
  r <- w$parametry$r
  p <- w$ryzyka$ryzyko_w_brakach
  q0 <- (r[, 2] - p) / (r[, 2] - r[, 1])
  # prawdziwe P(U = 0 | E = e, R = 1) z modelu
  pe1 <- odwrotny(-0.5 + u)
  pr1 <- rbind(odwrotny(-1 + 1.2 * u), odwrotny(-1 + 0.5 + 1.2 * u))
  prawdziwe <- sapply(0:1, function(e) {
    masa <- p_u * (if (e == 1) pe1 else 1 - pe1) * pr1[e + 1, ]
    masa[1] / sum(masa)
  })
  expect_equal(unname(q0), prawdziwe, tolerance = 1e-12)
  expect_equal(prawdziwe[1], 0.547442, tolerance = 1e-6)
})
