# Warstwa pierwsza: przyklad kontrolny briefu policzony recznie.
#
# Dla e = 0: a_0 = P(D = 1, E = 0) = 0,3 * (0,1 + 0,3) / 2 + 0,2 * 0,2
#            = 0,06 + 0,04 = 0,10,
#   r_0 = (0,1; 0,3), m_0 = 0,1, M_0 = 0,3,
#   c_0 = sum_u r_0u P(U = u | E = 1, R = 0) = 0,5 * 0,1 + 0,5 * 0,3 = 0,2,
#   b_0 = P(R = 0, E = 1) = 0,3, h_0 = P(R = 1, E = 1) = 0,2,
#   L_0 = 0,10 + 0,2 * 0,3 + 0,1 * 0,2 = 0,18,
#   U_0 = 0,10 + 0,06 + 0,3 * 0,2 = 0,22.
# Dla e = 1: a_1 = 0,3 * 0,5 + 0,2 * 0,5 = 0,25, c_1 = 0,5,
#   L_1 = 0,25 + 0,5 * 0,3 + 0,4 * 0,2 = 0,48,
#   U_1 = 0,25 + 0,15 + 0,6 * 0,2 = 0,52.
# RD = [0,48 - 0,22; 0,52 - 0,18] = [0,26; 0,34],
# RR = [0,48 / 0,22; 0,52 / 0,18] = [24/11; 26/9].

test_that("granice podstawowe ryzyk w przykladzie kontrolnym", {
  tk <- tablica_kontrolna()
  w <- granice_mnar(przygotuj_mnar(tk, wagi = tk$p))
  expect_equal(w$ryzyka$a, c(0.10, 0.25))
  expect_equal(w$ryzyka$c, c(0.2, 0.5))
  expect_equal(w$ryzyka$dolna, c(0.18, 0.48))
  expect_equal(w$ryzyka$gorna, c(0.22, 0.52))
})

test_that("granice roznicy i ilorazu ryzyk w przykladzie kontrolnym", {
  tk <- tablica_kontrolna()
  tab <- przygotuj_mnar(tk, wagi = tk$p)
  rd <- granice_mnar(tab, kontrast = "roznica")$kontrasty
  expect_equal(c(rd$dolna, rd$gorna), c(0.26, 0.34))
  rr <- granice_mnar(tab, kontrast = "iloraz")$kontrasty
  expect_equal(c(rr$dolna, rr$gorna), c(24 / 11, 26 / 9))
  expect_identical(rr$status, "zdefiniowany")
})

test_that("alfa = beta = 0,5 daje punktowa identyfikacje ryzyk", {
  # Rozklad U w brakach jest wtedy znany: (0,5; 0,5).
  # Dla e = 0: 0,10 + 0,06 + 0,5 * 0,2 * (0,1 + 0,3) = 0,20,
  # dla e = 1: 0,25 + 0,15 + 0,5 * 0,2 * (0,4 + 0,6) = 0,50.
  # RD = 0,3, RR = 2,5.
  tk <- tablica_kontrolna()
  tab <- przygotuj_mnar(tk, wagi = tk$p)
  polowa <- c(`0` = 0.5, `1` = 0.5)
  w <- wrazliwosc_mnar(tab, polowa, polowa)
  expect_equal(w$wrazliwosc$dolna_przeciecie, c(0.2, 0.5))
  expect_equal(w$wrazliwosc$gorna_przeciecie, c(0.2, 0.5))
  expect_equal(c(w$kontrasty$dolna, w$kontrasty$gorna), c(0.3, 0.3))
  rr <- wrazliwosc_mnar(tab, polowa, polowa, kontrast = "iloraz")$kontrasty
  expect_equal(c(rr$dolna, rr$gorna), c(2.5, 2.5))
})

test_that("ryzyko w brakach i ryzyka r_eu sa zgodne z tablica", {
  # P(D = 1 | E = 0, R = 1) = 0,04 / 0,2 = 0,2; P(D = 1 | E = 1, R = 1) = 0,5
  tk <- tablica_kontrolna()
  w <- granice_mnar(przygotuj_mnar(tk, wagi = tk$p))
  expect_equal(w$ryzyka$ryzyko_w_brakach, c(0.2, 0.5))
  expect_equal(unname(w$parametry$r), rbind(c(0.1, 0.3), c(0.4, 0.6)))
})

test_that("reguly zer dla ilorazu ryzyk", {
  rr <- function(l0, u0, l1, u1) {
    BrakiDanychUX:::przedzial_kontrastu(l0, u0, l1, u1, "iloraz")
  }
  # L0 = U0 = 0 i L1 > 0: iloraz nieograniczony z obu stron
  w <- rr(0, 0, 0.2, 0.4)
  expect_equal(c(w$dolna, w$gorna), c(Inf, Inf))
  expect_identical(w$status, "iloraz_nieograniczony")
  # oba ryzyka zerowe: iloraz niezdefiniowany, bez NaN
  w <- rr(0, 0, 0, 0)
  expect_true(all(is.na(c(w$dolna, w$gorna))))
  expect_false(any(is.nan(c(w$dolna, w$gorna))))
  expect_identical(w$status, "iloraz_niezdefiniowany")
  # L1 = 0 przy dodatnim U0: dolna granica 0
  w <- rr(0.1, 0.2, 0, 0.3)
  expect_equal(c(w$dolna, w$gorna), c(0, 3))
})

test_that("brak ryzyka w ramieniu odniesienia daje iloraz nieograniczony", {
  # W ramieniu E = 0 nikt nie ma wyniku 1, wiec a_0 = 0, r_0u = 0, L_0 = U_0 = 0.
  tab <- tablica_z_modelu(
    p_u = c(0.5, 0.3, 0.2), p_e1 = c(0.4, 0.5, 0.6),
    p_d1 = rbind(c(0, 0, 0), c(0.3, 0.5, 0.7)),
    p_r1 = rbind(c(0.1, 0.3, 0.5), c(0.2, 0.4, 0.6))
  )
  w <- granice_mnar(przygotuj_mnar(tab, wagi = tab$p), kontrast = "iloraz")
  expect_equal(w$ryzyka$dolna[1], 0)
  expect_equal(w$ryzyka$gorna[1], 0)
  expect_equal(c(w$kontrasty$dolna, w$kontrasty$gorna), c(Inf, Inf))
})
