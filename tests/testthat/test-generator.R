# Procedura generowania danych o znanym efekcie przyczynowym.

test_that("generator jest powtarzalny i nie zmienia stanu generatora", {
  set.seed(1)
  przed <- .Random.seed
  a <- generuj_mnar(n = 300)
  expect_identical(.Random.seed, przed)
  b <- generuj_mnar(n = 300)
  expect_identical(a, b)
})

test_that("dokladna tablica sumuje sie do 1 i odtwarza prawde", {
  g <- generuj_mnar()
  expect_equal(sum(g$tablica$p), 1)
  # P(D_0 = 1) = sum_u logit^-1(-1 + 0,6 u) P(u)
  p_u <- c(0.4, 0.35, 0.25)
  r0 <- sum(stats::plogis(-1 + 0.6 * 0:2) * p_u)
  r1 <- sum(stats::plogis(-0.3 + 0.6 * 0:2) * p_u)
  expect_equal(unname(g$prawda[c("ryzyko0", "ryzyko1")]), c(r0, r1))
  expect_equal(unname(g$prawda["RD"]), r1 - r0)
})

test_that("granice podstawowe z dokladnej tablicy zawieraja prawde", {
  g <- generuj_mnar()
  w <- granice_mnar(przygotuj_mnar(g$tablica, wagi = g$tablica$p))
  prawda <- g$prawda[c("ryzyko0", "ryzyko1")]
  expect_true(all(w$ryzyka$dolna <= prawda))
  expect_true(all(prawda <= w$ryzyka$gorna))
  expect_true(w$kontrasty$dolna <= g$prawda[["RD"]])
  expect_true(g$prawda[["RD"]] <= w$kontrasty$gorna)
})

test_that("braki sa nielosowe: rozklad U w brakach rozni sie od obserwowanego", {
  g <- generuj_mnar()
  tab <- przygotuj_mnar(g$tablica, wagi = g$tablica$p)
  obserwowany <- apply(tab$obserwowane, c(2, 3), sum)
  obserwowany <- obserwowany / rowSums(obserwowany)
  expect_gt(max(abs(obserwowany - g$rozklad_w_brakach)), 0.1)
})

test_that("czestosci z duzej proby zbiegaja do dokladnej tablicy", {
  g <- generuj_mnar(n = 50000)
  z_proby <- granice_mnar(g$dane)$ryzyka
  dokladne <- granice_mnar(przygotuj_mnar(g$tablica, wagi = g$tablica$p))$ryzyka
  expect_equal(z_proby$dolna, dokladne$dolna, tolerance = 0.03)
  expect_equal(z_proby$gorna, dokladne$gorna, tolerance = 0.03)
})

test_that("wariant z naruszeniem zmienia mechanizm brakow, nie prawde", {
  a <- generuj_mnar()
  b <- generuj_mnar(naruszenie = 1)
  expect_equal(a$prawda, b$prawda)
  expect_false(isTRUE(all.equal(a$tablica$p, b$tablica$p)))
})
