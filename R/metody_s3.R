#' Metody dla granic przy brakach nielosowych
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{granice_mnar}.
#' Wykres pokazuje granice ryzyka po interwencji dla obu wartosci zabiegu:
#' granice podstawowe jako szeroki odcinek, a przy analizie wrazliwosci takze
#' przeciecie z granicami z rownania (6).
#'
#' @param x,object Obiekt klasy \code{granice_mnar} albo
#'   \code{summary.granice_mnar}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca obiekt klasy \code{summary.granice_mnar} z tabela granic,
#'   skladnikami wzorow i kontrolami wykonalnosci. \code{plot()}
#'   niewidocznie zwraca \code{x}.
#'
#' @examples
#' wynik <- wrazliwosc_mnar(przyklad_mnar, alfa = c(0.2, 0.2),
#'                          beta = c(0.5, 0.5))
#' print(wynik)
#' summary(wynik)
#' plot(wynik)
#'
#' @name metody_granice_mnar
NULL

#' @rdname metody_granice_mnar
#' @export
print.granice_mnar <- function(x, cyfry = 4, ...) {
  cat("Granice efektu przy brakach nielosowych (", x$status, ")\n\n", sep = "")
  granice <- if (is.null(x$wrazliwosc)) x$ryzyka[c("dolna", "gorna")] else
    data.frame(dolna = x$wrazliwosc$dolna_przeciecie,
               gorna = x$wrazliwosc$gorna_przeciecie)
  tabela <- data.frame(
    wielkosc = c("P(D_0 = 1)", "P(D_1 = 1)", x$kontrasty$kontrast),
    dolna = signif(c(granice$dolna, x$kontrasty$dolna), cyfry),
    gorna = signif(c(granice$gorna, x$kontrasty$gorna), cyfry)
  )
  print(tabela, row.names = FALSE)
  if (x$kontrasty$status != "zdefiniowany") {
    cat("\nStatus kontrastu:", x$kontrasty$status, "\n")
  }
  invisible(x)
}

#' @rdname metody_granice_mnar
#' @export
summary.granice_mnar <- function(object, ...) {
  structure(
    list(
      ryzyka = object$ryzyka,
      wrazliwosc = object$wrazliwosc,
      kontrasty = object$kontrasty,
      wykonalnosc = object$parametry$wykonalnosc,
      r = object$parametry$r,
      liczebnosci = object$liczebnosci,
      status = object$status
    ),
    class = "summary.granice_mnar"
  )
}

#' @rdname metody_granice_mnar
#' @export
print.summary.granice_mnar <- function(x, cyfry = 4, ...) {
  zaokraglij <- function(d) {
    liczbowe <- vapply(d, is.double, logical(1))
    d[liczbowe] <- lapply(d[liczbowe], signif, digits = cyfry)
    d
  }
  cat("Status:", x$status, "\n\n")
  cat("Granice podstawowe i skladniki wzoru (3):\n")
  print(zaokraglij(x$ryzyka), row.names = FALSE)
  cat("\nRyzyka r_eu = P(D = 1 | E = e, U = u, R = 0):\n")
  print(signif(x$r, cyfry))
  if (!is.null(x$wrazliwosc)) {
    cat("\nAnaliza wrazliwosci, rownanie (6) i przeciecie:\n")
    print(zaokraglij(x$wrazliwosc), row.names = FALSE)
    cat("\nKontrole wykonalnosci parametrow:\n")
    print(zaokraglij(x$wykonalnosc), row.names = FALSE)
  }
  cat("\nKontrast:\n")
  print(zaokraglij(x$kontrasty), row.names = FALSE)
  invisible(x)
}

#' @rdname metody_granice_mnar
#' @export
plot.granice_mnar <- function(x, ...) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0.5, 2.5))
  graphics::axis(1)
  graphics::axis(2, at = 1:2, labels = c("E = 0", "E = 1"), las = 1,
                 tick = FALSE)
  graphics::title(main = "Granice ryzyka po interwencji",
                  xlab = "P(D_e = 1)")
  graphics::segments(x$ryzyka$dolna, 1:2, x$ryzyka$gorna, 1:2,
                     lwd = 12, col = "grey80", lend = 1)
  legenda <- "granice podstawowe"
  kolory <- "grey80"
  if (!is.null(x$wrazliwosc)) {
    graphics::segments(x$wrazliwosc$dolna_przeciecie, 1:2,
                       x$wrazliwosc$gorna_przeciecie, 1:2,
                       lwd = 5, col = "#2E5E9E", lend = 1)
    legenda <- c(legenda, "po analizie wrazliwosci")
    kolory <- c(kolory, "#2E5E9E")
  }
  graphics::legend("topleft", legend = legenda, col = kolory, lwd = c(12, 5),
                   bty = "n")
  invisible(x)
}


#' Metody dla siatki wariantow wrazliwosci
#'
#' Wypisanie i wykres obiektu klasy \code{siatka_mnar}. Wykres pokazuje
#' granice kontrastu dla kazdego wykonalnego wariantu na tle granic
#' podstawowych.
#'
#' @param x Obiekt klasy \code{siatka_mnar}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return \code{print()} i \code{plot()} niewidocznie zwracaja \code{x}.
#'
#' @examples
#' siatka <- expand.grid(alfa0 = c(0, 0.2), alfa1 = c(0, 0.2),
#'                       beta0 = c(0.5, 1), beta1 = c(0.5, 1))
#' wynik <- siatka_wrazliwosci(przyklad_mnar, siatka)
#' print(wynik)
#' plot(wynik)
#'
#' @name metody_siatka_mnar
NULL

#' @rdname metody_siatka_mnar
#' @export
print.siatka_mnar <- function(x, cyfry = 4, ...) {
  w <- x$wyniki
  cat(sprintf("Siatka wrazliwosci: %d wariantow, wykonalnych %d, zawezajacych %d\n\n",
              nrow(w), sum(w$wykonalna), sum(w$zaweza, na.rm = TRUE)))
  pokaz <- data.frame(
    nazwa = w$nazwa, alfa0 = w$alfa0, alfa1 = w$alfa1, beta0 = w$beta0,
    beta1 = w$beta1, wykonalna = w$wykonalna,
    dolna = signif(w$kontrast_dolna, cyfry),
    gorna = signif(w$kontrast_gorna, cyfry),
    zaweza = w$zaweza
  )
  cat(sprintf("Granice kontrastu %s po przecieciu:
",
              x$podstawowe$kontrasty$kontrast))
  print(pokaz, row.names = FALSE)
  invisible(x)
}

#' @rdname metody_siatka_mnar
#' @export
plot.siatka_mnar <- function(x, ...) {
  w <- x$wyniki[x$wyniki$wykonalna, , drop = FALSE]
  podstawowy <- x$podstawowe$kontrasty
  zakres <- range(c(podstawowy$dolna, podstawowy$gorna, w$kontrast_dolna,
                    w$kontrast_gorna), finite = TRUE)
  pozycje <- seq_len(nrow(w))
  graphics::plot.new()
  graphics::plot.window(xlim = zakres, ylim = c(0.5, max(1, nrow(w)) + 0.5))
  graphics::rect(podstawowy$dolna, 0.5, podstawowy$gorna, nrow(w) + 0.5,
                 col = "grey90", border = NA)
  graphics::segments(w$kontrast_dolna, pozycje, w$kontrast_gorna, pozycje,
                     lwd = 3, col = ifelse(w$zaweza, "#2E5E9E", "grey50"))
  graphics::axis(1)
  graphics::axis(2, at = pozycje, labels = w$nazwa, las = 1, cex.axis = 0.6,
                 tick = FALSE)
  graphics::title(main = "Granice kontrastu w wariantach wrazliwosci",
                  xlab = podstawowy$kontrast)
  invisible(x)
}


#' Wypisanie tablic prawdopodobienstw
#'
#' @param x Obiekt klasy \code{tablice_mnar}.
#' @param ... Argumenty przekazywane dalej, obecnie nieuzywane.
#'
#' @return Niewidocznie zwraca \code{x}.
#'
#' @examples
#' przygotuj_mnar(przyklad_mnar)
#'
#' @export
print.tablice_mnar <- function(x, ...) {
  cat(sprintf("Tablice prawdopodobienstw (%s), poziomy czynnika: %s\n",
              x$typ, paste(x$poziomy, collapse = ", ")))
  cat(sprintf("Masa obserwowanych U: %.4f, masa brakow: %.4f\n",
              sum(x$obserwowane), sum(x$braki)))
  cat("Wsparcie komorek zabieg x zaklocenie:\n")
  print(x$wsparcie)
  invisible(x)
}
