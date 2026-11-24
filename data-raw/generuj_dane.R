# Zbior przykladowy pakietu.
#
# Skrypt nie wchodzi w sklad zbudowanego pakietu; zapisuje zbior do katalogu
# data/. Uruchomienie z katalogu glownego pakietu:
#     Rscript -e "devtools::load_all(); source('data-raw/generuj_dane.R')"
#
# Zbior powstaje ta sama procedura co dane eksperymentu (generuj_mnar),
# w mniejszej skali: 2000 jednostek, ziarno 202706. Prawdziwe ryzyka po
# interwencji wynikaja z parametrow modelu, wiec dla kazdego wyniku granic
# wiadomo, czy zawiera prawde.

g <- generuj_mnar(n = 2000, ziarno = 202706)
przyklad_mnar <- g$dane

stopifnot(
  nrow(przyklad_mnar) == 2000,
  all(przyklad_mnar$zabieg %in% 0:1),
  all(przyklad_mnar$wynik %in% 0:1),
  all(is.na(przyklad_mnar$zaklocenie) == (przyklad_mnar$brak == 1)),
  all(table(przyklad_mnar$zabieg[przyklad_mnar$brak == 0],
            przyklad_mnar$zaklocenie[przyklad_mnar$brak == 0]) > 0)
)

dir.create("data", showWarnings = FALSE)
save(przyklad_mnar, file = "data/przyklad_mnar.rda",
     compress = "xz", version = 3)

# TODO: osobny skrypt pobrania i przygotowania danych empirycznych (zbior
#   Adult z UCI albo dane z badania ankietowego UX): zapis zrodla, wersji,
#   licencji i daty pobrania, regula laczenia rzadkich kategorii czynnika
#   zaklocajacego ustalona przed analiza, kontrola wsparcia w obu ramionach;
#   pobranie poza funkcjami pakietu i testami.
# TODO: skrypt eksperymentu symulacyjnego: n = 500, 2000, 5000 w 500
#   powtorzeniach, osobno wariant z naruszeniem zalozenia (naruszenie != 0),
#   z zapisem konfiguracji, ziaren i tabeli wynikow.
