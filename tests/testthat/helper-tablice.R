# Dokladna tablica z przykladu kontrolnego briefu: dla kazdego e masa
# P(E = e, R = 0) = 0,3 oraz P(E = e, R = 1) = 0,2, rownomierny rozklad U
# (dwa poziomy) w obu czesciach, ryzyka P(D = 1 | E = e, U = u):
# e = 0: (0,1; 0,3), e = 1: (0,4; 0,6). Brak nie zalezy od D przy danych E, U.
tablica_kontrolna <- function() {
  ryzyko <- rbind(c(0.1, 0.3), c(0.4, 0.6))
  obs <- expand.grid(zaklocenie = 0:1, wynik = 0:1, zabieg = 0:1)
  r_obs <- ryzyko[cbind(obs$zabieg + 1, obs$zaklocenie + 1)]
  obs$p <- 0.3 * 0.5 * ifelse(obs$wynik == 1, r_obs, 1 - r_obs)
  obs$brak <- 0L
  bra <- expand.grid(wynik = 0:1, zabieg = 0:1)
  r_bra <- rowMeans(ryzyko)[bra$zabieg + 1]
  bra$p <- 0.2 * ifelse(bra$wynik == 1, r_bra, 1 - r_bra)
  bra$zaklocenie <- NA_integer_
  bra$brak <- 1L
  kolumny <- c("zabieg", "wynik", "zaklocenie", "brak", "p")
  rbind(obs[kolumny], bra[kolumny])
}

# Dokladna tablica obserwowanych prawdopodobienstw z dowolnego modelu
# z dyskretnym U: p_u, P(E = 1 | u), P(D = 1 | e, u) (macierz 2 x K)
# i P(R = 1 | e, u) (macierz 2 x K).
tablica_z_modelu <- function(p_u, p_e1, p_d1, p_r1) {
  k <- length(p_u)
  s <- expand.grid(u = seq_len(k), e = 0:1, d = 0:1, r = 0:1)
  pe <- ifelse(s$e == 1, p_e1[s$u], 1 - p_e1[s$u])
  pd <- ifelse(s$d == 1, p_d1[cbind(s$e + 1, s$u)], 1 - p_d1[cbind(s$e + 1, s$u)])
  pr <- ifelse(s$r == 1, p_r1[cbind(s$e + 1, s$u)], 1 - p_r1[cbind(s$e + 1, s$u)])
  s$p <- p_u[s$u] * pe * pd * pr
  obs <- s[s$r == 0, ]
  bra <- stats::aggregate(p ~ e + d, data = s[s$r == 1, ], FUN = sum)
  rbind(
    data.frame(zabieg = obs$e, wynik = obs$d, zaklocenie = obs$u - 1L,
               brak = 0L, p = obs$p),
    data.frame(zabieg = bra$e, wynik = bra$d, zaklocenie = NA_integer_,
               brak = 1L, p = bra$p)
  )
}
