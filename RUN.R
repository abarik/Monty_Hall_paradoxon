# Abari Kálmán
# 2025.08.22
# Monty Hall probléma szimulációja R-ben

# Beállítások ------------------------------------------------------------
# A vizsgálat hány elemű mintán alapuljon?
n <- 1000
# Mi van az ajtók mögött?
ajtok <- c("autó", "kecske", "kecske")
# Mit szeretnénk nyerni?
fodij <- "autó"


# Szimuláció --------------------------------------------------------------
# 1. Létrehozunk n darab esetet, ez lesz a kiindulópontunk 
#    Mindegyik esetben 3 ajtó van, az egyik mögött a fődíj, a másik kettő mögött nincs fődíj
esetek <- lapply(1:n, \(x) sample(x = ajtok, size = 3, replace = F))

# 2. Mindegyik esetben választunk egy ajtót
valaszt.NV <- sample(x = 1:3, size = n, replace = T)

# 3. Megnézzük, hogy az esetek hány százalékában nyertünk, ha 
#    nem változtatunk (NV) a döntésünkön (ennek kb 1/3-nak kell lennie)
nyert.NV <- 0
for (i in 1:n) {
  if (fodij == esetek[[i]][valaszt.NV[i]]) {
    nyert.NV <- nyert.NV + 1
  }
}

# 4. Megnézzük, hogy az esetek hány százalékában nyertünk, ha
#    változtatunk a döntésünkön azután hogy megmutattak nekünk egy nem-fődíjas ajtót 
#    Ennek nagyobbnak kell lennie 1/3-nál, épp ez a paradoxon lényege
valaszt.V <- integer(n)
for (i in 1:n) {
  # megnézzük, hol van az autó
  auto.i <- which(esetek[[i]] == fodij)
  # kivesszük az autót és az aktuális választását, és feldobjuk az (egyik) kecskét
  mutat <- sample(setdiff(1:3, c(auto.i, valaszt.NV[i])), size = 1)
  # változtatunk
  valaszt.V[i] <- setdiff(1:3, c(valaszt.NV[i], mutat))
}

# megnézzük, az esetek hány százalékában nyertünk
nyert.V <- 0
for (i in 1:n) {
  if (fodij == esetek[[i]][valaszt.V[i]]) {
    nyert.V <- nyert.V + 1
  }
}
# 5. Eredmények kiírása --------------------------------------------------------------
print(c("P (nem változtattunk)"=nyert.NV, "P (változtattunk)"=nyert.V)/n)
