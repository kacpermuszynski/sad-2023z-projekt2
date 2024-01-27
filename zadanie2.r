# Załaduj potrzebne pakiety
library(nortest) # dla testu Andersona-Darlinga

# Ustal parametry symulacji
n <- 20 # rozmiar próby
alpha <- 0.05 # poziom istotności
simulations <- 10000 # liczba symulacji

# Inicjalizuj liczniki
shapiro_rejections <- 0
anderson_rejections <- 0

# Przeprowadź symulacje dla rozkładu normalnego
for (i in 1:simulations) {
  sample <- rnorm(n)
  if (shapiro.test(sample)$p.value < alpha) {
    shapiro_rejections <- shapiro_rejections + 1
  }
  if (ad.test(sample)$p.value < alpha) {
    anderson_rejections <- anderson_rejections + 1
  }
}

# Wydrukuj wyniki
cat("Shapiro-Wilk test rejected H0", shapiro_rejections, "times out of", simulations, "\n")
cat("Anderson-Darling test rejected H0", anderson_rejections, "times out of", simulations, "\n")

# Przeprowadź symulacje dla rozkładu t-Studenta
df_values <- c(5, 10, 30) # wybrane wartości stopni swobody

for (df in df_values) {
  shapiro_rejections <- 0
  anderson_rejections <- 0
  for (i in 1:simulations) {
    sample <- rt(n, df)
    if (shapiro.test(sample)$p.value < alpha) {
      shapiro_rejections <- shapiro_rejections + 1
    }
    if (ad.test(sample)$p.value < alpha) {
      anderson_rejections <- anderson_rejections + 1
    }
  }
  # Wydrukuj wyniki
  cat("For df =", df, ":\n")
  cat("Shapiro-Wilk test correctly rejected H0", shapiro_rejections, "times out of", simulations, "\n")
  cat("Anderson-Darling test correctly rejected H0", anderson_rejections, "times out of", simulations, "\n")
}