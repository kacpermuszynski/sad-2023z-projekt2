# Wypadki zgrupowane przez dni tygodnia w roku 2022
accidents_per_weekday <- c(3248, 3083, 3252, 3203, 3617, 2727, 2192)

test_result <- chisq.test(accidents_per_weekday)

print(test_result)

# Liczba Wypadków zgrupowanych przez miesiące w roku 2022
accidents_per_month <- c(1300, 1220, 1584, 1479, 2119, 2349, 2153, 2199, 1941, 2009, 1506, 1463)
days_in_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
accidents_by_days <- accidents_per_month / days_in_months

test_result <- chisq.test(accidents_by_days)

print(test_result)

# Liczba zgonów zgrupowanych przez miesiące w roku 2022
deaths_per_month <- c(137, 96, 114, 111, 147, 156, 169, 173, 157, 151, 143, 115)
days_in_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
deaths_by_days <- deaths_per_month / days_in_months

test_result <- chisq.test(deaths_by_days)

print(test_result)

# Tworzenie ramki danych
df <- data.frame(
  deaths = rep(deaths_per_month, days_in_months),
  month = rep(1:12, days_in_months)
)

# Wykonanie testu Kruskala-Wallisa
kruskal <- kruskal.test(deaths ~ month, data = df)
print(kruskal)