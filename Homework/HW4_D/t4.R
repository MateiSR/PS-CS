# D1
cat("D1\n")

interval_incredere_1 = function(fisier, alfa, sigma) {
  date = read.csv(fisier, header = TRUE)
  n = nrow(date)
  xn = mean(date$probabilitati)
  crit = -qnorm(alfa / 2)
  a = xn - (crit * (sigma / sqrt(n)))
  b = xn + (crit * (sigma / sqrt(n)))
  print(a)
  print(b)
}

sigma_sq = 92.16
sigma = sqrt(sigma_sq)
conf_95 = interval_incredere_1("probabilitati.csv", 1 - 0.95, sigma)
conf_99 = interval_incredere_1("probabilitati.csv", 1 - 0.99, sigma)

# D2
cat("D2\n")

interval_incredere_2 = function(fisier, alfa) {
  date = read.csv(fisier, header = TRUE)
  xn = mean(date$statistica)
  n = nrow(date)
  s = sd(date$statistica)
  crit2 = -qt(alfa / 2, n - 1)
  a = xn - crit2 * s / sqrt(n)
  b = xn + crit2 * s / sqrt(n)
  print(a)
  print(b)
}

interval_incredere_2("statistica.csv", 1 - 0.95)
interval_incredere_2("statistica.csv", 1 - 0.99)