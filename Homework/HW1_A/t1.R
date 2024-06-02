# ex A1
# A
cat("ex A1 A\n")

exA1_a = function(lambda, p, k, m, n) {
  interval = k:m
  
  poisson_dist = dpois(interval, lambda)
  
  geometric_dist = dgeom(interval - 1, p)
  
  binomial_dist = dbinom(interval, n, p)
  
  print(poisson_dist)
  
  print(geometric_dist)
  
  print(binomial_dist)
  
}
exA1_a(10, 0.5, 7, 18, 25)


# B
cat("ex A1 B\n-- grafic\n")

exA1_b = function(lambda, p, k, m, n) {
  interval = k:m
  
  poisson_dist = dpois(interval, lambda)
  
  geometrica_dist = dgeom(interval - 1, p)
  
  binomiala_dist = dbinom(interval, n, p)
  
  barplot(poisson_dist, main = "Dist Poisson", names.arg = interval)
  
  barplot(geometrica_dist,
          main = "Dist Geometrica",
          names.arg = (interval - 1))
  
  barplot(binomiala_dist, main = "Dist Binomiala", names.arg = interval)
  
}
par(mfrow = c(3, 2))
# layout
exA1_b(10, 0.5, 0, 8, 10)


# C
cat("ex A1 C\n")

exA1_c = function(lambda) {
  k0 = qpois(1 - 10 ^ (-6), lambda)
  print(k0)
  
}
exA1_c(10)



# ex A2
# A
cat("ex A2 A\n")

exA2_a = function() {
  fisier = read.csv("note_PS.csv", header = TRUE, sep = ',')
  
  prob = fisier[['P']]
  
  stat = fisier[['S']]
  
  fr_abs_prob = as.vector(table(prob))
  
  fr_abs_stat = as.vector(table(stat))
  
  cat("Fr abs prob, stat\n")
  print(fr_abs_prob)
  
  print(fr_abs_stat)
  
  fr_rel_prob = fr_abs_prob / length(prob)
  
  fr_rel_stat = fr_abs_stat / length(stat)
  
  cat("Fr rel prob, stat\n")
  
  print(fr_rel_prob)
  
  print(fr_rel_stat)
  
  
  media_prob = mean(prob)
  
  media_stat = mean(stat)
  
  cat("Media prob, media stat\n")
  
  print(media_prob)
  
  print(media_stat)
  
}
exA2_a()


# B
cat("ex A2 B\n")

exA2_b = function(nume_fisier, esantion) {
  fisier = read.csv(nume_fisier, header = T, sep = ',')
  
  if (esantion == 'P') {
    x = fisier[['P']]
  }
  else if (esantion == 'S') {
    x = fisier[['S']]
  }
  media_x = mean(x)
  
  deviatia_standard_x = sd(x)
  
  valori_filtrate = vector()
  
  h = 1
  
  for (i in 1:length(x)) {
    if (x[i] < media_x + 2 * deviatia_standard_x &&
        x[i] > media_x - 2 * deviatia_standard_x) {
      valori_filtrate[h] = x[i]
      
      h = h + 1
      
    }
  }
  print(as.vector(table(valori_filtrate)))
  
  hist(valori_filtrate, right = TRUE, breaks = 0:10)
  
  return(valori_filtrate)
  
}
exA2_b("note_PS.csv", 'P')

exA2_b("note_PS.csv", 'S')
