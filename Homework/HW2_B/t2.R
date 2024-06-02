# ex B1
cat("ex B1\n")

r = 3

R = 10

pi = 3.14


ex_B1 = function(N) {
  numar_puncte_interior = 0
  
  for (i in 1:N) {
    x = runif(1, -R - r, R + r)
    
    y = runif(1, -R - r, R + r)
    
    z = runif(1, -r, r)
    
    
    if (z ^ 2 + (sqrt(x ^ 2 + y ^ 2) - R) ^ 2 < r ^ 2) {
      numar_puncte_interior = numar_puncte_interior + 1
      
    }
  }
  volum = ((2 * (R + r)) ^ 2 * (2 * r)) * numar_puncte_interior / N
  
  return(volum)
  
}

suprafata_tor = 2 * pi ^ 2 * R * r ^ 2

cat("Volum tor\n")

print(suprafata_tor)

cat("Volum tor calculat\n")

print(ex_B1(10000))

print(ex_B1(20000))

print(ex_B1(50000))

cat("Eroare: Volum tor\n")

print(abs(suprafata_tor - ex_B1(10000)) / suprafata_tor)

print(abs(suprafata_tor - ex_B1(20000)) / suprafata_tor)

print(abs(suprafata_tor - ex_B1(50000)) / suprafata_tor)


# ex B2
cat("ex B2\n")

ex_B2 = function(N) {
  numar_puncte_interior = 0
  
  for (i in 1:N) {
    x = runif(1, 0, 2)
    
    y = runif(1, 0, 3)
    
    if (y >= 0 && y <= 2 * x && y <= 6 - 3 * x) {
      numar_puncte_interior = numar_puncte_interior + 1
      
    }
  }
  return((numar_puncte_interior / N) * 6)
  
}
cat("Arie triunghi\n")
print(ex_B2(20000))

# ex B3
cat("ex B3 A\n")
ex_B3_a = function(N) {
  suma = 0
  
  for (i in 1:N) {
    x = runif(1, -1, 1)
    
    suma = suma + (2 * x - 1) / (x * x - x - 6)
    
  }
  cat("Val integrala\n")
  print(log(3) - log(2))
  
  cat("Val integrala (Monte-Carlo)\n")
  print(2 * suma / N)
  
}
ex_B3_a(10000)


cat("ex B3 B\n")
ex_B3_b = function(N) {
  suma = 0
  
  for (i in 1:N) {
    x = runif(1, 3 + 10 ^ (-100), 11)
    
    suma = suma + ((x + 4) / ((x - 3) ^ (1 / 3)))
    
  }
  cat("Val integrala\n")
  print(61.2)
  
  cat("Val integrala (Monte-Carlo)\n")
  print((8 - 10 ^ (-100)) * suma / N)
  
}
ex_B3_b(10000)


cat("ex B3 C\n")
ex_B3_c = function(N, lambda) {
  suma = 0
  
  for (i in 1:N) {
    u = rexp(1, lambda)
    
    suma = suma + (u * exp(-u * u)) / (lambda * exp(-lambda * u))
    
  }
  cat("Val integrala\n")
  print(1 / 2)
  cat("Val integrala (Monte-Carlo)\n")
  print(suma / N)
  
}
ex_B3_c(10000, 1)


# ex B4
cat("ex B4\n")

cat("A\n")

simulate_growth = function(n,
                           p,
                           q,
                           utilizatori_initiali,
                           utilizatori_tinta,
                           ani_maxim) {
  ani = numeric()
  
  
  for (i in 1:n) {
    utilizatori = utilizatori_initiali
    
    an = 0
    
    while (utilizatori < utilizatori_tinta && an < ani_maxim) {
      an = an + 1
      
      utilizatori_noi = rbinom(1, 1000, p)
      
      utilizatori_pierduti = rbinom(utilizatori, 1, q)
      
      utilizatori = utilizatori + utilizatori_noi - sum(utilizatori_pierduti)
      
    }
    ani = c(ani, an)
    
  }
  
  return(ani)
  
}

util_init = 10000
util_target = 15000
n_sim = 1000
n = 1000
p = 0.25
q = 0.01
ani_max = 100

yrs = simulate_growth(n_sim, p, q, util_init, util_target, ani_max)

print(mean(yrs))


cat("B\n")


simulate_users_after_time = function(n,
                                     p,
                                     q,
                                     utilizatori_initiali,
                                     utilizatori_tinta,
                                     ani) {
  succes = numeric(n)
  
  for (i in 1:n) {
    utilizatori = utilizatori_initiali
    for (an in 1:ani) {
      utilizatori_noi = rbinom(1, 1000, p)
      utilizatori_pierduti = rbinom(utilizatori, 1, q)
      utilizatori = utilizatori + utilizatori_noi - sum(utilizatori_pierduti)
    }
    succes[i] = (utilizatori >= utilizatori_tinta)
  }
  
  return(mean(succes))
}

ani = 40 + 10 / 12

users_after_prob = simulate_users_after_time(n_simulations, p, q, util_init, util_target, ani)

print(users_after_prob)


cat("C\n")

margine_eroare = 0.01
probabilitati = 0.99
z = qnorm(1 - (1 - probabilitati) / 2)

n_necesar = (z ^ 2 * users_after_prob * (1 - users_after_prob)) / margine_eroare ^
  2
cat("nr necesari simulari: ", ceiling(n_necesar), "\n")

numar_succese = sum(runif(n_necesar) < users_after_prob)

probabilitate_estimata = numar_succese / n_necesar

cat("prob estimata: ", probabilitate_estimata, "\n")