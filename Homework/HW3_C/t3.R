# C1
cat("C1\n")

cat("A\n")

generare_permutatie = function(n) {
  U = runif(n, 0, 1)
  
  permutatie = sort(U)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (permutatie[i] == U[j]) {
        permutatie[i] = j
        
      }
    }
  }
  return(permutatie)
  
  
}

permutatie = generare_permutatie(10)

print(permutatie)


generare_sir_bit = function(n) {
  x = c(0, 1)
  
  sir_bit = sample(x, n, replace = TRUE)
  
  return(sir_bit)
  
}

print(generare_sir_bit(10))


generare_sir_bits = function(n, k) {
  sir_bits = c()
  
  x = c()
  
  for (i in 1:n) {
    sir_bit = sample(c(0, 1), k, replace = TRUE)
    
    sir_bits[i] = sir_bit
    
    x = sir_bit
    
  }
  return(sir_bits)
  
}

print(generare_sir_bits(5, 6))

generare_bit = function() {
  return(sample(c(0, 1), 1))
  
}

cat("B\nfunctie cmpLexicographic\n")

cmpLexicographic = function(WordI, CuvantJ) {
  LengthIJ = min(nchar(WordI), nchar(CuvantJ)) # shortest len
  
  for (l in 1:LengthIJ) {
    if (substr(WordI, l, l) < substr(CuvantJ, l, l)) {
      return(TRUE)
      
    } else if (substr(WordI, l, l) > substr(CuvantJ, l, l)) {
      return(FALSE)
      
    }
  }
  
  while (TRUE) {
    if (nchar(WordI) < nchar(CuvantJ)) {
      WordI = paste(WordI, sep = "", generare_bit())
      
    } else if (nchar(CuvantJ) < nchar(WordI)) {
      CuvantJ = paste(CuvantJ, sep = "", generare_bit())
      
    } else{
      WordI = paste(WordI, sep = "", generare_bit())
      
      CuvantJ = paste(CuvantJ, sep = "", generare_bit())
      
    }
    
    if (substr(WordI, nchar(WordI), nchar(WordI)) < substr(CuvantJ, nchar(CuvantJ), nchar(CuvantJ))) {
      return(TRUE)
      
    } else if (substr(WordI, nchar(WordI), nchar(WordI)) > substr(CuvantJ, nchar(CuvantJ), nchar(CuvantJ))) {
      return(FALSE)
      
    }
  }
}

cat("C\nfunctie QuickSort\n")

QuickSort = function(sir_bits) {
  if (length(sir_bits) <= 1)
    return(sir_bits)
  
  
  pivot_index = sample(length(sir_bits), 1)
  
  pivot = sir_bits[pivot_index]
  
  less = c()
  equal = c()
  greater = c()
  
  
  for (str in sir_bits) {
    cmp = cmpLexicographic(str, pivot)
    
    if (cmp == TRUE) {
      less = c(less, str)
      
    } else if (cmp == FALSE) {
      greater = c(greater, str)
      
    } else {
      equal = c(equal, str)
      
    }
  }
  
  return(c(QuickSort(less), equal, QuickSort(greater)))
  
}

QuickSort(generare_sir_bits(5, 6))


cat("D\n")

generare_sortata_permutatie = function(n, k) {
  sir_bits = generare_sir_bits(n, k)
  
  sortata_strings = QuickSort(sir_bits)
  
  
  permutatie = vector()
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (sir_bits[i] == sortata_strings[j]) {
        permutatie[i] = j
        
      }
    }
  }
  return(permutatie)
  
}

print(generare_sortata_permutatie(10, 6))


cat("C2\n")
cat("A\n")


cut = function(noduri, varfuri) {
  half_size = length(noduri) / 2
  
  A = sample(noduri, half_size)
  
  B = setdiff(noduri, A)
  
  taieturi = 0
  
  for (varf in varfuri) {
    u = varf[1]
    
    v = varf[2]
    
    if ((u %in% A && v %in% B) || (u %in% B && v %in% A)) {
      taieturi = taieturi + 1
      
    }
  }
  return(list(A = A, B = B, taieturi = taieturi))
  
}
noduri = 1:5

varfuri = list(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(3, 5), c(2, 5))

rezultat = cut(noduri, varfuri)

print(rezultat$A)

print(rezultat$B)

print(rezultat$taieturi)

cat("B\nFie prin mai multe rulari\nFie prin bipartitie aleatore\n")
