# Hier Helferfunktionen implementieren, welche nur in
# Funktionen-R-Skript 1 benutzt werden duerfen.


#quad_Kontingenz
#
# Funktion: berechnet die quadratische Kontingenz Chi quadrat auf zwei Arten
# 
# input:  x ______ ein Vektor/ein Faktor mit Merkmalsauspraegungen oder eine 
#                  Tabelle/Matrix
#         y ______ wenn x ein Vektor/Faktor ist, dann ein Vektor/Faktor mit den 
#                  Auspraegungen des zweiten Merkmals
#         method _ entweder 1 oder 2. 1) allgemeine Vorgehensweise ueber 
#                                        Fehlerquadrate
#                                     2) umgeformte Formel (ueber die mittlere 
#                                        quadratische Kontingenz/Phi-Koeff.^2).
#                  Beide Methoden liefern analytisch das selbe Ergebnis, koennen 
#                  aber numerische Unterschiede aufweisen.
#
# output: die quadratische Kontingenz
#
quad_Kontingenz = function(x, y = NULL, method = 1){
  if(!is.table(x) & !is.matrix(x) & is.null(y)){
    stop("Zwei Vektoren muessen angegeben werden, alternativ eine Tabelle oder",
         " eine Matrix.")
  }
  stopifnot(method %in% 1:2,
            is.null(y) || length(x) == length(y))
  
  if(!is.null(dim(x))){
    vals = as.numeric(x)
    n = sum(x)
    k = nrow(x)
    l = ncol(x)
    Hk = rowSums(x)
    Hl = colSums(x)
  }
  else{
    tab <- table(x, y)
    vals = as.numeric(tab)
    n = length(x)
    k = nrow(tab)
    l = ncol(tab)
    Hk = rowSums(tab)
    Hl = colSums(tab)
  }
  Hkl = matrix(Hk) %*% matrix(Hl, nrow = 1)
  Hkl = as.numeric(Hkl)
  
  if(method == 1){
    Hkl = Hkl / n
    error = ((vals - Hkl)^2) / Hkl
    error = sum(error)
  }
  else{
    error = (vals^2) / Hkl
    error = (sum(error) - 1) * n
  }
  
  return(chi_quad = error)
}
