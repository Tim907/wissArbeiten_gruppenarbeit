# Hier Helferfunktionen implementieren, welche nur in
# Funktionen-R-Skript 1 benutzt werden duerfen.

#Helferfunktionen fuer b

#qualitativ - eine Funktion, die die qualitativen Merkmale bearbeitet
#
# Input
# 
# a - ein character-Vektor
#
# Output
#
# eine Liste mit den Ergebnissen(Merkmal, Modus, AbsoluteHaeufigkeit, 
#                                RelativeHaeufigkeit)

qualitativ = function(a){
  modus = names(table(a)[which.max(table(a))])
  absoluteHaeufigkeit = data.frame(Auspraegung = names(table(a)),
                                   AbsoluteHaeufigkeit = as.integer(table(a)))
  relativeHaeufigkeit = data.frame(Auspraegung = names(table(a)),
                        RelativeHaeufigkeit = as.integer(table(a)) / length(a))
  
  return(list(Merkmal = "qualitativ",
              Modus = modus,
              AbsoluteHaeufigkeit = absoluteHaeufigkeit,
              RelativeHaeufigkeit = relativeHaeufigkeit))
}

#ordinal - eine Funktion, die die ordinalen Merkmale bearbeitet
#
# Input
# 
# a - ein character-Vektor
# Anordnung - ein character-Vektor: die Anordnung von a
#
# Output
#
# eine Liste mit den Ergebnissen(Merkmal, Modus, AbsoluteHaeufigkeit, 
#                                RelativeHaeufigkeit, Median)

ordinal = function(a, Anordnung){
  if(is.null(Anordnung)) stop("Anordnung fehlt!")
  stopifnot(length(names(table(a))) == length(Anordnung))
  
  modus = names(table(a)[which.max(table(a))])
  absoluteHaeufigkeit = data.frame(Auspraegung = names(table(a)),
                                   AbsoluteHaeufigkeit = as.integer(table(a)))
  relativeHaeufigkeit = data.frame(Auspraegung = names(table(a)),
                        RelativeHaeufigkeit = as.integer(table(a)) / length(a))
  
  #res ist der nach Anordnung sortierte Vektor von a, um Median zu berechnen
  res = character(length(a))
  counter = 0
  
  for(i in 1:length(Anordnung)){
    ziel = counter + absoluteHaeufigkeit$AbsoluteHaeufigkeit[
      absoluteHaeufigkeit$Auspraegung == Anordnung[i]]
    res[(counter + 1):ziel] = Anordnung[i]
    counter = ziel
  }
  
  median = res[length(a) %/% 2]
  
  return(list(Merkmal = "ordinal",
              Modus = modus,
              AbsoluteHaeufigkeit = absoluteHaeufigkeit,
              RelativeHaeufigkeit = relativeHaeufigkeit,
              Median = median))
}


#weitere Helferfunktionen

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
    stop("Es muessen ZWEI Vektoren angegeben werden! Alternativ eine Tabelle",
         " oder eine Matrix.")
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
  
  return(Chi_quad = error)
}


# raenge:
#   bestimmt die Raenge in einem Vektor bei beliebiger vorgegebener Anordnung
# 
# input:  x         Vektor; Variable
#         Anordnung Vektor; einzelne Realisierungen in sortierter Reihenfolge
#                   wenn keine Angabe erfolgt, dann wird die Sortierung aus R
#                   genutzt
#         Bindungen "average"/"max"/"min"/"random": Art des Umgangs mit Bindungen
# 
# output: der Vektor der Raenge
#
raenge = function(x, Anordnung = NULL, Bindungen = "average"){
  
  if(!is.null(Anordnung) && !all(unique(x) %in% Anordnung)){
    stop("In Anordnung muessen alle Elemente des Vektors vorkommen.")
  }
  stopifnot(length(Anordnung) == length(unique(Anordnung)), 
            # Anordung muss eindeutig sein und ohne Doppelungen
            Bindungen %in% c("average", "min", "max", "random"))
  
  if(!is.vector(x))x = as.vector(x)
  n = length(x)
  if(is.null(Anordnung)){
    ranks = rank(x, ties.method = Bindungen)
  }
  else{
    matches = match(x, Anordnung)
    uni_matches = unique(matches)
    num_matches = length(uni_matches)
    
    if(num_matches == length(Anordnung) & num_matches == length(matches)){
      ranks = matches 
    }
    else{
      anz = numeric(num_matches)
      uni_matches = sort(uni_matches)
      for(i in 1:num_matches){
        anz[i] = sum(matches == uni_matches[i])
      }
      ranks = numeric(n)
      pos = c(0, cumsum(anz))
      if(Bindungen == "average"){
        for(j in 1:num_matches){
          ranks[matches == uni_matches[j]] = mean(seq((pos[j] + 1), pos[j + 1]))
        }
      }else if(Bindungen == "max"){
        for(j in 1:num_matches){
          ranks[matches == uni_matches[j]] = pos[j + 1]
        }
      }else if(Bindungen == "min"){
        for(j in 1:num_matches){
          ranks[matches == uni_matches[j]] = pos[j] + 1
        }
      }else{
        for(j in 1:num_matches){
          ranks[matches == uni_matches[j]] = sample(seq((pos[j] + 1), pos[j + 1]))
        }
      }
    }
  }
  return(ranks)
}

################################################################################
