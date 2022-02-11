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
