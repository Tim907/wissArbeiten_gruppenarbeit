
# Bearbeitung von Aufgabe 3

# Falls Probleme beim Laden, probiert: Session/Set Working Directory/To Source File Location
source("Funktionen-R-Skript 2.R")

# a

#metrischeVariablen - berechnet Minimum, Maximum, 1. Quatil, 3.Quatil, Median,
#                     Mean und die empirische Varianz
#                     
# Input
# 
# a - ein numerischer Vektor
#
# Output
#
# res - eine Liste mit den Ergebnissen (Min, Max, Quatil.1, Quatil.3, Median,
#                                       Mean, Var)

metrischeVariablen = function(a){
  res = summary(a)
  
  minimum = as.double(res[1])
  maximum = as.double(res[6])
  quatil1 = as.double(res[3])
  quatil3 = as.double(res[5])
  median = as.double(res[3])
  mean = as.double(res[4])
  empirischeVarianz = var(a)
  
  res = list(Min = minimum,
             Max = maximum,
             Quatil.1 = quatil1,
             Quatil.3 = quatil3,
             Median = median,
             Mean = mean,
             Var = empirischeVarianz)
  
  return(res)
}

# b

#kategorialeVariablen - berechnet Modus, absolute und relative Haeufigkeiten
#                       der Auspraegungen, Median (bei ordinal)
#                     
# Input
# 
# a - ein character-Vektor
# Merkmal - entweder "ordinal" oder "qualitativ"
# Anordnung - falls Merkmal == "ordinal": die Anordnung des Vektors
#                                         (ein character-Vektor)
#             sonst ist Anordnung NULL
#
# Output
#
# res - eine Liste mit den Ergebnissen (Merkmal, Modus, AbsoluteHaeufigkeit,
#                                       RelativeHaeufigkeit, Median (ordinal))

kategorialeVariablen = function(a, Merkmal, Anordnung = NULL){
  stopifnot(Merkmal == "ordinal" | Merkmal == "qualitativ")
  
  if(Merkmal == "ordinal"){
    return(ordinal(a, Anordnung))
  }
  else{
    return(qualitativ(a))
  }
}

# todo (c)

# todo (d)

#e)

#quantilKategorisierung -  Funktion, die eine mindestens ordinal skalierte Variable quantilsbasiert kategorisiert
#
# Input
# 
# data - numerisch oder ordinalskalierte Variable: Daten, die kategorisiert werden sollen
# grenzen: num. Vektor mit 2 gewünschten Quantilen, die die Grenzen festlegen
#
#
# Output
#
# dataframe: Spalte 1 "data" mit den urspürnglichen Daten, Spalte 2 namens "Kategorie", die berechnete Kategorie

quantilKategorisierung = function(data, grenzen = c(0.25, 0.75)) {
  
  data = as.numeric(data)
  
  quants = quantile(data, probs = grenzen)
  data = as.data.frame(data)
  data$Kategorie = ""
  data[data$data <= quants[1], ]$Kategorie = "niedrig"
  data[data$data >= quants[2], ]$Kategorie = "hoch"
  data[data$data < quants[2] & data$data > quants[1], ]$Kategorie = "mittel"

  return(data)
  }

# todo (f)
