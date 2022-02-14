
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

#quantilKategorisierung -  die mindestens ordinal skalierte Variablen quantilsbasiert kategorisiert
#
# Input
# 
# data - num. Vektor: Daten, die kategorisiert werden sollen
# grenzen: num. Vektor mit gewünschten 2 Quantilen, die die Grenzen festlegen
# Anordnung - ein character-Vektor: die Anordnung der Variable von klein zu groß
#
# Output
#
# dataframe: Spalte 1 die urspürnglichen Daten, Spalte 2 "Kategorie" die berechnete Kategorie

quantilKategorisierung = function(data, grenzen = c(0.25, 0.75), Anordnung = NA) {
  
  if(is.na(Anordnung )) { #Daten sind numerisch, agiere daher mit normaler Ordnungsrelation <, >
    quants = quantile(data, probs = grenzen)
    data = as.data.frame(data)
    data$Kategorie = ""
    data[data$data <= quants[1], ]$Kategorie = "niedrig"
    data[data$data >= quants[2], ]$Kategorie = "hoch"
    data[data$data < quants[2] & data$data > quants[1], ]$Kategorie = "mittel"
  }
  else {
    ### todo für andere Ordnungsrelationen
  }
  return(data) #dataframe okay?
  }



#Test
data = sample(1000, 100)
quantilKategorisierung(data)
table(quantilKategorisierung(data)$Kategorie)

# todo (f)
