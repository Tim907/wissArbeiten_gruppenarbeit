
# Bearbeitung von Aufgabe 3
setwd("C:/Users/User/Desktop/UNI/Semester 3/Wissenschaftliches Arbeiten")
library(ggplot2)
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

# todo (e)

# todo (f)
# Visualisierung erstellt 4 Grafiken
# Input - data - Dataframe 
# Output - eine Grafik
person = read.csv2("person.csv")
Visualisierung = function(data = person){
  par(mfrow = c(2,2))
  barplot(table(data$Mathe.LK), main = "Belegung von Mathe Lks", ylab = "Anzahl")
  barplot(table(data$Studienfach), main = "Häufigkeit der Studienfächer", ylab = "Anzahl")
  barplot(table(data$Interesse.an.Mathematik), main = "Interesse an Mathematik",
          ylab = "Anzahl",xlab ="wie stark das Interesse ist")
  barplot(table(person$Interesse.an.Programmieren), main = "Interesse an Programmieren",
          ylab = "Anzahl",xlab ="wie stark das Interesse ist")
  par(mfrow = c(1,1))
}


