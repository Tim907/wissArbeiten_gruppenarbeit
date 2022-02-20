
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
# data: numerisch oder odinalskalierte Variable: Daten, die kategorisiert werden sollen. Fuer nicht numerische Daten muss eine entsprechende Ordnungsrelation
#        uebergeben werden, zum Beispiel mit data = ordered(sample(letters, 50, TRUE))
# grenzen: num. Vektor mit 2 gewuenschten Quantilen, die die Grenzen festlegen. Der erste Eintrag im Vektor bestimmt die untere Grenze,
#           der zweite Eintrag entsprechend die obere Grenze. Muessen beiden zwischen 0 und 1 liegen.
#
#
# Output
#
# dataframe: Spalte 1 "Daten" mit den urspruenglichen Daten, Spalte 2 namens "Kategorie", die berechnete Kategorie

quantilKategorisierung = function(data, grenzen = c(0.25, 0.75)) {
  
  if(length(grenzen) != 2 | grenzen[1] >= grenzen[2] | grenzen[1] > 1 | grenzen[1] < 0 | grenzen[2] > 1 | grenzen[2] < 0) {
    return("ungueltige Grenzen")
  }
  
  tmp = data #urspruengliche Daten
  data = as.numeric(data) #ggf umwandeln in numerische Werte mit bekannter Anordnung
  data = as.data.frame(data) # umwandeln in Dataframe
  data$Daten = tmp #uspruengliche Daten dem dataframe hinzufuegen
  quants = quantile(data$data, probs = grenzen) #Quantile bilden auf numerisierten Daten

  data$Kategorie = ""
  data[data$data <= quants[1], ]$Kategorie = "niedrig"
  data[data$data > quants[2], ]$Kategorie = "hoch"
  data[data$data <= quants[2] & data$data > quants[1], ]$Kategorie = "mittel" #Kategorisierung geschieht auf numerisierten Daten
  

  return(data[-1]) #numerisierte Daten entfernen, return dataframe mit urspruenglichen Daten und Kategorie
}


# todo (f)
