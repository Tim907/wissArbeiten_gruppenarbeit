
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

# cor_metric_dicho liefert mittels logistischer Regression deskriptive Statistiken 
# ueber den Zusammenhang zwischen einer metrischen und einer bivariaten Variable.
# Input:
#   x, numerischer Vektor der Eingabe-Variable
#   y, numerischer Vektor der Ziel-Variable
# Output:
#   prediction, die Vorhersage fuer alle x-Werte des berechneten Modells.
#   summary, das summary-Objekt des Modells zur Beurteilung der Guete.
cor_metric_dicho = function(x, y) {
  
  model = glm(y ~ x, family=binomial(link = "logit"))
  predictions = model$fitted.values
  info = summary(model)
  return(list(prediction = predictions, summary = summary(model)))
}
# data = read.csv2("person.csv")
# correlation = cor_metric_dicho(data$Interesse.an.Mathematik,
#   ifelse(data$Mathe.LK == "ja", 1, 0))

# todo (e)

# todo (f)