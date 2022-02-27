
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


# c

# relate_categorial
#
# Funktion: beschreibt den Zusammenhang zwischen zwei kategorialen Variablen mit
#           einer Kontingenztafel, den bedingten Verteilungen und Cramers und 
#           Pearsons korrigiertes Kontingenzmass.
#
# input:  X       - Vektor mit den Merkmalsauspraegungen eines kategorialen 
#                   Merkmals
#         Y       - Vektor mit den Merkmalsauspraegungen eines kategorialen
#                   Merkmals, selbe Laenge wie X
#         Merkmal - Vektor Laenge 2; jeweils entweder "qualitativ" oder "ordinal", 
#                   "ordinal", wenn Mermal eine Ordnung hat
#         absolut - logisch; gibt an ob die absoluten Haeufigkeiten gegeben 
#                   werden sollen
#         dnames  - Vektor Laenge 2; die Namen der beiden Variablen. Wenn ein 
#                   Name unbekannt ist, dann NA angeben.
#
# output: Benannte Liste mit
#           - Kontingenztafel/Korrelationstabelle
#           - Tabelle der bedingten Haeufigkeitsverteilungen h(X|Y) und h(Y|X)
#           - 2 normierte Kontingenzmasse
#           - maximale und minimale Auspraegung der Kontingenzmasse.
#
relate_categorial = function( X, Y, Merkmal = c("qualitativ", "qualitativ"), 
                              absolut = FALSE, dnames = c(NULL, NULL) ){
  stopifnot(length(X) == length(Y),
            Merkmal %in% c("qualitativ", "ordinal"),
            length(Merkmal) == 2,
            length(dnames) <= 2)
  
  n = length(X)
  normer = ifelse(absolut, 1, n)
  
  if(is.null(dnames))dnames = c("X", "Y")
  if(length(dnames == 1)){
    length(dnames) = 2
    dnames[which(is.na(dnames))] = c("X", "Y")[which(is.na(dnames))]
  }
  if(dnames[1] != "X")dnames[1] = paste("X:", dnames[1])
  if(dnames[2] != "Y")dnames[2] = paste("Y:", dnames[2])
  
  bedXY = table(Y, X, dnn = c("", "h(X|Y)")) / as.numeric(table(Y))
  bedYX = table(X, Y, dnn = c("", "h(Y|X)")) / as.numeric(table(X))
  
  chi = quad_Kontingenz(X, Y)
  m = min(nrow(bedXY), ncol(bedXY))
  cramer = sqrt(chi / (n * (m - 1)))
  kPearson = sqrt(chi / (chi + n)) * sqrt(m / (m - 1))
  
  if(all(Merkmal == "ordinal")){
    return(list(Korrelationstabelle = table(X, Y, dnn = dnames) / normer,
                bedingte_Verteilung_X = bedXY,
                bedingte_Verteilung_Y = bedYX,
                Kontingenzmass = c(Cramers_Kontingenzmass = cramer, 
                                   korr_Pearsons_Kontingezmass = kPearson),
                Intervall = c(unabhaengig = 0, abhaengig = 1)))
  }
  else{
    return(list(Kontingenztafel = table(X, Y, dnn = dnames) / normer,
                bedingte_Verteilung_X = bedXY,
                bedingte_Verteilung_Y = bedYX,
                Kontingenzmass = c(Cramers_Kontingenzmass = cramer, 
                                   korr_Pearsons_Kontingezmass = kPearson),
                Intervall = c(unabhaengig = 0, abhaengig = 1)))
  }
}


# d

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

#e)

#quantilKategorisierung -  Funktion, die eine mindestens ordinal skalierte Variable 
#                          quantilsbasiert kategorisiert
#
# Input
# 
# data: numerisch oder odinalskalierte Variable: Daten, die kategorisiert werden sollen. 
# grenzen: num. Vektor mit 2 gewuenschten Quantilen, die die Grenzen festlegen. 
#          Der erste Eintrag im Vektor bestimmt die untere Grenze, der zweite Eintrag entsprechend 
#          die obere Grenze. Muessen beiden zwischen 0 und 1 liegen.
# Anordnung: bestimmte Anordnung für nicht numerische Variablen
#
# Output
#
# dataframe: Spalte 1 "Daten" mit den urspruenglichen Daten, 
#            Spalte 2 namens "Kategorie", die berechnete Kategorie

quantilKategorisierung = function(data, grenzen = c(0.25, 0.75), Anordnung = NULL) {
  
  if(length(grenzen) != 2 | grenzen[1] >= grenzen[2] | grenzen[1] < 0 | grenzen[2] >= 1) {
    stop("ungueltige Grenzen")
  }
  
  tmp = data #urspruengliche Daten
  
  if(!is.null(Anordnung)){
    data = raenge(data, Anordnung, Bindungen = "average") 
  }
  
  else if(!is.numeric(data)){
    data = ordered(data) #Ordnung hinzufuegen
  }
  
  data = as.numeric(data) #ggf umwandeln in numerische Werte mit bekannter Anordnung
  df = as.data.frame(data) # umwandeln in Dataframe
  df$Daten = tmp #uspruengliche Daten dem dataframe hinzufuegen
  
  
  quants = quantile(df$data, probs = grenzen) #Quantile bilden auf numerisierten Daten
  
  df$Kategorie = NA
  
  df$Kategorie[which(df$data <= quants[1])] = "niedrig"
  df$Kategorie[which(df$data > quants[2])] = "hoch"
  df$Kategorie[which(df$data > quants[1] & df$data <= quants[2]) ] = "mittel"
  
  return(df[-1]) 
  #numerisierte Daten entfernen, return dataframe mit urspruenglichen Daten und Kategorie
}


# f)
# Visualisierung erstellt  maximal 4 Grafiken ueber kategoriale Variable, inklusive ganzzahliger Daten. Numerische, nicht ganzzahlige Variablen werden nicht visualisiert
# Input - data - Dataframe 
# Output - eine Grafik
Visualisierung = function(data){
  par(mfrow = c(2,2))
  i = 2
  n = 1
  while(i <= dim(data)[2] && n <= 4){
    if(!is.numeric(data[[i]]) | is.integer(data[[i]])){
      barplot(table(data[[i]]), main = names(data)[i], ylab = "Anzahl")
      n = n+1
    }
    i= i+1
  }
  par(mfrow = c(1,1))
}


# Zusatz

# rangkorr_koeff
# 
# Funktion: berechnet den Spearmanschen Rangkorrelationskoeffizienten r_x,y.
#           Geeignet fuer zwei mindestens ordinal skalierte Merkmale, wobei die 
#           Anordnung selbst bestimmt werden kann.
# 
# input:    X         - Vektor; erste Variable
#           Y         - Vektor; zweite Variable
#           xAnordnung- alle (meoglichen) Realisierungen von X in sortierter 
#                       Reihenfolge, sonst Sortierung nach Ordnung in R
#           yAnordnung- alle Realisierungen von Y in sortierter Reihenfolge, ...
#           Bindungen - Verfahren zum Umgang mit Bindungen: "average" (a. Mittel)/
#                       "max"/"min"/"random" der Raenge
# 
# output:   der Rangkorrelationskoeffizient; numerisch
#
rangkorr_koeff = function(X, Y, xAnordnung = NULL, yAnordnung = NULL, 
                          Bindungen = "average"){
  stopifnot(length(X) == length(Y), 
            Bindungen %in% c("average", "max", "min", "random"))
  
  RgX = raenge(X, Anordnung = xAnordnung, Bindungen)
  RgY = raenge(Y, Anordnung = yAnordnung, Bindungen)
  
  RgX = RgX - mean(RgX)
  RgY = RgY - mean(RgY)
  
  r = (sum(RgX * RgY)) / sqrt(sum(RgX^2) * sum(RgY^2))
  
  print(c("monoton fallender" = -1, "kein Zusammenhang" = 0, 
          "monoton wachsender Zusammenhang" = 1))
  return(Rangkorrelationskoeffizient = r)
}

################################################################################
