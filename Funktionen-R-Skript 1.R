
# Bearbeitung von Aufgabe 3

# Falls Probleme beim Laden, probiert: Session/Set Working Directory/To Source File Location
source("Funktionen-R-Skript 2.R")

# todo (a)

# todo (b)

# todo (c)

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
#         Merkmal - Vektor Laenge 2; jeweils entweder "nominal" oder "ordinal", 
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
relate_categorial = function( X, Y, Merkmal = c("nominal", "nominal"), 
                              absolut = FALSE, dnames = c(NULL, NULL) ){
  stopifnot(length(X) == length(Y),
            Merkmal %in% c("nominal", "ordinal"),
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


# todo (d)

# todo (e)

# todo (f)

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
