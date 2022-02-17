
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

