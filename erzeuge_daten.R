
# Dieses R-Skript dient dem Simulieren eines Datensatzes nach Aufgabe 1.
# Er soll 100 Beobachtungen auf 5 Variablen beinhalten, exclusive der ersten 
# Spalte ID.

# todo dataFrame mit 6 Spalten erzeugen
N = 100
person = data.frame(ID = 1:N,
                  "Alter" = numeric(N),
                  "Studienfach" = character(N),
                  "Interesse an Mathematik" = numeric(N), # auch factor moeglich
                  "Interesse an Programmieren" = numeric(N),
                  "Mathe-LK" = character(N)) #factor() geht nicht, weil Laenge 0
#Replizierbarkeit
#set.seed()


# todo (1)
set.seed(11)
person$Alter = rnorm(N, 25, 2)


# todo (2)
auswahl = c("Statistik", "Data Science", "Informatik", "Mathe")

# Fachwahl simuleirt die Fachwahl von n Studenten in einem Vektor mit 4 
# verschiedenen Faechern und willkuerlichen, aber festen relativen Haeufigkeiten
#
Fachwahl = function(faecher, n){
  stopifnot(length(faecher) == 4,
            is.numeric(n))
  
  # Willkuerliches Festlegen der wahren Wahrscheinlichkeiten:
  
  p_F1.2 = 3.5 - 2
  # 3.5 entspricht dem Erwartungswert eines Wuerfelwurfs
  # 3.5 - 2 = 1.5
  
  p_F3 = pi - 2
  # ungefaehr 3.14 - 2 = 1.14
  
  p_F4 = exp(1) - 2
  # ungefaehr 2.72 - 2 = 0.72
  
  # Wahrscheinlichkeiten zwischen 0 und 1 normieren:
  total = 2 * p_F1.2 + p_F3 + p_F4
  p_F1.2 = p_F1.2 / total
  p_F3 = p_F3 / total
  p_F4 = p_F4 / total
  
  Wahl = sample(faecher, n, replace = TRUE, c(p_F1.2, p_F1.2, p_F3, p_F4))
  
  return(Wahl)
}

set.seed(22)
person$Studienfach = Fachwahl(auswahl, N)
#table(person$Studienfach)


# todo (3)
M_Interesse = as.numeric(as.vector(factor(person$Studienfach, 
                            labels = c(0.3, 0.4, 0.6, 0.85), 
                            levels = c("Informatik", "Data Science", "Statistik", 
                                       "Mathe"))))
set.seed(33)
person$Interesse.an.Mathematik = sapply(M_Interesse, rbinom, n = 1, size = 6) + 1
#split(person$Interesse.an.Mathematik, person$Studienfach)


# todo (4)
P_Interesse = 
#  as.numeric(as.vector(factor(person$Studienfach, 
#                              labels = c(0.25, 0.42, 0.7, 0.9),
#                              levels = c("Mathe", "Statistik", "Data Science", 
#                                         "Informatik"))))
  #oder
#  sample(1:7, N, replace = TRUE)
  
set.seed(44)
#person$Interesse.an.Programmieren = P_Interesse


# todo (5)
P_Mathe.LK = 0.5 + (person$Interesse.an.Mathematik - person$Interesse.an.Programmieren) / 6 * 0.35
person$Mathe.LK = rbinom(nrow(person), 1, P_Mathe.LK)
person$Mathe.LK = factor(person$Mathe.LK, labels = c("ja", "nein"), levels = c(1, 0))

#data = cbind(data, col)


# todo 2. \\ Datensatz als .csv abspeichern
