
# Dieses R-Skript dient dem Simulieren eines Datensatzes nach Aufgabe 1.
# Er soll 100 Beobachtungen auf 5 Variablen beinhalten, exclusive der ersten 
# Spalte ID.

# Beginn: dataFrame mit 6 Spalten erzeugen
N = 100
person = data.frame(ID = 1:N,
                  "Alter" = numeric(N),
                  "Studienfach" = character(N),
                  "Interesse an Mathematik" = numeric(N),
                  "Interesse an Programmieren" = numeric(N),
                  "Mathe-LK" = character(N)) #factor spaeter


# (1)
set.seed(11)
person$Alter = rnorm(N, 25, 2)


# (2)
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


# (3)
M_Interesse = as.numeric(as.vector(factor(person$Studienfach, 
                            labels = c(0.3, 0.4, 0.6, 0.85), 
                            levels = c("Informatik", "Data Science", "Statistik", 
                                       "Mathe"))))
set.seed(33)
person$Interesse.an.Mathematik = sapply(M_Interesse, rbinom, n = 1, size = 6) + 1
#split(person$Interesse.an.Mathematik, person$Studienfach)
#sapply(split(person$Interesse.an.Mathematik, person$Studienfach), mean)


# (4)
P_Interesse = as.numeric(as.vector(factor(person$Studienfach, 
                                labels = c(0.25, 0.42, 0.7, 0.9),
                                levels = c("Mathe", "Statistik", "Data Science", 
                                         "Informatik"))))

set.seed(44)
person$Interesse.an.Programmieren = 
  sapply(P_Interesse, rbinom, n = 1, size = 6) + 1
#split(person$Interesse.an.Programmieren, person$Studienfach)
#sapply(split(person$Interesse.an.Programmieren, person$Studienfach), mean) #/ 7


# (5)
P_Mathe.LK = 0.67 + (person$Interesse.an.Mathematik - 
                      person$Interesse.an.Programmieren) / 6 * 0.3
# nur W'keiten zwischen 0.37 und 0.97 moeglich  #(0.3/6) * c(-6, 0, 6) + 0.67

set.seed(5566)
person$Mathe.LK = rbinom(nrow(person), 1, P_Mathe.LK)
# par(mfrow = c(2, 2))
# mapply(hist, split(person$Mathe.LK, person$Studienfach), main = auswahl[c(2:4,1)],
#       breaks = 2, xlab = "nein   |   ja", ylab = "Haeufigkeit", xaxt = "n",
#       cex.lab = 1.25)
# par(mfrow = c(1, 1))
person$Mathe.LK = factor(person$Mathe.LK, labels = c("ja", "nein"), levels = c(1, 0))
# cat("Mathe LK?\n"); sapply(split(person$Mathe.LK, person$Studienfach), table)


# Ende: Datensatz als .csv abspeichern
write.csv2(person, file = "person.csv", row.names = FALSE)
#read.csv2("person.csv")

################################################################################