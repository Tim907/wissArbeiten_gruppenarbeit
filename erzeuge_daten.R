
# Dieses R-Skript dient dem Simulieren eines Datensatzes nach Aufgabe 1.
# Er soll 100 Beobachtungen auf 5 Variablen beinhalten, exclusive der ersten Spalte ID.

# todo dataFrame mit 6 Spalten erzeugen
N = 100
data = data.frame(ID = 1:N)

# todo (1)
col = rnorm(N, 25, 2)
data = cbind(data, Alter = col)

# todo (2)
auswahl = c("Statistik", "Data Science", "Mathe", "Informatik")
# Willkuerliches Festlegen der wahren Wahrscheinlichkeiten:
p_statistik = 3.5 - 2
# 3.5 entspricht dem Erwartungswert eines Wuerfelwurfs
# 3.5 - 2 = 1.5

p_informatik = pi - 2
# ungefaehr 3.14 - 2 = 1.14

p_mathe = exp(1) - 2
# ungefaehr 2.72 - 2 = 0.72

# Wahrscheinlichkeiten zwischen 0 und 1 normieren:
total = 2 * p_statistik + p_mathe + p_informatik
p_statistik = p_statistik / total
p_mathe = p_mathe / total
p_informatik = p_informatik / total

col = sample(auswahl, N, replace = TRUE, c(p_statistik, p_statistik, p_mathe, p_informatik))
#table(col)
data = cbind(data, Studienfach = col)

# todo (3)

#data = cbind(data, col)

# todo (4)

#data = cbind(data, col)

# todo (5)

#data = cbind(data, col)

# todo 2. \\ Datensatz als .csv abspeichern
