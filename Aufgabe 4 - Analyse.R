# Aufgabe 4

# Hier sollen wir den Datensatz aus (2.) mit Hilfe der in (3.) erstellen 
# Funktionen analysieren. (Deskription und Visualisierung)
# Jede der Funktionen (a) bis (f) aus Funktionen-R-Skript 1.R sollen mindestens
# einmal angewendet werden.

# setwd(...)

df = read.csv("person.csv", header = TRUE, sep = ";", dec = ",")

source("Funktionen-R-Skript 1.R")
source("Funktionen-R-Skript 2.R")

df$Mathe.LK = df$Mathe.LK == "ja" #in Boolean umformen


metrischeVariablen(df$Alter)
# Die Personen sind etwa zwischen 20 und 30 Jahren alt und im Durchschnitt knapp 25 Jahre alt

metrischeVariablen(df$Interesse.an.Mathematik)
#im Durchschnitt liegt das Interesse bei etwa 3.9

metrischeVariablen(df$Interesse.an.Programmieren)
# im Durchschnitt liegt das Interesse bei etwa 4.6,also hoeher als das Interessant an mathematik

cor_metric_dicho(df$Interesse.an.Mathematik, df$Mathe.LK)
# Es gibt einen leicht positiven Zusammenhang zwischen Interesse an Mathematik und Mathe LK

cor_metric_dicho(df$Interesse.an.Programmieren, df$Mathe.LK)
# Es gibt einen deutlichen,  negativen Zusammenhang zwischen Interesse am Programmieren und Mathe LK


kategorialeVariablen(df$Studienfach, Merkmal = qualitativ)
# funktioniert nicht?

Visualisierung(df)
# Hier erkennen wir, dass in der Grafik mit den Studienfaechern das Studienfach 
# Statistik am Häufigsten angegeben worden ist, dicht gefolgt von Data Science,deutlich
# geringer ist das Studienfach Mathe vertreten. In der rechten, oberen Grafik 
# können wir ablesen, dass die meisten eine Merkmalsauspraegung vo 5 angeben 
# haben und die wenigsten ein Interesse mit 1 oder 7 angegeben haben. Bei der 
# untereren linken Grafik sehen wir das die meisten ein Interesse von 5 bei der 
# Programmierung haben und deutlich mehr Studierende ein höheres Interesse an
# Programmierung haben, als an Mathe (vergleich 2 mit 3 Grafik).
# Bei der unteren rechten Grafik können wir erkennen das deutlich mehr als die 
# haelfte der Studierenden den Mathe LK besucht haben.