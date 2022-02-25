
# Aufgabe 4

# Hier sollen wir den Datensatz aus (2.) mit Hilfe der in (3.) erstellen 
# Funktionen analysieren. (Deskription und Visualisierung)
# Jede der Funktionen (a) bis (f) aus Funktionen-R-Skript 1.R sollen mindestens
# einmal angewendet werden.

# setwd(...)

df = read.csv("person.csv", header = TRUE, sep = ";", dec = ",")

source("Funktionen-R-Skript 1.R")

df$Mathe.LK = df$Mathe.LK == "ja" #in Boolean umformen


metrischeVariablen(df$Alter)
# Die Personen sind etwa zwischen 20 und 30 Jahren alt und im Durchschnitt knapp 25 Jahre alt

metrischeVariablen(df$Interesse.an.Mathematik)
#im Durchschnitt liegt das Interesse bei etwa 3.9

metrischeVariablen(df$Interesse.an.Programmieren)
# im Durchschnitt liegt das Interesse bei etwa 4.6, also hoeher als das Interessant an Mathematik

cor_metric_dicho(df$Interesse.an.Mathematik, df$Mathe.LK)
# Es gibt einen leicht positiven Zusammenhang zwischen Interesse an Mathematik und Mathe LK

cor_metric_dicho(df$Interesse.an.Programmieren, df$Mathe.LK)
# Es gibt einen deutlichen, negativen Zusammenhang zwischen Interesse am Programmieren und Mathe LK

kategorialeVariablen(df$Studienfach, Merkmal = "qualitativ")
# Die meisten Personen studieren Statistik (36%) und Data Science (30%).
# Nur 23% von den befragten Personen haben das Studienfach Informatik.
# Nur 11% von den befragten Personen studieren Mathematik.

Visualisierung(df)
# Hier erkennen wir, dass in der Grafik mit den Studienfaechern das Studienfach 
# Statistik am Haeufigsten angegeben worden ist, dicht gefolgt von Data Science, deutlich
# geringer ist das Studienfach Mathematik vertreten.
# Bei der rechten Grafik koennen wir erkennen, dass deutlich mehr als die 
# haelfte der Studierenden den Mathe LK besucht haben.

relate_categorial(df$Mathe.LK,df$Interesse.an.Mathematik)
# Kontigenztafel
# Mathe.LK\Interesse an Mathe   1    2    3    4    5    6    7
#       FALSE                   0.01 0.05 0.10 0.09 0.07 0.01 0.00
#       TRUE                    0.04 0.10 0.12 0.11 0.18 0.07 0.05
# Hier an der Kontigenztafel koennen wir sehen, dass 18% des Datensatzes eine Zusammensetzung
# von Interesse an Mathe 5 mit Mathe LK ja hat und 0% der Daten eine Zusammensetzung
# von Interesse an Mathe 7 mit Mathe LK ja hat.

# bedingte Verteilung h(Mathe.LK|Interesse an Mathe)
# Interesse an Mathe\ Mathe.LK   FALSE      TRUE
#             1                  0.2000000  0.8000000
#             2                  0.3333333  0.6666667
#             3                  0.4545455  0.5454545
#             4                  0.4500000  0.5500000
#             5                  0.2800000  0.7200000
#             6                  0.1250000  0.8750000
#             7                  0.0000000  1.0000000
# Hiermit haben wir die relative Haeufigkeit angeben, von Mathe LK ja/nein, die 
# unter der Bedingung, dass das Interesse an Mathematik den Wert i (i = 1,..., 7) annimmt.

#bedingte Verteilung h(Interesse an Mathe|Mathe.LK)
#Mathe.LK\Interesse an Mathe   1          2          3          4          5          6          7
#       FALSE                  0.03030303 0.15151515 0.30303030 0.27272727 0.21212121 0.03030303 0.00000000
#       TRUE                   0.05970149 0.14925373 0.17910448 0.16417910 0.26865672 0.10447761 0.07462687
# Hiermit haben wir die relative Haeufigkeit angegeben, von Interesse an Mathematik mit den
# Werten i (i = 1,..., 7) , die unter der Bedingung, dass Mathe LK ja/nein  annimmt.

# Kontigenzmass
# Cramers_Kontingenzmass korr_Pearsons_Kontingezmass 
# 0.2737681                   0.3734254 
# Kontigenzmass gibt den Zusammenhang der Variablen an. Der Zusammenhang von Mathe
# LK und Interesse an Mathe liegt bei 0.274 (Cramer) bzw. 0,373 (Pearson), also gibt
# es einen mittleren Zusammenhang.

relate_categorial(df$Mathe.LK,df$Interesse.an.Programmieren)
# Kontigenztafel
# Mathe.LK\Interesse an Programmieren  1    2    3    4    5    6    7
#           FALSE                      0.00 0.02 0.01 0.04 0.10 0.04 0.12
#           TRUE                       0.04 0.08 0.13 0.12 0.11 0.13 0.06
# Hier an der Kontigenztafel koennen wir sehen, dass 13% des Datensatzes eine Zusammensetzung
# von Interesse an Programmieren 3 mit Mathe LK ja hat und 0% der Daten eine
# Zusammensetzung von Interesse an Programmieren 1 mit Mathe LK nein hat.

# bedingte Verteilung h(Mathe.LK|Interesse an Programmieren)
# Interesse an Programmieren\ Mathe.LK   FALSE      TRUE
#             1                          0.00000000 1.00000000
#             2                          0.20000000 0.80000000
#             3                          0.07142857 0.92857143
#             4                          0.25000000 0.75000000
#             5                          0.47619048 0.52380952
#             6                          0.23529412 0.76470588
#             7                          0.66666667 0.33333333
# Hiermit haben wir die relative Haeufigkeit angegeben, von Mathe LK ja/nein, die 
# unter der Bedingung, dass das Interesse an Programmieren den Wert i (i=1,...,7) annimmt.

#bedingte Verteilung h(Interesse an Programmieren|Mathe.LK)
#Mathe.LK\Interesse an Programmieren   1          2          3          4          5          6          7
#             FALSE                    0.00000000 0.06060606 0.03030303 0.12121212 0.30303030 0.12121212 0.36363636
#             TRUE                     0.05970149 0.11940299 0.19402985 0.17910448 0.16417910 0.19402985 0.08955224
# Hiermit haben wir die relative Haeufigkeit angegeben, von Interesse an Programmieren mit den 
# Wert i (i = 1,..., 7) , die unter der Bedingung, dass Mathe LK ja/nein annimmt.
#Kontingenzmass
# Cramers_Kontingenzmass korr_Pearsons_Kontingezmass 
# 0.4402062                   0.5697821 
# Kontigenzmass gibt den Zusammenhang der Variablen an. Der Zusammenhang von Mathe
# LK und Interesse an Programmieren liegt bei 0.44 (Cramer) bzw. 0,57 (Pearson), 
# also gibt es einen mittleren Zusammenhang.

quantilKategorisierung(df$Alter)
quantilKategorisierung(df$Interesse.an.Mathematik)
#1-3: niedrig, 4-5: mittel, 6-7: hoch
quantilKategorisierung(df$Interesse.an.Programmieren)
#1-3: niedrig, 4-6: mittel, 7: hoch

relate_categorial(df$Studienfach, df$Interesse.an.Mathematik)
# Data Scientisten und Informatiker haben ein eher geringes Interesse an Mathematik 
# verglichen mit Statistikern  und Mathematikern
relate_categorial(df$Studienfach, df$Interesse.an.Programmieren)
# Data Scientisten haben ein hohes Interesse am Programmieren, Informatiker ein sehr hohes, 
# Statistiker ein mittleres und Mathematiker ein eher niedriges



rangkorr_koeff(df$Interesse.an.Mathematik, df$Interesse.an.Programmieren)
# stark negativer Zusammenhang zwischen diesen Interessen
