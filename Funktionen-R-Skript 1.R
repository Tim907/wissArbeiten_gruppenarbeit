
# Bearbeitung von Aufgabe 3

# Falls Probleme beim Laden, probiert: Session/Set Working Directory/To Source File Location
source("Funktionen-R-Skript 2.R")

# todo (a)

# todo (b)

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
  predictions = model1$fitted.values
  info = summary(model)
  return(list(prediction = predictions, summary = summary(model)))
}
# data = read.csv2("person.csv")
# correlation = cor_metric_dicho(data$Interesse.an.Mathematik,
#   ifelse(data$Mathe.LK == "ja", 1, 0))

# todo (e)

# todo (f)