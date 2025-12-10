library(openxlsx)

# 1) Daten einlesen
guess <- read.xlsx("GUESSS.xlsx", sheet = 1)

# 2) Q5.1 als dichotome Variable (0/1)
guess$Q5_1_dicho <- ifelse(
  guess$`Q5.1` == 0, 0,
  ifelse(guess$`Q5.1` %in% c(1, 2, 3), 1, NA)
)

# Faktor mit Labels für Grafiken
guess$Q5_1_dicho_f <- factor(
  guess$Q5_1_dicho,
  levels = c(0, 1),
  labels = c("kein Elternteil selbstständig", "mind. ein Elternteil selbstständig")
)

# 3) Unterstützungsindex (wie in Aufgabe 2)
guess$support_index <- rowMeans(
  data.frame(
    as.numeric(guess$`Q6.2_1`),
    as.numeric(guess$`Q6.2_2`),
    as.numeric(guess$`Q6.2_3`)
  ),
  na.rm = TRUE
)
# 4) Histogramm des Unterstützungsindex
hist(
  guess$support_index,
  breaks = 20,
  main   = "Histogramm des Unterstützungsindex",
  xlab   = "Unterstützungsindex (1–7)",
  ylab   = "Anzahl Studierende"
)

# 5) Boxplot: Unterstützungsindex nach Selbstständigkeit der Eltern
boxplot(
  support_index ~ Q5_1_dicho_f,
  data = guess,
  main = "Unterstützungsindex nach Selbstständigkeit der Eltern",
  xlab = "Selbstständigkeit der Eltern",
  ylab = "Unterstützungsindex"
)

# 6) Kurze Interpretation (nur Kommentare)
# - Histogramm: Werte konzentrieren sich im Bereich ca. 4–7, Peak um 5–6.
#   -> Studierende erwarten eher hohe Unterstützung.
# - Boxplot: Gruppe mit mind. einem selbstständigen Elternteil zeigt
#   einen leicht höheren Median im Unterstützungsindex (falls der Plot das zeigt),
#   die Unterschiede sind jedoch visuell moderat.



