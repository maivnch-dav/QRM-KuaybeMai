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

# Create frequency table
freq <- table(guess$Q5_1_dicho_f)

# Create frequency table
freq <- table(guess$Q5_1_dicho_f)

# Create percentage labels
pct <- round(freq / sum(freq) * 100, 1)

# Combine label + percentage
labels <- paste0(names(freq), " (", pct, "%)")

# Pie chart with labels
pie(
  freq,
  labels = labels,
  main = "Selbstständigkeit der Eltern",
  col = c("lightblue", "orange")
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

#round index to integer
guess$support_index_rounded <- round(guess$support_index)

#factor with labels for visualising
guess$support_index_f <- factor(
  guess$support_index_rounded,
  levels = 1:7,
  labels = c("Stark ablehnen",
             "Ablehnen",
             "Eher ablehnen",
             "Neutral",
             "Eher zustimmen",
             "Zustimmen",
             "Stark zustimmen")
)

# frequency table
table(guess$support_index_f)
# with percentage
prop.table(table(guess$support_index_f)) * 100



# 4) Histogramm des Unterstützungsindex
hist(
  guess$support_index,
  breaks = 20,
  main   = "Histogramm des Unterstützungsindex",
  xlab   = "Unterstützungsindex (1–7)",
  ylab   = "Anzahl Studierende"
)



# Frequency table
freq <- table(guess$support_index_f)



table(guess$support_index_f, useNA = "always")


# Bar plot
barplot(
  freq,
  main = "Verteilung des Unterstützungsindex",
  xlab = "Antwortkategorie",
  ylab = "Anzahl Studierende",
  col  = rainbow(length(freq)),
  las  = 2      # makes labels vertical for better readability
)



# 6) Kurze Interpretation (nur Kommentare)
# - Histogramm: Werte konzentrieren sich im Bereich ca. 4–7, Peak um 5–6.
#   -> Studierende erwarten eher hohe Unterstützung.
# - Boxplot: Gruppe mit mind. einem selbstständigen Elternteil zeigt
#   einen leicht höheren Median im Unterstützungsindex (falls der Plot das zeigt),
#   die Unterschiede sind jedoch visuell moderat.



