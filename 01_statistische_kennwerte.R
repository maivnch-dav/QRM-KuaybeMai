library(openxlsx)

# Datei einlesen
guess <- read.xlsx("GUESSS.xlsx", sheet = 1)

# Überblick über die Datenstruktur
# ----------------------------------------------------------
str(guess)
summary(guess)


# Dichotomisierung
guess$Q5_1_dicho <- ifelse(
  guess$`Q5.1` == 0, 
  0,
  ifelse(guess$`Q5.1` %in% c(1, 2, 3), 1, NA)
)



# --- 3) Unterstützungsindex sicher numerisch
guess$support_index <- rowMeans(
  data.frame(
    as.numeric(guess$`Q6.2_1`),
    as.numeric(guess$`Q6.2_2`),
    as.numeric(guess$`Q6.2_3`)
  ),
  na.rm = TRUE
)

# --- 4) Funktionen
mode_stat <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

kennwerte <- function(x) {
  x_num <- as.numeric(x)
  c(
    n      = sum(!is.na(x_num)),
    mean   = mean(x_num, na.rm = TRUE),
    median = median(x_num, na.rm = TRUE),
    mode   = mode_stat(x_num),
    var    = var(x_num, na.rm = TRUE),
    sd     = sd(x_num, na.rm = TRUE)
  )
}

# --- 5) Kennwerte berechnen
kennwerte(guess$Q5_1_dicho)
kennwerte(guess$support_index)

<<<<<<< HEAD
#MAI -> Varianz und Standardabweichung

=======
# nominale Variable Q5.1
table(guess$`Q5.1`)
prop.table(table(guess$`Q5.1`))
  
>>>>>>> c53901550f64ecccf2913d6f79643dfe7da55990
