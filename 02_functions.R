############################################################
# 02_functions.R
# Zweck: Sammlung aller Hilfsfunktionen für das Projekt
# Autor: Mai Nguyen (oder dein Name)
# Projekt: Quantitative Research Project mit R
############################################################

library(dplyr)
library(ggplot2)

# ----------------------------------------------------------
# 1. Funktion: Deskriptivstatistik für numerische Variablen
# ----------------------------------------------------------

describe_numeric <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
  
  result <- list(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    skewness = mean((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / sd(x, na.rm = TRUE)^3
  )
  
  return(result)
}

# ----------------------------------------------------------
# 2. Funktion: Deskriptivstatistik für kategoriale Variablen
# ----------------------------------------------------------

describe_categorical <- function(x) {
  if (!is.factor(x) && !is.character(x)) stop("Input must be a factor or character variable")
  
  result <- table(x, useNA = "ifany")
  return(result)
}

# ----------------------------------------------------------
# 3. Funktion: Normalverteilungstest (Shapiro-Wilk)
# Hinweis: nur für n <= 5000 empfohlen
# ----------------------------------------------------------

test_normality <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) stop("Not enough data for test")
  
  shapiro.test(x)
}

# ----------------------------------------------------------
# 4. Funktion: Outlier Detection (IQR-Methode)
# ----------------------------------------------------------

detect_outliers <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  outliers <- which(x < lower_bound | x > upper_bound)
  
  return(list(
    outlier_indices = outliers,
    outlier_values = x[outliers],
    lower_bound = lower_bound,
    upper_bound = upper_bound
  ))
}

# ----------------------------------------------------------
# 5. Funktion: Recode von Likert-Skalen
# Beispiel: 1–7 Skala invertieren (7 -> 1, 6 -> 2 ...)
# ----------------------------------------------------------

reverse_likert <- function(x, max_value = 7) {
  if (!is.numeric(x)) stop("Input must be numeric")
  
  return(max_value + 1 - x)
}

# ----------------------------------------------------------
# 6. Funktion: Automatisierter Histogram-Plot
# ----------------------------------------------------------

plot_histogram <- function(data, var, bins = 30) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = bins, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(
      title = paste("Histogram of", var),
      x = var,
      y = "Frequency"
    )
}

# ----------------------------------------------------------
# 7. Funktion: Automatisierter Boxplot (numerische Variable)
# ----------------------------------------------------------

plot_box <- function(data, var) {
  ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "orange") +
    theme_minimal() +
    labs(
      title = paste("Boxplot of", var),
      y = var
    )
}

# ----------------------------------------------------------
# 8. Funktion: Häufigkeitstabelle als Data Frame
# ----------------------------------------------------------

freq_table <- function(x) {
  as.data.frame(table(x, useNA = "ifany"))
}

# ----------------------------------------------------------
# 9. Funktion: Prozentwerte für kategoriale Variablen
# ----------------------------------------------------------

percent_table <- function(x) {
  tab <- prop.table(table(x, useNA = "ifany")) * 100
  return(round(tab, 2))
}

# ----------------------------------------------------------
# 10. Funktion: Korrelationstest zwischen zwei numerischen Variablen
# ----------------------------------------------------------

cor_test_numeric <- function(x, y) {
  if (!is.numeric(x) | !is.numeric(y)) stop("Both inputs must be numeric")
  
  return(cor.test(x, y))
}

############################################################
# Ende der Datei
############################################################
