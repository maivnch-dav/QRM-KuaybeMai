library(openxlsx)

# --- 1) Daten einlesen
guess <- read.xlsx("GUESSS.xlsx", sheet = 1)

# --- 2) Q5.1 als dichotom (0/1)
guess$Q5_1_dicho <- ifelse(
  guess$`Q5.1` == 0, 0,
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
