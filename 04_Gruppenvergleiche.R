
#  t-Test 
# Vergleich: support_index (Q6.2_1–Q6.2_3) nach
#            Selbstständigkeit der Eltern (Q5.1 -> dichotom)


library(openxlsx)

# -----------------------------
# 1) Daten einlesen
# -----------------------------
guess <- read.xlsx("GUESSS.xlsx", sheet = 1)

# -----------------------------
# 2) Gruppenvariable (Q5.1) dichotomisieren (0 vs 1/2/3)
# -----------------------------
# 0 = kein Elternteil selbstständig
# 1/2/3 = mind. ein Elternteil selbstständig
guess$Q5_1_dicho <- ifelse(
  guess$`Q5.1` == 0, 0,
  ifelse(guess$`Q5.1` %in% c(1, 2, 3), 1, NA)
)

guess$Q5_1_dicho_f <- factor(
  guess$Q5_1_dicho,
  levels = c(0, 1),
  labels = c("Kein Elternteil", "Mind. 1 Elternteil")
)

# -----------------------------
# 3) Unterstützungsindex berechnen (Mittelwert der 3 Items)
# -----------------------------
guess$support_index <- rowMeans(
  data.frame(
    as.numeric(guess$`Q6.2_1`),
    as.numeric(guess$`Q6.2_2`),
    as.numeric(guess$`Q6.2_3`)
  ),
  na.rm = TRUE
)

# -----------------------------
# 4) Daten für t-Test vorbereiten (NA entfernen)
# -----------------------------
df_test <- guess[!is.na(guess$Q5_1_dicho_f) & !is.na(guess$support_index),
                 c("Q5_1_dicho_f", "support_index")]
df_test <- as.data.frame(df_test)
df_test$Q5_1_dicho_f <- droplevels(df_test$Q5_1_dicho_f)

# -----------------------------
# 5) Deskriptive Statistik (n, mean, sd) pro Gruppe
# -----------------------------
n_by    <- tapply(df_test$support_index, df_test$Q5_1_dicho_f, length)
mean_by <- tapply(df_test$support_index, df_test$Q5_1_dicho_f, mean)
sd_by   <- tapply(df_test$support_index, df_test$Q5_1_dicho_f, sd)

desc <- data.frame(
  Gruppe = names(n_by),
  n      = as.integer(n_by),
  mean   = as.numeric(mean_by),
  sd     = as.numeric(sd_by)
)

cat("\n--- Deskriptive Statistik pro Gruppe ---\n")
print(desc)

# Optional: Deskriptive Tabelle als CSV speichern (für Report)
# write.csv(desc, "ttest_deskriptiv.csv", row.names = FALSE)

# -----------------------------
# 6) t-Test durchführen (Welch t-Test)
# -----------------------------
# Welch ist Standard (robust bei ungleichen Varianzen)
tt <- t.test(support_index ~ Q5_1_dicho_f, data = df_test, var.equal = FALSE)

cat("\n--- Welch t-Test Ergebnis ---\n")
print(tt)

# Optional: Kerndaten kompakt ausgeben (p, t, df)
cat("\n--- Kurzfassung ---\n")
cat("t =", round(tt$statistic, 3),
    "| df =", round(tt$parameter, 2),
    "| p =", signif(tt$p.value, 4), "\n")
cat("Mittelwerte:", round(tt$estimate[1], 3), "vs", round(tt$estimate[2], 3), "\n")
cat("95%-KI (Differenz): [", round(tt$conf.int[1], 3), ",", round(tt$conf.int[2], 3), "]\n")

# -----------------------------
# Zusammenfassung
# -----------------------------
# Welch-t-Test: signifikanter Unterschied im Unterstützungsindex (p < 2.2e-16).
# Gruppe „Mind. 1 Elternteil“ hat höheren Mittelwert als „Kein Elternteil“ (5.62 vs. 5.42),
# 95%-KI der Differenz (Kein − Mind.1): [-0.251; -0.158].



# ============================================================
# 7) Wilcoxon Rangsummentest (Mann–Whitney U)
# ============================================================
# Identisch bleibt:
# - Daten einlesen
# - Gruppenvariable (Q5.1 -> 2 Gruppen)
# - support_index berechnen
# - NA filtern (df_test)
# Ändert sich:
# - statt t-Test wird Wilcoxon Rangsummentest genutzt (robust, keine Normalverteilung nötig)

wt <- wilcox.test(
  support_index ~ Q5_1_dicho_f,
  data = df_test,
  exact = FALSE,
  conf.int = TRUE
)

cat("\n--- Wilcoxon Rangsummentest Ergebnis ---\n")
print(wt)

# Zusammenfassung (kurz)
# Wilcoxon: signifikanter Unterschied, wenn p < 0.05.
# Teststatistik W wird ausgegeben; Interpretation bezieht sich auf Lage/Verteilung.

# Interpretation (Wilcoxon):
# Der Wilcoxon-Rangsummentest zeigt einen statistisch signifikanten Unterschied
# im Unterstützungsindex zwischen den beiden Gruppen (W = 9348404, p < 2.2e-16).
# Das 95%-Konfidenzintervall für die Lageverschiebung liegt bei [-0.333; -0.00008]
# und schließt 0 nicht ein -> Hinweis auf einen Unterschied in der Lage/Verteilung.

