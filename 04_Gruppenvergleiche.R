
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




# ============================================================
# 8) One-Way ANOVA (4 Gruppen): support_index ~ Q5.1
# ============================================================
# Ziel: Vergleich des Unterstützungsindex zwischen 4 Gruppen:
# 0 = No, 1 = Yes, father, 2 = Yes, mother, 3 = Yes, both

# Q5.1 sicher als numerisch (falls als Text eingelesen)
guess$Q5_1_num <- suppressWarnings(as.numeric(guess$`Q5.1`))

# 4 Gruppen als Faktor mit klaren Labels (kurz für Grafiken)
guess$Q5_1_4grp <- factor(
  guess$Q5_1_num,
  levels = c(0, 1, 2, 3),
  labels = c("No", "Yes: father", "Yes: mother", "Yes: both")
)

# Daten filtern (NA entfernen)
df_aov <- guess[!is.na(guess$Q5_1_4grp) & !is.na(guess$support_index),
                c("Q5_1_4grp", "support_index")]
df_aov <- as.data.frame(df_aov)
df_aov$Q5_1_4grp <- droplevels(df_aov$Q5_1_4grp)

cat("\n--- Gruppengrößen (ANOVA) ---\n")
print(table(df_aov$Q5_1_4grp))

# Deskriptive Statistik pro Gruppe (n, mean, sd)
n_by4    <- tapply(df_aov$support_index, df_aov$Q5_1_4grp, length)
mean_by4 <- tapply(df_aov$support_index, df_aov$Q5_1_4grp, mean)
sd_by4   <- tapply(df_aov$support_index, df_aov$Q5_1_4grp, sd)

desc_aov <- data.frame(
  Gruppe = names(n_by4),
  n      = as.integer(n_by4),
  mean   = round(as.numeric(mean_by4), 3),
  sd     = round(as.numeric(sd_by4), 3)
)

cat("\n--- Deskriptive Statistik (ANOVA) ---\n")
print(desc_aov)

# ANOVA durchführen
fit_aov <- aov(support_index ~ Q5_1_4grp, data = df_aov)
aov_sum <- summary(fit_aov)

cat("\n--- One-Way ANOVA Ergebnis ---\n")
print(aov_sum)

# p-Wert für Plot-Titel extrahieren
p_aov <- aov_sum[[1]][["Pr(>F)"]][1]

# Post-hoc (TukeyHSD)
tuk <- TukeyHSD(fit_aov)
cat("\n--- Post-hoc (TukeyHSD) ---\n")
print(tuk)

# Tukey in Dataframe (für klare CI-Grafik)
tuk_df <- as.data.frame(tuk$Q5_1_4grp)
tuk_df$comparison <- rownames(tuk_df)
tuk_df$sig <- tuk_df$`p adj` < 0.05


# ============================================================
# 9) Grafiken (optimiert für Lesbarkeit)
# ============================================================

# --- Grafik 1: Boxplot + Jitter (Verteilungen sichtbar) ---
op <- par(no.readonly = TRUE)
par(mar = c(8, 5, 4, 2) + 0.1)

boxplot(
  support_index ~ Q5_1_4grp,
  data = df_aov,
  ylim = c(1, 7),
  main = paste0("support_index nach Q5.1 (ANOVA p = ", signif(p_aov, 3), ")"),
  xlab = "",
  ylab = "Unterstützungsindex (1–7)",
  las  = 2
)

set.seed(42)
stripchart(
  support_index ~ Q5_1_4grp,
  data = df_aov,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  cex = 0.25,
  add = TRUE
)

# --- Grafik 2: TukeyHSD als CI-Dotchart ---

ord <- order(tuk_df$diff)
par(mar = c(8, 8, 4, 2) + 0.1)
ypos <- seq_along(ord)

plot(
  tuk_df$diff[ord], ypos,
  xlim = range(c(tuk_df$lwr, tuk_df$upr)),
  pch = 16,
  yaxt = "n",
  xlab = "Differenz der Mittelwerte (A - B)",
  ylab = "",
  main = "TukeyHSD: Paarvergleiche (95%-KI)"
)
axis(2, at = ypos, labels = tuk_df$comparison[ord], las = 2)
segments(tuk_df$lwr[ord], ypos, tuk_df$upr[ord], ypos)
abline(v = 0, lty = 2)

text(
  x = par("usr")[2],
  y = ypos,
  labels = ifelse(tuk_df$sig[ord], "sig", "n.s."),
  pos = 2,
  cex = 0.9
)

par(op[names(op) != "pin"])



# Interpretation (ANOVA + TukeyHSD):
# Die einfaktorielle ANOVA zeigt, dass sich der support_index zwischen den vier Q5.1-Gruppen insgesamt unterscheidet (p < 0.001).
# Die Tukey-Paarvergleiche präzisieren: „Yes: father“ und „Yes: both“ liegen signifikant über „No“ (95%-KI der Differenz komplett > 0).
# Zusätzlich ist „Yes: both“ signifikant höher als „Yes: mother“ (p_adj < 0.01).
# Die übrigen Vergleiche sind nicht signifikant (Konfidenzintervalle schneiden 0 → n.s.).
# Insgesamt deutet das auf tendenziell höhere Unterstützungswerte hin, wenn (v. a. beide) Elternteile selbstständig sind.

