# plot

library(wnominate)

# show wnominate results

load("archive/wnom/wnominate_results2.rds")

wnominate_results <- wnominate_result_P6


P6_results <- wnominate_results$"6"

P7_results <- wnominate_results$"7"

P8_results <- wnominate_results2$"8"

P9_results <- wnominate_results2$"9"

summary(result_P6)

summary(result_P7)

summary(result_P8)

summary(result_P9)

plot(result_P8, type = "commonspace", main = "Parliament 6", xlab = "First Dimension", ylab = "Second Dimension")

plot(P9_results, type = "commonspace", main = "Parliament 6", xlab = "First Dimension", ylab = "Second Dimension")


# Ergebnisse in Textdateien speichern
capture.output(summary(result_P6), file = "P6_results_summary_red.txt")
capture.output(summary(result_P7), file = "P7_results_summary_red.txt")
capture.output(summary(result_P8), file = "P8_results_summary_red.txt")
capture.output(summary(result_P9), file = "P9_results_summary_red.txt")


# Erstellen eines Datenrahmens für die Tabelle
comparison_table <- data.frame(
  Legislature = c("Parliament 6", "Parliament 7", "Parliament 8", "Parliament 9"),
  
  # Vollständiger Datensatz
  Full_Legislators = c(930, 765, 820, 763),
  Full_Votes = c(5061, 6041, 9671, 11521),
  Full_Accuracy = c(89.07, 90.42, 87.23, 90.76),
  Full_APRE = c(0.534, 0.567, 0.477, 0.604),
  Full_GMP = c(0.737, 0.766, 0.717, 0.773),
  
  # Reduzierter Datensatz
  Reduced_Legislators = c(928, 809, 831, 785),
  Reduced_Votes = c(905, 1394, 1935, 990),
  Reduced_Accuracy = c(92.03, 92.78, 91.08, 92.31),
  Reduced_APRE = c(0.3, 0.41, 0.472, 0.487),
  Reduced_GMP = c(0.832, 0.84, 0.817, 0.824)
)

# Generieren des LaTeX-Codes
library(knitr)
library(kableExtra)

latex_table <- comparison_table %>%
  kable("latex", booktabs = TRUE, align = "c", col.names = c(
    "Legislature", 
    "Legislators", "Votes", "Accuracy (%)", "APRE", "GMP",
    "Legislators", "Votes", "Accuracy (%)", "APRE", "GMP"
  )) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" " = 1, "Full Dataset" = 5, "Reduced Dataset" = 5))

# LaTeX-Code anzeigen
cat(latex_table, file = "comparison_table.tex")




