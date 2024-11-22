library(umap)
library(dplyr)
library(ggplot2)
library(gridExtra)


P6 <- read.csv("data/P6_nominate_scores.csv")[, -1]
P7 <- read.csv("data/P7_nominate_scores.csv")[, -1]
P8 <- read.csv("data/P8_nominate_scores.csv")[, -1]
P9 <- read.csv("data/P9_nominate_scores.csv")[, -1]

EP6_9_Voted <- read_excel("data/EP6_9_Voted_docs_new_datesfixed.xlsx")

# Define party colors for each period
party_colors_list <- list(
  "P6" = c(
    "Group of the European People's Party (Christian Democrats) and European Democrats" = "blue",
    "Confederal Group of the European United Left - Nordic Green Left" = "darkred",
    "Non-attached Members" = "grey",
    "Union for Europe of the Nations Group" = "brown",
    "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
    "Group of the Greens/European Free Alliance" = "green",
    "Socialist Group in the European Parliament" = "red",
    "Independence/Democracy Group" = "lightblue"
  ),
  "P7" = c(
    "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
    "Group of the European People's Party (Christian Democrats)" = "blue",
    "Europe of Freedom and Democracy Group" = "lightblue",
    "Non-attached Members" = "grey", 
    "Group of the Greens/European Free Alliance" = "green",
    "European Conservatives and Reformists Group" = "darkblue",
    "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "red",
    "Confederal Group of the European United Left - Nordic Green Left" = "darkred"
  ),
  "P8" = c(
    "Group of the Alliance of Liberals and Democrats for Europe" = "gold",
    "Non-attached Members" = "grey",
    "Group of the European People's Party (Christian Democrats)" = "blue",
    "European Conservatives and Reformists Group" = "darkblue",
    "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament" = "red",
    "Group of the Greens/European Free Alliance" = "green",
    "Europe of Freedom and Direct Democracy Group" = "lightblue",
    "Europe of Nations and Freedom Group" = "purple",
    "Confederal Group of the European United Left - Nordic Green Left" = "darkred"
  ),
  "P9" = c(
    "REG" = "gold",
    "Socialists_Democrats" = "red",
    "EPP" = "blue",
    "Greens_EFA" = "green",
    "ECR" = "darkblue",
    "NI" = "grey",
    "The Left" = "darkred",
    "IDG" = "purple",
    "Non-attached Members" = "lightblue"
  )
)



# Nur Vote_IDs mit Legislature == 6 und final_vote == 1 auswählen
relevant_votes <- EP6_9_Voted %>%
  filter(Legislature == 6) %>%
  pull(Vote_ID)

# Nur die Spalten im P9-Datensatz auswählen, die den relevanten Vote_IDs entsprechen
relevant_columns <- paste0("X", relevant_votes)
P6_filtered <- P6 %>% select(all_of(relevant_columns), EPG)

# Dynamische Filterung basierend auf Fehlzeiten-Schwelle (in Prozent)
fehlzeiten_schwelle <- 13  # Hier den gewünschten Schwellenwert in Prozent anpassen

filter_mep_by_absence <- function(data, threshold) {
  data %>%
    rowwise() %>%
    mutate(absence_percentage = sum(c_across(starts_with("X")) == 0) / length(c_across(starts_with("X"))) * 100) %>%
    ungroup() %>%
    filter(absence_percentage <= threshold) %>%
    select(-absence_percentage)  # Optional: Entferne die Hilfsspalte
}

# Filtere Personen basierend auf dem Schwellenwert
P6_filtered <- filter_mep_by_absence(P6_filtered, fehlzeiten_schwelle)

# One-Hot Encoding auf die gefilterten Daten anwenden
P6_encoded <- P6_filtered %>%
  mutate(across(all_of(relevant_columns), as.factor)) %>%  # Als Faktoren umwandeln für One-Hot Encoding
  model.matrix(~ . - 1, data = .) %>%  # One-Hot Encoding durchführen, "-1" entfernt den Intercept
  as.data.frame()

# EPG-Spalte wieder hinzufügen
P6_encoded$EPG <- P6_filtered$EPG

# Define UMAP configuration
umap_config <- umap.defaults
umap_config$n_neighbors <- 15   # Adjust this value
umap_config$min_dist <- 0.1     # Adjust this value
umap_config$metric <- 'euclidean'  # Try 'cosine', 'manhattan', etc.

# Apply UMAP with custom settings
umap_results <- umap(P6_encoded %>% select(-EPG), config = umap_config)

# UMAP-Ergebnisse in ein DataFrame konvertieren
umap_data <- as.data.frame(umap_results$layout)
colnames(umap_data) <- c("UMAP1", "UMAP2")

# Die EPG-Spalte zu den UMAP-Daten hinzufügen
umap_data$EPG <- P6_encoded$EPG

# Farben für EPG festlegen
party_colors <- party_colors_list[["P6"]]


# UMAP-Ergebnisse mit EPG als Farbe visualisieren
ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = EPG)) +
  geom_point() +
  labs(title = "UMAP Clustering Darstellung mit EPG-Farben", x = "UMAP Dimension 1", y = "UMAP Dimension 2") +
  theme_minimal() +
  scale_color_manual(values = party_colors)




n_neighbors_values <- c(15, 100, 200)
min_dist_values <- c(0.05, 0.1)
plots <- list()

for (n in n_neighbors_values) {
  for (d in min_dist_values) {
    umap_config <- umap.defaults
    umap_config$n_neighbors <- n
    umap_config$min_dist <- d
    umap_config$input <- "dist"
    
    umap_results <- umap(as.matrix(gower_dist), config = umap_config)
    umap_data <- as.data.frame(umap_results$layout)
    colnames(umap_data) <- c("UMAP1", "UMAP2")
    umap_data$EPG <- P6_encoded$EPG
    
    p <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = EPG)) +
      geom_point(size = 1) +
      labs(title = paste("n_neighbors =", n, ", min_dist =", d)) +
      theme_minimal() +
      scale_color_manual(values = party_colors) +
      theme(legend.position = "none")
    
    plots[[length(plots) + 1]] <- p
  }
}

# Arrange plots in a grid
do.call("grid.arrange", c(plots, ncol = length(min_dist_values)))

