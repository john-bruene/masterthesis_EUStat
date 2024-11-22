library(cluster)
library(umap)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)

P6 <- read.csv("data/P6_nominate_scores.csv")
P7 <- read.csv("data/P7_umap_scores.csv")
P8 <- read.csv("data/P8_umap_scores.csv")
P9 <- read.csv("data/P9_umap_scores.csv")


P8[, c("UMAP1", "UMAP2")]

summary(P6[, c("Attendance_Score", "Activity_Index")])



# Filtere Personen basierend auf dem Schwellenwert

EP6_9_Voted <- read_excel("data/EP6_9_Voted_docs_new_datesfixed.xlsx")





# Dynamische Filterung basierend auf Fehlzeiten-Schwelle (in Prozent)
fehlzeiten_schwelle <- 15  # Hier den gewünschten Schwellenwert anpassen

filter_mep_by_absence <- function(data, threshold) {
  data %>%
    rowwise() %>%
    mutate(absence_percentage = sum(c_across(starts_with("X")) == 0) / length(c_across(starts_with("X"))) * 100) %>%
    ungroup() %>%
    filter(absence_percentage <= threshold) %>%
    select(-absence_percentage)  # Optional: Entferne die Hilfsspalte
}

# Filtere Abgeordnete basierend auf dem Schwellenwert
P6_filtered <- filter_mep_by_absence(P6, fehlzeiten_schwelle)

# Nur Vote_IDs mit Legislature == 6 auswählen
relevant_votes <- EP6_9_Voted %>%
  filter(Legislature == 6) %>%
  filter(final_vote == 1) %>%
  pull(Vote_ID)


# Nur die Spalten im P6-Datensatz auswählen, die den relevanten Vote_IDs entsprechen
relevant_columns <- paste0("X", relevant_votes)
P6_filtered <- P6_filtered %>% select(all_of(relevant_columns), EPG, FullName)


# Vorbereitung der Daten für die MCA
# Konvertiere alle Abstimmungsvariablen in Faktoren
df <- P6_filtered %>%
  select(-EPG) %>%
  select(-FullName) %>%
  mutate(across(all_of(relevant_columns), as.factor))

#############################################################################


# Berechnen der Gower-Distanzmatrix
gower_dist <- daisy(df, metric = "gower")


# UMAP-Konfiguration
# UMAP Configuration
umap_config <- umap.defaults
umap_config$input <- "dist"
umap_config$n_neighbors <- 40
umap_config$min_dist <- 0.5
umap_config$spread <- 1

# Anwendung von UMAP auf die Distanzmatrix
umap_results <- umap(as.matrix(gower_dist), config = umap_config)

# Vorbereitung der Daten für die Visualisierung
umap_data <- as.data.frame(umap_results$layout)
colnames(umap_data) <- c("UMAP1_red", "UMAP2_red")
umap_data$EPG <- P6_filtered$EPG
umap_data$FullName <- P6_filtered$FullName

# Visualisierung
ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = EPG)) +
  geom_point() +
  labs(title = "UMAP mit Gower-Distanz", x = "UMAP1", y = "UMAP2") +
  theme_minimal() +
  scale_color_manual(values = party_colors_list[["P6"]])

#füge UMAP Koordinaten zu P6 hinzu

# Nehme an, dass 'FullName' die gemeinsame ID-Spalte ist
umap_data_filtered <- umap_data %>% filter(FullName %in% P6$FullName)

# Abgleichen der Reihenfolge basierend auf FullName, um die Reihenfolge beizubehalten
P6 <- P6 %>%
  left_join(umap_data_filtered %>% select(FullName, UMAP1_red, UMAP2_red), by = "FullName")

######################################################################################


# Durchführung der MCA
mca_result <- MCA(df, ncp = 2, graph = FALSE)

# Extrahieren der Koordinaten der Individuen
ind_coords <- as.data.frame(mca_result$ind$coord)



#rename columns
colnames(ind_coords) <- c("MCA1_red", "MCA2_red")

ind_coords$EPG <- P6_filtered$EPG
ind_coords$FullName <- P6_filtered$FullName



# Plotten der Individuen mit ggplot2
ggplot(ind_coords, aes(x = MCA1, y = MCA2, color = EPG)) +
  geom_point(size = 2) +
  labs(title = "MCA der Abgeordneten basierend auf dem Abstimmungsverhalten",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal() +
  scale_color_manual(values = party_colors_list[["P6"]])


# Nehme an, dass 'FullName' die gemeinsame ID-Spalte ist
mca_data_filtered <- ind_coords %>% filter(FullName %in% P6$FullName)

# Abgleichen der Reihenfolge basierend auf FullName, um die Reihenfolge beizubehalten
P6 <- P6 %>%
  left_join(mca_data_filtered %>% select(FullName, MCA1_red, MCA2_red), by = "FullName")


#save P6 as csv

write.csv(P6, "data/P6_umap_scores_red.csv")




