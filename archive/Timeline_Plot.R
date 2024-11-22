# Load Required Libraries
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

# Optional: Use Google Fonts
font_add_google("Lato")
showtext_auto()

# Load Data
P6 <- read.csv("data/P6_umap_scores_red_NEW.csv")
P7 <- read.csv("data/P7_umap_scores_red_NEW.csv")
P8 <- read.csv("data/P8_umap_scores_red_NEW.csv")
P9 <- read.csv("data/P9_umap_scores_red_NEW.csv")

# Add Legislature column to each dataset
P6$Legislature <- "P6"
P7$Legislature <- "P7"
P8$Legislature <- "P8"
P9$Legislature <- "P9"

# Combine datasets
combined_data <- bind_rows(P6, P7, P8, P9)

# Filter for Attendance_Score > 0.8
combined_data <- combined_data %>%
  filter(Attendance_Score > 0.8)

# Mapping Table with Similar Colors
epg_mapping <- tribble(
  ~Legislature, ~EPG, ~EPG_ID, ~Color, ~Axis_Label,
  "P6", "Group of the European People's Party (Christian Democrats) and European Democrats", "EPP", "#0000FF", "EPP",
  "P7", "Group of the European People's Party (Christian Democrats)", "EPP", "#0000FF", "EPP",
  "P8", "Group of the European People's Party (Christian Democrats)", "EPP", "#0000FF", "EPP",
  "P9", "EPP", "EPP", "#0000FF", "EPP",
  "P6", "Socialist Group in the European Parliament", "S&D", "#FF0000", "Socialists & Democrats",
  "P7", "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament", "S&D", "#FF0000", "Socialists & Democrats",
  "P8", "Group of the Progressive Alliance of Socialists and Democrats in the European Parliament", "S&D", "#FF0000", "Socialists & Democrats",
  "P9", "Socialists_Democrats", "S&D", "#FF0000", "Socialists & Democrats",
  "P6", "Group of the Alliance of Liberals and Democrats for Europe", "Liberals", "#FFD700", "Liberals",
  "P7", "Group of the Alliance of Liberals and Democrats for Europe", "Liberals", "#FFD700", "Liberals",
  "P8", "Group of the Alliance of Liberals and Democrats for Europe", "Liberals", "#FFD700", "Liberals",
  "P9", "REG", "Liberals", "#FFD700", "Liberals",
  "P6", "Group of the Greens/European Free Alliance", "Greens/EFA", "#008000", "Greens/EFA",
  "P7", "Group of the Greens/European Free Alliance", "Greens/EFA", "#008000", "Greens/EFA",
  "P8", "Group of the Greens/European Free Alliance", "Greens/EFA", "#008000", "Greens/EFA",
  "P9", "Greens_EFA", "Greens/EFA", "#008000", "Greens/EFA",
  "P6", "Union for Europe of the Nations Group", "Far-right", "#8B4513", "Far-right",
  "P7", "Europe of Freedom and Democracy Group", "Far-right", "#8B4513", "Far-right",
  "P8", "Europe of Nations and Freedom Group", "Far-right", "#8B4513", "Far-right",
  "P9", "IDG", "Far-right", "#8B4513", "Far-right",
  "P6", "Confederal Group of the European United Left - Nordic Green Left", "The Left", "#800000", "The Left",
  "P7", "Confederal Group of the European United Left - Nordic Green Left", "The Left", "#800000", "The Left",
  "P8", "Confederal Group of the European United Left - Nordic Green Left", "The Left", "#800000", "The Left",
  "P9", "The Left", "The Left", "#800000", "The Left",
  "P6", "Non-attached Members", "Non-attached", "#808080", "Non-attached",
  "P7", "Non-attached Members", "Non-attached", "#808080", "Non-attached",
  "P8", "Non-attached Members", "Non-attached", "#808080", "Non-attached",
  "P9", "NI", "Non-attached", "#808080", "Non-attached"
)

# Apply Mapping
combined_data <- combined_data %>%
  left_join(epg_mapping, by = c("Legislature", "EPG")) %>%
  filter(!is.na(EPG_ID))

# Calculate average scores for EPGs in P6 and P9
epg_percentages <- combined_data %>%
  filter(Legislature %in% c("P6", "P9")) %>%
  group_by(Legislature, EPG_ID) %>%
  summarise(Average_Score = mean(loyalty_score, na.rm = TRUE), .groups = "drop") %>%
  spread(Legislature, Average_Score, fill = 0) %>%
  rename(Start_Average = P6, End_Average = P9)

epg_scores <- combined_data %>%
  filter(Legislature %in% c("P6", "P9")) %>%
  group_by(Legislature, EPG_ID, Axis_Label) %>%
  summarise(Average_Score = mean(loyalty_score, na.rm = TRUE), .groups = "drop")

epg_scores <- epg_scores %>%
  left_join(epg_percentages, by = "EPG_ID")

# Ensure epg_colors is correctly set
epg_colors <- epg_mapping %>%
  select(EPG_ID, Color) %>%
  distinct() %>%
  deframe()

# Plot the data
plot <- ggplot(epg_scores, aes(x = Legislature, y = Average_Score, group = EPG_ID, color = EPG_ID)) +
  geom_line(size = 1.5) +
  geom_label_repel(
    data = epg_scores %>% filter(Legislature == "P6"),
    aes(label = round(Start_Average, 2)),
    nudge_x = -0.3,
    hjust = 1,
    size = 5,
    family = "Lato",
    box.padding = 0.6,
    point.padding = 0.4,
    direction = "y",
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = epg_scores %>% filter(Legislature == "P9"),
    aes(label = round(End_Average, 2)),
    nudge_x = 0.3,
    hjust = 0,
    size = 5,
    family = "Lato",
    box.padding = 0.6,
    point.padding = 0.4,
    direction = "y",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = epg_scores %>% filter(Legislature == "P9"),
    aes(label = Axis_Label),
    nudge_x = 0.5,
    size = 5,
    family = "Lato",
    max.overlaps = Inf,
    direction = "both",
    box.padding = 0.5
  ) +
  scale_color_manual(values = epg_colors) +
  labs(
    title = "Political Group Trends in the European Parliament",
    subtitle = "Loyalty score trends with start and end averages for each group",
    x = "Legislative Period",
    y = "Loyalty Score",
    caption = "Data Source: European Parliament Voting Data"
  ) +
  theme_minimal(base_size = 18, base_family = "Lato") +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18),
    legend.position = "none"
  )

plot

# Save the plot
ggsave(
  filename = "epg_trends_with_better_label_placement.png",
  plot = plot,
  width = 1920 / 100, 
  height = 1080 / 100, 
  dpi = 100
)

ggsave(
  filename = "epg_trends_with_label_boxes.pdf",
  plot = plot,
  width = 1920 / 100,
  height = 1080 / 100,
  dpi = 100,
  device = "pdf"
)
