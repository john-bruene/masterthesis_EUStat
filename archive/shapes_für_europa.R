library(tidyverse)
library(eurostat)
library(sf)
library(viridis) # Für die Farbpalette

# Laden der Eurostat-Geodaten
SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

# Auswahl der EU28-Länder
EU28 <- eu_countries %>% 
  select(geo = code, name)

# Zusammenführen der Geodaten mit den Länderinformationen
SHP_28 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

# Konvertieren in das richtige CRS (WGS 84)
SHP_28 <- st_transform(SHP_28, crs = 4326)

# Zuordnungstabelle erstellen, um Ländernamen in geo-Codes umzuwandeln
country_codes <- tibble(
  Country = c("Germany", "Portugal", "Luxembourg", "United Kingdom", "France", "Italy", "Netherlands",
              "Denmark", "Ireland", "Spain", "Belgium", "Austria", "Finland", "Sweden", "Greece",
              "Cyprus", "Lithuania", "Poland", "Hungary", "Latvia", "Estonia", "Slovakia", "Malta",
              "Slovenia", "Czech Republic", "Romania", "Bulgaria"),
  geo = c("DE", "PT", "LU", "UK", "FR", "IT", "NL", "DK", "IE", "ES", "BE", "AT", "FI", "SE",
          "GR", "CY", "LT", "PL", "HU", "LV", "EE", "SK", "MT", "SI", "CZ", "RO", "BG")
)

# Daten vorbereiten: transformierte Daten aggregieren nach Land
country_data <- P6 %>%
  group_by(Country) %>%
  summarize(mean_value = mean(Age_At_Start, na.rm = TRUE)) %>%
  left_join(country_codes, by = "Country")  # Länder-Codes hinzufügen

# Zusammenführen der Geodaten mit den aggregierten Werten
map_data <- SHP_28 %>%
  left_join(country_data, by = "geo")

# ggplot Karte erstellen
ggplot(map_data) +
  geom_sf(aes(fill = mean_value), color = "white", size = 0.2) +
  scale_fill_viridis(
    name = "Durchschnittsalter",
    option = "magma",
    direction = -1,
    begin = 0.2, end = 0.9,
    labels = scales::label_number(accuracy = 1)
  ) +
  labs(
    title = "Durchschnittsalter bei Mandatsbeginn in der EU (2016)",
    subtitle = "Durchschnittsalter der Abgeordneten in den EU-Ländern"
  ) +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +  # Zoom auf Europa
  theme_void() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


