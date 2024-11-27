library(readxl)
library(dplyr)
library(tidyr)
library(jsonlite)


# Laden der JSON-Datei
json_data <- fromJSON("ep_mep_activities.json", simplifyVector = FALSE)

# Daten in ein formatiertes Dataframe umwandeln
extract_data <- function(data) {
  do.call(rbind, lapply(data, function(item) {
    if (!is.null(item$CRE)) {
      data.frame(
        mep_id = item$mep_id,
        url = sapply(item$CRE, function(cre) paste(cre$url, collapse=", ")),
        date = sapply(item$CRE, function(cre) paste(cre$date, collapse=", ")),
        date_type = sapply(item$CRE, function(cre) paste(cre$`date-type`, collapse=", ")),
        reference = sapply(item$CRE, function(cre) paste(cre$reference, collapse=", ")),
        title = sapply(item$CRE, function(cre) paste(cre$title, collapse=", ")),
        dossiers = sapply(item$CRE, function(cre) if(!is.null(cre$dossiers)) paste(unlist(cre$dossiers), collapse=", ") else NA),
        term = sapply(item$CRE, function(cre) paste(cre$term, collapse=", ")),
        stringsAsFactors = FALSE
      )
    }
  }))
}

# Daten extrahieren
formatted_data <- extract_data(json_data)

# Zeigen Sie die ersten Zeilen der Tabelle an
print(head(formatted_data))

# count how many entries per mep_id

a <-  formatted_data %>%
  group_by(mep_id) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Angenommen, deine Daten sind in einer Variable namens json_data gespeichert
all_keys <- unique(unlist(lapply(json_data, names)))

print(all_keys)


# Erstellen eines leeren Dataframes mit allen Schlüsseln als Spalten und mep_id als zusätzliche Spalte
activity_counts <- data.frame(mep_id = integer(), CRE = integer(), WDECL = integer(),
                              COMPARL = integer(), REPORT = integer(), REPORT_SHADOW = integer(),
                              COMPARL_SHADOW = integer(), MOTION = integer(), OQ = integer(),
                              WEXP = integer(), WQ = integer(), MINT = integer(),
                              IMOTION = integer(), PRUNACT = integer(), stringsAsFactors = FALSE)

# Durchlaufen aller Einträge im JSON-Datensatz
for(i in seq_along(json_data)) {
  # Aktuelle MEP-ID extrahieren
  current_mep_id <- json_data[[i]]$mep_id
  
  # Zählen der Einträge für jeden Aktivitätstyp
  temp_counts <- sapply(names(activity_counts)[-1], function(key) {
    if(!is.null(json_data[[i]][[key]])) {
      length(json_data[[i]][[key]])
    } else {
      0
    }
  })
  
  # Erstellen eines temporären Dataframes für den aktuellen Abgeordneten
  temp_df <- data.frame(mep_id = current_mep_id, t(temp_counts))
  
  # Hinzufügen des temporären Dataframes zum Haupt-Dataframe
  activity_counts <- rbind(activity_counts, temp_df)
}

# Entfernen von Duplikaten, falls eine mep_id mehrmals vorkommt, um Summen zu aggregieren
activity_counts <- aggregate(. ~ mep_id, data = activity_counts, sum)

# Überprüfen der Ergebnisse
print(activity_counts)


# export activity_counts to csv

write.csv(activity_counts, "activity_counts.csv", row.names = FALSE)

##################################################################


# Initialisiere eine Liste von Dataframes für die terms 5 bis 9
activity_counts_list <- setNames(vector("list", 9), paste0("term_", 1:9))

# Vorbereiten der Struktur jedes Dataframes in der Liste
for (term in names(activity_counts_list)) {
  activity_counts_list[[term]] <- data.frame(mep_id = integer(), CRE = integer(), WDECL = integer(),
                                             COMPARL = integer(), REPORT = integer(), REPORT_SHADOW = integer(),
                                             COMPARL_SHADOW = integer(), MOTION = integer(), OQ = integer(),
                                             WEXP = integer(), WQ = integer(), MINT = integer(),
                                             IMOTION = integer(), PRUNACT = integer(), stringsAsFactors = FALSE)
}

# Durchlaufen aller Einträge im JSON-Datensatz
for(i in seq_along(json_data)) {
  current_mep_id <- json_data[[i]]$mep_id
  
  # Durchgehen aller möglichen Aktivitäten
  for (key in names(activity_counts_list[[1]])[-1]) {
    if (!is.null(json_data[[i]][[key]])) {
      # Durchgehen aller Einträge unter einem Aktivitätsschlüssel
      for (entry in json_data[[i]][[key]]) {
        if (!is.null(entry[["term"]])) {
          current_term <- entry[["term"]]
          term_key <- paste0("term_", current_term)
          if (term_key %in% names(activity_counts_list)) {
            # Aktualisiere den entsprechenden Dataframe
            index <- match(current_mep_id, activity_counts_list[[term_key]]$mep_id)
            if (is.na(index)) {
              # Neuer Eintrag
              new_row <- setNames(as.list(rep(0, length(activity_counts_list[[term_key]]) - 1)), names(activity_counts_list[[term_key]])[-1])
              new_row$CRE <- ifelse(key == "CRE", 1, 0)
              new_row$mep_id <- current_mep_id
              activity_counts_list[[term_key]] <- rbind(activity_counts_list[[term_key]], new_row)
            } else {
              # Aktualisiere bestehenden Eintrag
              activity_counts_list[[term_key]][index, key] <- activity_counts_list[[term_key]][index, key] + 1
            }
          }
        }
      }
    }
  }
}

# Aggregiere die Daten, um Duplikate zu entfernen
for (term in names(activity_counts_list)) {
  if (nrow(activity_counts_list[[term]]) > 0) {
    activity_counts_list[[term]] <- aggregate(. ~ mep_id, data = activity_counts_list[[term]], sum)
  }
}

# Ausgabe der Dataframes für jeden Term
for (term in names(activity_counts_list)) {
  print(term)
  print(activity_counts_list[[term]])
}

# Ausgabe der Dataframes für jeden Term als CSV-Dateien
for (term in names(activity_counts_list)) {
  if (nrow(activity_counts_list[[term]]) > 0) {  # Stellt sicher, dass der Dataframe nicht leer ist
    filename <- paste0("activity_counts_term_", term, ".csv")  # Pfad und Dateiname
    write.csv(activity_counts_list[[term]], file = filename, row.names = FALSE)
    print(paste("Gespeichert:", filename))  # Ausgabe der gespeicherten Datei
  } else {
    print(paste("Keine Daten verfügbar für Term", term))  # Hinweis, wenn keine Daten vorhanden sind
  }
}

