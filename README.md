# CooRTok

*[RuudGaled](https://github.com/RuudGaled), [Rigor64](https://github.com/Rigor64)*

CoorTok utilizza la liberia `CoorTweet` fornendo funzionalità specifiche per l'analisi delle reti coordinate su TikTok. 

# Requisiti

1. File `.csv` contenente informazioni relative a TikTok. Tramite la libreria [traktok](https://github.com/JBGruber/traktok) è possibile ricavare il database utile per l'analisi dei dati relativi a i dettagli sull'utente che ha condiviso il video, la descrizione e altre dati collegati. 
2. Una chiave ("*key*") API di ChatGPT (modello gpt-3.5-turbo) per la generazione delle *labels*[^1]
3. Una chiave ("*key*") API di TikTok per ricavere informazioni ulteriori sugli account[^2]

[^1]: Inserire la chiave nel file .Renviron con dicitura 'OPENAI_API_KEY = '
[^2]: Inserire la chiave nel file .Renviron con dicitura 'TIKTOK_CLIENT_KEY = ' e  'TIKTOK_CLIENT_SECRET = '

# Import Library

```r
devtools::install_github("Rigor64/CooRTok")
devtools::install_github("nicolarighetti/CooRTweet")
library(CooRTok)
library(CooRTweet)
library(igraph)
library(traktok)
library(readr)
library(dplyr)
```

# Quick Start

Importiamo il database:

```r
tryCatch({
  database <- readr::read_csv("./data/tiktok_database.csv",
                                    col_types = cols(video_id = col_character(),
                                                     music_id = col_character()))
}, error = function(e) {
  stop("Failed to read TikTok coordinated accounts ID CSV. Error: ", e$message)
})
```

Per l'inizializzazione del progetto è necessario utilizzare funzioni del `CoorTweet` per la generazione del grafo contenente i comportamenti coordinati. Per fare ciò, eseguire le funzioni sottostanti 

```r

#Cambio le colonne per renderle compatibili con l'analisi di CoorTweet
change_column <- CooRTweet::prep_data(x = database,
                                  object_id = "video_description", # contenuto del video
                                  account_id = "author_name",      # autore del video
                                  content_id = "video_id",         # id del video
                                  timestamp_share = "create_time") # orario della creazione del video

#Uniformiamo i caratteri della video description
change_column$object_id <- tolower(change_column$object_id)


#Avvio l'analisi dul dataframe modificato, dati i parametri di coordinamento
#Tutti i video che sono stati condivisi in un certo lasso di tempo da un account ad un altro
result <- CooRTweet::detect_groups(x = change_column,
                                   time_window = 150, # intervallo di tempo
                                   min_participation = 2, # numero minimo di ripetizioni
                                   remove_loops = T)

#genero il grafo relativo ai risultati ottenuti
graph <- CooRTweet::generate_coordinated_network(x = result,
                                                       edge_weight = 0.5, # default 0.5
                                                       objects = TRUE)
```

Una volta fatto ciò, vengono implementate le seguneti funzioni di CooRTok:

```r
#dataframe che somma tutte le informazzioni che abbiamo riguardo i component di account coordinati e le relative descrizioni dei video
summary_entity <- create_entity(graph = graph, database = database, get_cluster = TRUE)

#creazione di un dataframe con i soli account che hanno presentato un compontamento coordinato
summary_accounts <- account_stats(graph, result, weight_threshold = "none")

#aggiunta delle informazioni sull'account, utilizzando le API di TikTok
summary_accounts <- tiktok_account_info(summary_accounts, summary_entity)

#generiamo le label a partire dalla descrizione dei video
tiktok_df <- generate_label(summary_entity, get_cluster = TRUE)
```

# Risultati

Alla fine dell'esecuzione del codice, si avranno due dataset:
- **summary_accounts**: tabella relativa agli account che hanno maggiormente contribuito al comportamento coordinato, ricercando maggiori informazioni tramite l’utilizzo di API di TikTok, attraverso la funzione tiktok_account_info.
- **tiktok_df**: tramite la funzione generate_label, viene fatta una richiesta a ChatGPT il quale restituisce un'etichetta riassuntiva delle descrizioni dei video che fanno parte di ciascun cluster o component.

# References

- **[CooRTweet](https://github.com/nicolarighetti/CooRTweet)**
- **[CooRNet](https://github.com/fabiogiglietto/CooRnet)**
