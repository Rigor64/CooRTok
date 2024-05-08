#' Create entity function
#'
#' This function given, refer to the input, a dataset with specific data related to the graph of the coordinated behaviours,
#' with an indication about the clusters or components on each network
#' @param graph the coordinated behaviours graph
#' @param database the database cats
#' @param get_cluster boolean parameter for chosing if the cluster would be detected
#' @export
#' @examples
#' create_entity(graph = graph, database = database, get_cluster = TRUE)

create_entity <- function(graph, database, get_cluster = TRUE) {

  source("./R/detect_cluster.R")

  # Individuazione dei componenti
  data <- components(graph)$membership

  # Aggiunta del parametro componente al grafo
  V(graph)$component <- data

	if (get_cluster == TRUE) {

	  # Aggiunta del parametro componente al grafo
	  V(graph)$cluster <- cluster_louvain(
	    graph,
	    weights = NULL,
	    resolution = 1
	  )$membership

	}

  graph_vertices_df <- igraph::as_data_frame(graph, what = "vertices")

  #Calcoliamo le statistiche relative ai gruppi e quelle relative agli account
  tryCatch({
    group_stat <- CooRTweet::group_stats(coord_graph = graph, weight_threshold = "full")
    account_stat <- CooRTweet::account_stats(coord_graph = graph, result = result, weight_threshold = "full")
  },
  error = function(e) {
    stop("Failed to calculate summary statistics for groups or accounts: ", e$message)
  })

  # Uniamo i dataframe sulla base della corrispondenza tra video_description e object_id
  correlated_accounts <- database %>%  filter(video_description %in% group_stat$object_id) %>%
    select(author_name)
  # Aggiungiamo la nuova colonna al dataframe group_stat
  group_stat <- group_stat %>%
    mutate(correlated_account = list(correlated_accounts$author_name))

  #aggiungiamo al dataframe account_stat le descrizioni dei video associate, sulla base del dataframe originale
  # Aggiungo le informazioni del database iniziale agli account coordinati trovati
  coordinated_account_stat <- account_stat %>%  left_join(database, by = c("account_id" = "author_name"))

  coordinated_account_stat <- merge(coordinated_account_stat, graph_vertices_df, by.x ="account_id", by.y  = "name")


	return(coordinated_account_stat)
}
