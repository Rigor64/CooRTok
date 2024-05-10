#' Create entity function
#'
#' This function given, refer to the input, a dataset with specific data related to the graph of the coordinated behaviours,
#' with an indication about the clusters or components on each network
#' @name create_entity
#' @param graph the coordinated behaviours graph
#' @param database the database
#' @param get_cluster boolean parameter for chosing if the cluster would be detected
#' @export
#' @import igraph
#' @import CooRTweet
#' @import dplyr
#' @examples
#' create_entity(graph = graph, database = database, get_cluster = TRUE)

create_entity <- function(graph, database, get_cluster = TRUE) {

  source("./R/detect_cluster.R")

  #detect component
  data <- components(graph)$membership

  #add component to the graph
  igraph::V(graph)$component <- data

	if (get_cluster == TRUE) {

	  #add cluster to the graph
	  igraph::V(graph)$cluster <- cluster_louvain(
	    graph,
	    weights = NULL,
	    resolution = 1
	  )$membership

	}

  graph_vertices_df <- igraph::as_data_frame(graph, what = "vertices")

  #Summarize accounts statistics and groups statisti
  tryCatch({
    group_stat <- CooRTweet::group_stats(coord_graph = graph, weight_threshold = "full")
    account_stat <- CooRTweet::account_stats(coord_graph = graph, result = result, weight_threshold = "full")
  },
  error = function(e) {
    stop("Failed to calculate summary statistics for groups or accounts: ", e$message)
  })

  # Uniamo i dataframe sulla base della corrispondenza tra video_description e object_id
  correlated_accounts <- database %>%  dplyr::filter(video_description %in% group_stat$object_id) %>%
    dplyr::select(author_name)
  # Aggiungiamo la nuova colonna al dataframe group_stat
  group_stat <- group_stat %>%
    dplyr::mutate(correlated_account = list(correlated_accounts$author_name))

  #aggiungiamo al dataframe account_stat le descrizioni dei video associate, sulla base del dataframe originale
  # Aggiungo le informazioni del database iniziale agli account coordinati trovati
  coordinated_account_stat <- account_stat %>%  dplyr::left_join(database, by = c("account_id" = "author_name"))

  coordinated_account_stat <- dplyr::merge(coordinated_account_stat, graph_vertices_df, by.x ="account_id", by.y  = "name")

	return(coordinated_account_stat)
}
