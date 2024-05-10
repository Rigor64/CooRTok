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

  #Summarize accounts statistics and groups statistics
  tryCatch({
    group_stat <- CooRTweet::group_stats(coord_graph = graph, weight_threshold = "full")
    account_stat <- CooRTweet::account_stats(coord_graph = graph, result = result, weight_threshold = "full")
  },
  error = function(e) {
    stop("Failed to calculate summary statistics for groups or accounts: ", e$message)
  })

  # Merge the dataframes based on the correspondence between video_description and object_id
  correlated_accounts <- database %>%  filter(video_description %in% group_stat$object_id) %>%
    select(author_name)
  # Add colums to dataframe group_stat
  group_stat <- group_stat %>%
    mutate(correlated_account = list(correlated_accounts$author_name))

  #Add to the account_stat dataframe the associated video descriptions, based on the original dataframe
  coordinated_account_stat <- account_stat %>%  left_join(database, by = c("account_id" = "author_name"))

  #join between account_id and name from coordinated_account_stat and graph_vertices_df
  coordinated_account_stat <- merge(coordinated_account_stat, graph_vertices_df, by.x ="account_id", by.y  = "name")

	return(coordinated_account_stat)
}
