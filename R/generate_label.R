#'  Function generate_label()
#'
#' This function allows you to make a request to ChatGPT which, processing the data taken as input,
#' returns a summary label of the descriptions of the videos that are part of each cluster or component.
#'
#' @name generate_label
#' @param dataframe dataframe containing information about tiktok accounts
#' @param model variable containing the version of ChatGPT to be used
#' @param get_cluste boolean variable that specifies whether to also analyze clusters
#' @export
#' @import dplyr
#' @import utils
#' @import openai
#' @examples
#' generate_label(dataframe = dataframe, model = "gpt-3.5-turbo", get_cluster = TRUE)


generate_label <- function(summary_entity, model = "gpt-3.5-turbo", get_cluster = TRUE) {

  #Create new dataframe and group by component
  dataframe <- summary_entity %>%
    dplyr::group_by(component) %>%
    dplyr::reframe(
      num_account = n(),
      avg.views = mean(view_count),
      avg.comments = mean(comment_count),
      avg.shares = mean(share_count),
      avg.likes = mean(like_count),
      most_frequent_region = names(sort(table(region_code), decreasing = TRUE))[1],
      video_descriptions = list(names(sort(table(video_description), decreasing = TRUE)))
    )

  if(get_cluster == TRUE) {

    #add cluster column to the database
    dataframe <- summary_entity %>%
      dplyr::group_by(cluster)
  }

  #check the number of the components
  n_components <- length(unique(dataframe$component))

  #Verify API key
  if (Sys.getenv("OPENAI_API_KEY") != "") {

    #Initialize temporary database
    temp_df <- data.frame(cluster_id = integer(), label = character())
    cat("\nAuto-labelling clusters with OpenAI gpt-3.5-turbo (https://platform.openai.com/)...\n")

    #Initialize progressbar
    pb <- utils::txtProgressBar(min = 0, max = n_components, width = 100, style = 3)

    for (j in 1:n_components) {

      #Select the j-th component
      component_edges <- subset(dataframe,dataframe$component == j)

      #Take a few number of description
      n <- ifelse(nrow(component_edges) / 100 * 20 > 5,
                  round(nrow(component_edges) / 100 * 20, 0), 5)

      cropped_text <- lapply(component_edges$video_descriptions, function(desc) head(desc, n))

      msg <- list(list("role" = "system",
                       "content" = "You are a researcher investigating coordinated and inauthentic behavior on TikTok. Your objective is to generate only one concise, descriptive label in English that capture the shared video description of clusters of TikTok.\n\n"),
                  list("role" = "user",
                       "content" = "I will supply a list of video descriptions for each cluster. Identify the shared features among these descriptions and descrive them in one label:\n\n"),
                  cropped_text)

      res <- tryCatch(
        {
          openai::create_chat_completion(model = model,
                                         messages = msg,
                                         temperature = 0,
                                         top_p = 1,
                                         max_tokens = 256)
        },
        error = function(cond) {
          return(NULL)
        })

      #Add j-th description to the component
      if (!is.null(res)) {
        temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = res$choices$message.content))
      }else{
        temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = "System error"))
      }
      utils::setTxtProgressBar(pb, pb$getVal() + 1)
      Sys.sleep(0.5)
    }

    #Add all descriptions to dataframe
    dataframe <- merge(dataframe, temp_df, by.x = "component", by.y = "cluster_id")
  }

  return(dataframe)
}
