


generate_label <- function(dataframe, model = "gpt-3.5-turbo", get_cluster = TRUE) {

  dataframe <- coordinated_account_stat %>%
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

    dataframe <- coordinated_account_stat %>%
      dplyr::group_by(cluster)
  }

  #salvo il numero di component
  n_components <- length(unique(dataframe$component))

  #-----------------------------------------------------------------------------------------------------------------------------------------------

  #Chiamata all'API di ChatGPT

  #Verifico che ci sia la chiave
  if (Sys.getenv("OPENAI_API_KEY") != "") {

    #inizializzo un dataframe temporaneo
    temp_df <- data.frame(cluster_id = integer(), label = character())

    cat("\nAuto-labelling clusters with OpenAI gpt-3.5-turbo (https://platform.openai.com/)...\n")

    #inizializzo la progressbar
    pb <- utils::txtProgressBar(min = 0, max = n_components, width = 100, style = 3)

    for (j in 1:n_components) {

      #seleziona il j-esimo component
      component_edges <- subset(dataframe,dataframe$component == j)

      #calcolo il numero massimo di descrizioni da prendere
      n <- ifelse(nrow(component_edges) / 100 * 20 > 5,
                  round(nrow(component_edges) / 100 * 20, 0), 5)

      cropped_text <- lapply(component_edges$video_descriptions, function(desc) head(desc, n))

      #compongo la lista di tutte le descrizioni dei video associate a quel component
      #text <- unlist(cropped_text)

      #scrivo il messaggio da inviare a chatgpt
      msg <- list(list("role" = "system",
                       "content" = "You are a researcher investigating coordinated and inauthentic behavior on TikTok. Your objective is to generate only one concise, descriptive label in English that capture the shared video description of clusters of TikTok.\n\n"),
                  list("role" = "user",
                       "content" = "I will supply a list of video descriptions for each cluster. Identify the shared features among these descriptions and descrive them in one label:\n\n"),
                  cropped_text)

      #richiamiamo la versione di chatgtp da utilizzare e passo il messaggio
      res <- tryCatch(
        {
          openai::create_chat_completion(model = "gpt-3.5-turbo",
                                         messages = msg,
                                         temperature = 0,
                                         top_p = 1,
                                         max_tokens = 256)
        },
        error = function(cond) {
          return(NULL)
        })

      #se abbiamo avuto risoslta da chatgpt, accodiamo il risultato del component j-esimo
      if (!is.null(res)) {
        temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = res$choices$message.content))
        # print(paste(j, "-", res$choices$message.content))
      }else{
        temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = "System error"))
      }
      utils::setTxtProgressBar(pb, pb$getVal() + 1)
      Sys.sleep(0.5)
    }

    #aggiungo al dataframe che abbiamo tutte le descrizioni dei component che abbiamo
    dataframe <- merge(dataframe, temp_df, by.x = "component", by.y = "cluster_id")
  }

  return(dataframe)
}
