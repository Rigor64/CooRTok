#'  tiktok_account_info function
#'
#' This function add information about the accounts that are detected for their coordinated behaviours
#' @name tiktok_account_info
#' @param dataframe the result of account_stats function from CoorTweet library
#' @param summary_entity  the result of create_entity function
#' @export
#' @import utils
#' @import traktok
#' @import jsonlite
#' @import dplyr
#' @examples
#' tiktok_account_info(summary_accounts, summary_entity)

# Load required libraries
library(jsonlite)
library(traktok)
library(dplyr)

tiktok_account_info <- function(dataframe, summary_entity) {

  #Declare function get_tiktok_account_info
  get_tiktok_account_info <- function(account_name){

    #Add data from api call
    data <- tryCatch({
      tt_user_info_api(account_name,
                               fields = "all",
                               verbose = TRUE)

    },error = function(e){
      #Add NA columns if api call don't find anything
      data.frame(
        following_count = NA,
        is_verified = NA,
        likes_count = NA,
        video_count = NA,
        avatar_url = NA,
        bio_description = NA,
        display_name = NA,
        follower_count = NA,
        stringsAsFactors = FALSE
      )
    })

    data$account_id <- NA

    return(data)
  }

  # Ensure environment variables are set
  if (is.na(Sys.getenv("TIKTOK_CLIENT_KEY")) | is.na(Sys.getenv("TIKTOK_CLIENT_SECRET"))) {
    stop("Environment variables for TikTok API not set. Please set TIKTOK_CLIENT_KEY and TIKTOK_CLIENT_SECRET.")
  }

  tryCatch({
    #Obtain Tiktok token
    auth_research(client_key = Sys.getenv("TIKTOK_CLIENT_KEY"), client_secret = Sys.getenv("TIKTOK_CLIENT_SECRET"))
  }, error = function(e) {
    stop("Authentication with TikTok API failed. Error message: ", e$message)
  })

  account_name <- "username"
  #create a temporary table
  account_info <- data.frame()

  #Initialize progressbar
  pb <- utils::txtProgressBar(min = 0, max = nrow(dataframe) , width = 100, style = 3)

  #Create account_info
  for (i in 1:nrow(dataframe)) {
    account_name <- dataframe$account_id[i]
    account_info <- rbind(account_info, get_tiktok_account_info(account_name))
    account_info$account_id[i] <- account_name
    utils::setTxtProgressBar(pb, pb$getVal() + 1)

  }

  cat("\n")

  #Join account_info into dataframe
  dataframe <- merge(dataframe, account_info, by="account_id")

  #Add component and cluster columns
  if("cluster" %in% names(summary_entity)){
    summary_entity <- summary_entity %>%
      dplyr::select(account_id, cluster, component) %>%
      unique()

  }else{
    summary_entity <- summary_entity %>%
      dplyr::select(account_id, component) %>%
      unique()
  }

  dataframe <- merge(dataframe, summary_entity, by="account_id")

  return(dataframe)
}
