# CooRTok

*[Edoardo Galantini](https://github.com/RuudGaled), [Matteo Leopizzi](https://github.com/Rigor64)*

CooRTok utilizes the `CoorTweet` library, providing specific functionalities for analyzing coordinated networks on TikTok.

# Requirements

1. A `.csv` file containing TikTok-related information. Through the [traktok](https://github.com/JBGruber/traktok) library, you can obtain the necessary database for analyzing data related to user details who shared the video, the description, and other connected data.
2. An API key from ChatGPT (gpt-3.5-turbo model) for generating the *labels*[^1].
3. An API key from TikTok to retrieve additional information about accounts[^2].

[^1]: Insert the key into the .Renviron file with the statement 'OPENAI_API_KEY = '
[^2]: Insert the key into the .Renviron file with the statements 'TIKTOK_CLIENT_KEY = ' and  'TIKTOK_CLIENT_SECRET = '

# Design Choices

- The default version of ChatGTP is the `gpt-3.5-turbo model`, but it can be manually set.
- For managing coordinated behavior, a time interval related to sharing a post was set to 150 seconds, and a minimum number of participants in sharing was set to 2.
- You can decide whether to perform cluster analysis as well. To do this, set the `get_cluster` parameter of the `create_entity` function to TRUE.
- For cluster analysis, the Louvain method algorithm, present in the `cluster_louvain` function of the `igraph` library, is used.
- For both components and clusters, it was decided to create the `summary_entity` dataframe with the following characteristics:

```r
    dplyr::reframe(
          num_account = n(),
          avg.views = mean(view_count),
          avg.comments = mean(comment_count),
          avg.shares = mean(share_count),
          avg.likes = mean(like_count),
          most_frequent_region = names(sort(table(region_code), decreasing = TRUE))[1],
          video_descriptions = list(names(sort(table(video_description), decreasing = TRUE)))
        )
```

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

Let's import the database:

```r
tryCatch({
  database <- readr::read_csv("./data/tiktok_database.csv",
                                    col_types = cols(video_id = col_character(),
                                                     music_id = col_character()))
}, error = function(e) {
  stop("Failed to read TikTok coordinated accounts ID CSV. Error: ", e$message)
})
```

Through the functions of `CoorTweet` we generate the graph, containing the elements with coordinated behaviors. To do this, execute the functions below:

```r

# Changing columns to make them compatible with CoorTweet analysis
change_column <- CooRTweet::prep_data(x = database,
                                  object_id = "video_description", # video description
                                  account_id = "author_name",      # video author
                                  content_id = "video_id",         # video ID
                                  timestamp_share = "create_time") # video creation time

# Standardizing characters in the video description
change_column$object_id <- tolower(change_column$object_id)


# Running analysis on the modified dataframe, with coordination parameters
result <- CooRTweet::detect_groups(x = change_column,
                                   time_window = 150, # time interval
                                   min_participation = 2, # minimum number of repetitions
                                   remove_loops = T)

# Generating the graph related to the obtained results
graph <- CooRTweet::generate_coordinated_network(x = result,
                                                  edge_weight = 0.5, # default 0.5
                                                  objects = TRUE)
```

Then, the following functions of `CooRTok` are implemented:

```r
# Dataframe summarizing all information regarding coordinated account components and their video descriptions
summary_entity <- create_entity(graph = graph, database = database, get_cluster = TRUE)

# Creating a dataframe with only accounts that exhibited coordinated behavior
summary_accounts <- account_stats(graph, result, weight_threshold = "none")

# Adding account information using TikTok APIs
summary_accounts <- tiktok_account_info(summary_accounts, summary_entity)

# Generating labels from video descriptions
tiktok_df <- generate_label(summary_entity, get_cluster = TRUE)
```

# Risultati

Upon code execution, two datasets are obtained:
- **summary_accounts**: a table related to accounts that contributed most to coordinated behavior, fetching additional information using TikTok APIs through the `tiktok_account_info` function.
- **tiktok_df**: through the `generate_label` function, a request is made to ChatGPT, which returns a summarizing label for the descriptions of videos belonging to each cluster or component.

# References

- **[CooRTweet](https://github.com/nicolarighetti/CooRTweet)**
- **[CooRNet](https://github.com/fabiogiglietto/CooRnet)**
