#'  Function gui()
#'
#' Create the necessary gui to use the library
#'
#' @name gui
#' @export
#' @import shiny
#' @import shinyjs
#' @import igraph
#' @import devtools
#' @import readr
#' @importFrom utils read.csv
#' @import CooRTweet
#' @examples
#' gui()

gui <- function(){

  ui <- fluidPage(
    useShinyjs(),

    # App title
    titlePanel("CooRTok GUI"),

    # Sidebar layout with input and output definitions
    sidebarLayout(

      # Sidebar panel for inputs
      sidebarPanel(
        numericInput("timewindow","", value = 150),
        numericInput("percentileedgeweight","", value = 0.5),
        numericInput("minparticipation","", value = 2),

        # Input: Select a file
        fileInput("file1", "Choose CSV File with your URLs",
                  multiple = TRUE,
                  accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if the user want to calculate clusters
        checkboxInput(
          "get_cluster", "Click to also analyzes cluster",
          value = FALSE
        ),

        actionButton("create_df", "Run the coordination",disabled=TRUE),
        downloadButton("downloadData1", label = "Account informations df",disabled=TRUE),
        downloadButton("downloadData2", label = "Descriptions df",disabled=TRUE),
        # Horizontal line ----
        tags$hr()

      ),

      # Main panel for displaying outputs ----
      mainPanel(
        # Output: Data file ----
        tableOutput("contents")
      )

    )
  )

  # Define server logic to read selected file
  server <- function(input, output, session) {

      # List to contain variables over the observer
      reactiveValues <- reactiveVal(list(NULL))
      tiktok_df <- NULL
      summary_accounts <- NULL

      # Valueble to contain the final dataframe
      #final_df <- reactiveValues()

      # GUI options
      options(shiny.maxRequestSize = 15000 * 1024)

      validateInput <- function(inputText) {
        if (!stringr::str_detect(inputText, "^[0-9]+[.]?[0-9]*$")) {
          return("The value must be a positive number!")
        }
      }

      observeEvent(input$timewindow, {
        inputText <- input$timewindow
        validationMsg <- validateInput(inputText)
        if (!is.null(validationMsg)) {
          avviso_html <- paste0(" (",validationMsg,")")
        }
        else{
          avviso_html <- ""
        }
        updateTextInput(inputId = "timewindow", label = paste0("Insert Coordination Interval", avviso_html))
      })

      observeEvent(input$percentileedgeweight, {
        inputText <- input$percentileedgeweight
        validationMsg <- validateInput(inputText)
        if (!is.null(validationMsg)) {
          avviso_html <- paste0(" (",validationMsg,")")
        }
        else{
          avviso_html <- ""
        }
        updateTextInput(inputId = "percentileedgeweight", label = paste0("Insert Percentile Edge Weight", avviso_html))
      })

      observeEvent(input$minparticipation, {
        inputText <- input$minparticipation
        validationMsg <- validateInput(inputText)
        if (!is.null(validationMsg)) {
          avviso_html <- paste0(" (",validationMsg,")")
        }
        else{
          avviso_html <- ""
        }
        updateTextInput(inputId = "minparticipation", label = paste0("Insert minimum number of repetitions", avviso_html))
      })

      observeEvent(input$file1, {
        withProgress(message = 'calculating...',value = 0,{

          tryCatch({
            database <- readr::read_csv(input$file1$datapath,
                          col_types = cols(video_id = col_character(),
                                             music_id = col_character()))
            }, error = function(e) {
              stop("Failed to read TikTok coordinated accounts ID CSV. Error: ", e$message)
            })

          incProgress(0.3,detail = paste0("I'm making API requests, please wait as long as necessary"))

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
             time_window = input$timewindow, # time interval
             min_participation = input$minparticipation, # minimum number of repetitions
             remove_loops = T)

          incProgress(0.5,detail = paste0("creating graph..."))

          # Generating the graph related to the obtained results
          graph <- CooRTweet::generate_coordinated_network(x = result,
                                                    edge_weight = as.double(input$percentileedgeweight), # default 0.5
                                                    objects = TRUE)

          reactiveValues(list(graph = graph, database = database, result = result))

          incProgress(0.2,detail = paste0("I finished"))

          enable("create_df")

        })
      })

      # L'utente decide se vuole calcolare anche i cluster
      observeEvent(input$create_df, {

        p <- reactiveValues()
        withProgress(message = 'calculating...',value = 0,{

          # Dataframe summarizing all information regarding coordinated account components and their video descriptions
          incProgress(0.25,detail = paste0("I'm creating summary entity ..."))
          summary_entity <- create_entity(graph = p$graph, database = p$database, result = p$result, get_cluster = input$get_cluster)

          # Creating a dataframe with only accounts that exhibited coordinated behavior
          incProgress(0.25,detail = paste0("I'm creating summary account ..."))
          summary_accounts <- CooRTweet::account_stats(coord_graph = p$graph, result = p$result, weight_threshold = "none")

          # Check TikTok API
          tryCatch({
            #Obtain Tiktok token
            auth_research(client_key = Sys.getenv("TIKTOK_CLIENT_KEY"), client_secret = Sys.getenv("TIKTOK_CLIENT_SECRET"))
            # Adding account information using TikTok APIs
            incProgress(0.25,detail = paste0("Calling TikTok API"))

            summary_accounts <<- tiktok_account_info(summary_accounts, summary_entity)
          }, error = function(e) {
            stop("Failed to read TikTokAPI. Error: ", e$message)
          })

          # Check OpenAI API
          if (is.na(Sys.getenv("OPENAI_API_KEY"))) {
            stop("Failed to read OpenAI API. Error: ", e$message)

          } else {
            # Generating labels from video descriptions
            incProgress(0.25,detail = paste0("Calling OpenAi API"))
            tiktok_df <<- generate_label(summary_entity = summary_entity, get_cluster = input$get_cluster)
          }

          #enabling download buttons
          enable("downloadData1")
          enable("downloadData2")
        })

      })

      output$contents <- renderText({
        paste("Graph of the coordinated behaviours:")
      })

      output$contents <- renderTable({

        #pass the graph and showing it

      })

      #Download dataframe of the addictional informations of coordinated accounts
      output$downloadData1 <- downloadHandler(
        #save the file in the format .csv
        filename = function() {
          paste("Account-df-", Sys.Date(), ".csv", sep="")
        },
        content = function(filename) {
          summary_accounts <- data.frame(lapply(summary_accounts, as.character), stringsAsFactors=FALSE)
          write.csv(summary_accounts, filename, row.names = TRUE)
        }
      )

      #Download dataframe with the openAI descriptions
      output$downloadData2 <- downloadHandler(
        #save the file in the format .csv
        filename = function() {
          paste("Descriptions-df-", Sys.Date(), ".csv", sep="")
        },
        content = function(filename) {
          tiktok_df <- data.frame(lapply(tiktok_df, as.character), stringsAsFactors=FALSE)
          write.csv(tiktok_df, filename, row.names = TRUE)
        }
      )

  }

  # Create Shiny app ----
  shinyApp(ui = ui, server = server)
}
