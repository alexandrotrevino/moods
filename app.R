# Load the necessary packages
library(shiny)
library(shinyWidgets)
library(shinyMobile)
library(rdrop2)
library(RSQLite)
library(googlesheets4)
library(ggplot2)
library(ggthemes)
library(dplyr)

table <- "moods"
SHEET_ID <- "https://docs.google.com/spreadsheets/d/1h72pc2dC8crEbhvvOTXpoifz5Fiuml65YlJZONF7MTU/edit#gid=0"

GoogleSheetsSaveData <- function(data) {
  # Add the data as a new row
  sheet_append(SHEET_ID, data, sheet = 1)
}

GoogleSheetsLoadData <- function() {
  # Read the data
  read_sheet(SHEET_ID)
}


# Functions to read and write from DropBox
outputDir <- "moods"

DropBoxSaveData <- function(data, file) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

DropBoxLoadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}



# Define UI for app that saves user inputs over time

ui <- f7Page(
  title = "Moods",
  options = list(theme = "md", navbar = list(hideOnPageScroll = TRUE)),
  tags$head(
    tags$style(HTML("
      .my-card {
        background-color: #b7e0dc; /* change this to your preferred color */
      }
      .sleep-card {
        background-color: #c0d6e2;
      }
      .my-slider-container {
        display: flex; /* use flexbox for layout */
        align-items: center; /* center items vertically */
        justify-content: space-between; /* space out items evenly */
        padding: 10px; /* add some padding */
      }
      .my-slider-container .icon {
        font-size: 30px; /* adjust size of icons */
      }
      .my-slider-container .slider-wrapper {
        flex-grow: 1; /* wrapper around slider will take up remaining space */
        margin: 0 10px; /* add some margin */
      }
      .my-slider-container .slider-handle {
        height: 40px; /* adjust size of slider handle */
        width: 20px;
        top: -6px;
      }
      .my-slider-container .range-slider {
        width: 100%;
      }
      "
    ))
  ),
  f7TabLayout(
    navbar = f7Navbar(
      title = "Moods App",
      bigger = T,
      hairline = TRUE,
      shadow = TRUE
    ),
    
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      
      # Tab 1 -----------------
      f7Tab(
        tabName = "Record",
        icon = f7Icon("recordingtape"),
        active = TRUE,
        tagList(
          f7Shadow(
            intensity = 16,
            hover = FALSE,
            f7Card(
              class = "my-card",
              title = "Emotions & Noticings",
              outline = T,
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  title = "How do you feel right now?",
                  outline = T,
                  div(
                    class = "my-slider-container",
                    f7Icon("trash"),
                    div(
                      class = "slider-wrapper",
                      f7Slider(
                        inputId = "how",
                        label = NULL,
                        min = 1,
                        max = 5,
                        step = 0.1,
                        value = 3
                      )
                    ),
                    f7Icon("sparkles")
                  )
                ),
              ),
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  title = "How is your pain right now?",
                  outline = T,
                  div(
                    class = "my-slider-container",
                    f7Icon("sparkles"),
                    div(
                      class = "slider-wrapper",
                      f7Slider(
                        inputId = "pain",
                        label = NULL,
                        min = 1,
                        max = 5,
                        step = 0.1,
                        value = 3
                      )
                    ),
                    f7Icon("flame_fill")
                  ),
                )
              ),
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  outline = TRUE,
                  f7Text("what", "What are you feeling right now?", placeholder = "I feel..."),
                  f7Text("notes", "Want to add more detail?", placeholder = "Notes")
                )
              )
            )
          ),
          f7Shadow(
            intensity = 16,
            hover = FALSE,
            f7Card(
              f7Toggle(inputId = "activate_slider", label = "Is this your first record of the day?", checked = FALSE),
              uiOutput("mySliderUI")  
            )
          ),
          f7Shadow(
            intensity = 16,
            hover = TRUE,
            pressed = TRUE,
            f7Card(
              f7Button("record", "Record")
            )
          )
        )
      ),
      
      # Tab 2 -----------------
      f7Tab(
        tabName = "Render",
        icon = f7Icon("graph_square_fill"),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          pressed = TRUE,
          f7Card(
            f7DownloadButton("download", "Download"),
          )
        ),
        f7Shadow(
          intensity = 16,
          f7Card(
            outline = T,
            title = "My Moods",
            tableOutput(outputId = "table")
          )
        ),
        f7Shadow(
          intensity = 16,
          f7Card(
            outline = T,
            title = "Mood over time",
            plotOutput(outputId = "plot")
          )
        ),
        f7Shadow(
          intensity = 16,
          f7Card(
            outline = T,
            title = "Pain/discomfort level over time",
            plotOutput(outputId = "pain_plot")
          )
        ),
        f7Shadow(
          intensity = 16,
          f7Card(
            outline = T,
            title = "Sleep quality over time",
            plotOutput(outputId = "sleep_plot")
          )
        )
      ),
      
      # Tab 3------------------------
      f7Tab(
        tabName = "SleepTab",
        icon = f7Icon("moon_fill"),
        uiOutput("sleep_cards")
      )
    )
  )
)

# SERVER ---------------------------------------------------------
# Define server logic for saving user inputs
server <- function(input, output, session) {
  
  # Define functions that load the database, and render the results
  
  LoadData <- function() {
    # Connect, query, disconnect
    db <- dbConnect(SQLite(), "moods_db")
    
    if (!dbExistsTable(db, "moods")) {
      dbCreateTable(db, "moods", fields = c(
        Overall = "numeric",
        Pain = "numeric",
        Emotions = "character",
        Notes = "character",
        Sleep = "numeric",
        Dreams = "character",
        Date = "character",
        Time = "character",
        Timestamp = "character"
      ))
    }
    
    query <- sprintf("SELECT * FROM %s", "moods")
    dat <- dbGetQuery(db, query)
    dbDisconnect(db)
    
    # Sort and return
    return(dat[order(dat$Timestamp, decreasing = T), ])
  }
  
  RenderResults <- function() {
    
    dat <- LoadData()
    
    output$table <- renderTable({
      dat[order(dat$Timestamp, decreasing = T), c("Date", "Time", "Overall", "Pain", "Emotions")]
    })
    
    # Plot the data----------
    
    # Create a data frame with the start and end times for each day.
    # We only do this if the data frame has some data in it.
    if (nrow(dat) > 1) {
      
      days <- data.frame(
        ymin = -Inf,
        ymax = Inf,
        xmin = as.POSIXct(strftime(dat$Timestamp, "%Y-%m-%d")),
        xmax = as.POSIXct(strftime(dat$Timestamp, "%Y-%m-%d")) + 86400
      )
      
      # Add a 'Day' column that indicates whether the day number is even or odd
      days$Day <- as.numeric(strftime(days$xmin, "%j")) %% 2
      
      # Compute shading logic
      plot_shading_logic <- difftime(max(dat$Timestamp),
                                     min(dat$Timestamp),
                                     units = "min")
      
      # Plot
      output$plot <- renderPlot({
        
        # ----------- Plot Mood
        gg <- ggplot(data = dat, aes(x = as.POSIXct(.data$Timestamp), y = .data$Overall)) +
          xlab("Day & Time") +
          ylab("Mood level") +
          ggtitle("How are you feeling?")
        
        if (as.numeric(plot_shading_logic) > 1440) {
          
          gg <- gg + geom_rect(data = days,
                               inherit.aes = FALSE,
                               aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(Day)),
                               show.legend = F,
                               alpha = 0.2)
        }
        
        gg <- gg + 
          geom_line() +
          scale_fill_manual(values = c("#eeeeee", "white")) +
          theme_few()
        gg
      })
      
      
      # ----------- Plot Pain Level
      output$pain_plot <- renderPlot({
        gg <- ggplot(dat, aes(x = as.POSIXct(.data$Timestamp), y = .data$Pain)) +
          xlab("Day & Time") +
          ylab("Pain level") +
          ggtitle("How bad is the pain/discomfort?")
        
        if (as.numeric(plot_shading_logic) > 1440) {
          
          gg <- gg + geom_rect(data = days,
                               inherit.aes = FALSE,
                               aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(Day)),
                               show.legend = F,
                               alpha = 0.2)
        }
        
        gg <- gg + 
          geom_line(color = "#ff5c5c") +
          scale_fill_manual(values = c("#eeeeee", "white")) +
          theme_few()
        gg
      })
      
      # ----------- Plot Sleep Quality
      output$sleep_plot <- renderPlot({
        sleep <- dat %>% filter(!is.na(Sleep))
        gg <- ggplot(data = sleep, aes(x = as.POSIXct(.data$Timestamp), y = .data$Sleep)) +
          xlab("Date") +
          ylab("Sleep quality") +
          ggtitle("How did you sleep last night?") +
          geom_point() +
          theme_few()
        gg
      })
    } else {
      renderUI({
        f7Card(
          title = "To plot, add more data!"
        )
      })
    }
  }
  
  #-------------
  # Run this function whenever the app is accessed:
  RenderResults()
  
  # Sleep toggle
  observeEvent(input$activate_slider, {
    if (input$activate_slider) {
      output$mySliderUI <- renderUI({
        if (input$activate_slider) {
          f7Shadow(
            intensity = 16,
            hover = FALSE,
            f7Card(
              class = "sleep-card",
              title = "Sleep & Dreams",
              outline = TRUE,
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  outline = T,
                  title = "How did you sleep?",
                  div(
                    class = "my-slider-container",
                    f7Icon("hand_thumbsdown_fill"),
                    div(
                      class = "slider-wrapper",
                      f7Slider(inputId = "sleep", label = NULL, min = 1, max = 5, step = 0.1, value = 3)
                    ),
                    f7Icon("hand_thumbsup_fill")
                  )
                )
              ),
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  outline = T,
                  title = "How did you sleep?",
                  
                )
              ),
              f7Shadow(
                intensity = 24,
                hover = TRUE,
                f7Card(
                  outline = T,
                  f7Text("dreams", "Write about your dreams, if you can:", placeholder = "I dreamed...")
                )
              )
            )
          )
        }
      })
    }
  })
  
  
  
  # Whenever the 'Record' button is pressed, append the inputs and timestamp to the data
  observeEvent(input$record, {
    
    # Create a reactiveValues object to store the data
    data <-
      reactiveValues(df = data.frame(
        Overall = numeric(),
        Pain = numeric(),
        Emotions = character(),
        Notes = character(),
        Sleep = numeric(),
        Dreams = character(),
        Date = character(),
        Time = character(),
        Timestamp = character()
      ))
    
    if (input$activate_slider) {
      sleep_input <- as.numeric(input$sleep)
      dream_input <- input$dreams
    } else {
      sleep_input <- NA
      dream_input <- NA
    }
    
    data$df <-
      rbind(
        data$df,
        data.frame(
          Overall = as.numeric(input$how),
          Pain = as.numeric(input$pain),
          Emotions = input$what,
          Notes = input$notes,
          Sleep = sleep_input,
          Dreams = dream_input,
          Date = as.character(format(Sys.Date(), "%a, %b %d")),
          Time = as.character(format(Sys.time(), "%I:%M %p")),
          Timestamp = as.character(Sys.time())
        )
      )
    
    # SQL code ------
    db <- dbConnect(SQLite(), "moods_db")
    
    # Construct the update query by looping over the data fields
    dbWriteTable(db, "moods", data$df, append = TRUE)
    
    # Submit the update query and disconnect
    dbDisconnect(db)
    # --------    
    
    # Reset the input fields
    updateF7Radio(inputId = "how", session = session, choices = list("", "1", "2", "3", "4", "5"), selected = "")
    updateF7Radio(inputId = "pain", session = session, choices = list("", "1", "2", "3", "4", "5"), selected = "")
    updateTextInput(session, inputId = "what", value = "")
    updateTextInput(session, inputId = "notes", value = "")
    updateF7Radio(inputId = "sleep", session = session, choices = list("", "1", "2", "3", "4", "5"), selected = "")
    updateF7Radio(inputId = "wake", session = session, choices = list("", "1", "2", "3", "4", "5"), selected = "")
    updateTextInput(session, inputId = "dreams", value = "")
    
    # Notify the user
    showNotification("Your response has been recorded", type = "message", duration = 3)
    
    # Reload results
    RenderResults()
  })
  
  # Download the data locally
  output$download = downloadHandler(
    filename = function() {
      paste("mood-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dat <- LoadData()
      write.csv(dat, file)
    }
  )
  
  
  # Render the sleep UI
  
  # This reactive will check the value of the current active tab
  active_tab <- reactive({input$tabs})
  
  output$sleep_cards <- renderUI({
    # Check if the active tab is 'MyTab'
    if (active_tab() == "SleepTab") {
      
      # Connect to the database
      db <- dbConnect(SQLite(), "moods_db")
      query <- sprintf("SELECT * FROM %s", "moods")
      dat <- dbGetQuery(db, query)
      dbDisconnect(db)
      
      dreams <- dat %>%
        filter(Dreams != "") %>% 
        select(Date, Sleep, Dreams)
      
      lapply(nrow(dreams):1, function(i) {
        f7Card(
          title = dreams[i, "Date"], 
          subtitle = sprintf("Sleep quality: %s", dreams[i, "Sleep"]),
          dreams[i, "Dreams"]
        )
      })
    }
  })
  
  # Update tabs when button is clicked
  observeEvent(input$SleepTab, {
    updateF7Tabs(session, "tabs", selected = "SleepTab")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
