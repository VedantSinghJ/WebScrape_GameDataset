# Library Imports
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(shinydashboard)
library(shinythemes)
library(tidyr)

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Game Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tabsetPanel(
        # Filter Data Tab
        tabPanel("Filter Data",
                 h4("Filter Options"),
                 selectInput("genreFilter", "Genre", choices = c("All", ""), selected = "All"),
                 sliderInput("yearFilter", "Release Year", min = 1980, max = 2024, 
                             value = c(1980, 2024), step = 1, sep = ""),
                 sliderInput("ratingFilter", "Rating", min = 0, max = 5, 
                             value = c(0, 5), step = 0.1),
                 actionButton("applyFilters", "Apply Filters", class = "btn-primary")
        ),
        # Add Record Tab
        tabPanel("Add Record",
                 h4("Add New Game Record"),
                 textInput("newGame", "Game Title"),
                 textInput("newSeries", "Series"),
                 dateInput("newReleaseDate", "Release Date", value = "2024-01-01"),
                 textInput("newGenre", "Genre(s)"),
                 textInput("newDeveloper", "Developer(s)"),
                 textInput("newPublisher", "Publisher(s)"),
                 numericInput("newRating", "Rating", value = 3.5, min = 0, max = 5, step = 0.01),
                 numericInput("newMetacritic", "Metacritic", value = NA, min = 0, max = 100),
                 numericInput("newCopies", "Total Copies (millions)", value = 1, min = 0),
                 actionButton("addRecord", "Add Record", class = "btn-success")
        ),
        # Update/Delete Tab
        tabPanel("Update/Delete",
                 h4("Update or Delete Records"),
                 p("Select a row in the table to update or delete"),
                 uiOutput("selectedRecordInfo"),
                 conditionalPanel(
                   condition = "input.gameTable_rows_selected",
                   textInput("updateGame", "Game Title"),
                   textInput("updateSeries", "Series"),
                   textInput("updateGenre", "Genre(s)"),
                   numericInput("updateRating", "Rating", value = 3.5, min = 0, max = 5, step = 0.01),
                   numericInput("updateCopies", "Total Copies (millions)", value = 1, min = 0),
                   actionButton("updateRecord", "Update Record", class = "btn-warning"),
                   actionButton("deleteRecord", "Delete Record", class = "btn-danger")
                 )
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # Data Table Tab
        tabPanel("Data Table", 
                 h4("Game Dataset"),
                 DTOutput("gameTable")
        ),
        # Visualizations Tab
        tabPanel("Visualizations",
                 h4("Choose a Plot Type"),
                 selectInput("plotType", "Select Plot Type",
                             choices = c("Ratings Distribution", "Top Games by Copies", 
                                         "Release Year Trend", "Genre Popularity"),
                             selected = "Ratings Distribution"),
                 plotlyOutput("interactivePlot1", height = "400px"),
                 br(),
                 
                 h4("Secondary Plot"),
                 selectInput("secondPlotType", "Select Plot Type",
                             choices = c("Rating vs Metacritic", "Release Year vs Copies", 
                                         "Genre Breakdown", "Rating by Publisher"),
                             selected = "Rating vs Metacritic"),
                 plotlyOutput("interactivePlot2", height = "400px")
        ),
        # Summary Statistics Tab
        tabPanel("Summary Statistics",
                 h4("Dataset Summary"),
                 verbatimTextOutput("summary"),
                 h4("Top Publishers"),
                 uiOutput("topPublishersUI"),
                 h4("Rating Categories"),
                 plotlyOutput("ratingCategoryPlot")
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Initialize reactive values for the dataset
  rv <- reactiveValues()
  
  observe({
    if (is.null(rv$games_data)) {
      data <- read.csv("GameSet_Cleaned.csv", stringsAsFactors = FALSE)
      
      # Data cleaning and transformation
      if (any(grepl("e+", data$Total.copies))) {
        data$Total.copies <- as.numeric(data$Total.copies) / 1e6
      }
      data$rating <- as.numeric(data$rating)
      data$metacritic <- as.numeric(data$metacritic)
      data$Publisher.s. <- as.character(data$Publisher.s.)
      
      if (!("release_year" %in% colnames(data))) {
        data$release_year <- as.numeric(format(as.Date(data$Release.date), "%Y"))
      } else {
        data$release_year <- as.numeric(data$release_year)
      }
      
      if (!("rating_category" %in% colnames(data))) {
        data$rating_category <- case_when(
          data$rating >= 4.5 ~ "Excellent",
          data$rating >= 4.0 ~ "Good",
          data$rating >= 3.0 ~ "Average",
          TRUE ~ "Below Average"
        )
      }
      
      if (!("is_series" %in% colnames(data))) {
        data$is_series <- ifelse(data$Series == "Standalone", "No", "Yes")
      }
      
      rv$games_data <- data
    }
  })
  
  # Filter the dataset based on user input
  filtered_data <- reactive({
    req(rv$games_data)
    data <- rv$games_data
    
    if (input$applyFilters > 0) {
      if (input$genreFilter != "All") {
        data <- data %>% filter(grepl(input$genreFilter, Genre.s., ignore.case = TRUE))
      }
      
      data <- data %>% filter(release_year >= input$yearFilter[1] & release_year <= input$yearFilter[2])
      data <- data %>% filter(rating >= input$ratingFilter[1] & rating <= input$ratingFilter[2])
    }
    
    data <- data %>% mutate(RowNum = row_number()) %>% select(RowNum, everything())
    return(data)
  })
  
  # Update genre filter choices dynamically
  observe({
    req(rv$games_data)
    genres <- rv$games_data %>% pull(Genre.s.) %>% strsplit(", ") %>% unlist()
    genres_unique <- genres[!duplicated(tolower(genres))] %>% sort()
    updateSelectInput(session, "genreFilter", choices = c("All", genres_unique))
  })
  
  # Update year slider dynamically
  observe({
    req(rv$games_data)
    min_year <- min(rv$games_data$release_year, na.rm = TRUE)
    max_year <- max(rv$games_data$release_year, na.rm = TRUE)
    updateSliderInput(session, "yearFilter", min = min_year, max = max_year,
                      value = c(min_year, max_year))
  })
  
  # Render the data table
  output$gameTable <- renderDT({
    datatable(filtered_data(),
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                rowCallback = JS("
                  function(row, data, displayNum, displayIndex) {
                    $('td:first', row).html(displayIndex + 1);
                    return row;
                  }
                ")
              ),
              rownames = FALSE,
              selection = 'single')
  })
  
  # Create Record (Add new game)
  observeEvent(input$addRecord, {
    req(input$newGame, input$newRating, input$newCopies)
    new_game <- data.frame(
      Game = input$newGame,
      Series = input$newSeries,
      Release.date = as.character(input$newReleaseDate),
      Genre.s. = input$newGenre,
      Developer.s. = input$newDeveloper,
      Publisher.s. = input$newPublisher,
      rating = input$newRating,
      metacritic = as.numeric(input$newMetacritic),
      Total.copies = input$newCopies,
      release_year = as.numeric(format(input$newReleaseDate, "%Y")),
      is_series = ifelse(input$newSeries == "Standalone", "No", "Yes"),
      rating_category = case_when(
        input$newRating >= 4.5 ~ "Excellent",
        input$newRating >= 4.0 ~ "Good",
        input$newRating >= 3.0 ~ "Average",
        TRUE ~ "Below Average"
      ),
      stringsAsFactors = FALSE
    )
    rv$games_data <- rbind(rv$games_data, new_game)
    showNotification("Game added successfully!", type = "message")
    
    # Reset form fields
    updateTextInput(session, "newGame", value = "")
    updateTextInput(session, "newSeries", value = "")
    updateTextInput(session, "newGenre", value = "")
    updateTextInput(session, "newDeveloper", value = "")
    updateTextInput(session, "newPublisher", value = "")
    updateNumericInput(session, "newRating", value = 3.5)
    updateNumericInput(session, "newMetacritic", value = NA)
    updateNumericInput(session, "newCopies", value = 1)
  })
  
  # Display selected record info for update/delete
  output$selectedRecordInfo <- renderUI({
    req(input$gameTable_rows_selected)
    selected <- filtered_data()[input$gameTable_rows_selected, ]
    updateTextInput(session, "updateGame", value = selected$Game)
    updateTextInput(session, "updateSeries", value = selected$Series)
    updateTextInput(session, "updateGenre", value = selected$Genre.s.)
    updateNumericInput(session, "updateRating", value = selected$rating)
    updateNumericInput(session, "updateCopies", value = selected$Total.copies)
    
    HTML(paste0("<strong>Selected Game:</strong> ", selected$Game, "<br/>",
                "<strong>Release Year:</strong> ", selected$release_year, "<br/>",
                "<strong>Current Rating:</strong> ", selected$rating))
  })
  
  # Update Record
  observeEvent(input$updateRecord, {
    req(input$gameTable_rows_selected)
    current_filtered_data <- filtered_data()
    selected_row <- input$gameTable_rows_selected
    selected_game <- current_filtered_data[selected_row, "Game"]
    idx <- which(rv$games_data$Game == selected_game)
    
    if(length(idx) > 0) {
      rv$games_data[idx, "Game"] <- input$updateGame
      rv$games_data[idx, "Series"] <- input$updateSeries
      rv$games_data[idx, "Genre.s."] <- input$updateGenre
      rv$games_data[idx, "rating"] <- input$updateRating
      rv$games_data[idx, "Total.copies"] <- input$updateCopies
      rv$games_data[idx, "rating_category"] <- case_when(
        input$updateRating >= 4.5 ~ "Excellent",
        input$updateRating >= 4.0 ~ "Good",
        input$updateRating >= 3.0 ~ "Average",
        TRUE ~ "Below Average"
      )
      rv$games_data[idx, "is_series"] <- ifelse(input$updateSeries == "Standalone", "No", "Yes")
      showNotification("Record updated successfully!", type = "message")
    } else {
      showNotification("Failed to update record. Game not found in dataset.", type = "error")
    }
  })
  
  # Delete Record
  observeEvent(input$deleteRecord, {
    req(input$gameTable_rows_selected)
    selected_game <- filtered_data()[input$gameTable_rows_selected, "Game"]
    rv$games_data <- rv$games_data[rv$games_data$Game != selected_game, ]
    showNotification("Record deleted successfully!", type = "warning")
  })
  
  # Primary Visualization Plot
  output$interactivePlot1 <- renderPlotly({
    data <- filtered_data()
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available with current filters"))
    }
    
    if(input$plotType == "Ratings Distribution") {
      p <- ggplot(data, aes(x = rating, fill = rating_category)) +
        geom_histogram(bins = 15) +
        labs(title = "Game Ratings Distribution", x = "Rating", y = "Count") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set1")
    } else if(input$plotType == "Top Games by Copies") {
      p <- data %>%
        arrange(desc(Total.copies)) %>%
        head(10) %>%
        ggplot(aes(x = reorder(Game, Total.copies), y = Total.copies, fill = rating_category)) +
        geom_col() +
        coord_flip() +
        labs(title = "Top 10 Games by Total Copies (Millions)", x = "Game", y = "Total Copies (Millions)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set1")
    } else if(input$plotType == "Release Year Trend") {
      yearly_data <- data %>%
        group_by(release_year) %>%
        summarize(avg_rating = mean(rating, na.rm = TRUE),
                  count = n(),
                  avg_copies = mean(Total.copies, na.rm = TRUE))
      p <- ggplot(yearly_data, aes(x = release_year)) +
        geom_line(aes(y = avg_rating, color = "Average Rating"), size = 1) +
        geom_point(aes(y = avg_rating, color = "Average Rating"), size = 3) +
        labs(title = "Average Rating by Release Year", x = "Release Year", y = "Average Rating") +
        theme_minimal() +
        scale_color_manual(values = c("Average Rating" = "steelblue"), name = "Metric")
    } else if(input$plotType == "Genre Popularity") {
      genre_data <- data %>%
        mutate(genre_split = strsplit(as.character(Genre.s.), ", ")) %>%
        unnest(genre_split) %>%
        group_by(genre_split) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        head(10)
      p <- ggplot(genre_data, aes(x = reorder(genre_split, count), y = count, fill = count)) +
        geom_col() +
        coord_flip() +
        labs(title = "Top 10 Most Popular Genres", x = "Genre", y = "Count") +
        theme_minimal() +
        scale_fill_gradient(low = "skyblue", high = "darkblue")
    }
    
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # Secondary Visualization Plot
  output$interactivePlot2 <- renderPlotly({
    data <- filtered_data()
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available with current filters"))
    }
    
    if(input$secondPlotType == "Rating vs Metacritic") {
      data <- data[!is.na(data$metacritic), ]
      if(nrow(data) == 0) {
        return(plotly_empty() %>% layout(title = "No games with Metacritic scores available"))
      }
      plot_ly(data, x = ~metacritic, y = ~rating, color = ~rating_category, 
              text = ~Game, type = "scatter", mode = "markers",
              marker = list(size = 10, opacity = 0.8)) %>%
        layout(title = "Rating vs Metacritic Score",
               xaxis = list(title = "Metacritic Score"),
               yaxis = list(title = "User Rating"))
    } else if(input$secondPlotType == "Release Year vs Copies") {
      plot_ly(data, x = ~release_year, y = ~Total.copies, color = ~rating_category,
              text = ~Game, type = "scatter", mode = "markers",
              marker = list(size = ~Total.copies * 2, opacity = 0.7)) %>%
        layout(title = "Game Sales by Release Year",
               xaxis = list(title = "Release Year"),
               yaxis = list(title = "Total Copies (Millions)"))
    } else if(input$secondPlotType == "Genre Breakdown") {
      genre_data <- data %>%
        mutate(genre_split = strsplit(as.character(Genre.s.), ", ")) %>%
        unnest(genre_split) %>%
        group_by(genre_split) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        head(8)
      plot_ly(genre_data, labels = ~genre_split, values = ~count, type = "pie",
              hole = 0.4, marker = list(colors = rainbow(8))) %>%
        layout(title = "Genre Distribution", showlegend = TRUE)
    } else if(input$secondPlotType == "Rating by Publisher") {
      publisher_data <- data %>%
        group_by(Publisher.s.) %>%
        summarize(avg_rating = mean(rating, na.rm = TRUE),
                  games_count = n(),
                  total_copies = sum(Total.copies, na.rm = TRUE)) %>%
        arrange(desc(games_count)) %>%
        head(10)
      publisher_data$Publisher.s. <- as.character(publisher_data$Publisher.s.)
      plot_ly(publisher_data, x = ~Publisher.s., y = ~avg_rating, 
              size = ~games_count, color = ~total_copies,
              text = ~paste("Games: ", games_count, "<br>Total Copies: ", round(total_copies, 1), "M"),
              type = "scatter", mode = "markers",
              marker = list(opacity = 0.8)) %>%
        layout(title = "Top Publishers by Average Rating",
               xaxis = list(title = "Publisher", categoryorder = "array", 
                            categoryarray = publisher_data$Publisher.s.[order(publisher_data$avg_rating)]),
               yaxis = list(title = "Average Rating"))
    }
  })
  
  # Summary Statistics Output
  output$summary <- renderPrint({
    data <- filtered_data()
    if(nrow(data) == 0) {
      cat("No data available with current filters")
      return()
    }
    cat("Dataset Overview:\n")
    cat("Number of Games:", nrow(data), "\n")
    cat("Average Rating:", round(mean(data$rating, na.rm = TRUE), 2), "\n")
    cat("Average Metacritic Score:", round(mean(data$metacritic, na.rm = TRUE), 2), "\n")
    cat("Total Copies Sold (Millions):", round(sum(data$Total.copies, na.rm = TRUE), 2), "\n\n")
    cat("Rating Categories:\n")
    print(table(data$rating_category))
    cat("\n")
    cat("Top 5 Games by Rating:\n")
    print(data %>% arrange(desc(rating)) %>% select(Game, rating, release_year) %>% head(5))
    cat("\n")
    cat("Top 5 Publishers by Total Copies Sold:\n")
    top_publishers <- data %>%
      group_by(Publisher.s.) %>%
      summarize(total_copies = sum(Total.copies, na.rm = TRUE),
                avg_rating = mean(rating, na.rm = TRUE),
                games_count = n()) %>%
      arrange(desc(total_copies)) %>%
      head(5) %>%
      select(Publisher = Publisher.s_, `Total Copies (M)` = total_copies, 
             `Avg Rating` = avg_rating, `Games Count` = games_count)
    print(top_publishers)
  })
  
  # Top Publishers Visual UI
  output$topPublishersUI <- renderUI({
    data <- filtered_data()
    req(nrow(data) > 0)
    top_publishers <- data %>%
      group_by(Publisher.s.) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)
    colors <- c("#FF6B6B", "#6BCB77", "#4D96FF", "#FFC300", "#A66DD4", 
                "#FF9671", "#00C49A", "#FFB6C1", "#9EDAE5", "#F4A261")
    tagList(
      lapply(seq_along(top_publishers$Publisher.s.), function(i) {
        span(
          style = paste0(
            "display: inline-block; margin: 4px 6px; padding: 8px 14px; ",
            "background-color: ", colors[i %% length(colors) + 1], "; ",
            "color: white; border-radius: 16px; font-weight: 500; font-size: 14px;"
          ),
          top_publishers$Publisher.s.[i]
        )
      })
    )
  })
  
  # summary output section in the server function
  output$summary <- renderPrint({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      cat("No data available with current filters")
      return()
    }
    
    # statistics
    cat("Dataset Overview:\n")
    cat("Number of Games:", nrow(data), "\n")
    cat("Average Rating:", round(mean(data$rating, na.rm = TRUE), 2), "\n")
    cat("Average Metacritic Score:", round(mean(data$metacritic, na.rm = TRUE), 2), "\n")
    cat("Total Copies Sold (Millions):", round(sum(data$Total.copies, na.rm = TRUE), 2), "\n\n")
    
    cat("Rating Categories:\n")
    print(table(data$rating_category))
    cat("\n")
    
    cat("Top 5 Games by Rating:\n")
    print(data %>% arrange(desc(rating)) %>% select(Game, rating, release_year) %>% head(5))
    cat("\n")
    
    cat("Top 5 Publishers by Total Copies Sold:\n")
    top_publishers <- data %>%
      group_by(Publisher.s.) %>%
      summarize(total_copies = sum(Total.copies, na.rm = TRUE),
                avg_rating = mean(rating, na.rm = TRUE),
                games_count = n()) %>%
      arrange(desc(total_copies)) %>%
      head(5) %>%
      select(Publisher = Publisher.s., `Total Copies (M)` = total_copies, 
             `Avg Rating` = avg_rating, `Games Count` = games_count)
    
    print(top_publishers)
  })
  
  # Rating Category Plot
  output$ratingCategoryPlot <- renderPlotly({
    data <- filtered_data()
    if(nrow(data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available with current filters"))
    }
    rating_data <- data %>%
      group_by(rating_category) %>%
      summarize(count = n(),
                avg_copies = mean(Total.copies, na.rm = TRUE))
    plot_ly(rating_data, labels = ~rating_category, values = ~count, type = "pie",
            textinfo = "label+percent",
            insidetextorientation = "radial",
            marker = list(colors = c("darkgreen", "lightgreen", "orange", "red"))) %>%
      layout(title = "Games by Rating Category")
  })
}

# Run the Application
shinyApp(ui = ui, server = server)

#rsconnect::deployApp(
#  appName = "gameexplorer_shinyapp",
#  appPrimaryDoc = "GameExplorer_web_app.R"
#)

#deployed link - https://gameexplorer.shinyapps.io/gameexplorer_shinyapp/

