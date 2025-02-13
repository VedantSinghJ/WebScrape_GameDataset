# Install packages if needed:
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("tidyr")
# install.packages("stringr")

library(rvest)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(stringr)

# Define the Wikipedia URL for best-selling PC games
wiki_url <- "https://en.wikipedia.org/wiki/List_of_best-selling_PC_games"

# Read the page
wiki_page <- read_html(wiki_url)

# Extract all tables with the "wikitable" class.
tables <- wiki_page %>% html_nodes("table.wikitable")

# For this page, check which table contains the game data.
# Often the first table is the one we need. Inspect its structure.
best_selling_table <- tables[[1]] %>% html_table(fill = TRUE)

# View the first few rows of the table to verify its structure.
head(best_selling_table)

# Rename the title column to "Game" (adjust if the column name is different)
games_wiki <- best_selling_table %>%
  rename(Game = Game) %>%
  # Optionally, select only the columns you need. Here we keep all columns.
  select(Game, everything())

# Inspect the cleaned Wikipedia dataset
print(games_wiki)



# Replace with your actual RAWG API key
api_key <- "d3044c3ce36a482c9c275abcd8c82147"

get_game_details_rawg <- function(game_name, api_key) {
  base_url <- "https://api.rawg.io/api/games"
  query_params <- list(search = game_name, key = api_key)
  
  res <- GET(base_url, query = query_params)
  
  # If the HTTP request fails, return NA values
  if (http_error(res)) {
    return(data.frame(
      Game = game_name,
      rating = NA,
      metacritic = NA,
      released = NA,
      platforms = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  content_json <- content(res, as = "text", encoding = "UTF-8")
  content_list <- fromJSON(content_json, flatten = TRUE)
  
  # If no results are found, return NA values
  if (length(content_list$results) == 0) {
    return(data.frame(
      Game = game_name,
      rating = NA,
      metacritic = NA,
      released = NA,
      platforms = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Use the first result from the search results (as a data frame row)
  game_data <- content_list$results[1, ]
  
  # Check if "platforms" exists in the result
  if ("platforms" %in% names(game_data)) {
    platforms_data <- game_data[["platforms"]]
    if (!is.null(platforms_data) && is.list(platforms_data)) {
      platforms_str <- paste(sapply(platforms_data, function(x) x$platform$name), collapse = ", ")
    } else {
      platforms_str <- NA
    }
  } else {
    platforms_str <- NA
  }
  
  data.frame(
    Game = game_name,
    rating = if (!is.null(game_data$rating)) game_data$rating else NA,
    metacritic = if (!is.null(game_data$metacritic)) game_data$metacritic else NA,
    released = if (!is.null(game_data$released)) game_data$released else NA,
    platforms = platforms_str,
    stringsAsFactors = FALSE
  )
}

# Test the function with a known game (e.g., "Minecraft" or another popular PC game)
test_rawg <- get_game_details_rawg("Minecraft", api_key)
print(test_rawg)



# Set a delay to be polite to the API (in seconds)
delay_seconds <- 1

# Loop over each game title in the Wikipedia dataset
rawg_list <- lapply(games_wiki$Game, function(game) {
  Sys.sleep(delay_seconds)  # Delay to avoid hitting rate limits
  get_game_details_rawg(game, api_key)
})

# Combine the list of data frames into one data frame
rawg_data <- bind_rows(rawg_list)

# Inspect the RAWG data collected
head(rawg_data)


# Merge the two datasets on the "Game" column
final_data <- left_join(games_wiki, rawg_data, by = "Game")

# View the final merged dataset
print(head(final_data))
View(final_data)

write.csv(final_data, file = "Game_dataset.csv", row.names = FALSE)


