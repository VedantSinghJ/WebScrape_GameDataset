# Load necessary libraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# a. Discover ------------------------------------------------------------
# Reading data with proper NA handling
games <- read.csv("Game_dataset_testing.csv", na.strings = c("NA", "NULL"))

# Initial examination
dim(games) # Check dimensions
str(games) # View structure
summary(games) # Basic stats
glimpse(games)

# Checking missing values
colSums(is.na(games))

# Visualizations
ggplot(games, aes(x = rating)) + 
  geom_histogram(bins = 20, fill = "blue") +
  ggtitle("Rating Distribution")

# b. Structure -----------------------------------------------------------
# Converting date columns to proper format
games <- games %>%
  mutate(
    Release.date = as.Date(Release.date, format = "%B %d, %Y"),
    released = as.Date(released)
  )


# c. Cleaning ------------------------------------------------------------
# Cleaning 'Total copies sold'
games <- games %>%
  mutate(
    copies_millions = as.numeric(str_extract(Total.copies.sold, "\\d+\\.?\\d*")),
    Total.copies = copies_millions * 1000000
  ) %>%
  select(-Total.copies.sold, -copies_millions)



# Handles date discrepancies
games <- games %>%
  mutate(
    Release.date = coalesce(Release.date, released),
    release_year = year(Release.date)
  ) %>%
  select(-released)


# Remove redundant columns
games <- games %>% select(-platforms)

# Handle missing values
games <- games %>%
  mutate(
    metacritic = ifelse(metacritic < 0 | metacritic > 100, NA, metacritic)
  )


# Remove duplicates
games <- games[!duplicated(games$Game), ]


# Handles missing values in Categorical column
games <- games%>%
  mutate(Series = ifelse(Series == "—" | is.na(Series), "Standalone", Series))


# d. Enrich -------------------------------------------------------------
# Creating series indicator
games <- games %>%
  mutate(
    is_series = ifelse(Series == "—", "No", "Yes")
  )


# Creating rating categories
games <- games %>%
  mutate(
    rating_category = case_when(
      rating >= 4.5 ~ "Excellent",
      rating >= 4.0 ~ "Good",
      rating >= 3.0 ~ "Average",
      TRUE ~ "Below Average"
    )
  )


# e. Validate ------------------------------------------------------------
# Final quality checks
glimpse(games)
summary(games)


# Check date ranges
ggplot(games, aes(x = release_year)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Release Year Distribution")


# Validate numeric ranges
ggplot(games, aes(x = metacritic)) +
  geom_histogram(bins = 20) +
  ggtitle("Metacritic Score Distribution")


summary(games$Total.copies)


view(games)


write.csv(games,"GameSet_Cleaned.csv",row.names = FALSE)


#-----------------------------#
#----VISULIZATION ANALYSIS----#
#-----------------------------#


# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Load Data ------------------------------------------------------------
games <- read.csv("GameSet_Cleaned.csv", stringsAsFactors = FALSE) %>%
  mutate(Release.date = as.Date(Release.date))

# 2. Basic Exploration ----------------------------------------------------
cat("=== Basic Dataset Summary ===\n")
glimpse(games)
summary(games)



# 3. Sales Analysis -------------------------------------------------------
# Top 10 best-selling games
top_sellers <- games %>%
  arrange(desc(Total.copies)) %>%
  head(10)

ggplot(top_sellers, aes(x = reorder(Game, Total.copies), y = Total.copies/1e6)) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 10 Best-Selling Games",
       x = "Game", y = "Copies Sold (Millions)") +
  coord_flip()

# 4. Temporal Trends ------------------------------------------------------
# Sales by release year
yearly_sales <- games %>%
  group_by(release_year) %>%
  summarise(Total_Sales = sum(Total.copies, na.rm = TRUE))

ggplot(yearly_sales, aes(x = release_year, y = Total_Sales/1e6)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  labs(title = "Game Sales Trends Over Time",
       x = "Release Year", y = "Total Copies Sold (Millions)")

# 5. Rating Analysis ------------------------------------------------------
ggplot(games, aes(x = rating, y = Total.copies/1e6)) +
  geom_smooth(method = "loess", color = "darkgreen", fill = "lightgreen") +
  labs(title = "Trend of Ratings vs Sales",
       x = "User Rating", y = "Copies Sold (Millions)") +
  theme_light()


# Rating distribution by series status
ggplot(games, aes(x = is_series, y = rating, fill = is_series)) +
  geom_boxplot() +
  labs(title = "Rating Distribution: Series vs Standalone Games",
       x = "Part of Series", y = "Rating")

# 6. Genre Analysis -------------------------------------------------------
# Most common genres (top 10)
genre_counts <- games %>%
  count(Genre.s.) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(genre_counts, aes(x = reorder(Genre.s., n), y = n)) +
  geom_col(fill = "orange") +
  labs(title = "Most Common Game Genres",
       x = "Genre", y = "Count") +
  coord_flip()

# 7. Publisher Analysis ---------------------------------------------------
# Top publishers by total sales
publisher_sales <- games %>%
  group_by(Publisher.s.) %>%
  summarise(Total_Sales = sum(Total.copies, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) %>%
  head(10)

ggplot(publisher_sales, aes(x = reorder(Publisher.s., Total_Sales), y = Total_Sales/1e6)) +
  geom_col(fill = "darkred") +
  labs(title = "Top Publishers by Total Sales",
       x = "Publisher", y = "Total Copies Sold (Millions)") +
  coord_flip()



