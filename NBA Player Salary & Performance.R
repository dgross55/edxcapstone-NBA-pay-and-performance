#list of packages
packages_to_install <- c("dplyr", "tidyverse", "psych", "data.table", "rvest", "ggplot2", "caret", "corrplot", "randomForest")

#automatic installation & loading of packages
for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

#download data from github
# Set the URL of the raw CSV file on GitHub
github_csv_url1 <- "https://raw.githubusercontent.com/dgross55/edxcapstone-NBA-pay-and-performance/main/Per%2036%20Minutes.csv"
github_csv_url2 <- "https://raw.githubusercontent.com/dgross55/edxcapstone-NBA-pay-and-performance/main/Advanced.csv"
# Set the destination folder where you want to save the dataset
destination_folder <- "data/"
# Create the destination folder if it doesn't exist
if (!dir.exists(destination_folder)) {
  dir.create(destination_folder, recursive = TRUE)
}
# Use the download.file function to download the datasets
download.file(github_csv_url1, destfile = file.path(destination_folder, "Per 36 Minutes.csv"), mode = "wb")
download.file(github_csv_url2, destfile = file.path(destination_folder, "Advanced.csv"), mode = "wb")
#grab the specific datasets
player_per36 <- fread("data/Per 36 Minutes.csv")
player_advanced <- fread("data/Advanced.csv")

#scrape for player salaries
#for loop years, links, & empty table
seasons1 <- seq(2003,2022)
seasons2 <- seq(2004,2023)
colseasons <- sprintf("%02d", 4:23)
link <- paste0("https://hoopshype.com/salaries/players/",seasons1,"-",seasons2,"/")
num_links <- length(link)
salaries <- data.frame()

#create handle_error function
handle_error <- function(page) {
  cat("Error processing value:", page, "\n")
  cat("Downloading CSV from GitHub...\n")
  
  # URL of the Salaries CSV file on GitHub
  githubfs_csv_url <- "https://raw.githubusercontent.com/dgross55/edxcapstone-NBA-pay-and-performance/main/Salaries.csv"
  # Set the destination folder where you want to save the dataset
  destination_folder <- "data/"
  # Download the dataset
  download.file(githubfs_csv_url, destfile = file.path(destination_folder, "Salaries.csv"), mode = "wb")
  # Import downloaded dataset
  salaries2 <- fread("data/Salaries.csv")
}

#flag to tell the loop to continue or not
continue_loop <- TRUE

#scraper for loop for player salaries
scraper <- for(page in 1:num_links){
  if (!continue_loop){
    break
  }
  #error handling since hoopshype not always reliable
  tryCatch({
  #to track for loop progress
  cat("Processing value:", page, "\n")
  #read the page
  sal <- read_html(link[page])
  #get the table
  sal <- sal |> html_table(header = TRUE)
  #make it a data frame
  sal <- as.data.frame(sal)
  #dynamic column names based on current loop
  current_column <- paste0("X",seasons1[page],".",colseasons[page],"...")
  #specify columns & rename
  sal <- select(sal, Player, !!as.name(current_column)) %>%
    rename("Salary" = !!as.name(current_column))
  #add the season to the table
  sal <- cbind(sal, Season = seasons2[page])
  
  #add output to data frame
  salaries <- rbind(salaries, sal)
  
  #to track for loop progress
  cat("Finished processing value:", page, "\n\n")
  }, 
  #error handling to just get the dataset from github repository
  error = function(e) {
    salaries <- handle_error(page)
    assign("continue_loop", FALSE, envir = .GlobalEnv) #set at global environment so that the for loop stops
  })
}

#change salaries column names to all lowercase to match other tables
colnames(salaries) <- tolower(colnames(salaries))
#convert salary column from character to numeric (take out $ signs and ,)
salaries$salary <- as.numeric(gsub("[\\$,]", "", salaries$salary))

#combine tables
#list common columns
common_cols <- c("seas_id", "season", "player_id", "player", "birth_year", "pos", "age", "experience", "lg", "tm", "g", "mp")
#get list of cols from player_advanced that are not common to both tables
padvanced_desired_cols <- setdiff(names(player_advanced), common_cols)
#create data frame that has values to join on, seas_id & player_id, and cols that are unique to player_advanced
player_advanced_sel <- player_advanced %>%
  select("seas_id", "player_id", padvanced_desired_cols)
#combine desired player_advanced cols & per36
player_stats <- merge(
  player_per36, 
  player_advanced_sel, 
  by = c("seas_id", "player_id")
  )
#combine player_stats & salaries, only keep rows that match, 31633 obs to 10842 obs
player_stats_sals <- merge(
  player_stats,
  salaries,
  by = c("player", "season")
)

#summary of table
summary(player_stats_sals)

#data preprocessing
#NAs
missing_values <- sapply(player_stats_sals, function(x) sum(is.na(x)))
print(missing_values)

#remove all NAs - 10842 obs to 9100
player_stats_sals <- player_stats_sals %>% select(-birth_year)
player_stats_sals <- na.omit(player_stats_sals)

#remove tables & objects no longer needed
rm(player_advanced, player_advanced_sel, player_per36, player_stats, sal, colseasons, 
   common_cols, current_column, link, num_links, package, packages_to_install,
   padvanced_desired_cols, page, scraper, seasons1, seasons2, missing_values, destination_folder,
   github_csv_url1, github_csv_url2)


#EDA
colnames(player_stats_sals)

#demographics
#visualization of age
player_stats_sals[, .(players = .N), by = age][, age := factor(age)][
  , ggplot(.SD, aes(age, players)) +
    geom_col(fill = "black", color = "white") +
    labs(title = "Player Age Frequency",
         x = "Age",
         y = "Frequency")]

#visualization of age by season
player_stats_sals[, .(players = .N), by = .(season, age)][, age := factor(age)][
  , ggplot(.SD, aes(age, players, fill = as.factor(age))) +
    geom_col(fill = "black", color = "white") +
    facet_wrap(~season, scales = "free_y", ncol = 4) +
    labs(title = "Player Age Frequency by Season",
         x = "Age",
         y = "Frequency")]

#age stats
describe(player_stats_sals [ , c('age')], fast=TRUE)   
table(player_stats_sals$season, player_stats_sals$age)
  
#visualization of experience
player_stats_sals[, .(players = .N), by = experience][, experience := factor(experience)][
  , ggplot(.SD, aes(experience, players)) +
    geom_col(fill = "black", color = "white") +
    labs(title = "Player Experience Frequency",
         x = "Experience",
         y = "Frequency")]

#visualization of experience by season
player_stats_sals[, .(players = .N), by = .(season, experience)][, experience := factor(experience)][
  , ggplot(.SD, aes(experience, players, fill = as.factor(experience))) +
    geom_col(fill = "black", color = "white") +
    facet_wrap(~season, scales = "free_y", ncol = 4) +
    labs(title = "Player Experience Frequency by Season",
         x = "Experience",
         y = "Frequency")]

#experience stats
describe(player_stats_sals [ , c('experience')], fast=TRUE) 
table(player_stats_sals$season, player_stats_sals$experience)

#pos visualization
player_stats_sals[, .(players = .N), by = pos][, pos := factor(pos)][
  , ggplot(.SD, aes(pos, players)) +
    geom_col(fill = "black", color = "white") +
    labs(title = "Position Frequency",
         x = "Position",
         y = "Frequency")]

#pos stats
table(player_stats_sals$pos)

#games & minutes played stats
describe(player_stats_sals [ , c('g')], fast=TRUE)
describe(player_stats_sals [, c('gs')], fast=TRUE)
describe(player_stats_sals [, c('mp')], fast=TRUE)

#per_36_min
#visualize fg_per_36_min
player_stats_sals[, .(fg_per_36_min)] %>%
  ggplot(aes(x = fg_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Field Goals Per 36 Min Frequency",
       x = "Field Goals Per 36 Min",
       y = "Frequency")

#visualize fg_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = fg_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Field Goals Per 36 Min by Season",
         x = "Season",
         y = "Field Goal Per 36 Min")]

#visualize fg_percent
player_stats_sals[, .(fg_percent)] %>%
  ggplot(aes(x = fg_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Field Goal Percent Frequency",
       x = "Field Goal Percent",
       y = "Frequency")

#visualize fg_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = fg_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Field Goal Percent by Season",
         x = "Season",
         y = "Field Goal Percent")]

#visualize x3p_per_36_min
player_stats_sals[, .(x3p_per_36_min)] %>%
  ggplot(aes(x = x3p_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "3s Per 36 Min Frequency",
       x = "3s Per 36 Min",
       y = "Frequency")

#visualize x3p_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = x3p_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "3s Per 36 Min by Season",
         x = "Season",
         y = "3s Per 36 Min")]

#visualize x3p_percent
player_stats_sals[, .(x3p_percent)] %>%
  ggplot(aes(x = x3p_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "3 Percent Frequency",
       x = "3 Percent",
       y = "Frequency")

#visualize x3p_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = x3p_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "3 Percent by Season",
         x = "Season",
         y = "3 Percent")]

#visualize x2p_per_36_min
player_stats_sals[, .(x2p_per_36_min)] %>%
  ggplot(aes(x = x2p_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "2s Per 36 Min Frequency",
       x = "2s Per 36 Min",
       y = "Frequency")

#visualize x2p_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = x2p_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "2s Per 36 Min by Season",
         x = "Season",
         y = "2s Per 36 Min")]

#visualize x2p_percent
player_stats_sals[, .(x2p_percent)] %>%
  ggplot(aes(x = x2p_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "2 Percent Frequency",
       x = "2 Percent",
       y = "Frequency")

#visualize x2p_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = x2p_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "2 Percent by Season",
         x = "Season",
         y = "2 Percent")]

#visualize ft_per_36_min
player_stats_sals[, .(ft_per_36_min)] %>%
  ggplot(aes(x = ft_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Free Throws Per 36 Min Frequency",
       x = "Free Throws Per 36 Min",
       y = "Frequency")

#visualize ft_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ft_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Free Throws Per 36 Min by Season",
         x = "Season",
         y = "Free Throws Per 36 Min")]

#visualize ft_percent
player_stats_sals[, .(ft_percent)] %>%
  ggplot(aes(x = ft_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Free Throw Percent Frequency",
       x = "Free Throw Percent",
       y = "Frequency")

#visualize ft_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ft_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Free Throw Percent by Season",
         x = "Season",
         y = "Free Throw Percent")]

#visualize orb_per_36_min
player_stats_sals[, .(orb_per_36_min)] %>%
  ggplot(aes(x = orb_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Offensive Rebounds Per 36 Min Frequency",
       x = "Offensive Rebounds Per 36 Min",
       y = "Frequency")

#visualize orb_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = orb_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Offensive Rebounds Per 36 Min by Season",
         x = "Season",
         y = "Offensive Rebounds Per 36 Min")]

#visualize drb_per_36_min
player_stats_sals[, .(drb_per_36_min)] %>%
  ggplot(aes(x = drb_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Defensive Rebounds Per 36 Min Frequency",
       x = "Defensive Rebounds Per 36 Min",
       y = "Frequency")

#visualize drb_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = drb_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Defensive Rebounds Per 36 Min by Season",
         x = "Season",
         y = "Defensive Rebounds Per 36 Min")]

#visualize ast_per_36_min
player_stats_sals[, .(ast_per_36_min)] %>%
  ggplot(aes(x = ast_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Assists Per 36 Min Frequency",
       x = "Assists  Per 36 Min",
       y = "Frequency")

#visualize ast_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ast_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Assists Per 36 Min by Season",
         x = "Season",
         y = "Assists Per 36 Min")]

#visualize stl_per_36_min
player_stats_sals[, .(stl_per_36_min)] %>%
  ggplot(aes(x = stl_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Steals Per 36 Min Frequency",
       x = "Steals  Per 36 Min",
       y = "Frequency")

#visualize stl_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = stl_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Steals Per 36 Min by Season",
         x = "Season",
         y = "Steals Per 36 Min")]

#visualize blk_per_36_min
player_stats_sals[, .(blk_per_36_min)] %>%
  ggplot(aes(x = blk_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Blocks Per 36 Min Frequency",
       x = "Blocks Per 36 Min",
       y = "Frequency")

#visualize blk_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = blk_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Blocks Per 36 Min by Season",
         x = "Season",
         y = "Blocks Per 36 Min")]

#visualize tov_per_36_min
player_stats_sals[, .(tov_per_36_min)] %>%
  ggplot(aes(x = tov_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Turnovers Per 36 Min Frequency",
       x = "Turnovers Per 36 Min",
       y = "Frequency")

#visualize tov_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = tov_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Turnovers Per 36 Min by Season",
         x = "Season",
         y = "Turnovers Per 36 Min")]

#visualize pf_per_36_min
player_stats_sals[, .(pf_per_36_min)] %>%
  ggplot(aes(x = pf_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Fouls Per 36 Min Frequency",
       x = "Fouls Per 36 Min",
       y = "Frequency")

#visualize pf_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = pf_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Fouls Per 36 Min by Season",
         x = "Season",
         y = "Fouls Per 36 Min")]

#visualize pts_per_36_min
player_stats_sals[, .(pts_per_36_min)] %>%
  ggplot(aes(x = pts_per_36_min)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Points Per 36 Min Frequency",
       x = "Points Per 36 Min",
       y = "Frequency")

#visualize pts_per_36_min by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = pts_per_36_min)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Points Per 36 Min by Season",
         x = "Season",
         y = "Points Per 36 Min")]

#advanced stats
#visualize per
player_stats_sals[, .(per)] %>%
  ggplot(aes(x = per)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Player Efficiency Rating Frequency",
       x = "Player Efficiency Rating",
       y = "Frequency")

#visualize per by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = per)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Player Efficiency Rating by Season",
         x = "Season",
         y = "Player Efficiency Rating")]

#per stats
describe(player_stats_sals [ , c('per')], fast=TRUE)

#visualize ts_percent
player_stats_sals[, .(ts_percent)] %>%
  ggplot(aes(x = ts_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "True Shooting Percentage Frequency",
       x = "True Shooting Percentage",
       y = "Frequency")

#visualize ts_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ts_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "True Shooting Percentage by Season",
         x = "Season",
         y = "True Shooting Percentage")]

#ts_percent stats
describe(player_stats_sals [ , c('ts_percent')], fast=TRUE)

#visualize usg_percent
player_stats_sals[, .(usg_percent)] %>%
  ggplot(aes(x = usg_percent)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Usage Percent Frequency",
       x = "Usage Percent",
       y = "Frequency")

#visualize usg_percent by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = usg_percent)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Usage Percent by Season",
         x = "Season",
         y = "Usage Percent")]

#usg_percent stats
describe(player_stats_sals [ , c('usg_percent')], fast=TRUE)

#visualize ows
player_stats_sals[, .(ows)] %>%
  ggplot(aes(x = ows)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Offensive Win Shares Frequency",
       x = "Offensive Win Shares",
       y = "Frequency")

#visualize ows by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ows)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Offensive Win Shares by Season",
         x = "Season",
         y = "Offensive Win Shares")]

#ows stats
describe(player_stats_sals [ , c('ows')], fast=TRUE)

#visualize dws
player_stats_sals[, .(dws)] %>%
  ggplot(aes(x = dws)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Defensive Win Shares Frequency",
       x = "Defensive Win Shares",
       y = "Frequency")

#visualize dws by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = dws)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Defensive Win Shares by Season",
         x = "Season",
         y = "Defensive Win Shares")]

#dws stats
describe(player_stats_sals [ , c('dws')], fast=TRUE)

#visualize ws
player_stats_sals[, .(ws)] %>%
  ggplot(aes(x = ws)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Win Shares Frequency",
       x = "Win Shares",
       y = "Frequency")

#visualize ws by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = ws)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Win Shares by Season",
         x = "Season",
         y = "Win Shares")]

#ws stats
describe(player_stats_sals [ , c('ws')], fast=TRUE)

#visualize obpm
player_stats_sals[, .(obpm)] %>%
  ggplot(aes(x = obpm)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Offensive Box Plus/Minus Frequency",
       x = "Offensive Box Plus/Minus",
       y = "Frequency")

#visualize obpm by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = obpm)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Offensive Box Plus/Minus by Season",
         x = "Season",
         y = "Offensive Box Plus/Minus")]

#obpm stats
describe(player_stats_sals [ , c('obpm')], fast=TRUE)

#visualize dbpm
player_stats_sals[, .(dbpm)] %>%
  ggplot(aes(x = dbpm)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Defensive Box Plus/Minus Frequency",
       x = "Defensive Box Plus/Minus",
       y = "Frequency")

#visualize dbpm by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = dbpm)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Defensive Box Plus/Minus by Season",
         x = "Season",
         y = "Defensive Box Plus/Minus")]

#dbpm stats
describe(player_stats_sals [ , c('dbpm')], fast=TRUE)

#visualize bpm
player_stats_sals[, .(bpm)] %>%
  ggplot(aes(x = bpm)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Box Plus/Minus Frequency",
       x = "Box Plus/Minus",
       y = "Frequency")

#visualize bpm by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = bpm)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Box Plus/Minus by Season",
         x = "Season",
         y = "Box Plus/Minus")]

#bpm stats
describe(player_stats_sals [ , c('bpm')], fast=TRUE)

#visualize vorp
player_stats_sals[, .(vorp)] %>%
  ggplot(aes(x = vorp)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Value Over Replacement Player Frequency",
       x = "Value Over Replacement Player",
       y = "Frequency")

#visualize vorp by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = vorp)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Value Over Replacement Player by Season",
         x = "Season",
         y = "Value Over Replacement Player")]

#vorp stats
describe(player_stats_sals [ , c('vorp')], fast=TRUE)

#salary
#visualize salary
player_stats_sals[, .(salary)] %>%
  ggplot(aes(x = salary)) +
  geom_density(fill = "black", color = "white", alpha = 0.7) +
  labs(title = "Salary Frequency",
       x = "Salary",
       y = "Frequency")

#visualize salary by season
player_stats_sals[, season := factor(season)][
  , ggplot(.SD, aes(x = season, y = salary)) +
    geom_boxplot(fill = "black", color = "black", alpha = 0.7) +
    labs(title = "Salary by Season",
         x = "Season",
         y = "Salary")]

#salary stats
describe(player_stats_sals [ , c('salary')], fast=TRUE)

#correlation plot
#remove non-numeric variables for corrplot
num_player_stats_sals <- player_stats_sals %>% select (-player, -season, -pos, -lg, -tm)
#relationship between different variables - specifically salary - what variables to use in model
cor_matrix <- cor(num_player_stats_sals)
corrplot(cor_matrix, type = "upper", method = "ellipse", tl.cex = 0.9)

#correlation coefficients for salary sorted by correlation
sort(cor_matrix[49, ], decreasing = TRUE)

#split into train & test
set.seed(1991)
split_index <- createDataPartition(player_stats_sals$salary, p = 0.8, list = FALSE)
train <- player_stats_sals[split_index, ]
test <- player_stats_sals[-split_index, ]

#linear model - top 10
linearm <- lm(salary ~ vorp + experience + ws + obpm + gs + pts_per_36_min + per + bpm + fg_per_36_min + ows, data = train)
summary(linearm)

#calculate RMSE - linear model
lm_sal_prediction <- predict(linearm, newdata = test)
res <- test$salary - lm_sal_prediction
lm_rmse <- sqrt(mean(res^2))
print(lm_rmse)

#table with differences - linear model
lm_results <- data.frame(
  true_sal = test$salary,
  modeled_sal = lm_sal_prediction,
  sal_diff = test$salary - lm_sal_prediction,
  player = test$player,
  season = test$season,
  team = test$tm
)

print(lm_results)

#random forest model
rfm <- randomForest(salary ~ ., data = train)
print (rfm)

#calculate RMSE - random forest
rf_sal_pred <- predict(rfm, newdata = test)
rf_rmse <- sqrt(mean((test$salary - rf_sal_pred)^2))
print(rf_rmse)

#table with differences - random forest
rf_results <- data.frame(
  true_sal = test$salary,
  modeled_sal = rf_sal_pred,
  sal_diff = test$salary - rf_sal_pred,
  player = test$player,
  season = test$season,
  team = test$tm
)

print(rf_results)

#compare results for a single player in both models
target_player <- "Kevin Durant"
print(subset(lm_results, player == target_player))
print(subset(rf_results, player == target_player))

