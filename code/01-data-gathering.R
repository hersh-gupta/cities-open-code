library(tidyverse)
library(httr)
library(glue)

# Read in data
cities <- read_csv("data/USCities_Github.csv")

# Github API auth
auth <- authenticate("xxxxx",
                     "xxxxxxxxxxxxxxxxxxxxxxxxxxxx")

# Add urls
cities_urls <- cities %>%
  mutate(on_github = !is.na(`Github URL`),
         profile = str_extract(cities$`Github URL`, "(?<=(\\.com\\/)).*"),
         user_url = if_else(!is.na(profile), paste0("https://api.github.com/users/",profile),NA_character_))

# Number of public repos
cities_urls$public_repos <- NA_integer_
cities_urls$repos_url <- NA_character_

for(i in seq_along(cities_urls$user_url)){
  if(is.na(cities_urls$user_url[i])) {
    next
  }
  else{
    user <- GET(cities_urls$user_url[i], auth)
    user_cont <- content(user)
    cities_urls$repos_url[i] <- user_cont$repos_url
    cities_urls$public_repos[i] <- user_cont$public_repos
    }
}

# Get repos

loops <- ceiling(cities_urls$public_repos/100)

cities_urls$total_forks <- NA_integer_
cities_urls$total_watchers <- NA_integer_
cities_urls$commits_year <- NA_integer_


for(i in seq_along(cities_urls$repos_url)) {
  if(is.na(cities_urls$repos_url[i]) | cities_urls$public_repos[i] == 0) {
    next
  } else {
    
    # Page through all repos and get all
    repos_cont_full <- list()
    
    for(j in 1:loops[i]) {
      repos <- GET(cities_urls$repos_url[i], query = list(type = "public", per_page = 100, page = j), auth)
      repos_cont <- content(repos)
      repos_cont_full <- append(repos_cont_full, repos_cont)
    }
    
    # Summarize basic stats info and save
    basic_stats <- tibble()
    
    basic_stats <- repos_cont_full %>% 
      map_df(`[`,c("forks_count","watchers_count")) %>%
      summarize_all(~sum(.x, na.rm = T))
    
    cities_urls$total_forks[i] <- basic_stats$forks_count[1]
    cities_urls$total_watchers[i] <- basic_stats$watchers_count[1]
    
    # Loop through each repo for commit activity in the last year
    commits_repo <- numeric()
      
    for(k in 1:length(repos_cont_full)){
    commits <- GET(paste0(repos_cont_full[[k]]$url,"/stats/commit_activity"), auth)
    commits_cont <- content(commits)
    if(is_empty(commits_cont)) {next} else {
    commits_repo[k] <- commits_cont %>% map_dfr(`[`, "total") %>% summarize(commits = sum(total, na.rm = T)) %>% pluck("commits")
    }
    }
    cities_urls$commits_year[i] <- sum(commits_repo, na.rm = T)
  }
}

# Create grades per city
cities_scores <- cities_urls %>%
  select(City,State,public_repos,total_forks,total_watchers,commits_year) %>%
  mutate_at(vars(public_repos:commits_year), ~(.-min(.,na.rm = T))/(max(.,na.rm = T)-min(.,na.rm = T))) %>% 
  rowwise() %>%
  mutate(score = sum(c_across(public_repos:commits_year),na.rm = T),
         grade = case_when(score > 2 ~ "A",
                           score > 1 & score < 2  ~ "A-",
                           score > .5 & score < 1  ~ "B",
                           score > 0.1 & score < .5  ~ "C",
                           score > 0.005 & score < .1  ~ "D",
                           score < 0.005  ~ "F"
         ) %>% factor(levels = c("A","A-","B","C","D","F"))) %>%
  arrange(desc(score)) %>%
  select(City, State, score, grade)

# Join scores and cities
cities_table <- cities_urls %>% 
  select(City,State,`Github URL`,public_repos,total_forks,total_watchers,commits_year) %>%
  mutate_at(vars(public_repos:commits_year), ~ifelse(is.na(.), 0L, .)) %>%
  left_join(cities_scores, by = c("City","State")) %>%
  arrange(desc(score)) %>%
  select(City, State, grade, everything(), -score) %>%
  mutate(City = if_else(!is.na(`Github URL`), paste0("<a href='",`Github URL`,"'>", City, "</a>"), City)) %>%
  select(-`Github URL`) %>%
  mutate(grade = case_when(grade == "A" ~ '<img src="assets/A.svg" />',
                           grade == "A-" ~ '<img src="assets/A-.svg" />',
                           grade == "B" ~ '<img src="assets/B.svg" />',
                           grade == "C" ~ '<img src="assets/C.svg" />',
                           grade == "D" ~ '<img src="assets/D.svg" />',
                           grade == "F" ~ '<img src="assets/F.svg" />')) 

# Export
write_rds(cities_table, "cities_table.rds")
