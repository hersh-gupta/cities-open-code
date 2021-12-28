library(tidygeocoder)
library(leaflet)
library(htmltools)

# Geocode cities
cities_geo <- geocode(cities_urls, city = City, state = State, method = 'osm')

# Link icons
github_icons <- iconList(
  yes = makeIcon("assets/github-dark.png", "assets/github-dark.png", 16, 13),
  no = makeIcon("assets/none-red-half.png", "assets/none-red-half.png", 16, 16)
)

# Map cities
cities_geo %>%
  mutate(github = if_else(on_github,"yes","no"),
         popup = if_else(on_github, paste0("<b>",City,", ",State,"</b>","<br>",
                        "Public Repos: ", public_repos,"<br>",
                        "<a href='",`Github URL`,"'>GitHub</a>"),
                        NULL
                        )) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(icon = ~github_icons[github],label = ~htmlEscape(City),
             popup = ~popup, options = popupOptions(autoPan = F,closeButton = T,keepInView = F,autoClose = T, closeOnClick = F))

  