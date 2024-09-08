# The order of loading is important, leave the dependencies in this order

library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)

# data here - https://dark-star-161610.appspot.com/secured/_book/data/australian_marriage_law_postal_survey_2017.csv

vote <- read.csv("australian_marriage_law_postal_survey_2017.csv")

# shapefile - https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016?OpenDocument
# Commonwealth Electoral Divisions ASGS Ed 2016 Digital Boundaries in ESRI Shapefile
national<-readShapeSpatial("1270055003_ced_2016_aust_shape/CED_2016_AUST.shp",
                           delete_null_obj=TRUE)
class(national)
head(national@data)

print(object.size(national), units = "MB")
# This is too large. Speed it up with
national_simp <- gSimplify(national, tol = .001, topologyPreserve=TRUE)
national_simp <- SpatialPolygonsDataFrame(national_simp,
                                          data=national@data)
print(object.size(national_simp), units ="MB")

# before we bind data, ensure that each dataset has the same variable (column)
# both should contain CED_NAME16
names(vote)
names(national_simp)

# merge
electorates_vote<-sp::merge(national_simp, vote, by="CED_NAME16")

# map object
map <- leaflet(electorates_vote) %>%
  setView(lng = 134, lat = -28, zoom = 4)
map %>% addTiles()

# outlines
map %>% addPolygons()

# informative
pal <- colorNumeric(
  "RdBu",
  domain = electorates_vote$yes_perc
)
map %>% addPolygons(
  fillColor = ~pal(yes_perc),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)

# Now let's add some information, title, labels etc

labels <- sprintf(
  "<strong>%s</strong><br/>%g%% Yes",
  electorates_vote$CED_NAME16,
  electorates_vote$yes_perc
) %>% lapply(htmltools::HTML)

library(htmlwidgets)
library(htmltools)

title <- tags$div(
  HTML('<h3>Australian Marriage Law Postal Vote Survey 2017</h3>')
)

map %>% addPolygons(
  fillColor = ~pal(yes_perc),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal,
            values = ~yes_perc,
            opacity = 0.7, title = "Voted Yes",
            position = "bottomright",
            labFormat = labelFormat(suffix = "%")) %>%
  addControl(title, position = "topright")