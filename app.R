library(shiny) # for the app
library(ggplot2) # for the graphs
library(ggimage) # for images in the graphs
library(leaflet) # for the map
library(dplyr) # for data manipulation
library(sp) # for spatial data
library(geosphere) # for curved flight paths

### Parameters
lunar_distance_km <- 400000
asp.ratio <- 2.3

### Import data
GA2019 <- read.csv("data/iAtlantic_KickOff2019_CO2_emission.csv")
GA2019_minimum_travel <- read.csv("data/iAtlantic_KickOff2019_Detailed_Attendee_Data_wp2.csv")
travel_distance_km <- sum(GA2019$distance_km)

### Data frames
image_pos <- data.frame(
  x = c(1, lunar_distance_km, travel_distance_km),
  y = c(5,5,5),
  image = c("figures/earth.png","figures/moon.png","figures/rocket.png"),
  size_correction = c(0.25, 0.15, 0.25)
)
label_pos <- data.frame(
  x = c(rep(lunar_distance_km/2,2)), 
  y = c(7,6), 
  txt = c(paste("Lunar distance: 400000 km"),
          paste("iAtlantic travel:",round(travel_distance_km),"km")
  )
)

iAtlantic_icon <- makeIcon("figures/iatlantic.png",
                           iconWidth = 20, iconHeight = 40)

edinburgh <- c(-3.1882670, 55.95325) #lon lat
pretoria <- c(28.2294000, -25.70690) # lon lat

cities <- GA2019_minimum_travel %>%
  select(City, Country, Longitude, Latitude) %>%
  distinct() %>%
  group_by(City) %>%
  group_split()
travel_lines <- list()
for(i in 1:length(cities)){
  temp <- gcIntermediate(edinburgh, as.numeric(cities[[i]][,3:4]),
                         n=100, addStartEnd=TRUE,
                         sp=FALSE)
  travel_lines[[i]] <- sp::Line(temp)
}
travel_lines <- sp::Lines(travel_lines, ID = "id")
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("iAtlantic Carbon Footprint"),
   # To the moon and back
   fluidRow(
     column(width = 12, align="center",
            plotOutput("total_travel_distance", width = "900px",  height = "300px")
      )
    ),
  # Map
  fluidRow(
    column(width = 12, align="center",
           leafletOutput("mymap", width = "900px",  height = "500px")
    )
  )
)
server <- function(input, output) {
   output$total_travel_distance <- renderPlot({
     plot <- ggplot() +
       geom_image(
         data=image_pos, 
         aes(x=x, y=y, image=image),
         size = image_pos$size_correction, by = "width", 
         asp = asp.ratio) +
       geom_segment(
         aes(x=1,y=5,xend=travel_distance_km,yend=5),
         arrow = arrow(length = unit(0.5, "cm")),
         colour="orange", size = 2
       ) +
       geom_text(
         data = label_pos,
         aes(x=x, y=y, label=txt),
         size=10
       ) +
       xlim(-lunar_distance_km/10,lunar_distance_km+lunar_distance_km/10) +
       ylim(4,8) +
       theme(
         aspect.ratio = asp.ratio
      ) +
        theme_nothing()
    plot
   })
   
   output$mymap <- renderLeaflet(
     m <- leaflet() %>%
       addProviderTiles(providers$Stamen.Watercolor) %>% #Stamen.TonerLite Esri.OceanBasemap "OpenStreetMap.HOT"
       #addTiles() %>%
       addMarkers(lat = GA2019_minimum_travel$Latitude, 
                  lng = GA2019_minimum_travel$Longitude,
                  popup = GA2019_minimum_travel$City,
                  icon = iAtlantic_icon) %>%
       leaflet::addPolylines(data = travel_lines, # spatial lines from sp package
                             color = "orange",
                             opacity = 1) 
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

