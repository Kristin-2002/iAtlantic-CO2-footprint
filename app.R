library(shiny) # for the app
library(ggplot2) # for the graphs
library(ggimage) # for images in the graphs
library(leaflet) # for the map
library(dplyr) # for data manipulation
library(sp) # for spatial data
library(geosphere) # for curved flight paths

### Parameters
# Distance of heavenly objects in km
planets <- data.frame(
  object = c("Earth", "Moon", "Venus", "Mars", "Mercury"),
  x = c(1, 400000, 41400000, 78340000, 91691000),
  y = c(5,5,5,5, 5),
  image = c("figures/earth.png","figures/moon.png","figures/venus.png",
            "figures/mars.png", "figures/mercury.png"),
  size_correction = c(0.25, 0.15, 0.25, 0.25, 0.25)
  )
asp.ratio <- 2.3

### Import data
GA2019 <- read.csv("data/iAtlantic_KickOff2019_CO2_emission.csv")
GA2020 <- read.csv("data/iAtlantic_GA2020_CO2_emission.csv")
travel_data <- dplyr::bind_rows(
  "GA2019" = GA2019, "GA2020" = GA2020, .id = "Meeting")
GA2019_minimum_travel <- read.csv("data/iAtlantic_KickOff2019_Detailed_Attendee_Data_wp2.csv")

### Data frames
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
   # To the stars!
   fluidRow(
     h1("Space travel!", align = "center"),
     h3("Sum of all traveled kilometers.", align = "center"),
     column(width = 2, 
            checkboxGroupInput(
              inputId = "meetings",  label = "What meetings to include?",
              selected = c("GA2019","GA2020"),
              choices = c(
                "2019 Kick-off meeting" = "GA2019", 
                "2020 GA Cape Town" = "GA2020"))),  
     column(width = 9, align="center",
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
  # Reactive calculations
  travel_distance_km <- reactive({
    temp <- travel_data %>%
      dplyr::filter(Meeting %in% input$meetings)
    sum(temp$distance_km)
  })
  
  rocket_pos <- reactive({
    data.frame(
      x = travel_distance_km(),
      y = 5,
      image = "figures/rocket.png",
      size_correction = 0.25
      )
  })
  x_lim <- reactive({
    x1 <- travel_distance_km()
    temp <- planets$x - x1
    temp <- temp[temp > 0]
    min(temp) + x1
  })
  
  label_pos <- reactive({
    x_lim <- x_lim()
    data.frame(
      x = c(rep(x_lim/2,2)), 
      y = c(7,6), 
      txt = c(paste("Distance Earth to next planet:",x_lim,"km"),
              paste("iAtlantic travel:",round(travel_distance_km()),"km")
      )
    )
  })
  
  output$total_travel_distance <- renderPlot({
    label_position <- label_pos()
    rocket <- rocket_pos()
    plot <- ggplot() +
      # Add planet images and names
      geom_image(
         data=planets, 
         aes(x=x, y=y, image=image),
         size = planets$size_correction, by = "width", 
         asp = asp.ratio) +
      geom_text(
        data = planets,
        aes(x=x, y=y-2, label = object),
        size = 5
      ) +
      # Add rocket image and arrow
      geom_image(
        data=rocket,
        aes(x=x, y=y, image=image),
        size = rocket$size_correction, by = "width", 
        asp = asp.ratio) +
       geom_segment(
         aes(x=1,y=5,xend=travel_distance_km(),yend=5),
         arrow = arrow(length = unit(0.5, "cm")),
         colour="orange", size = 2
       ) +
      # Add distance travelled
       geom_text(
         data = label_position,
         aes(x=x, y=y, label=txt),
         size=10
       ) +
       xlim(0-x_lim()/10,x_lim()+x_lim()/10) +
       ylim(2,8) +
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

