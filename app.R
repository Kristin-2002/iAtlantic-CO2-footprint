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

iAtlantic_icon <- makeIcon("figures/iatlantic.png",
                           iconWidth = 20, iconHeight = 40)
image_house <- "figures/house.png"

algae_co2_uptake_ton_perKM2_perYEAR <- 1500
trees_co2_uptake_ton_perKM2_perYEAR <- 213
latitude_in_km <- 111
longitude_in_km_EQUATORIAL <- 111

typical_US_household_ton_co2_perYEAR <- 48

### Import data
GA2019 <- read.csv("data/iAtlantic_KickOff2019_CO2_emission.csv")
MSM75a <- read.csv("data/2018_MSM75_emissions_to_harbour.csv")
MSM75b <- read.csv("data/2018_MSM75_emissions_back.csv")
PS116_1a <- read.csv("data/2018_PS116_1_emissions_to_harbour.csv")
PS116_1b <- read.csv("data/2018_PS116_1_emissions_back.csv")
PS116_12a <- read.csv("data/2018_PS116_1and2_emissions_to_harbour.csv")
PS116_12b <- read.csv("data/2018_PS116_1and2_emissions_back.csv")
PS116_2a <- read.csv("data/2018_PS116_2_emissions_to_harbour.csv")
PS116_2b <- read.csv("data/2018_PS116_2_emissions_back.csv")

### Data frames
travel_data <- dplyr::bind_rows(
  "GA2019" = GA2019, 
  "MSM75a" = MSM75a, 
  "MSM75b" = MSM75b,
  "PS116_1a" = PS116_1a, 
  "PS116_1b" = PS116_1b, 
  "PS116_12a" = PS116_12a, 
  "PS116_12b" = PS116_12b, 
  "PS116_2a" = PS116_2a, 
  "PS116_2b" = PS116_2b,
  .id = "Meeting")

TravelCities <- data.frame(
  Meeting = c("GA2019", 
              "MSM75a",
              "MSM75b", 
              "PS116_1a",
              "PS116_1b", 
              "PS116_12a", 
              "PS116_12b", 
              "PS116_2a", 
              "PS116_2b"),
  TravelCity = c("Edinburgh",
                 "Reykjavik",
                 "Reykjavik",
                 "Bremerhaven",
                 "Las Palmas",
                 "Bremerhaven",
                 "Cape Town",
                 "Las Palmas",
                 "Cape Town"),
  toLon = c(-3.1883749,
            -21.9422367,
            -21.9422367,
            8.5865509,
            -15.43798,
            8.5865509,
            18.420499,
            -15.43798,
            18.420499),
  toLat = c(55.95335,
            64.14598,
            64.14598,
            53.55223,
            28.12003,
            53.55223,
            -33.9197,
            28.12003,
            -33.9197)
)

travel_data <- dplyr::left_join(travel_data, TravelCities, by = "Meeting") %>%
  dplyr::mutate(tripID = 1:nrow(travel_data))

# trip_CO2_palette <- colorNumeric(palette = "inferno",
#                                  domain = travel_data$co2_kg)
  
# User Interface
ui <- fluidPage(
   titlePanel("iAtlantic Carbon Footprint"),
   # To the stars!
   fluidRow(
     h1("How far have we travelled?", align = "center"),
     column(width = 2, 
            checkboxGroupInput(
              inputId = "meetings",  label = "What meetings to include?",
              selected = c("GA2019", "MSM75", "PS116"),
              choices = c(
                "2019 Kick-off meeting" = "GA2019", 
                "2018 Cruise MSM75" = "MSM75",
                "2018 Cruise PS116 (Leg 1 and 2)" = "PS116"))),  
     column(width = 9, align="center",
            plotOutput("total_travel_distance", width = "900px",  height = "300px")
      )
    ),
  
   # Map
  fluidRow(
    h1("Where have we travelled?", align = "center"),
    column(width = 12, align="center",
           #leafletOutput("mymap", width = "900px",  height = "500px")
           leafletOutput("mymap")
    )
  ),
  
  # CO2 sum
  fluidRow(
    h1("What is our CO2 emission?", align = "center"),
    h3(textOutput("co2_sum"), align = "center"),
    column(width = 12, align="center",
           plotOutput("total_co2"))
  ),
  
  # Houses
  fluidRow(
    h1("How many households is that?", align = "center"),
    column(width = 12, align = "center", plotOutput("householdsPlot"))
  ),
  
  fluidRow(
    h1("How much ocean is needed to compensate our emissions?", align = "center"),
    column(width = 12, align = "center", leafletOutput("algal_map"))
  )
)

server <- function(input, output) {
  # Reactive calculations
  subset_data <- reactive({
      dplyr::filter(travel_data, grepl(paste(input$meetings, collapse = "|"), Meeting))
  })
  
  travel_distance_km <- reactive({
    temp <- subset_data()
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
      x = c(x_lim/2), 
      y = c(7), 
      txt = c(paste(round(travel_distance_km()),"km"))
      # x = c(rep(x_lim/2,2)), 
      # y = c(7,6), 
      # txt = c(paste("Distance Earth to next planet:",x_lim,"km"),
      #         paste("iAtlantic travel:",round(travel_distance_km()),"km")
      # )
    )
  })
  
  # Render
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
   
  travel_lines <- reactive({
    dt <- subset_data()
    trlns <- list()
    for(i in 1:nrow(dt)){
      temp <- geosphere::gcIntermediate(
        p1 = dt[i,c("longitude", "latitude")], 
        p2 = dt[i,c("toLon", "toLat")],
        n=100, addStartEnd=TRUE,
        sp=FALSE)
      trlns[[i]] <- sp::Line(temp)
    }
    sp::Lines(trlns, ID = "id")
  })
  
   output$mymap <- renderLeaflet(
     m <- leaflet() %>%
       addProviderTiles(providers$Stamen.Watercolor) %>% #Stamen.TonerLite Esri.OceanBasemap "OpenStreetMap.HOT"
       addMarkers(data = subset_data(),
                  lat = ~latitude, 
                  lng = ~longitude,
                  popup = ~city,
                  icon = iAtlantic_icon) %>%
        leaflet::addPolylines(data = travel_lines(),  # spatial lines from sp package
                              color = "blue", #trip_CO2_palette() Give line colour based on co2 emissions
                              opacity = 1) 
   )
   
   co2_totals <- reactive({
     subset_data() %>%
       dplyr::group_by(Meeting) %>%
       dplyr::summarise(co2_ton = sum(co2_kg)/1000)
     
   })
   output$co2_sum <- renderText(
     paste(round(sum(co2_totals()$co2_ton)), " tons")
   )
   output$total_co2 <- renderPlot({
     plot <- ggplot() +
       geom_col(data = co2_totals(),
                aes(x=Meeting, y=co2_ton)) +
       theme_bw()
     plot
   })
   
   number_of_households <- reactive({
     ceiling(sum(co2_totals()$co2_ton)/typical_US_household_ton_co2_perYEAR)
   })
   
   house_holds <- reactive({
     data.frame(
        x=c(1:number_of_households()), 
        y=rep(1, number_of_households()), 
        image=rep(image_house, number_of_households())
    )
   })
   
   output$householdsPlot <- renderPlot({
     plot <- ggplot(data = house_holds()) +
       geom_image(aes(x=x,y=y,image=image), asp = asp.ratio) +
       theme_nothing()
     plot
   })
   
   #0, -20
  compensation_area_km2 <- reactive({
    total <- sum(co2_totals()$co2_ton)
    total / algae_co2_uptake_ton_perKM2_perYEAR
  })
   
   output$algal_map <- renderLeaflet(
     m <- leaflet() %>%
       setView(lng = -20, lat = 0, zoom = 10) %>%
       addProviderTiles(providers$Esri.OceanBasemap) %>% #Stamen.TonerLite Esri.OceanBasemap "OpenStreetMap.HOT"
       addRectangles(lng1 = -20, lat1 = 0,
                     lng2 = -20+(sqrt(compensation_area_km2())/longitude_in_km_EQUATORIAL),
                     lat2 = sqrt(compensation_area_km2())/latitude_in_km,
                     popup = paste0(compensation_area_km2(), " km2"))
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

