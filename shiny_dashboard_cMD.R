##### Load Packages #####
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(tidyverse)
library(leaflet)
library(scales)
library(rworldmap)
library(maps)
library(ggplot2)
library(terra)
library(enmSdmX)
library(forcats)
library(OmicsMLRepoR)
library(maptools)
library(bslib)

##### Load  & Tranform Data #####

# Load cMD metadata
cMD_meta <- getMetadata("cMD")
# Load in coordinates data
country_cords <- read.csv(file.path("C:/Users/kaigr/Desktop/country_coordinates.csv"))
colnames(country_cords)[1] <- "country"
# Load in the regions shapefile
map_reg <- terra::vect('C:/Users/kaigr/Desktop/regions.shp')
map_reg <- spatVectorToSpatial(map_reg)
map_reg <- readShapePoly('C:/Users/kaigr/Desktop/regions.shp')

# Transform NA values to "NA"
n_data <- cMD_meta %>%
  mutate(across(everything(), as.character))
n_data <- n_data %>% replace_na(as.list(rep("NA", ncol(n_data))))
# Set up base levels for each feature
ages <- c("Infant", "Children 2-11 Years Old",
          "Adolescent", "Adult", "Elderly", "NA")
sexes <- c("Female", "Male", "NA")
countries <- as.character(country_cords$country)
# Set up default filters for each feature
col_filters <- reactiveValues(agef = ages,
                              sexf = sexes,
                              countriesf = countries)


##### Dashboard Layout and User Interface #####

ui <- fluidPage(theme = shinytheme("cerulean"),
                useShinydashboard(),
                fluidRow(
                  layout_column_wrap(
                    value_box(
                      title = "Total Samples",
                      value = nrow(cMD_meta),
                      showcase = bsicons::bs_icon("bar-chart"),
                      theme = "blue"
                    ),
                    value_box(
                      title = "Total Countries",
                      value = length(unique(cMD_meta$country)),
                      showcase = bsicons::bs_icon("globe-europe-africa"),
                      theme = "teal"
                    ),
                    value_box(
                      title = "Total Disease/Cancer Types",
                      value = length(unique(cMD_meta$disease)),
                      showcase = bsicons::bs_icon("clipboard-pulse"),
                      theme = "blue"
                    )
                  ),
                  actionButton("do", "Reset Selection", icon("box-arrow-left"), 
                               style="color: #000000; background-color: #337ab7; border-color: #2e6da4"),
                  layout_column_wrap(
                    card(height=440,
                         card_title("World Map"),
                         card_body(
                           leafletOutput("worldmap"),
                           absolutePanel(top = 10, right = 10)
                         )
                    ),
                    card(height = 440,
                         card_body(card_title("Age Distribution"),
                                   plotOutput("ageplot", click = "ageplot_click"),
                                   verbatimTextOutput("printagef")
                         ),
                         card_body(card_title("Sex Distribution"),
                                   plotOutput("sexplot", click = "sexplot_click"),
                                   verbatimTextOutput("printsexf")
                         )
                    )
                  )
                )
)

##### Server inputs and outputs #####

server <- function(input, output){
  
  output$worldmap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1)) %>%
      addMiniMap(width = 100, height = 100, zoomLevelOffset = -5, toggleDisplay = T) %>%
      fitBounds(-180, -180, 200, 180) %>%
      setMaxBounds(-180, -180, 200, 180)
  })
  
  observe({
    # Get filtered data
    fdata <- n_data %>%
      filter(sex %in% col_filters$sexf) %>%
      filter(age_group %in% col_filters$agef)%>%
      filter(country %in% col_filters$countriesf)
    # Get curated_country data
    curated_country <- as.data.frame(fdata$country) %>% rename(curated_country = `fdata$country`)
    # Get country counts
    country_counts <- curated_country %>% group_by(curated_country) %>% dplyr::summarise(counts= n())
    # Join country codes to counts
    by <- join_by("country" == "curated_country")
    country_counts <- left_join(country_cords, country_counts, by=by)
    # Get regional counts
    regional_counts <- country_counts %>% group_by(REGION) %>% dplyr::summarise(reg_counts= sum(counts, na.rm = T))
    # Subset country_counts to relevant columns
    country_counts <- country_counts %>% select(country, ISO3, counts)
    
    # Creating the Map Data
    pal <- colorBin("Reds", domain = country_counts$counts, bins = 8)
    pal2 <- colorBin("Reds", domain = regional_counts$reg_counts, bins = 8)
    
    map_reg@data <- left_join(map_reg@data, regional_counts, 
                              by=join_by("REGION"))
    
    map <- joinCountryData2Map(country_counts, joinCode = "ISO3",
                               nameCountryColumn = "country",
                               nameJoinColumn = "ISO3")
    map@data <- left_join(map@data[,1:52], regional_counts, 
                          by=join_by("REGION"))
    leafletProxy("worldmap", data = map) %>%
      addTiles() %>% 
      clearShapes() %>% 
      addPolygons(fillColor = ~pal(map$counts),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  layerId = as.character(map$country),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = .8,
                    bringToFront = TRUE),
                  group = "countries",
                  label = ~paste(as.character(map$country),
                                 "Total Data Samples: ", as.character(map$counts))) %>%
      groupOptions("countries", zoomLevels = 2:20) %>%
      addPolygons(data = map_reg, fillColor = ~pal2(map_reg$reg_counts),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  layerId = as.character(map_reg$REGION),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = .8,
                    bringToFront = TRUE),
                  group = "regions",
                  label = ~paste(as.character(map_reg$REGION),
                                 "Total Data Samples: ", as.character(map_reg$reg_counts))) %>%
      groupOptions("regions", zoomLevels = 1)
  })
  
  ## Function to update filters
  update_filter <- function(current, input, original) {
    if (identical(current, original)) {
      new_filter <- input
    } else if (identical(current, input)) {
      new_filter <- original
    } else if (input %in% current) {
      new_filter <- current[-which(current == input)]
    } else if (!input %in% current) {
      new_filter <- c(current, input)
      new_filter <- new_filter[order(match(new_filter, original))]
    }
    return(new_filter)
  }
  
  ## Observing clicks
  observeEvent(input$ageplot_click, {
    current_age <- col_filters$agef
    input_age <- c(ages[round(input$ageplot_click$x, 0)])
    col_filters$agef <- update_filter(current_age, input_age, ages)
  })
  
  observeEvent(input$sexplot_click, {
    current_sex <- col_filters$sexf
    input_sex <- c(sexes[round(input$sexplot_click$x, 0)])
    col_filters$sexf <- update_filter(current_sex, input_sex, sexes)
  })
  
  observeEvent(input$worldmap_shape_click, {
    #capture the info of the clicked polygon
    click <- input$worldmap_shape_click
    #subset your table with the id of the clicked polygon
    if(click$group =="countries"){
      selected <- click$id
    }else if(click$group =="regions"){
      selected <- country_cords$country[which(country_cords$REGION == click$id)]
    }
    print(selected)
    current_countries <- col_filters$countriesf
    input_countries <- selected
    col_filters$countriesf <- update_filter(current_countries, input_countries, countries)
  })
  
  observeEvent(input$do, {
    #current_countries <- col_filters$countriesf
    #current_sex <- col_filters$sexf
    #current_age <- col_filters$agef
    
    
    col_filters$sexf <- sexes
    col_filters$agef <- ages
    col_filters$countriesf <- countries
    
    
    #col_filters$sexf <- update_filter(current_sex, input_sex, sexes)
    #col_filters$countriesf <- update_filter(current_countries, input_countries, countries)
    #col_filters$agef <- update_filter(current_age, input_age, ages)
  })

  output$ageplot <- renderPlot({
    # Get filtered data
    fdata <- n_data %>%
      filter(sex %in% col_filters$sexf)%>%
      filter(country %in% col_filters$countriesf)
    
    # Bin data for bar chart
    age_data <- as.data.frame(table(factor(fdata$age_group, levels = ages),
                                    useNA = "no"))
    age_data <- age_data[match(ages, age_data$Var1),]
    
    # Get bar colors based on click status
    bar_colors <- rep("darkgray", length(ages))
    bar_colors[which(ages %in% col_filters$agef)] <- "lightblue"
    
    # Plot data
    p <- age_data %>%
      mutate(Var1 = fct_relevel(Var1,
                                ages)) %>%
      ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = bar_colors, guide = "none")
    p
  })
  
  output$sexplot <- renderPlot({
    # Get filtered data
    fdata <- n_data %>%
      filter(age_group %in% col_filters$agef) %>%
      filter(country %in% col_filters$countriesf)
    
    # Bin data for bar chart
    sex_data <- as.data.frame(table(factor(fdata$sex, levels = sexes),
                                    useNA = "no"))
    sex_data <- sex_data[match(sexes, sex_data$Var1),]
    
    # Get bar colors based on click status
    bar_colors <- rep("darkgray", length(sexes))
    bar_colors[which(sexes %in% col_filters$sexf)] <- "lightblue"
    
    # Plot data
    p <- sex_data %>%
      mutate(Var1 = fct_relevel(Var1,
                                sexes)) %>%
      ggplot(aes(x = Var1, y = Freq, fill = Var1)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = bar_colors, guide = "none")
    p
  })
  
}


##### Run app #####

shinyApp(ui, server)