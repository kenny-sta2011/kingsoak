library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(DT)
library(htmltools)
library(stringr)

# =============================================================================
# 1. DATA LOADING & PREPARATION
# =============================================================================

states <- st_read("geoBoundaries-NGA-ADM1.geojson", quiet = TRUE)
population <- read_csv("nga_admpop_adm1_2022.csv", show_col_types = FALSE)

states$shapeName <- str_trim(states$shapeName)
population$ADM1_EN <- str_trim(population$ADM1_EN)

# TYPO FIXED: Changed %in= back to the correct %in% operator
states$shapeName[states$shapeName %in% c("Abuja", "Federal Capital Territory", "FCT", "Federal Capital Territory (Abuja)")] <- "Federal Capital Territory"
population$ADM1_EN[population$ADM1_EN %in% c("Abuja", "Federal Capital Territory", "FCT", "Federal Capital Territory (Abuja)")] <- "Federal Capital Territory"

population <- population %>%
  mutate(Population_Density = if_else(Area_km2 > 0, T_TL / Area_km2, 0))

map_data <- states %>%
  left_join(population, by = c("shapeName" = "ADM1_EN")) %>%
  st_transform(4326)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = map_data$Population_Density,
  na.color = "#E2E8F0"
)

total_pop <- sum(map_data$T_TL, na.rm = TRUE)
total_area <- sum(map_data$Area_km2, na.rm = TRUE)
national_density <- round(total_pop / total_area, 1)

# =============================================================================
# 2. USER INTERFACE DESIGN (UI)
# =============================================================================

ui <- fluidPage(
  style = "background-color: #F8F9FA; color: #333333; padding: 15px;", 
  
  h3("Nigeria Data Map", style = "margin-top: 5px; font-weight: bold;"),
  p("Assignment Framework - Bambi - SS1 Geography - Everest Secondary School"),
  
  tags$div(
    style = "margin: 10px 0; padding: 5px;",
    selectInput(
      inputId = "selected_state",
      label = "Choose State Target:",
      choices = c("All States", sort(map_data$shapeName)),
      selected = "All States"
    )
  ),
  
  p(paste0("Pop Count: ", format(total_pop, big.mark = ","))),
  p(paste0("Size: ", format(round(total_area), big.mark = ","), " km²")),
  p(paste0("Density: ", national_density)),
  
  br(),
  leafletOutput("map", height = "500px"),
  
  br(),
  h4("Raw Spreadsheet Grid Table", style = "font-size: 14px;"),
  DTOutput("dataTable")
)

# =============================================================================
# 3. INTERACTIVE SERVER PROCESSING LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    bbox <- st_bbox(map_data)
    
    leaflet(map_data) %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(Population_Density),
        weight = 1,
        opacity = 1,
        color = "#FFFFFF", 
        fillOpacity = 0.8,
        layerId = ~shapeName
      ) %>%
      addLegend(
        pal = pal, 
        values = ~Population_Density, 
        opacity = 0.8, 
        title = "Pop Density (/km²)",
        position = "bottomright"
      )
  })
  
  observe({
    req(input$selected_state)
    proxy <- leafletProxy("map")
    
    if (input$selected_state == "All States") {
      bbox <- st_bbox(map_data)
      proxy %>% fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    } else {
      selected_polygon <- map_data %>% filter(shapeName == input$selected_state)
      bbox <- st_bbox(selected_polygon)
      proxy %>% fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    }
  })
  
  output$dataTable <- renderDT({
    datatable(
      map_data %>% st_drop_geometry() %>% select(shapeName, T_TL, Area_km2, Population_Density),
      options = list(dom = 't', pageLength = 40)
    )
  })
}

shinyApp(ui = ui, server = server)
