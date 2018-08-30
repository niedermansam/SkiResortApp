#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(ggplot2)
library(RCurl)
library(tidyverse)
library(leaflet)

resorts <- read.csv("http://142.93.183.164:3838/data/skiResorts_geocoded3.csv") %>% as.tibble()

# header board
header <- dashboardHeader(
  title = "Ski Resort Explorer"
  , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Map', tabName = 'map') 
    , menuItem('Table', tabName = 'table')
  ),
  sliderInput(inputId = "price", 
              label = "Maximum Ticket Price:", 
              min = 0, max = max(resorts$ticket, na.rm = T), value = max(resorts$ticket, na.rm = T), step = 1),
  sliderInput(inputId = "vert", 
              label = "Minimum Vertical Rise:", 
              min = 0, max = 4000, value = 0, step = 100),
  numericInput("lifts","Minimum Lifts:",0,0,40),
  selectInput("state","State/Province:",
              c("No Filter", resorts$state %>% as.character() %>% unique())
  ),
  radioButtons("labels","Popups:",
               list("On Click" = "popup","On Hover" = "label"), inline = T
  ),
  radioButtons("base_map", "Base Map:",
               c("Stamen Terrain" = "Stamen.Terrain",
                 "Open Street Map" = "OpenStreetMap",
                 "Open Topo Map" = "OpenTopoMap"),inline = T)
  
)

# Body board
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'map', 
      fluidRow(column(12,leafletOutput('map', height=500))),
      fluidRow(column(6, plotOutput("histPrice", width = "auto")),
               column(6, plotOutput("scatterPriceVert", width = "auto")))),
  tabItem(
    tabName = "table",
    fluidRow(
      column(12, dataTableOutput('table')))
  ))
  )


# Shiny UI
ui <- dashboardPage(
  title = 'test',
  header,
  sidebar,
  body
)

server <- function(input, output, session) { 
  
  resorts <- read.csv("http://142.93.183.164:3838/data/skiResorts_geocoded3.csv", stringsAsFactors = F) %>% as.tibble()
  
  output$map <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    setView(lng = -100, lat = 50, zoom = 4)
})



# A reactive expression that returns the set of resorts that are
# in bounds right now
resortInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(resorts[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  max_price <- input$price
  min_vert <- input$vert
  min_lifts <- input$lifts
  state_select <- input$state
  state_select <- ifelse(state_select == "No Filter",".",state_select)
  state_select <- paste(state_select,sep="|") %>% str_remove("\\|$")
  
  subset(resorts,
         lat >= latRng[1] & lat <= latRng[2] &
           lon >= lngRng[1] & lon <= lngRng[2] &
           ticket <= max_price & vertical >= min_vert &
           lifts >= min_lifts & str_detect(state, state_select))
})

## Make Ticket Prices Histogram
priceBreaks <- hist(plot = FALSE, resorts$ticket, breaks = 20)$breaks

output$histPrice <- renderPlot({
  # If no resorts are in view, don't plot
  if (nrow(resortInBounds()) == 0)
    return(NULL)
  
  print(ggplot(resortInBounds(), aes(x=ticket)) +
          geom_histogram(fill = "lightblue", color="black", bins = 30) +
          labs(title = "Histogram of Ticket Prices",
               subtitle = "Resorts in View",
               x = "Ticket Price") + 
          theme(plot.title = element_text(hjust = .5,size=16),
                plot.subtitle = element_text(hjust = .5,size=14),
                plot.background = element_rect(color = "black"), 
                axis.title = element_text(size=14),
                plot.margin = margin(10,20,10,10)))
})

output$scatterPriceVert <- renderPlot({
  # If no resorts are in view, don't plot
  if (nrow(resortInBounds()) == 0)
    return(NULL)
  
  print(ggplot(resortInBounds(),
               aes(y = acres, x = vertical,color = ticket)) + 
          geom_point(size=2) + 
          theme_grey() +
          scale_x_continuous(trans='log2', 
                             breaks = c(100,200,400,800,1600,3200,6400,12800), 
                             minor_breaks = NULL) + 
          scale_y_continuous(trans='log2', 
                             breaks = c(3,6,12,25,50,100,200,400,800,1600,3200,6400),
                             minor_breaks = NULL) + 
          labs(title = "Ski Resort Acres vs. Vertical Feet", 
               subtitle = "Colored by Ticket Price", 
               y = "Skiable Acres",
               x =" Vertical Rise") + 
          theme(plot.title = element_text(hjust = .5,size=16),
                plot.subtitle = element_text(hjust = .5,size=14),
                plot.background = element_rect(color = "black"), 
                axis.title = element_text(size=14),
                plot.margin = margin(10,1,10,10)) + 
          labs(color = "Price"))
})


observe({
  
  sites <- resortInBounds()
  label_type <- input$labels
  base_map <- input$base_map
  
  sites$label <- sprintf("<div style='text-align:center'><a href='%s' target='_blank'><strong>%s</strong></a><br/> %s %s %s %s</div>",
                           sites$url,sites$name, 
                           ifelse(!is.na(sites$vertical), paste0("<br/>Vertical Rise: <strong>",sites$vertical," ft.</strong>"),""),
                           ifelse(!is.na(sites$acres), paste0("<br/>Skiable Acres: <strong>",sites$acres," acres</strong>"),""),
                           ifelse(!is.na(sites$lifts), paste0("<br/>Number of Lifts: <strong>",sites$lifts," lifts</strong>"),""),
                           ifelse(!is.na(sites$ticket), paste0("<br/><br/><strong>Ticket Price: $",sites$ticket,"</strong>"),"")) %>% lapply(htmltools::HTML)

  
  if(label_type == "popup"){
    leafletProxy("map") %>% clearMarkers() %>% 
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 popup = sites$label)
    }
  
  if(label_type == "label"){
    leafletProxy("map") %>% clearMarkers() %>% 
      addProviderTiles(base_map) %>%
      addMarkers(lng = sites$lon,
                 lat = sites$lat,
                 label = sites$label)
    }
  })

observe({
  
  sites.table <- resortInBounds()
  
  sites.table <- sites.table %>% dplyr::select(name,state,ticket,vertical,acres, url)
  
  output$table <- renderDataTable(sites.table[order(sites.table$ticket),])
  
  
})

}

shinyApp(ui,server)


  