
# R Shiny Tool including 
# 1. Leaflet map 
# 2. Tablet showing covid19 cases by province and day
# 3. Predictive model(s)

# Exploring Leaflet:  https://rstudio.github.io/leaflet/ 
# Save images https://stackoverflow.com/questions/31336898/how-to-save-leaflet-in-r-map-as-png-or-jpg-file/34672309 
# https://github.com/rstudio/leaflet/blob/master/inst/examples/polygon-colors.R

library(shiny)
library(leaflet)
library(ggplot2)
library(DT)

### Read geojson file for DA16s:
geo <- geojsonio::geojson_read("geo/canada.geojson", what = "sp") #sp = SpatialPoints

### Read in Covid19 Data
# df=read.csv("data/covid19.csv")
df=read.csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")

# Convert date variable to date format 
df$date2 <- as.Date(df$date, format = "%d-%m-%Y")

# Subset dateframe to keep only complete information i.e. everything one day behind 
todays_date <- Sys.Date()
df <- df[df[["date2"]] < todays_date, ]

# Add pr_id to based on prname. This is required to link to the geo file 
pr_id=read.csv("data/lookup_pr_id.csv",colClasses="character",na.strings="?")
pr_id=pr_id[match(df$prname,pr_id$pr_name),]
df$pr_id=pr_id$pr_id #Add in to geographic file

# Restrict to latest data and data of interest to add to geo data
geo_info <- df[df$date2 == max(df$date2),]
tidy_table1 <- df[df$date2 == max(df$date2),]

# Match covid19 data to the geo data and add variables of interest to geo data
geo_info=geo_info[match(geo$cartodb_id,geo_info$pr_id),]
geo$total_cases=geo_info$numtotal #Add in to geographic file
geo$deaths=geo_info$numdeaths #Add in to geographic file

# Create a colour palette between the listed colours"
pal <- colorNumeric(colorRamp(c("#4682b4","firebrick2"), interpolate = "spline"), na.color="grey",NULL)

ui <- fluidPage(
  
  titlePanel("Canada's Covid-19 Cases"),
  
  # Add Text: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  p("This data for this app was retrieved from https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html", style = "font-family: 'times'; font-si16pt"),
  p(paste("Data displayed is up to: ",Sys.Date()-1, sep = ""), style = "font-family: 'times'; font-si16pt"),
  
  br(),
  
  mainPanel( 
    
    tabsetPanel(type="tab", 
                
                tabPanel("Data",tableOutput('table')),
                
                tabPanel(
                  "Map",
                  
                  br(),
                  
                  sidebarPanel(
                    selectInput('map_measure', 'Select Measure:', 
                                choices = list("Total Cases" = "total_cases", 
                                               "Deaths" = "deaths"),
                                selected = 'Total Cases'), 
                    width = 3
                  ), 
                  
                  mainPanel(leafletOutput("mymap"), width = 9)
                  ),
                
                tabPanel(
                  "Plots",
                  
                  br(),
                  
                  fluidRow(
                    column(6,
                           selectInput('Province', 'Select Province:', 
                                       unique(as.character(df$prname)), 
                                       selected = 'Ontario')
                    ),
                    column(6,
                           selectInput('plot_measure', 'Select Measure:', 
                                       choices = list("Total Cases" = "total_cases", 
                                                      "Deaths" = "deaths"),
                                       selected = 'Total Cases')
                           )
                  ),
                  
                  fluidRow(
                    column(6,
                           p("Linear Growth"),
                           plotOutput("myplot1")
                           ),
                    column(6,
                           p("Logarithmic Growth"),
                           plotOutput("myplot2")
                    )
                  )
                )
    ),
    
    hr() 
    
  )
)

server <- function(input, output, session) {
  
  tidy_table <- subset(tidy_table1, select=c("prname","numtotal","numdeaths","numtested"))
  tidy_table <- tidy_table[order(tidy_table$numtotal,decreasing = TRUE),]
  tidy_table$numtotal = format(round(as.numeric(tidy_table$numtotal), 0),  big.mark=",", na.encode = FALSE)
  tidy_table$numdeaths = format(round(as.numeric(tidy_table$numdeaths), 0),  big.mark=",", na.encode = FALSE)
  tidy_table$numtested = format(round(as.numeric(tidy_table$numtested), 0),  big.mark=",", na.encode = FALSE)
  colnames(tidy_table) <- c("Province Name", "Total Cases", "Deaths","Tested")
  
  output$table <- renderTable(tidy_table)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      
      setView(lng=-103.322068, lat=59.810956, zoom = 3) %>% 
      
      # addTiles() %>%  
      
      addPolygons(data = geo,
                  group="Provinces and Territories", #Layer name
                  fillColor = ~pal(if(input$map_measure == "total_cases"){total_cases} else {deaths}), 
                  fillOpacity = 0.7, # Colour based on property
                  stroke = TRUE, weight=3, color="black", # Stroke colour and thickness
                  label = if(input$map_measure == "total_cases"){~paste0(geo$name, ": ", formatC(geo$total_cases, big.mark = ","), " Cases")}
                          else {~paste0(geo$name, ": ", formatC(geo$deaths, big.mark = ","), " Deaths")},
                  highlightOptions = highlightOptions(color = "black", weight = 3, opacity = 1,
                                                      bringToFront = TRUE) # Highlight polygon when hover
      ) %>%
      #Add legend for Polygons
      addLegend(pal = pal,
                group="Provinces and Territories", #Layer
                position="bottomleft",
                values = if(input$map_measure == "total_cases"){geo$total_cases} else {geo$deaths}, #Colour based on property
                opacity = 0.8,
                title=if(input$map_measure == "total_cases"){"Total Cases"} else {"Deaths"}
      ) 
    
  })
  
  myplot1 <- function(){
    
    df_prov <- df[df$prname == input$Province,]
    df_prov <- df_prov[order(df_prov$date2),]
    
    ggplot(df_prov,
           aes(date2,
               if(input$plot_measure == "total_cases"){numconf} else {numdeaths}
               )
    )+
      geom_line(color="steelblue4", lwd = 1)+
      xlab(label='Date')+
      ylab(label=if(input$plot_measure == "total_cases"){"Total Cases"} else {"Deaths"})
    
  }
  
  output$myplot1 <- renderPlot({myplot1()}) 
  
  myplot2 <- function(){
    
    df_prov <- df[df$prname == input$Province,]
    df_prov <- df_prov[order(df_prov$date2),]
    
    ggplot(df_prov,
           aes(date2,
               if(input$plot_measure == "total_cases"){numconf} else {numdeaths}
           )
    )+
      scale_y_continuous(trans = 'log10')+
      geom_line(color="firebrick2", lwd = 1)+
      xlab(label='Date')+
      ylab(label=if(input$plot_measure == "total_cases"){"Total Cases"} else {"Deaths"})
    
  }
  
  output$myplot2 <- renderPlot({myplot2()}) 
}

shinyApp(ui, server)