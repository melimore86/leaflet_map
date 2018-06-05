#https://plot.ly/r/shiny-coupled-events/#shiny-app

library(shiny)
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)

# Load data
data(BreastCancer)

setwd("C:/Users/melimore86/Desktop/Deployment/Lone Cabbage WQ Data/LC_WQ1/CSV")
LC_WQ1 <- read.csv("LC_WQ1_All_Days_R.csv", header= T)
colnames(LC_WQ1) <- c("DateTime_Serial", "Pressure", "Temperature", "Conductivity")
LC_WQ1$newDate <- as.POSIXct(as.Date(LC_WQ1$DateTime_Serial,origin= "1899-12-30"))
library(marelac)

standard= 42.914

LC_WQ1$Salinity <- convert_RtoS(LC_WQ1$Conductivity/standard, 
                                t= LC_WQ1$Temperature, p= 0)

# Remove NAs
LC_WQ1 <- na.omit(LC_WQ1)



# Store features and actual class in seprate variables
featureList <- colnames(LC_WQ1)[-6]
Salinity <- LC_WQ1$Salinity
Temperature <- LC_WQ1$Temperature

# Convert to numeric
BreastCancer[,1:9] <- apply(BreastCancer[,-10], 2, as.numeric)

# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  
  # Some help text
  h2("Coupled events in plotly charts using Shiny"),
  h4("This Shiny app showcases coupled events using Plotly's ", tags$code("event_data()"), " function."),
  tags$ol(
    tags$li("The first chart showcases", tags$code("plotly_selected")),
    tags$li("The third chart showcases", tags$code("plotly_click"))
  ),
  
  # Vertical space
  tags$hr(),
  
  # Feature selection
  fixedRow(
    column(3, selectInput(inputId = "featureInput1", label = "Select first feature", choices = featureList, selected = "Salinity")),
    column(4, selectInput(inputId = "featureInput2", label = "Select second feature (observed event)", choices = featureList, selected = "Temperature"))),
  
  # First row
  fixedRow(
    column(6, plotlyOutput("Salinity", height = "600px")),
    column(6, plotlyOutput("Temperature", height = "600px"))),
  
  tags$hr(),
  tags$blockquote("First drag a selection box in the scatter plot to populate the barchart. Then select one of the bars in the barchat
                  to populate the boxplot"),
  
  
  # Second row
  fixedRow(
    column(3, plotlyOutput("Plot3", height = "600px")),
    column(9, plotlyOutput("Plot4", height = "600px"))))





# server.R definition
server <- function(input, output){
  
  # Observes the second feature input for a change
  observeEvent(input$featureInput2,{
    
    # Create a convenience data.frame which can be used for charting
    plot.df <- data.frame(LC_WQ1[,input$featureInput1],
                          LC_WQ1[,input$featureInput2],
                          Class = LC_WQ1$Salinity)
    
    # Add column names
    colnames(plot.df) <- c("x", "y", "Class")
    
    # Do a plotly contour plot to visualize the two featres with
    # the number of malignant cases as size
    # Note the use of 'source' argument
    output$Plot1 <- renderPlotly({
      plot_ly(plot.df, x = ~newDate, y = ~Salinity, mode = "markers", type = "scatter", source = "subset",
              marker = list(size = 30)) %>%
        layout(title = paste(input$featureInput1, "vs ", input$featureInput2),
               xaxis = list(title = input$featureInput1),
               yaxis = list(title = input$featureInput2),
               dragmode =  "select",
               plot_bgcolor = "6A446F")
    })
    
  
    # Assign to parent environment
    plot.df <<- plot.df
  })
  
  # Coupled event 1
  output$Plot3 <- renderPlotly({
    
    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")
    
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)
    

  
    
    # Plot
    plot_ly(LC_WQ1, x = ~newDate, y = ~Salinity, type = "point", source = "select") %>%
      layout(title = "Salinity (ppt) <br> in Temperature (C)",
             plot_bgcolor = "6A446F",
             yaxis = list(domain = c(0, 40)))
  })
  
  
  plot_ly(LC_WQ1, x = ~newDate, y = ~Salinity, type = "point", source = "select") %>%
    layout(title = "Salinity (ppt) <br> in Temperature (C)",
           plot_bgcolor = "6A446F",
           yaxis = list(domain = c(0, 40)))
  
  
  
  
  
  
  library(plotly)
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  d <- LC_WQ1 %>%
    accumulate_by(~date)
  
  p <- d %>%
    plot_ly(
      x = ~newDate, 
      y = ~Salinity,
      frame = ~newDate, 
      type = 'scatter',
      mode = 'lines', 
      line = list(simplyfy = F)
    ) %>% 
    layout(
      xaxis = list(
        title = "Date",
        zeroline = F
      ),
      yaxis = list(
        title = "Salinity",
        zeroline = F
      )
    ) %>% 
    animation_opts(
      frame = 100, 
      transition = 0, 
      redraw = FALSE
    ) %>%
    animation_slider(
      hide = T
    ) %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )

  