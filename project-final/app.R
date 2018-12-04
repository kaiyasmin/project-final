
library(shiny) 
library(ggvis)
library(dplyr)
library(RSQLite) 
library(readr)
library(tidyverse)
library(knitr)
library(fs)
library(lubridate)
library(formattable)
library(foreign)
library(xml2)
library(stringr)
library(plotly)
library(DT)

data <- read_rds("data.rds")
  

y_choices <- c("Loves" = "Loves",
                  "Stars" = "Stars",
                  "Number of Reviews" = "Reviews", 
               "Price" = "Price")

x_choices <- c("Loves" = "Loves",
               "Stars" = "Stars",
               "Number of Reviews" = "Reviews", 
               "Price" = "Price")

clean_choices <- c("Parabens" = "Parabens",
                    "Clean at Sephora" =  "CleanSephora")



# Define UI for application that draws scatterplot
ui <- fluidPage(
   # Application title
   titlePanel("Sephora Product Explorer"), 
   tabsetPanel(
     tabPanel("Explorer",
   
   # Sidebar with a slider inputs
   sidebarLayout(position = "left",
      sidebarPanel(
sliderInput("Reviews", "Amount of reviews",
                                 345, 1000, value=c(345, 1000)),
sliderInput("Price", "Price",
                  26, 215, value=c(26, 215)), 
sliderInput("Loves", "Amount of loves (level of interest)",
               20000, 150000, value=c(20000, 150000)),
sliderInput("Stars", "Star rating out of five ",
            3.6, 4.6, value=c(3.6, 4.6)),
selectInput("Category", "Product Category",
            c("All","Moisturizer","SPF", "Eye Cream", "Mask", "Treatments", 
              "Toner/Essence", "Exfoliator", "Cleanser")),
selectInput(inputId = "y_choices",
            label = "Y-axis",
            choices = y_choices,
            selected = "Stars"),
selectInput(inputId = "x_choices",
            label = "X-axis",
            choices = x_choices,
            selected = "Loves"), 
selectInput(inputId = "clean_choices",
            label = "Clean choices",
            choices = clean_choices,
            selected = "Parabens")

),



mainPanel(
  plotlyOutput("barPlot"),
  DT::dataTableOutput("table")
  
 
  
   )
  )
),
tabPanel("About", 
         h3("lalala"),
            h4("lalala")),
            

tabPanel("How it works",
h3("lalala"),
h4("lalala"))


)
)

# Define server logic 
server <- function(input, output) { 

  
  output$barPlot <- renderPlotly({
    output$table <- DT::renderDataTable({data %>% rename('Clean at Sephora' = "CleanSephora", 
                                                         'Product Name' = "Product", 
                                                         'Product Brand' = "Brand", 
                                                         'Number of reviews' = "Reviews", 
                                                         'Type' = "Type"
                                                         
                                                         )})
    
    
if (input$Category != "All") {
data <- data %>% filter(Category == input$Category)
 }
    
     data %>% 
      filter(Price >= input$Price[1] & Price <= input$Price[2]) %>%
      filter(Stars >= input$Stars[1] & Stars <= input$Stars[2]) %>%
      filter(Loves >= input$Loves[1] & Loves <= input$Loves[2]) %>%
      filter(Reviews>= input$Reviews[1] & Reviews <= input$Reviews[2]) %>%
        ggplot(aes_string(x = input$x_choices, y = input$y_choices, color = input$clean_choices)) + 
    geom_point(aes(label1= Product, label2= Brand, label3=Price)) + theme_minimal() 
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

