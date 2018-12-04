
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
                  "Number of Reviews" = "Number_of_reviews", 
               "Price" = "Price")

x_choices <- c("Loves" = "Loves",
               "Stars" = "Stars",
               "Number of Reviews" = "Number_of_reviews", 
               "Price" = "Price")

clean_choices <- c("Parabens" = "Parabens",
                    "Clean at Sephora" =  "Clean_at_Sephora")
              

# Define UI for application that draws scatterplot
ui <- fluidPage(
   
   # Application title
   titlePanel("Sephora Product Explorer"), 
   
   # Sidebar with a slider inputs
   sidebarLayout(position = "left",
      sidebarPanel(
sliderInput("Number_of_reviews", "Amount of reviews",
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
)

# Define server logic 
server <- function(input, output) { 

  
  output$barPlot <- renderPlotly({
    output$table <- DT::renderDataTable({data}) 
if (input$Category != "All") {
data <- data %>% filter(Category == input$Category)
 }
    
     data %>% 
      filter(Price >= input$Price[1] & Price <= input$Price[2]) %>%
      filter(Stars >= input$Stars[1] & Stars <= input$Stars[2]) %>%
      filter(Loves >= input$Loves[1] & Loves <= input$Loves[2]) %>%
      filter(Number_of_reviews>= input$Number_of_reviews[1] & Number_of_reviews <= input$Number_of_reviews[2]) %>%
    ggplot(aes_string(x = input$x_choices, y = input$y_choices, color = input$clean_choices)) + 
    geom_point(aes(text = Product_name)) 
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

