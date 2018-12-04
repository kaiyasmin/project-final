
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

data <- read_rds("data.rds") 

axis_vars <- c(
  "Number of reviews" = "Number_of_reviews",
  "Number of stars" = "Stars",
  "Number of loves" = "Loves"
)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Product Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(position = "left",
      sidebarPanel(
sliderInput("Number_of_reviews", "Reviews",
                                 345, 1000, 635, step = 10),
sliderInput("Price", "Price",
                  26, 215, 80, step = 10),
sliderInput("Loves", "Minimum amount of loves",
               20000, 150000, 40000, step = 10),
sliderInput("Stars", "Minimum number of stars ",
            3.6, 4.6, 4.0, step = 0.1),
selectInput("Category", "Product Category",
            c("Moisturizer","SPF", "Eye Cream", "Mask", "Treatments", 
              "Toner/Essence", "Exfoliator", "Cleanser"))
),
      
mainPanel(
  plotOutput("barPlot")
   )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
  output$barPlot <- renderPlot({
  
  data %>% 
    ggplot(aes_string(x = "Stars", y = "Loves", col = "Parabens")) + geom_point() 
 
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

