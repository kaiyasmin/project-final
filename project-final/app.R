
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
   titlePanel("Sephora Skincare Bestsellers Explorer"), 
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
         h4("This Sephora Skincare Bestsellers Explorer was made to help
you get acquainted with skincare in an accessible and informed way."),
         h5("Creating a skincare routine from scratch can be overwhelming,
               due to the wide variety of options. This app takes the top 30 
            bestsellers from each product category, and shows the review 
            statistics for each one all in one place, so you can see for
            yourself if a product lives up to the hype, whether you really
            need that expensive exfoliator, or if you can go with another 
            brand that is just as good?"),
         h5("If you're curious about a product but you're not sure if 
it will work for you, why don't you click on the SkinCarisma.com 
link for a  thorough ingredient breakdown? If you're acne prone, 
you're going to want to steer clear of ingredients with a high comedogenic rating."),
         h5("If a product claims to be anti-aging, you can check to see if the
            ingredients actually support its claims. If it has super-star ingredients
            such as retinoids or niacinamide for brightening, it's a go! 
            Don't worry about not knowing exactly what these ingredients are,
            SkinCarisma simply tells you the properties of each ingredient so
            YOU can decide if a product is right for you, instead of letting 
            marketing fluff tell you what is right."),
         h5("While the skincare industry is the biggest segment in the beauty market,
            it is also one of the most unregulated, meaning that the FDA takes a 
            hands off approach when it comes to monitoring ingredients that go
            into products. With the rise of hormone related health issues, 
            such as breast cancer and poly cystic ovary syndrome, the fact 
            that skincare can contain hormone or endocrine disruptors is disturbing, 
            especially as it is women that are making up most of the market."),
            h5("Sephora has recently started to give products with clean ingredients 
a 'Clean at Sephora' seal, meaning that they are free of 'sulfates SLS and SLES,
parabens, formaldehydes, formaldehyde-releasing agents, phthalates, mineral oil, 
retinyl palmitate, oxybenzone, coal tar, hydroquinone, triclosan, and triclocarban.'
You can color code ingredients with a 'Clean at Sephora' filter, or a 'Parabens' filter.")),
            

tabPanel("How it works",
h4("Simply hover over the points in the graph to see more information 
   about each product. For more information, consult the table for all
   information plus product and ingredient breakdown links. There are 
   five ways to filter the information in a way that you need:"),
h5("1. Amount of reviews. You can choose to see products with a 
certain amount of reviews, as the more reviews there are, the more
   accurate the rating is likely to be. "),
h5("2. Price. Have a budget? Don’t waste your time, filter by price 
to eliminate products that are out of your price range." ),
h5("3. Amount of loves. Want to see the products with the most hype?
   Filter by amount of loves to see the products that have the biggest buzz."),
h5("4. Stars. What really matters. Filter by star rating to get the
   products that perform the best, according to customers."),
h5("5. Clean choices. Concerned about ingredients? Check out the 
legend and color code products that have the ‘Clean by Sephora’ seal,
and products that have parabens." ),
h5("You can also change what is on the X and Y axis to view different insights,
or to simply customize the layout so it works for you." ))


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

