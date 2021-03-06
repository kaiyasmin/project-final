
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
#I am reading in the data I need to make the app using .rds. 
data <- read_rds("data.rds")
  
#Here I am defining the choices of variables that can go on the X & Y axis, so it is ready to use when I create select input functions in my UI.
y_choices <- c("Loves" = "Loves",
                  "Stars" = "Stars",
                  "Number of Reviews" = "Reviews", 
               "Price" = "Price")

x_choices <- c("Loves" = "Loves",
               "Stars" = "Stars",
               "Number of Reviews" = "Reviews", 
               "Price" = "Price")
# I also need to do the same thing for clean choices, the two variables that will be able to be seen through color and a legend.
#I had issues with variable names because they had spaces so that's why I went back into my raw data and changed names so it would be easier to work with. 
clean_choices <- c("Parabens" = "Parabens",
                    "Clean at Sephora" =  "CleanSephora")



# Here I am defining the UI for application that draws scatterplot.
ui <- fluidPage(
   # Here is my application title
   titlePanel("Sephora Skincare Bestsellers Explorer"), 
   #I want tabs to organize and provide more information in my app.
   tabsetPanel(
     #The first tab is the interactive graph so I labeled it Explorer. 
     tabPanel("Explorer",
   
   # Sidebar with a slider inputs
   # I wanted to give the user as much flexibility as possible in terms of narrowing and broadening the data to what they want to see and what will be relevant to them. 
   # I used slider input for various variables and set the values to the minimum and maximum, so they can get the full picture first, and then play around with the data themselves.
   sidebarLayout(position = "left",
      sidebarPanel(
sliderInput("Reviews", "Amount of reviews",
                                 1, 15000, value=c(1, 15000)),
sliderInput("Price", "Price",
                  5, 215, value=c(5, 215)), 
sliderInput("Loves", "Amount of loves (level of interest)",
               1, 150000, value=c(100, 150000, step = 1000)),
sliderInput("Stars", "Star rating out of five ",
            2.9, 5.0, value=c(2.9, 5.0, step = 0.1)),
# I want the user to be able to narrow down by category of product, so I used select input here. 
selectInput("Category", "Product Category",
            c("All","Moisturizer","SPF", "Mask", "Treatments", 
              "Toner/Essence", "Exfoliator", "Cleanser")),
# These select inputs allow the user to choose what they want to have on each of the axis.
# It also lets them color code the products that have parabens in them, or ones that have been given Clean At Sephora certification. 
# This is a clean way of showing many useful types of information without cluttering or overwhelming the user. 
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


# Here I define my outputs. I am using the package plotly because I needed tooltips for the explorer to work, hence it is plotlyOutput instead of plotOutput.
mainPanel(
  plotlyOutput("barPlot"),
  #I also want a table to supplement the information in the graph, so I added it here.
  DT::dataTableOutput("table")
  
 
  
   )
  )
),
# This tab panel is explaining the purpose of the app. 
tabPanel("About", 
         h4("The Sephora Skincare Bestsellers Explorer was made to help
you get acquainted with skincare in an accessible and informed way."),
         h5("Creating a skincare routine from scratch can be overwhelming,
               due to the wide variety of options. This app takes the top 20 
            bestsellers from each product category on the Sephora website, and shows the review 
            statistics for each one all in one place, so you can see for
            yourself if a product lives up to the hype, whether you really
            need that expensive exfoliator, or if you can go with another 
            brand that is just as good."),
         h5("If you're curious about a product but you're not sure if 
it will work for you, why don't you look at the ingredient breakdown? There is a URL
to SkinCarisma.com for every product, which explains the pros and cons of the product's 
ingredients. For example, if you're acne prone, 
you're going to want to steer clear of ingredients with a high comedogenic rating
            (this means there's a high chance it can clog pores!)"),
         h5("If a product claims to be anti-aging, you can check to see if the
            ingredients actually support its claims. If it has super-star ingredients
            such as retinoids or niacinamide for brightening, it's a go! 
            Don't worry about not knowing exactly what these ingredients are,
            the ingredients link simply tells you the properties of each ingredient so
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
You can color code ingredients with a 'Clean at Sephora' filter, or a 'Parabens' filter."), 
         h5("By Kai Potter for the GOV1005 Final Project.")),

# This tab explains the variables and how everything works. 
tabPanel("How does this work?",
         h4("Simply hover over the points in the graph to see more information 
   about each product. For even more information, consult the table for all
   information plus product and ingredient breakdown links. There are 
   five ways to filter the information in a way that you need:"),
         h5("1. Amount of reviews. You can choose to see products with a 
certain amount of reviews, as the more reviews there are, the more
   accurate the rating is likely to be. "),
         h5("2. Price. Have a budget? Don’t waste your time, filter by price 
to eliminate products that are out of your price range." ),
         h5("3. Amount of loves. This is an indicator of popularity, the number of loves 
correlates to the number of people who have expressed interest in the product, but they
may not have necessarily bought it yet. Want to see the products with the most hype?
   Filter by amount of loves to see the products that have the biggest buzz."),
         h5("4. Stars. What really matters. Filter by star rating to get the
   products that perform the best, according to customers."),
         h5("5. Clean choices. Concerned about ingredients? Check out the 
legend and color code products that have the ‘Clean by Sephora’ seal,
and products that have parabens." ),
         h5("You can also change what is on the X and Y axis to view different insights,
or to simply customize the layout so it works for you. You can also search for certain brands
or products in the table. " )),

      
#This tab panel is showing observations and analysis. To be honest, as seeing as the app is made for people to make their own informed decisions about skincare thats right for them, I didn't want to include a 'conclusion' or 'analysis' part of the app to stay impartial and let the data speak for itself, but for the sake of final project grading I will have such a tab. 
tabPanel("Observations",
h4("To be honest, as seeing as the app was made for people to make their own
   informed decisions about skincare thats right for them, I didn't want to 
   include a 'conclusion' or 'analysis' part of the app in order to stay impartial
   and let the data speak for itself. But, for the sake of final project grading 
   I will share a few interesting things I’ve noticed in playing around with the data."),
h5("Loves, an indicator of popularity, does not correlate to the star rating,
   which is indicative of quality. A lot of products don’t have a major number of 
   loves, but their rating is high. Actually, for products with 100,000 - 150,000 loves,
   only 5 of them had a rating above 4.5. For products with 0 - 50,000 loves,
   19 products had a rating above 4.5. There are some underrated skincare gems out there!"),
h5("‘Clean’ products are not always expensive. Most products with the ‘Clean at Sephora’ 
certification are between $25 to $50 dollars, and no ‘Clean at Sephora’ products 
surpassed a price tag of $100." ),
h5("On the other hand, there is a perception that only cheap products have parabens
in their ingredients, but according to the Sephora bestsellers list, this is not true. 
The two most expensive brands at Sephora, SK-II and La Mer, use parabens in the
majority of their products that are on the list."),
h5("Products containing parabens mostly have under 60,000 loves, with the exception
   of one product - The Purity Made Simple Cleanser by PHILOSOPHY which has a whopping 
   150,000 likes. After digging deep to find out why this was an anomaly amongst products
   containing parabens, I came to the conclusion that the company may be ‘Green-washing’
   this product.  This is the phenomenon where a product’s packaging or marketing claims
   or gives the impression that it’s natural or environmentally friendly, but in reality, 
   it isn’t. The bottle has a stripped back design and a block of text talking about 
   how clean, pure and natural the cleanser is, yet the ingredients are anything but. We should 
   be careful and actually inspect what is in the products we buy. "))


)
)

# Here I define the server logic 
server <- function(input, output) { 

  #Here I have to use renderPlotly, so my graph has the tooltips that are part of the plotly package. 
  output$barPlot <- renderPlotly({
    #My variables names are a bit messy in the table, so I rename them here. 
    output$table <- DT::renderDataTable({data %>% rename('Clean at Sephora' = "CleanSephora", 
                                                         'Product Name' = "Product", 
                                                         'Product Brand' = "Brand", 
                                                         'Number of reviews' = "Reviews", 
                                                         'Type' = "Type"
                                                         
                                                         )})
    
    # I wanted an All category as well as individual product categories, so I made an if statement and used filter to make it work. 
if (input$Category != "All") {
data <- data %>% filter(Category == input$Category)
 }
    #Here I am using filter to get the highest and lowest value for every variable that I want on a slider. 
    
     data %>% 
      filter(Price >= input$Price[1] & Price <= input$Price[2]) %>%
      filter(Stars >= input$Stars[1] & Stars <= input$Stars[2]) %>%
      filter(Loves >= input$Loves[1] & Loves <= input$Loves[2]) %>%
      filter(Reviews>= input$Reviews[1] & Reviews <= input$Reviews[2]) %>%
       # Here I use aes_string so my predefined choices can be assigned to x and y and color. 
        ggplot(aes_string(x = input$x_choices, y = input$y_choices, color = input$clean_choices)) + 
       # It was a struggle trying to change the information in the plotly tooltip.
       # as plotly can be quite rigid with customization, but I figured out that 
       #I could use labels for each info point I want to display. 
       #I added theme_minimal as I wanted to make the app look classier and more user friendly.
    geom_point(aes(label1= Product, label2= Brand, label3=Price)) + theme_minimal() 
  })
  
}



# Running the application 
shinyApp(ui = ui, server = server)

