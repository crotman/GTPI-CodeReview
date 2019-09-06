#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("tabela")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    parameters <- readRDS("../cache/parameters.rds")

    games_results <- readRDS("../cache/games_results.rds")
    
        
    
    output$tabela <- renderDataTable({
        
        table <- games_results %>% 
            select(-id_param ) %>% 
            summarise(
                resultado = paste(resultado, collapse = ", " )
            ) %>% 
            mutate(
                row = c(1,2,1,2),
                col = c("c1","c1","c2","c2")
            ) %>% 
            select(-game) %>% 
            spread(col, resultado) %>% 
            rename("dev1: fix" = "c1", "dev1: kludge" = "c2" ) %>% 
            mutate(Payoffs = c("dev0: fix", "dev0: kludge")) %>% 
            select(Payoffs, "dev1: fix", "dev1: kludge" )
        
        print(table)
        
        datatable(table)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
