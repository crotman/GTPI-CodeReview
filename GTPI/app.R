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
library(shinyWidgets)
library(scales)
library(magrittr )

parameters <- readRDS("../cache/parameters_cloud.rds") %>%  filter(Qk != 0.9)

games_results <- readRDS("../cache/games_results_cloud.rds")






Qks <- distinct(select(parameters,Qk))$Qk * 100
Rks <- distinct(select(parameters,Rk))$Rk * 100
Rk_Rf_factor <- distinct(select(parameters,Rk_Rf_factor))$Rk_Rf_factor * 100
Tk_Tf_factor <- distinct(select(parameters,Tk_Tf_factor))$Tk_Tf_factor * 100



best_dev_0 <- games_results %>% 
    filter(dev == "dev0") %>% 
    left_join(games_results, by = c("game", "id_param"), suffix = c("", "_info_other") ) %>% 
    filter(dev_info_other == "dev1") %>% 
    group_by(strategy.y_info_other, id_param) %>% 
    filter(completed_items == max(completed_items)) %>% 
    ungroup() 

best_dev_1 <- games_results %>% 
    filter(dev == "dev1") %>% 
    left_join(games_results, by = c("game", "id_param"), suffix = c("", "_info_other") ) %>% 
    filter(dev_info_other == "dev0") %>% 
    group_by(strategy.y_info_other, id_param) %>% 
    filter(completed_items == max(completed_items)) %>% 
    ungroup() 

equilibrio <- best_dev_0 %>% 
    inner_join(best_dev_1, 
               by = c(
                   "id_param",
                   "game" = "game"
               )
    ) %>% 
    select(id_param, game) %>% 
    mutate(equilibrio = TRUE) %>% 
    group_by(id_param) %>% 
    mutate(n_equilibrios = n())



games_results %<>% 
    left_join(equilibrio, by = c("id_param", "game"), keep = TRUE ) %>% 
    mutate(equilibrio = !is.na(equilibrio))


param_equilibriums <- parameters %>% 
    left_join(equilibrio, by = c("id_param")) %>% 
    group_by(id_param) %>% 
    summarise(game = mean(game), n = n())  %>% 
    mutate(
        equilibrium = case_when(
            n == 1 & game == 3 ~ "Kludge-Kludge",
            n == 1 & game == 1 ~ "Fix-Fix",
            TRUE ~ "No or Multi Equilibrium"
        )
    ) %>% 
    left_join(parameters, by = c("id_param"))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Results"),

    tabsetPanel(
        tabPanel("Overview",
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  selectInput("X",
                                              "X axis",
                                              choices = c(
                                                  "Qk" = "Qk",
                                                  "Rk" = "Rk",
                                                  "Rf/Rk" = "Tk_Tf_factor",
                                                  "Tk/Tf" = "Rk_Rf_factor"
                                                ),
                                              selected = "Rk_Rf_factor"
                                              
                                  ),
                                  selectInput("Y",
                                              "Y axis",
                                              choices = c(
                                                  "Qk" = "Qk",
                                                  "Rk" = "Rk",
                                                  "Rf/Rk" = "Tk_Tf_factor",
                                                  "Tk/Tf" = "Rk_Rf_factor"
                                              ),
                                              selected = "Tk_Tf_factor"
                                              
                                  ),
                                  uiOutput("Qk_overview_spot"),
                                  uiOutput("Rk_overview_spot"),
                                  uiOutput("Rk_Rf_factor_overview_spot"),
                                  uiOutput("Tk_Tf_factor_overview_spot")
                                  
                                  
                                  
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("overview", height =  600)
                     )
                 )
        ),         
        tabPanel("One parametrization",
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(width = 3,
                             sliderTextInput("Qk",
                                "Qk (a kludge degrades resolution time by Qk %):",
                                choices = Qks,
                                grid = TRUE,
                                selected = 5
                                ),
                             sliderTextInput("Rk",
                                             "Rk (Rework rate for kludges in %):",
                                             choices = Rks,
                                             grid = TRUE,
                                             selected = 7.245
                             ),
                             sliderTextInput("Rk_Rf_factor",
                                             "Rf/Rk (Rework Rate for fixes / Rework rate for kludges):",
                                             choices = Rk_Rf_factor,
                                             grid = TRUE,
                                             selected = 85.71429
                             ),
                             sliderTextInput("Tk_Tf_factor",
                                             "Tk/Tf (Time for fixes / Time for kludges):",
                                             choices = Tk_Tf_factor,
                                             grid = TRUE,
                                             selected = 68.18182
                             )
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                   dataTableOutput("tabela")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$Qk_overview_spot <- renderUI({
        
        if (input$X != "Qk" & input$Y != "Qk"  ){
            

            sliderTextInput("Qk_overview",
                            "Qk (a kludge degrades resolution time by Qk %):",
                            choices = Qks,
                            grid = TRUE,
                            selected = 5
                            
            )
        }
    })
    
    
    filtra_Qk <- reactive({
        input$X != "Qk" & input$Y != "Qk"
    })
    
    output$Rk_overview_spot <- renderUI({
        
        if (input$X != "Rk" & input$Y != "Rk"  ){
            
            filtra_Rk <- TRUE 
            
            sliderTextInput("Rk_overview",
                            "Rk (Rework rate for kludges in %):",
                            choices = Rks,
                            grid = TRUE,
                            selected = 7.245
                            
            )
        }
        else{
            filtra_Rk <- FALSE 
            
        }
            
    })

    filtra_Rk <- reactive({
        input$X != "Rk" & input$Y != "Rk"
    })
    
    
        
    output$Rk_Rf_factor_overview_spot <- renderUI({
        
        if (input$X != "Rk_Rf_factor" & input$Y != "Rk_Rf_factor"  ){
            

            sliderTextInput("Rk_Rf_factor_overview",
                            "Rf/Rk (Rework Rate for fixes / Rework rate for kludges):",
                            choices = Rk_Rf_factor,
                            grid = TRUE,
                            selected = 85.71429
                            
                            
            )
        }

    })
    
    filtra_Rk_Rf <- reactive({
        input$X != "Rk_Rf_factor" & input$Y != "Rk_Rf_factor"        
    })
    

    output$Tk_Tf_factor_overview_spot <- renderUI({
        
        if (input$X != "Tk_Tf_factor" & input$Y != "Tk_Tf_factor"  ){
            

            sliderTextInput("Tk_Tf_factor_overview",
                            "Tk/Tf (Time for fixes / Time for kludges):",
                            choices = Tk_Tf_factor,
                            grid = TRUE,
                            selected = 68.18182
                            
            )
        }
    })
    
    
    filtra_Tk_Tf <- reactive({
        input$X != "Tk_Tf_factor" & input$Y != "Tk_Tf_factor"  
    })
    
    output$overview <- renderPlot({
        
        chosen_param_equilibriums <- param_equilibriums
        

        if(filtra_Rk()){
            print("filtro Rk")
            chosen_param_equilibriums %<>% filter(near(Rk, input$Rk_overview/100))
        }

        if(filtra_Qk()){
            print("filtro Qk")
            chosen_param_equilibriums %<>% filter(near(Qk, input$Qk_overview/100))
        }


        if(filtra_Rk_Rf()){
            print("filtro Rk_Rf")
            chosen_param_equilibriums %<>% filter(near(Rk_Rf_factor, input$Rk_Rf_factor_overview/100))
        }

        
        if(filtra_Tk_Tf()){
            print("filtro Tk_Tf")
            chosen_param_equilibriums %<>% filter(near(Tk_Tf_factor, input$Tk_Tf_factor_overview/100))
        }
        
        print(chosen_param_equilibriums)
        
        x_col = sym(input$X)
        y_col = sym(input$Y)
        
        cols <- c("Kludge-Kludge" = "darkred","Fix-Fix" = "darkgreen", "No or Multi Equilibrium" = "darkblue"  )
        

        if (input$X == "Tk_Tf_factor")
        {
            labelX = "Tk/Tf"
        }
        else
        {
            labelX = input$X
        }
        if (input$X == "Rk_Rf_factor")
        {
            labelX = "Rf/Rk"
        }
        else
        {
            labelX = labelX
        }
        
        
        if (input$Y == "Tk_Tf_factor")
        {
            labelY = "Tk/Tf"
        }
        else
        {
            labelY = input$Y
        }
        if (input$Y == "Rk_Rf_factor")
        {
            labelY = "Rf/Rk"
        }
        else
        {
            labelY = labelY
        }
        
        ggplot(chosen_param_equilibriums) +
            geom_tile(aes(x = as.factor(!!x_col), y = as.factor(!!y_col), fill = equilibrium )  ) +
            scale_fill_manual(values = cols) +
            scale_x_discrete() +
            labs( x = labelX, y = labelY, fill = "Equilibrium type"  ) +
            theme_light() +
            theme(legend.position="top")
                

    })
    
        
    output$tabela <- renderDataTable({
        
        

        param_chosen <-  parameters %>% 
            filter(
                near(Qk, input$Qk/100),
                near(Rk, input$Rk/100),
                near(Rk_Rf_factor, input$Rk_Rf_factor/100),
                near(Tk_Tf_factor, input$Tk_Tf_factor/100)
                
                
            ) %>% 
            pull(id_param)
        
        print(param_chosen)
        

        table <- games_results %>%
            filter(id_param == param_chosen) %>% 
            ungroup() %>% 
            group_by(game, id_param) %>% 
            mutate(resultado = paste0(dev, " = ", number(completed_items, accuracy = 0.01) )) %>% 
            summarise(
                result = paste(resultado, collapse = ", " ),
                equilibrio = sum(equilibrio) > 0
            ) %>% 
            mutate(result = if_else(equilibrio, paste0(result,"*"), result)) %>% 
            select(-id_param, -equilibrio ) %>% 
            ungroup() %>% 
            mutate(
                row = c(1,2,1,2),
                col = c("c1","c1","c2","c2")
            ) %>% 
            select(-game) %>% 
            spread(col, result) %>% 
            rename("dev1: fix" = "c1", "dev1: kludge" = "c2" ) %>% 
            mutate(Payoffs = c("dev0: fix", "dev0: kludge")) %>% 
            select(Payoffs, "dev1: fix", "dev1: kludge" )
        
        print(table)
        
        datatable(table) %>% 
            formatStyle(1:3, color = JS("String(value).match(/\\*/) ? 'red' : ''")) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
