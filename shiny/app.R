
library(tidyverse)
library(ggplot2)
library(shiny)
library(dbplyr)
library(corrplot)


all_joined <- read_rds("all_joined.rds")
all_pivot <- read_rds("all_pivot.rds")
corr <- read_rds("corr.rds")

ui <- fluidPage(
    

    navbarPage(
    "The Sacred and the Profane: Social, Religious & Medical Effects on Emotional Wellbeing",
    
    tabPanel("Overview",
             fluidPage(
                 titlePanel("The Sacred and the Profane: Social + Religious + Medical Factors"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("religion_overview", "Variable:",
                                     c("1" = "wellbeing_index",
                                       "2" = "democrat",
                                       "3" = "republican"))
                                 ),
                     mainPanel(
                         plotOutput("overview")
                              )
                            )
                         )
             
             ),
                       
    tabPanel("Models",
             titlePanel("Discussion Title"),
             plotOutput("matrix"),
             selectInput("model_test", "Variable:",
                         c("drug" = "almost_every_day_drug_med_mood",
                           "income" = "income_2016",
                           "church" = "church_week")),
             
             selectInput("emotion", "Emotion:",
                         c("happy" = "yes_happiness",
                           "worry" = "yes_worry",
                           "anger" = "yes_anger")),
             plotOutput("correlation"),
             h4(),
            h5()
            ),
    
    tabPanel("Analysis", 
             titlePanel("XX"),
             h3("This webapp is the final project for the GOV 1005."),
             p(),
             h3("YY"),
             p("ZZZZ")
             ),

    tabPanel("About", 
             titlePanel("About Me"),
             h3("Benjamin Villa Wiesner, a student in the Master in Design 
               Engineering program at Harvard University."),
             titlePanel("Data"),
             h4("The Data...")
             )
    )
    )



server <- function(input, output, session) {
        
    output$overview <- renderPlot({
        all_pivot %>%
            filter(category == input$religion_overview) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    output$correlation <- renderPlot({
            all_joined %>%
            ggplot(aes_string(input$emotion,input$model_test)) + geom_point() + 
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
            theme_classic()+
            scale_y_log10() + scale_x_log10()
    }
    )  
    
    output$matrix <- renderPlot({
        corr %>%
            corrplot(order = "hclust", tl.offset = 2,tl.cex = .4,
                     tl.col = "black")
    })
    
    
    
        }
shinyApp(ui, server)






