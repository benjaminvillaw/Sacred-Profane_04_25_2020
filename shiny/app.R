
library(infer)
library(skimr)
library(broom)
library(gganimate)
library(tidyverse)
library(haven)
library(ggplot2)
library(infer)
library(gt)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(tidyselect)
library(fivethirtyeight)
library(devtools)
library(ggplot2)
library(patchwork)
library(broom)
library(scales)
library(readxl)
library(naniar)
library(janitor)
library(choroplethr)
library(viridis)
library(fuzzyjoin)
library(corrplot)
library(totalcensus)
library(devtools)
library(choroplethrZip)
library(ggfittext)
library(plotly)

all_joined <- read_rds("all_joined.rds")
all_pivot <- read_rds("all_pivot.rds")
corr <- read_rds("corr.rds")

ui <- fluidPage(
    

    navbarPage(
    "The Sacred and the Profane: Social, Religious & Medical Effects on Emotional Wellbeing",
    
    tabPanel("Overview",
            
                 titlePanel("Emotions: Social + Religious + Medical + Economic Factors"),
                h4("89 Metropolitan and Micropolitan Statistical Areas in the United States in the Year 2016"),
                         selectInput("religion_input", "Religious Variables:",
                                     c("Is Religion Important Part of Daily Life?" = "religion_imp",
                                       "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                                       "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                                       "Christian" = "christian",
                                       "Jewish" = "jewish",
                                       "Muslim" = "muslim")),
             
                  plotlyOutput("religion_overview"),
                                     
                          
                                     selectInput("social_input", "Social Variables:",
                                                 c("Social Wellbeing Index" = "wellbeing_index",
                                                   "Hours Spent Socially" = "social_hours",
                                                   "Very Weak Close Relationship" = "x1_strong_disagree_relationship",
                                                   "Very Strong Close Relationship" = "x5_strong_agree_relationship",
                                                   "Number of Social Jobs per 1,000" = "jobs_1000_orig_social"
                                                   )),  
                         
                  plotlyOutput("social_overview"),
             
                         selectInput("economic_input", "Economic Variables:",
                                     c("Income 2016" = "income_2016",
                                       "Financial Well-Being" = "value_financial_well_being",
                                       "Unemployment Rate" = "value_unemployment"
                                     )),     
                                     
             
                  plotlyOutput("economic_overview"),
             
                         selectInput("emotional_input", "Emotional Variables:",
                                     c(
                                       "Happiness" = "yes_happiness",
                                       "Enjoyment" = "yes_enjoyment",
                                       "Worry" = "yes_worry",
                                       "Anger" = "yes_anger",
                                       "Sadness" = "yes_sadness"
                                     )),                
                                         
                   plotlyOutput("emotional_overview"),
               
                         selectInput("medical_input", "Medical Variables:",
                                     c("Diagnosed Depression" = "depression",
                                       "Number of Psych Jobs per 1,000" = "jobs_1000_orig_psych",
                                       "Almost Everyday Use Drugs/Medication To Affect Mood" = "almost_every_day_drug_med_mood",
                                       "Sometimes Use Drugs/Medication To Affect Mood" = "sometimes_drug_med_mood",
                                       "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                                     )),          
                          
                    
                         plotlyOutput("medical_overview")
                 
             ),
                       
    tabPanel("Models",
           #  tabsetPanel(
             #    titlePanel("Religious"),
            # ),
             
             titlePanel("Correlations Between Variables"),
            
            plotOutput("matrix", height = 800),
           
           
             selectInput("variable_a", "Variable A:",
                         c("Is Religion Important Part of Daily Life?" = "religion_imp",
                             "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                             "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                             "Christian" = "christian",
                             "Jewish" = "jewish",
                             "Muslim" = "muslim",
                           
                           "Social Wellbeing Index" = "wellbeing_index",
                           "Hours Spent Socially" = "social_hours",
                           "Very Weak Close Relationship" = "x1_strong_disagree_relationship",
                           "Very Strong Close Relationship" = "x5_strong_agree_relationship",
                           "Number of Social Jobs per 1,000" = "jons_1000_orig_social",
             
                           "Income 2016" = "income_2016",
                           "Financial Well-Being" = "value_financial_well_being",
                           "Unemployment Rate" = "value_unemployment",
                             
                           "Happiness" = "yes_happiness",
                           "Enjoyment" = "yes_enjoyment",
                           "Worry" = "yes_worry",
                           "Anger" = "yes_anger",
                           "Sadness" = "yes_sadness", 
                           
                           "Diagnosed Depression" = "depression",
                           "Number of Psych Jobs per 1,000" = "jobs_1000_orig_psych",
                           "Almost Everyday Use Drugs/Medication To Affect Mood" = "almost_every_day_drug_med_mood",
                           "Sometimes Use Drugs/Medication To Affect Mood" = "sometimes_drug_med_mood",
                           "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood")),
                             
                             
             selectInput("variable_b", "Variable B:",
                         c("Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                           "Is Religion Important Part of Daily Life?" = "religion_imp",
                           "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                           "Christian" = "christian",
                           "Jewish" = "jewish",
                           "Muslim" = "muslim",
                           
                           "Social Wellbeing Index" = "wellbeing_index",
                           "Hours Spent Socially" = "social_hours",
                           "Very Weak Close Relationship" = "x1_strong_disagree_relationship",
                           "Very Strong Close Relationship" = "x5_strong_agree_relationship",
                           "Number of Social Jobs per 1,000" = "jons_1000_orig_social",
                           
                           "Income 2016" = "income_2016",
                           "Financial Well-Being" = "value_financial_well_being",
                           "Unemployment Rate" = "value_unemployment",
                           
                           "Happiness" = "yes_happiness",
                           "Enjoyment" = "yes_enjoyment",
                           "Worry" = "yes_worry",
                           "Anger" = "yes_anger",
                           "Sadness" = "yes_sadness", 
                           
                           "Diagnosed Depression" = "depression",
                           "Number of Psych Jobs per 1,000" = "jobs_1000_orig_psych",
                           "Almost Everyday Use Drugs/Medication To Affect Mood" = "almost_every_day_drug_med_mood",
                           "Sometimes Use Drugs/Medication To Affect Mood" = "sometimes_drug_med_mood",
                           "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood")),
                           
             plotlyOutput("correlation"),
             h4(),
            h5()
            ),
    
    tabPanel("Analysis", 
             titlePanel("Regression Table"),
             gt_output(outputId ="table"),
             h5("The follwing regression table holds depression as a dependent variable."),
             p(),
             ),

    tabPanel("About", 
             titlePanel("About the Project"),
             h5("The project attempts to examine data from 89 Metropolitan and Micropolitan 
                Statistical Areas in the United States in the Year 2016 to establish 
                social, religious & medical effects on emotional wellbeing by ananlyzing differnt variables."),
             
             titlePanel("Scales"),
             h5("The project could be further developed to include other scales,
                such as county level, and state level data. These could further
                establish relationships."),
            
             titlePanel("Data"),
             
             h5("Sources"),
             h5("The data was gathered from 89 Metropolitan and Micropolitan 
                Statistical Areas in the United States in the Year 2016"),
             
             h5("The sources for the data included:
             
             Gallup U.S. Dailies 2016 by Metropolitan and Micropolitan 
                Statistical Area:
             
                Religion Important
                Social Well-being Index
                Religion Preference
                Close Relationships
                Depression
                Hours Spent Socially
                Attend Church, Synagogue, or Mosque
                Financial Well-Being Index
                Unemployment Index
                Party Affiliation
                Use Drugs/Medicine to affect Mood
                
             Per Capita Personal income by Metropolitan Area 2016-2018
            
             Employment Data from: 
            
                "),
             
             titlePanel("About Me"),
             h5("Benjamin Villa Wiesner, a student in the Master in Design 
               Engineering program at Harvard University."),
             
             )
    )
    )

# Overview Graphs

server <- function(input, output, session) {
        
    output$religion_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$religion_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    output$social_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$social_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    output$economic_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$economic_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    output$emotional_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$emotional_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    output$medical_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$medical_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_minimal() + coord_flip()
    }
    )  
    
    # Model Graphs
    
    
    output$correlation <- renderPlotly({
            all_joined %>%
            ggplot(aes_string(input$variable_a, input$variable_b)) + geom_point() + 
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
            theme_classic()+
            scale_y_log10() + scale_x_log10()
    }
    )  
    
    output$matrix <- renderPlot({
        corr %>%
            corrplot(order = "hclust", tl.offset = 2, tl.cex = .8,
                     tl.col = "black")
    })
    
    
    # Table Discussion
    
    output$table <- render_gt({
        lm(depression ~ wellbeing_index + income_2016 + value_unemployment + almost_every_day_drug_med_mood + `x5_strong_agree_relationship`, data = all_joined) %>%
            tidy(conf.int = TRUE) %>%
            select(term,estimate, conf.low,conf.high) %>% gt()
    
    
    })
    
}


shinyApp(ui, server)






