

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
                 
                 
                 
                 
                 tabsetPanel(type = "tabs",
                             
                             tabPanel("Religious", 
                                      selectInput("religion_input", "Religious Variables:",
                                                  c("Is Religion Important Part of Daily Life?" = "religion_imp",
                                                    "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                                                    "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                                                    "Christian" = "christian",
                                                    "Jewish" = "jewish",
                                                    "Muslim" = "muslim"
                                                  )),
                                      
                                      plotlyOutput("religion_overview", height = 1000)
                             ),
                             
                             
                             tabPanel("Social", 
                                      selectInput("social_input", "Social Variables:",
                                                  c("Social Wellbeing Index" = "wellbeing_index",
                                                    "Hours Spent Socially" = "social_hours",
                                                    "Very Weak Close Relationship" = "x1_strong_disagree_relationship",
                                                    "Very Strong Close Relationship" = "x5_strong_agree_relationship",
                                                    "Number of Social Jobs per 1,000" = "jobs_1000_orig_social"
                                                  )),  
                                      
                                      plotlyOutput("social_overview", height = 1000)
                             ),
                             
                             tabPanel("Economic", 
                                      selectInput("economic_input", "Economic Variables:",
                                                  c("Income 2016" = "income_2016",
                                                    "Financial Well-Being" = "value_financial_well_being",
                                                    "Unemployment Rate" = "value_unemployment",
                                                    "Republican" = "republican",
                                                    "Democrat" = "democrat",
                                                    "Independent / Other" = "independent_other"
                                                  )),  
                                      plotlyOutput("economic_overview", height = 1000)
                             ),
                             
                             
                             tabPanel("Political", 
                                      selectInput("political_input", "Political Variables:",
                                                  c(
                                                      "Republican" = "republican",
                                                      "Democrat" = "democrat",
                                                      "Independent / Other" = "independent_other"
                                                  )),
                                      
                                      plotlyOutput("political_overview", height = 1000)
                             ),
                             
                             tabPanel("Emotional", 
                                      selectInput("emotional_input", "Emotional Variables:",
                                                  c(
                                                      "Happiness" = "yes_happiness",
                                                      "Enjoyment" = "yes_enjoyment",
                                                      "Worry" = "yes_worry",
                                                      "Anger" = "yes_anger",
                                                      "Sadness" = "yes_sadness"
                                                  )),                
                                      
                                      plotlyOutput("emotional_overview", height = 1000),
                             ),
                             
                             tabPanel("Medical", 
                                      selectInput("medical_input", "Medical Variables:",
                                                  c("Diagnosed Depression" = "depression",
                                                    "Number of Psych Jobs per 1,000" = "jobs_1000_orig_psych",
                                                    "Almost Everyday Use Drugs/Medication To Affect Mood" = "almost_every_day_drug_med_mood",
                                                    "Sometimes Use Drugs/Medication To Affect Mood" = "sometimes_drug_med_mood",
                                                    "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                                                  )),          
                                      
                                      plotlyOutput("medical_overview", height = 1000)
                             ))
        ),
        
        tabPanel("Models",
                 #  tabsetPanel(
                 #    titlePanel("Religious"),
                 # ),
                 
                 titlePanel("Correlations Between Variables"),
                 
                 plotOutput("matrix", height = 800),     
                 
                 sidebarPanel(
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
                                   "Number of Social Jobs per 1,000" = "jobs_1000_orig_social",
                                   
                                   "Republican" = "republican",
                                   "Democrat" = "democrat",
                                   "Independent / Other" = "independent_other",
                                   
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
                                   "Number of Social Jobs per 1,000" = "jobs_1000_orig_social",
                                   
                                   "Republican" = "republican",
                                   "Democrat" = "democrat",
                                   "Independent / Other" = "independent_other",
                                   
                                   
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
                     
                     
                     
                 ),
                 
                 mainPanel(
                     plotlyOutput("correlation", width = 600)
                 ),
                 
                 h4(),
                 h5()
        ),
        
        tabPanel("Analysis", 
                 titlePanel("Regression Table"),
                 gt_output(outputId ="table"),
                 h5("The follwing regression table holds depression as a 
                dependent variable and shows how income is negatively
                correlated with having diagnosed depression, while 
                taking a drug/medicine to enhance mood is, as expected,
                positively correlated."),
                 
                 titlePanel("Create Your Own Regression Table"),
                 
                 sidebarPanel(
                     selectInput("dependent", "Dependent Variable:",
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
                                   "Number of Social Jobs per 1,000" = "jobs_1000_orig_social",
                                   
                                   "Republican" = "republican",
                                   "Democrat" = "democrat",
                                   "Independent / Other" = "independent_other",
                                   
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
                     
                     selectInput("ind_religion", "Religious Variables:",
                                 c("Is Religion Important Part of Daily Life?" = "religion_imp",
                                   "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                                   "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                                   "Christian" = "christian",
                                   "Jewish" = "jewish",
                                   "Muslim" = "muslim")),
                     
                     selectInput("ind_social", "Social Variables:",
                                 c("Social Wellbeing Index" = "wellbeing_index",
                                   "Hours Spent Socially" = "social_hours",
                                   "Very Weak Close Relationship" = "x1_strong_disagree_relationship",
                                   "Very Strong Close Relationship" = "x5_strong_agree_relationship",
                                   "Number of Social Jobs per 1,000" = "jobs_1000_orig_social"
                                 )),  
                     
                     selectInput("ind_political", "Political Variables:",
                                 c( "Republican" = "republican",
                                    "Democrat" = "democrat",
                                    "Independent / Other" = "independent_other"
                                 )),  
                     
                     selectInput("ind_economic", "Economic Variables:",
                                 c("Income 2016" = "income_2016",
                                   "Financial Well-Being" = "value_financial_well_being",
                                   "Unemployment Rate" = "value_unemployment"
                                 )),     
                     
                     selectInput("ind_emotional", "Emotional Variables:",
                                 c(
                                     "Happiness" = "yes_happiness",
                                     "Enjoyment" = "yes_enjoyment",
                                     "Worry" = "yes_worry",
                                     "Anger" = "yes_anger",
                                     "Sadness" = "yes_sadness"
                                 )),         
                     
                     selectInput("ind_medical", "Medical Variables:",
                                 c("Diagnosed Depression" = "depression",
                                   "Number of Psych Jobs per 1,000" = "jobs_1000_orig_psych",
                                   "Almost Everyday Use Drugs/Medication To Affect Mood" = "almost_every_day_drug_med_mood",
                                   "Sometimes Use Drugs/Medication To Affect Mood" = "sometimes_drug_med_mood",
                                   "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                                 ))), 
                 
                 mainPanel(
                     gt_output(outputId ="table_variable")),
                 
                 
                 p(),
        ),
        
        tabPanel("About", 
                 titlePanel("About the Project"),
                 h5("The project attempts to examine data from 89 Metropolitan
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
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs(y = "Value", fill = "Value",
                 
                 title = "Religious Variables in 89 Metropolitan Statistical Areas", 
                 subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis") +
            theme(text = element_text(size=8))
    }
    )  
    
    output$social_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$social_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs(y = "Value", fill = "Value",
                 
                 title = "Social Variables in 89 Metropolitan Statistical Areas", 
                 subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis")+
            theme(text = element_text(size=8))
    }
    )  
    
    output$political_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$political_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs( y = "Value", fill = "Value",
                  
                  title = "Political Variables in 89 Metropolitan Statistical Areas", 
                  subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis")+
            theme(text = element_text(size=8))
    }
    )  
    
    output$economic_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$economic_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs( y = "Value", fill = "Value",
                  
                  title = "Economic Variables in 89 Metropolitan Statistical Areas", 
                  subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis")+
            theme(text = element_text(size=8))
    }
    )  
    
    output$emotional_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$emotional_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs( y = "Value", fill = "Value",
                  
                  title = "Emotional Variables in 89 Metropolitan Statistical Areas", 
                  subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis")+
            theme(text = element_text(size=8))
    }
    )  
    
    output$medical_overview <- renderPlotly({
        all_pivot %>%
            filter(category == input$medical_input) %>%
            ggplot(aes(x= reorder(area_title,-value),y = value)) + geom_col(aes(fill = value)) + 
            scale_fill_viridis_c() + 
            xlab(NULL) + ylab(NULL) + theme_classic() + coord_flip()+ 
            labs( y = "Value", fill = "Value",
                  
                  title = "Medical Variables in 89 Metropolitan Statistical Areas", 
                  subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis")+
            theme(text = element_text(size=8))
    }
    )  
    
    # Model Graphs
    
    
    output$correlation <- renderPlotly({
        all_joined %>%
            ggplot(aes_string(input$variable_a, input$variable_b)) + geom_point() + 
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
            scale_y_log10() + scale_x_log10() + 
            labs(x= input$variable_a, y = input$variable_b, 
                 
                 title = "Emotional Well-Being: Correlation of Two Variables in \n 89 Metropolitan
                Statistical Areas", 
                 subtitle = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis") +
            theme_classic()
        
        
        
        
    }
    )  
    
    
    output$matrix <- renderPlot({
        corr %>%
            corrplot(order = "hclust", tl.offset = 2, tl.cex = .8,
                     tl.col = "black")
    })
    
    
    # Table Discussion
    
    output$table <- render_gt({
        lm(depression ~ income_2016 + value_unemployment + `x5_strong_agree_relationship`, data = all_joined) %>%
            tidy(conf.int = TRUE) %>%
            select(term,estimate, conf.low,conf.high) %>% gt()%>%
            tab_header(
                title = "Effect of Income, Unemployment, Taking a Mood Enhancing Drug/Medicine, and Having Strong Relationships on Diagnosed Depression") %>%
            tab_spanner(label = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis", 
                        columns = vars(term,estimate,conf.low,conf.high)) %>%
            cols_label(term = "Variable", estimate = "Estimate", 
                       conf.low = "Lower bound", conf.high = "Upper bound") 
    })
    
    output$table_variable <- render_gt({
        lm(as.formula(paste(input$dependent," ~ ",paste(c(input$ind_religion,
                                                          input$ind_social,
                                                          input$ind_political,
                                                          input$ind_economic,
                                                          input$ind_emotional,
                                                          input$ind_medical
        ),collapse="+"))),data=all_joined) %>%
            tidy(conf.int = TRUE) %>%
            select(term,estimate, conf.low,conf.high) %>% gt() %>%
            tab_header(
                title = "Effect of Religious, Social, Economic, Emotional, and Medical Variables 
  on A Chosen Dependant Variable") %>%
            tab_spanner(label = "Data from Gallup Analytics + Bureau of Labor Statistics + Bureau of Economic Analysis", 
                        columns = vars(term,estimate,conf.low,conf.high)) %>%
            cols_label(term = "Variable", estimate = "Estimate", 
                       conf.low = "Lower bound", conf.high = "Upper bound") 
        
        
        
    })
    
}


shinyApp(ui, server)



