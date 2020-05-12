
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
    "The Sacred and the Profane: Metropolitan Emotions",
    
    tabPanel("Introduction",
    
            img(src='7.jpg', height = 600, width = 800),
             
           h3( "The Question:"),

p("How do different factors affect mental health and emotional well-being in metropolitan areas across the United States."),

h3("The Sample:"),

p("For the current study, data from 89 Metropolitan Statistical Areas (MSA) in the United States from the year 2016 was used.
The 89 Metropolitan Statistical Areas (MSA) studied were chosen considering the availability of data across multiple variables."),
             
h3("Metropolitan Statistical Areas (MSA):"),
  
  p("A geographical area that contains a high population density in the center but maintains economic and social integration with its surrounding communities.  According to the United States Census Bureau it is the United States Office of Management and Budget (OMB) that delineates metropolitan and micropolitan statistical areas depending on standards using the Census Bureau data."), 

p("Each metropolitan statistical area must have at least one urbanized area of 50,000 or more inhabitants. In 2020 there were 384 metropolitan statistical areas in the United States."),

tags$a(href="https://www.census.gov/programs-surveys/metro-micro/about.html", "MSA Census"),

tags$a(href="https://www.bls.gov/sae/additional-resources/metropolitan-statistical-area-definitions.htm", "MSA Definition")
             
    ),
    
    tabPanel("Overview",
            
             
            
                 titlePanel("Social,Religious,Medical & Economic Factors"),
                h4("For 89 Metropolitan Statistical Areas in the United States in the Year 2016"),
                       
             
             
             
             tabsetPanel(type = "tabs",
                         
                        
                      tabPanel("Religious", 
                           
                               sidebarPanel(   
                                
                                 selectInput("religion_input", "Religious Variables:",
                                     c("Is Religion Important Part of Daily Life?" = "religion_imp",
                                       "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
                                       "Number of Religious Jobs Per 1,000" = "jobs_1000_orig_religion",
                                       "Christian" = "christian",
                                       "Jewish" = "jewish",
                                       "Muslim" = "muslim"
                                       )),
                                 
                                 p("Religion is thought to affect emotional wellbeing in Metropolitan Areas throughout the United States. Through the following graphs you can explore how different Metropolitan Statistical Areas compare and contrast in regards to:"),
                                 
                                 p("-How important religion is in daily life"),
                                  p( "-How common it is to attend Church, Synagogue or Mosque once a week"),
                                   p("-The number of religious jobs that exist in a Metropolitan Statistical Area for every 1,000 jobs."),
                                   p("-What percent of the population is Christian"),
                                  p( "-What percent of the population is Muslim"),
                                   p("-What percent of the population is Jewish")
                                 
                                 
                      ),
                        
                      mainPanel(
                       plotlyOutput("religion_overview", height = 1000)
                                             )),
                                     
             
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
                                       "Unemployment Rate" = "value_unemployment"
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
           
           p("In the following correlation matrix one can observe how the different variables and factors that have been considered in the overview section relate to each other. "),
           
           plotOutput("matrix", height = 800), 
           
           titlePanel("Test Your Own Correlations"),
           
           p("Here, you can test different correlations yourself:"),
           
           p("Pick a first variable on the dropdown selection in Variable A, and a second variable in the dropdown section in Variable B to see if a correlation exists. "),
           
           p("You can use the correlation matrix above as a guide."),
           
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
             
             selectInput("ind_1", "1st Ind. Variable:",
                                    c(
                                      "Attend Church, Synagogue, or Mosque Once Per Week" = "church_week",
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
             
             selectInput("ind_2", "2nd Ind. Variable:",
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
                           "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                           )),
             
            selectInput("ind_3", "3rd Ind. Variable:",
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
             
             selectInput("ind_4", "4th Ind. Variable:",
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
                           "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                         )),
            
            selectInput("ind_5", "5th Ind. Variable:",
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
                          "Never Use Drugs/Medication To Affect Mood" = "never_drug_med_mood"
                        ))
       ),
       
       

mainPanel(
  
  tabsetPanel(type = "tabs",
              
              tabPanel("2 Ind. Variables", 
                       
    gt_output(outputId ="table_variable2")),
    
    tabPanel("3 Ind. Variables", 
            
              gt_output(outputId ="table_variable3")),
             
             tabPanel("4 Ind. Variables", 
                      
                      gt_output(outputId ="table_variable4")),
                      
                      tabPanel("5 Ind. Variables", 
                               
                               gt_output(outputId ="table_variable5"))
    
    
    
  
    
    
    
    
    
)),

             p(),
             ),

    tabPanel("About", 
             titlePanel("About the Project"),
             p("The project attempts to examine data from 89 Metropolitan
                Statistical Areas in the United States in the Year 2016 to establish 
                social, religious & medical effects on emotional wellbeing by ananlyzing differnt variables."),
             
             titlePanel("Scales"),
             p("The project could be further developed to include other scales,
                such as county, and state level data. These could further
                establish relationships."),
             
             titlePanel("Origin"),
             p("Initially, this study was focused on understanding how the 
             number of clergy compared to the number of psychologists, 
             psychiatrists, and social workers in different metropolitan areas.
             It was hypothesized that religion and spirituality play a significant
             role in the mental health of certain communities (See: “The role
             of religion and spirituality in mental health”
             (https://www.ncbi.nlm.nih.gov/pubmed/25046080), 
             “The Clergy as a Source of Mental Health Assistance: What Americans
             Believe” (https://www.jstor.org/stable/20058132?seq=1), “ 
             Patterns and Correlates of Contacting Clergy for Mental Disorders 
             in the United States” (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1360908/).
             
Principal Findings of the Study: “Patterns and Correlates of Contacting Clergy for Mental Disorders in the United States” stated:

“One-quarter of those who ever sought treatment for mental disorders did so from a clergy member. Although there has been a decline in this proportion between the 1950s (31.3 percent) and the early 1990s (23.5 percent), the clergy continue to be contacted by higher proportions than psychiatrists (16.7 percent) or general medical doctors (16.7 percent). Nearly one-quarter of those seeking help from clergy in a given year have the most seriously impairing mental disorders. The majority of these people are seen exclusively by the clergy, and not by a physician or mental health professional.”

With this in mind, the current study expanded to include additional factors that could also have an effect on the mental health and emotional well-being of people in metropolitan areas and should therefore, be controlled for.

These factors included:

Frequency of attending Church, Synagogue or Mosque
The importance of religion in daily life
The religion that is practiced (Christian, Jewish, or Muslim)
The amount of hours that are spent socially
The strength of close social relationships
The political party
Income
Financial Well-being Index
Diagnosed Depression
The Frequency with which Drugs/Medication is used to Affect Mood

Correlations between these factors can be observed in the Models Section.
"),
             
            
             titlePanel("Data"),
             
             h5("Sources"),
             h5("The data was gathered from 89 Metropolitan and Micropolitan 
                Statistical Areas in the United States in the Year 2016"),
             
             h5("The sources for the data included:
             
             Gallup U.S. Dailies 2016 by Metropolitan 
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
                
                U.S. Dailies – Gallup Analytics – 2016
https://www.gallup.com/174146/gallup-daily-methodology.aspx
https://www.gallup.com/analytics/213617/gallup-analytics.aspx
                
            Employment data for Metropolitan Statistical Areas (MSA) by the U.S. Bureau of Labor Statistics
https://www.bls.gov/oes/2016/may/oes_stru.htm

Income Data from 2016, 2017, 2018 was gathered from the Bureau of Economic Analysis (BEA) Personal Income by County, Metro, and Other Areas.
https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas

                "),
             
             titlePanel("About Me"),
             p("
Benjamin Villa Wiesner is an Architect and a Design Engineer from Harvard’s
Graduate School of Design and the School of Engineering and Applied Sciences. 
You can reach me at benjaminvilla@mde.harvard.edu and find me on GitHub."),

             br(),
        
p("The repo for this project can be found here (https://github.com/benjaminvillaw)."),

br(),


h3("Interests:"),
p("Architecture | Design | Cities | Data science in R."),
br(),

h3("Education:"),
p("MDE Master in Design Engineering"),
p("Harvard University"),
br(),
p("BA in Architecture"),
p("Universidad de los Andes"),
br(),
br()
             
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
    
    # Two Variables
    
    output$table_variable2 <- render_gt({
      lm(as.formula(paste(input$dependent," ~ ",paste(c(input$ind_1,
                                                        input$ind_2
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
    
    # Three Variables
    
    output$table_variable3 <- render_gt({
      lm(as.formula(paste(input$dependent," ~ ",paste(c(input$ind_1,
                                                        input$ind_2,
                                                        input$ind_3
                                                      
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
    
    # Four Variables
    
    output$table_variable4 <- render_gt({
        lm(as.formula(paste(input$dependent," ~ ",paste(c(input$ind_1,
                                                          input$ind_2,
                                                          input$ind_3,
                                                          input$ind_4
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
    
    
    # Five Variables
    
    output$table_variable5 <- render_gt({
      lm(as.formula(paste(input$dependent," ~ ",paste(c(input$ind_1,
                                                        input$ind_2,
                                                        input$ind_3,
                                                        input$ind_4,
                                                        input$ind_5
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






