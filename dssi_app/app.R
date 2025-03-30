library(shiny)
library(tidyverse)
library(RColorBrewer)

tb_data <- read.csv("tb_data.csv")

tb_data <- subset(tb_data, Region!="")

var_labels <- c(
  tb_incidence = "TB Incidence (Cases per 100,000)",
  tb_detect_rate = "TB Detection Rate (% Case Detection Rate)",
  tb_trt_success_rate = "TB Treatment Success Rate (% of New Case Treatment Success)",
  mortality_rate_infant = "Infant Mortality Rate (Per 1,000 Live Births)",
  poverty_headcount_ratio_pct = "Poverty Headcount Ratio (% Population Salaried < $2.15/day)",
  life_exp = "Life Expectancy (At Birth, Total Years)", 
  births_skilled_pct = "Births Attended by Skilled Health Staff (% of Total)",
  persistence_primary_male_pct = "Male Persistence to Last Grade of Primary School (% of Cohort)", 
  persistence_primary_female_pct = "Female Persistence to Last Grade of Primary School (% of Cohort)", 
  persistence_primary_total_pct = "Total Persistence to Last Grade of Primary School (% of Cohort)",
  internet_users = "Internet Users (Per 100 People)", 
  pct_undernourished = "Prevalence of Undernourishment (% of Population)",
  primary_completion_rate_total = "Total Primary School Completion Rate (% of Relevant Age Group)",
  lit_rate_15.24_female_pct = "Youth Female Literacy Rate (% of Relevant Age Group)",
  lit_rate_15.24_male_pct = "Youth Male Literacy Rate (% of Relevant Age Group)", 
  phone_subscriptions = "Fixed Telephone Subscriptions (Per 100 People)",
  lit_rate_15_plus_pct = "Total Adult Literacy Rate (% of Relevant Age Group)",
  immunization_measles_pct_12.23mos = "Measles Immunization (% of Children Aged 12-23 Months",
  pct_urban_pop_slums = "Urban Population Living in Slums (% of Urban Population)",
  cellular_subscriptions = "Mobile Cellular Subscriptions (Per 100 People)",
  prim_enroll_rate = "Primary School Enrollment Rate (% of Primary School Aged Children)"
)

var_labels <- var_labels[names(var_labels) %in% names(tb_data)]

ui <- fluidPage(
  titlePanel("Data Science for Social Impact Datathon - HMC 2025"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select a Variable:",
                  choices = setNames(names(var_labels), var_labels),
                  selected = "tb_incidence"),
      
      p('The', a('sixth Millennium Development Goal', href="https://www.un.org/millenniumgoals/aids.shtml", target='_blank'),
        ', as laid out by the United Nations, is to "combat HIV/AIDS, malaria, and other diseases." This goal has seen success with regard to HIV/AIDS – the UN reports a 40% reduction in incidence of HIV between 2000 and 2013. However, HIV/AIDS is only one of many public health crises that affect large swathes of the global population. In particular, Tuberculosis holds the title of the leading cause of death by a single infectious agent, and it has been named "the deadliest infectious disease in recorded history" (',
        a("Berida and Lindsley", href="https://doi.org/10.1021/acs.jmedchem.4c02876", target='_blank'), ")."),
      
      p("This dashboard visualizes how global tuberculosis (TB) death rates vary with various health-related, education-related, and economic-related factors.
        The data used for this Datathon are from World Bank's Millennium Development Goals dataset, which includes data from 263 countries and regional groupings, from 2006 to 2015. The dataset is available",
        a("here", href="https://drive.google.com/file/d/1RfGXKEtvP4KxMkkuE8AfuZOZRvuqLtUD/view?usp=sharing", target="_blank"), ".",
        "A full glossary of the variables used in this analysis is available",
        a("here", href="https://docs.google.com/spreadsheets/d/11Mz_s3ZJCqfCROZEXLbUthnOXpUL9KTD/edit?gid=1306304968#gid=1306304968", target="_blank"), ".")
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    req(input$xvar)
    
    ggplot(tb_data, aes(x = .data[[input$xvar]], y = tb_death_rate)) +
      geom_point(aes(size = pop_total, color = Region), alpha = 0.7) +
      scale_size_binned(name = "Total Population", breaks = c(5e7, 2e8, 5e8, 1e9),
                        labels = c("< 50M", "50M–200M", "200M–500M", "> 1B")) +
      scale_color_brewer(palette = "Set2") +
      theme_minimal() +
      labs(
        title = paste("TB Death Rate vs", var_labels[[input$xvar]]),
        x = var_labels[[input$xvar]],
        y = "TB Death Rate (Per 100,000 People)",
        size = "Total Population",
        color = "Region"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 37),
        axis.title = element_text(face = "bold", size = 25),
        legend.title = element_text(face = "bold", size = 25),
        legend.text = element_text(size = 20)
      )
  })
}

shinyApp(ui = ui, server = server)