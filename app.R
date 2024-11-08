# Load packages ----
library(shiny)
library(rvest)
library(ggplot2)
library(dplyr)
library(stringr)
library(zoo)
library(ggrepel)

#Function ----
get_args <- function(var) {
  switch(var,
         #Active franchises
         "Albany FireWolves" = list("Philadelphia Wings[1]", "red2", 
                                    "New England Black Wolves", "darkorange2", 
                                    "Albany FireWolves", "darkred"),
         "Buffalo Bandits" = list("Buffalo Bandits", "orange"),
         "Calgary Roughnecks" = list("Calgary Roughnecks", "gray70"),
         "Colorado Mammoth" = list("Baltimore Thunder", "gold", 
                                   "Pittsburgh Crossefire", "red", 
                                   "Washington Power", "midnightblue", 
                                   "Colorado Mammoth", "firebrick4"),
         "Georgia Swarm" = list("Montreal Express", "blue", 
                                "Minnesota Swarm", "skyblue3", 
                                "Georgia Swarm", "gold"),
         "Halifax Thunderbirds" = list("Rochester Knighthawks[1]", "cyan4", 
                                       "Halifax Thunderbirds", "darkorchid3"),
         "Las Vegas Desert Dogs" = list("Las Vegas Desert Dogs", "black"),
         "New York Riptide" = list("New York Riptide", "coral"),
         "Philadelphia Wings" = list("Philadelphia Wings", "lightyellow"),
         "Rochester Knighthawks" = list("Rochester Knighthawks", "green4"),
         "San Diego Seals" = list("San Diego Seals", "purple1"),
         "Saskatchewan Rush" = list("Syracuse Smash", "cyan2", 
                                    "Ottawa Rebel", "blue", 
                                    "Edmonton Rush", "black", 
                                    "Saskatchewan Rush", "green2"),
         "Toronto Rock" = list("Ontario Raiders", "gold4", 
                               "Toronto Rock", "royalblue"),
         "Vancouver Warriors" = list("Albany Attack", "cyan3", 
                                     "San Jose Stealth", "red2", 
                                     "Washington Stealth", "snow2", 
                                     "Vancouver Stealth", "black", 
                                     "Vancouver Warriors", "lightgoldenrod"),
         #Defunct Franchises
         "Anaheim Storm" = list("New Jersey Storm", "royalblue3",
                                "Anaheim Storm", "deepskyblue1"),
         "Arizona Sting" = list("Columbus Landsharks", "gray80",
                                "Arizona Sting", "indianred"),
         "Boston Blazers (1992-97)" = list("New England Blazers", "forestgreen",
                                        "Boston Blazers[1]", "springgreen2"),
         "Boston Blazers (2009-11)" = list("Boston Blazers", "red"),
         "Charlotte Cobras" = list("Charlotte Cobras", "salmon"),
         "Chicago Shamrox" = list("Chicago Shamrox", "lawngreen"),
         "Detroit Turbos" = list("Detroit Turbos", "magenta2"),
         "New York Saints" = list("New Jersey Saints", "darkorchid4",
                                  "New York Saints", "navy"),
         "Orlando Titans" = list("New York Titans", "mediumblue",
                                  "Orlando Titans", "orange2"),
         "Panther City Lacrosse Club" = list("Panther City Lacrosse Club", "purple"),
         "Portland Lumberjax" = list("Portland Lumberjax", "goldenrod1"),
         "Pittsburgh Bulls" = list("Pittsburgh Bulls", "yellow"),
         "Washington Wave" = list("Washington Wave", "red"))
}

active_teams <- c(
  "Albany FireWolves",
  "Buffalo Bandits",
  "Calgary Roughnecks",
  "Colorado Mammoth",
  "Georgia Swarm",
  "Halifax Thunderbirds",
  "Las Vegas Desert Dogs",
  "New York Riptide",
  "Philadelphia Wings",
  "Rochester Knighthawks",
  "San Diego Seals",
  "Saskatchewan Rush",
  "Toronto Rock",
  "Vancouver Warriors"
)

defunct_teams <- c(
  "Anaheim Storm",
  "Arizona Sting",
  "Boston Blazers (1992-97)",
  "Boston Blazers (2009-11)",
  "Charlotte Cobras",
  "Chicago Shamrox",
  "Detroit Turbos",
  "New York Saints",
  "Orlando Titans",
  "Panther City Lacrosse Club",
  "Portland Lumberjax",
  "Pittsburgh Bulls",
  "Washington Wave"
)

# Working Directory ----
setwd("C:/Users/whovatter/Documents/R/NLL")

# Source helpers ----
source("Template.R")

# User interface ----


ui <- fluidPage(
  titlePanel("NLL Franchise Attendance Records"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create NLL franchise timelines."),
      
      selectInput(
        inputId = "var",
        label = "Choose a team to display",
        choices = list(
          "Select a team" = "",
          "Active Franchises" = active_teams,
          "Defunct Franchises" = defunct_teams
        )
      ),
      
      checkboxInput("show_franch",
                    label = "Show all franchise locations?",
                    value = TRUE),
      
      checkboxInput("cons_yaxis", #An idea for the future (this is not feature creep I swear)
                    # The user can choose the range of the Y axis 
                    #(e.g. can manually input 3,000 - 17,000)
                    # Might break the labeling, now that I think about it
                    label = "Consistent Y axis (0-20,000)",
                    value = FALSE),
      
      sliderInput("date_range",
                  label = "Seasons",
                  min = 1987,
                  max = 2024,
                  value = c(1987, 2024),
                  step = 1,
                  round = TRUE,
                  ticks = FALSE,
                  sep = "")
      
    ),
    
    mainPanel(plotOutput("map", height = "600px", width ="1000px"))
  )
)

server <- function(input, output, session) {
  
  # Create a reactiveValues object to control rendering
  rv <- reactiveValues(
    render_trigger = 0,
    data = NULL,
    last_team = NULL
  )
  
  # Observe changes in input$var or input$show_franch and update the data
  observeEvent(list(input$var, input$show_franch, input$cons_yaxis), {
    req(input$var)  # Ensure input$var is not empty
    
    args <- c(input$show_franch, input$cons_yaxis, 
              1987, 2024, get_args(input$var))
    
    results <- do.call(nll_map, args) 
    
    # Store the results
    rv$data <- results
    rv$last_team <- input$var
    
    # Update the slider with new min and max values
    updateSliderInput(session, "date_range", 
                      min = results$slider_min, 
                      max = results$slider_max, 
                      value = c(results$slider_min, results$slider_max))
    
    # Trigger a render
    rv$render_trigger <- rv$render_trigger + 1
  })
  
  # Observe changes in date_range and trigger a re-render
  observeEvent(input$date_range, {
    req(rv$data, rv$last_team)  # Ensure that data is available and a team is selected
    
    # Always update when date range changes
    args <- c(input$show_franch, input$cons_yaxis, 
              min(input$date_range), max(input$date_range), 
              get_args(rv$last_team))
    
    rv$data <- do.call(nll_map, args)
    rv$render_trigger <- rv$render_trigger + 1
  })
  
  # Render the plot
  output$map <- renderPlot({
    # Trigger based on render_trigger changing
    req(rv$render_trigger)
    req(rv$data)
    
    print("plot rendered")
    # Render the plot
    rv$data$plot
  })
}


# Run app ----
shinyApp(ui, server)