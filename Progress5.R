# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Malaria Modelling Dashboard",
                  tabPanel("Intervention Mixes",
                           sidebarPanel(
                             tags$h3("Country Selection"),
                             selectInput("country_select", "Select Country", choices = c("Nigeria", "Sudan", "Malawi")),
                             h3("Intervention Mix Selection"),
                             checkboxGroupInput("intervention_select", "Select Intervention Mix",
                                                choices = c("CM+IPTp+ITN(IG2)+SMC", "CM+IPTp+ITN(std)+PMC", "CM+IPTp+SMC+UrbanITN", "CM+IPTp+ITN(PBO)+SMC")),
                             tags$label("Map of intervention mixes"),
                             actionButton("download_map", "Download Map")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Country Map Showing Interventions"),
                             h3("District level"),
                             verbatimTextOutput("txtout")
                           ) # mainPanel
                  ), # Navbar 1, tabPanel
                  tabPanel("Predictive Modelling",
                           sidebarPanel(
                             tags$h3("Current Implementation (BAU)"),
                             sliderInput(inputId = "intervention_coverage_smc",
                                         label = "SMC Coverage:",
                                         min = 0, max = 100, value = 50),
                             sliderInput(inputId = "intervention_coverage_itn",
                                         label = "ITN Use:",
                                         min = 0, max = 100, value = 50),
                             sliderInput(inputId = "intervention_coverage_testing",
                                         label = "Testing Rate:",
                                         min = 0, max = 100, value = 50),
                             sliderInput(inputId = "intervention_coverage_iptp",
                                         label = "IPTp3 Coverage:",
                                         min = 0, max = 100, value = 50),
                             sliderInput(inputId = "intervention_coverage_treatment",
                                         label = "Treatment Rate:",
                                         min = 0, max = 100, value = 50),
                             sliderInput(inputId = "intervention_coverage_irs",
                                         label = "Population Protected with IRS:",
                                         min = 0, max = 100, value = 50),

                             tags$h3("Intervention Scenarios"),
                             selectInput(inputId = "intervention_coverage_scenario",
                                         label = "Select Intervention Scenario:",
                                         choices = c("80% Effective Coverage", "Funded Intervention"),
                                         selected = "80% Coverage"),

                             actionButton("run_simulation", "Run Simulation")
                           ), # sidebarPanel
                           mainPanel(
                             h1("Time series for all burden metrics"),
                             h4("Output 1"),
                             verbatimTextOutput("txtout2")
                           ) # mainPanel
                  ), # Navbar 2, tabPanel
                  tabPanel("Burden Averted by each plan",
                           sidebarLayout(
                             sidebarPanel(
                               # Add input controls for parameters
                               numericInput("population", "Population Size:", value = 10000),
                               numericInput("infection_rate", "Infection Rate:", value = 0.05),
                               numericInput("recovery_rate", "Recovery Rate:", value = 0.1),
                               actionButton("test", "Test")
                             ),
                             mainPanel(
                               plotOutput("simulation_plot")
                             ) #main panel
                           ) # sidebarLayout
                  )
                )
) # fluidPage

# Define server function
server <- function(input, output, session) {

  output$txtout <- renderText({
    paste("Admin2 Intervention Mix:", input$admin2)
  })

  output$txtout2 <- renderText({
    paste(input$txt3, input$txt4, sep = " ")
  })

  # Run simulation based on user inputs
  observeEvent(input$run_simulation, {
    coverage_scenario <- input$intervention_coverage_scenario
    population <- input$population
    infection_rate <- input$infection_rate
    recovery_rate <- input$recovery_rate

    # Perform malaria modeling simulation using EMOD or other method
    # Replace the code below with your own malaria modeling simulation code
    # This is just a placeholder
    simulated_data <- data.frame(
      time = seq(0, 100, by = 1),
      infected = population * (1 - exp(-infection_rate * seq(0, 100, by = 1))),
      recovered = population * (1 - exp(-recovery_rate * seq(0, 100, by = 1)))
    )

    # Plot the simulation results
    output$simulation_plot <- renderPlot({
      ggplot(simulated_data, aes(x = time)) +
        geom_line(aes(y = infected, color = "Infected")) +
        geom_line(aes(y = recovered, color = "Recovered")) +
        labs(x = "Time", y = "Population") +
        scale_color_manual(values = c("Infected" = "red", "Recovered" = "blue")) +
        theme_minimal()
    })
  })

  # Download the map on button click
  observeEvent(input$download_map, {
    # Replace this code with the map download logic
    # For demonstration purposes, let's assume the map is downloaded and saved as "map.png"
    downloadHandler(
      filename = "map.png",
      content = function(file) {
        # Code to download the map goes here
        # Save the map file to the specified path (file)
        # Example code:
        # saveMapToFile(map, file)
      }
    )
  })

  # Update the labels of the numericInput based on the selected admin2 value
  observeEvent(input$admin2, {
    updateNumericInput(session, "admin2", label = paste("Admin2 Intervention Mix:", input$admin2))
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
