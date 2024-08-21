library(shiny)
library(plotly)
library(dplyr)

# Replace this with your actual data loading code
# For example: data <- read.csv("your_data.csv")
# Assume your dataframe is named `data`

# Sample structure of data based on your screenshot
data <- data.frame(
    Wavelength = rep(440:450, times = 3),
    Value = runif(33, min = 0.03, max = 0.04),
    Spectra = rep(c("HW2_00000", "HW2_00001", "HW2_00002"), each = 11)
    # Add other columns as needed
)

ui <- fluidPage(
    titlePanel("Radiometric Spectra Viewer"),
    sidebarLayout(
        sidebarPanel(
            selectInput("spectrum_select", "Select Spectrum to Delete:",
                        choices = unique(data$Spectra), selected = NULL),
            actionButton("delete", "Delete Selected Spectrum"),
            actionButton("save_exit", "Save and Exit")
        ),
        mainPanel(
            plotlyOutput("spectra_plot")
        )
    )
)

server <- function(input, output, session) {
    # Create a reactive dataframe
    spectra_data <- reactiveVal(data)
    
    output$spectra_plot <- renderPlotly({
        plot_ly(spectra_data(), x = ~Wavelength, y = ~Value, color = ~Spectra, showlegend = F,
                type = 'scatter', mode = 'lines', hoverinfo = 'text',
                text = ~paste("Spectrum:", Spectra)) %>%
            layout(showlegend = TRUE)
    })
    
    observeEvent(input$delete, {
        if (!is.null(input$spectrum_select) && input$spectrum_select != "") {
            spectrum_to_delete <- input$spectrum_select
            spectra_data(spectra_data() %>% filter(Spectra != spectrum_to_delete))
            
            # Update the dropdown list to reflect the remaining spectra
            updateSelectInput(session, "spectrum_select",
                              choices = unique(spectra_data()$Spectra), selected = NULL)
        }
    })
    
    observeEvent(input$save_exit, {
        # Save the reactive dataframe to the global environment
        assign("edited_data", spectra_data(), envir = .GlobalEnv)
        
        # Stop the Shiny app
        stopApp()
    })
}

shinyApp(ui = ui, server = server)
