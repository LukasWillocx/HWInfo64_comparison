library(shiny)
library(shinydashboard) # layout 
library(ggplot2) # plotting
library(gridExtra)
library(lubridate)  # datetime handling

# Load custom functions
source("app_functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "HWInfo64 Log Comparison"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    # Include external CSS file
    includeCSS("app_styles.css"),
    
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload CSV Files", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(6,
                             fileInput("csv1", "Upload CSV - Condition 1", accept = ".csv"),
                             textInput("label1", "Label for Condition 1", value = "Condition 1", placeholder = "e.g., Baseline")
                      ),
                      column(6,
                             fileInput("csv2", "Upload CSV - Condition 2", accept = ".csv"),
                             textInput("label2", "Label for Condition 2", value = "Condition 2", placeholder = "e.g., Modified")
                      )
                    ),
                    hr(),
                    conditionalPanel(
                      condition = "output.files_uploaded",
                      h4("Select Variables"),
                      uiOutput("time_selector"),
                      br(),
                      uiOutput("y_selector"),
                      br(),
                      actionButton("confirm_setup", "Confirm Setup", class = "btn-success", icon = icon("check"))
                    )
                )
              )
      ),
      
      # Visualization Tab
      tabItem(tabName = "viz",
              conditionalPanel(
                condition = "output.setup_confirmed",
                fluidRow(
                  box(title = "Plot Configuration", status = "primary", solidHeader = TRUE, width = 4,
                      h4("Selected Variables:"),
                      verbatimTextOutput("selected_vars"),
                      br(),
                      h4("Plot Display Options:"),
                      radioButtons("plot_style", "Plot Style:",
                                   choices = list(
                                     "Side-by-side (Separate)" = "separate",
                                     "Combined (Overlay)" = "combined"
                                   ),
                                   selected = "separate"),
                      br(),
                      actionButton("generate_plots", "Generate Plots", class = "btn-primary", icon = icon("chart-bar"))
                  ),
                  
                  box(title = "Plot Output", status = "success", solidHeader = TRUE, width = 8,
                      conditionalPanel(
                        condition = "output.plots_available",
                        uiOutput("plot_panels")
                      )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.setup_confirmed",
                fluidRow(
                  box(title = "Setup Required", status = "warning", solidHeader = TRUE, width = 12,
                      h4("Please upload and configure your data files in the Data Upload tab first.")
                  )
                )
              )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
              conditionalPanel(
                condition = "output.plots_available",
                fluidRow(
                  box(title = "Export Options", status = "primary", solidHeader = TRUE, width = 6,
                      h4("PDF Report Settings"),
                      numericInput("pdf_width", "Width (inches):", value = 14, min = 5, max = 20, step = 0.5),
                      numericInput("pdf_height", "Height per plot (inches):", value = 5, min = 3, max = 10, step = 0.5),
                      br(),
                      h4("Report Plot Style"),
                      radioButtons("export_plot_style", "Export Plot Style:",
                                   choices = list(
                                     "Side-by-side (Separate)" = "separate",
                                     "Combined (Overlay)" = "combined"
                                   ),
                                   selected = "separate"),
                      br(),
                      downloadButton("download_pdf", "Download PDF Report", class = "btn-success", icon = icon("file-pdf"))
                  ),
                  
                  box(title = "Report Preview", status = "info", solidHeader = TRUE, width = 6,
                      h4("Your report will include:"),
                      tags$ul(
                        tags$li("Comparison plots in selected style"),
                        tags$li("Condition labels and metadata"),
                        tags$li("High-resolution plots"),
                        tags$li("All selected variables")
                      )
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.plots_available",
                fluidRow(
                  box(title = "Setup Required", status = "warning", solidHeader = TRUE, width = 12,
                      h4("Please generate plots in the Visualization tab before exporting.")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to track state
  values <- reactiveValues(
    setup_confirmed = FALSE,
    plots_generated = FALSE
  )
  
  # Functions are now loaded from app_functions.R
  
  df1 <- reactive(load_csv(input$csv1))
  df2 <- reactive(load_csv(input$csv2))
  
  shared_columns <- reactive({
    req(df1(), df2())
    intersect(names(df1()), names(df2()))
  })
  
  output$time_selector <- renderUI({
    req(shared_columns())
    selectInput("time_var", "Time Variable (X-axis)", choices = shared_columns(), width = "300px")
  })
  
  output$y_selector <- renderUI({
    req(shared_columns(), input$time_var)
    y_choices <- setdiff(shared_columns(), input$time_var)
    selectInput("y_vars", "Y-Axis Variable(s) to Compare", choices = as.list(y_choices), multiple = TRUE, width = "100%")
  })
  
  # Setup confirmation
  observeEvent(input$confirm_setup, {
    req(input$time_var, input$y_vars)
    values$setup_confirmed <- TRUE
    showNotification("Setup confirmed! You can now proceed to visualization.", type = "message")
  })
  
  # Functions are now loaded from app_functions.R
  
  safe_data <- reactive({
    req(df1(), df2(), input$time_var, input$y_vars)
    
    # Clean and ensure numeric Y variables
    df1_clean <- df1()
    df2_clean <- df2()
    
    # Handle time variable - parse and convert to relative seconds
    time_col1 <- parse_time_column(df1_clean[[input$time_var]])
    time_col2 <- parse_time_column(df2_clean[[input$time_var]])
    
    # Convert to relative time in seconds
    df1_clean$time_relative <- create_relative_time(time_col1)
    df2_clean$time_relative <- create_relative_time(time_col2)
    
    # Clean Y variables
    for (var in input$y_vars) {
      df1_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df1_clean[[var]])))
      df2_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df2_clean[[var]])))
    }
    
    list(df1 = df1_clean, df2 = df2_clean)
  })
  
  # Function to create plots for a given style - now uses external function
  create_plots_wrapper <- function(plot_style) {
    req(safe_data(), input$time_var, input$y_vars, values$setup_confirmed)
    create_plots(plot_style, safe_data(), input$time_var, input$y_vars, input$label1, input$label2)
  }
  
  plot_pairs <- reactive({
    create_plots_wrapper(input$plot_style)
  })
  
  # Generate plots action
  observeEvent(input$generate_plots, {
    req(plot_pairs())
    values$plots_generated <- TRUE
    showNotification("Plots generated successfully!", type = "message")
  })
  
  output$plot_panels <- renderUI({
    req(plot_pairs(), values$plots_generated)
    
    if (input$plot_style == "combined") {
      # Combined plot layout - one plot per variable
      tagList(
        lapply(seq_along(plot_pairs()), function(i) {
          div(
            h4(paste("Variable:", input$y_vars[i]), style = "margin-top: 20px; margin-bottom: 15px; color: #333;"),
            plotOutput(paste0("plot_combined_", i), height = "500px"),
            hr()
          )
        })
      )
    } else {
      # Side-by-side plot layout
      tagList(
        lapply(seq_along(plot_pairs()), function(i) {
          div(
            h4(paste("Variable:", input$y_vars[i]), style = "margin-top: 20px; margin-bottom: 15px; color: #333;"),
            fluidRow(
              column(6, plotOutput(paste0("plot_left_", i), height = "400px")),
              column(6, plotOutput(paste0("plot_right_", i), height = "400px"))
            ),
            hr()
          )
        })
      )
    }
  })
  
  observe({
    plots <- plot_pairs()
    req(plots, values$plots_generated)
    
    if (input$plot_style == "combined") {
      # Render combined plots
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plot_combined_", idx)]] <- renderPlot({ plots[[idx]]$combined })
        })
      }
    } else {
      # Render separate plots
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plot_left_", idx)]] <- renderPlot({ plots[[idx]]$left })
          output[[paste0("plot_right_", idx)]] <- renderPlot({ plots[[idx]]$right })
        })
      }
    }
  })
  
  # Selected variables display
  output$selected_vars <- renderText({
    if (!is.null(input$y_vars) && length(input$y_vars) > 0) {
      paste(input$y_vars, collapse = "\n")
    } else {
      "No variables selected"
    }
  })
  
  # Output conditionals for UI state management
  output$files_uploaded <- reactive({
    !is.null(df1()) && !is.null(df2())
  })
  
  output$setup_confirmed <- reactive({
    values$setup_confirmed
  })
  
  output$plots_available <- reactive({
    values$plots_generated && !is.null(plot_pairs())
  })
  
  outputOptions(output, "files_uploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "setup_confirmed", suspendWhenHidden = FALSE)
  outputOptions(output, "plots_available", suspendWhenHidden = FALSE)
  
  # PDF download
  output$download_pdf <- downloadHandler(
    filename = function() paste0("hwinfo_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Use the export plot style preference
      export_plots <- create_plots(input$export_plot_style, safe_data(), input$time_var, input$y_vars, input$label1, input$label2)
      req(export_plots, values$plots_generated)
      
      # Use a temporary file approach for better reliability
      temp_file <- tempfile(fileext = ".pdf")
      
      tryCatch({
        # Open PDF device
        pdf(temp_file, width = input$pdf_width, height = input$pdf_height, onefile = TRUE)
        
        # Title page using base R graphics
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.8, "HWInfo64 Log Comparison Report", 
             cex = 2.5, font = 2, adj = 0.5)
        text(0.5, 0.65, paste("Generated on:", Sys.Date()), 
             cex = 1.4, adj = 0.5)
        text(0.5, 0.55, paste("Conditions:", input$label1, "vs", input$label2), 
             cex = 1.6, font = 2, adj = 0.5)
        text(0.5, 0.45, paste("Variables compared:", length(input$y_vars)), 
             cex = 1.2, adj = 0.5)
        text(0.5, 0.35, paste("Plot style:", if(input$export_plot_style == "combined") "Combined overlay" else "Side-by-side"), 
             cex = 1, adj = 0.5)
        text(0.5, 0.25, paste("Variables:", paste(input$y_vars, collapse = ", ")), 
             cex = 1, adj = 0.5)
        
        # Create plots based on export style
        for (i in seq_along(export_plots)) {
          if (input$export_plot_style == "combined") {
            # Combined plots - one per page, centered
            print(export_plots[[i]]$combined)
          } else {
            # Side-by-side plots using gridExtra
            gridExtra::grid.arrange(
              export_plots[[i]]$left, 
              export_plots[[i]]$right, 
              ncol = 2, 
              top = grid::textGrob(paste("Variable:", input$y_vars[i]), 
                                   gp = grid::gpar(fontsize = 16, fontface = "bold")),
              newpage = TRUE
            )
          }
        }
        
        # Close PDF device
        dev.off()
        
        # Copy the temporary file to the final destination
        file.copy(temp_file, file, overwrite = TRUE)
        
      }, error = function(e) {
        # If PDF creation fails, try alternative approach 
        if (dev.cur() != 1) dev.off()  # Close any open devices
        
        # Alternative method
        pdf(temp_file, width = input$pdf_width, height = input$pdf_height, onefile = TRUE)
        
        # Title page
        par(mar = c(0, 0, 0, 0))
        plot.new()
        title(main = "HWInfo64 Log Comparison Report", cex.main = 2.5, line = -2)
        mtext(paste("Generated:", Sys.Date()), side = 1, line = -8, cex = 1.4)
        mtext(paste("Conditions:", input$label1, "vs", input$label2), side = 1, line = -6, cex = 1.6)
        
        # Plot each variable
        for (i in seq_along(export_plots)) {
          if (input$export_plot_style == "combined") {
            print(export_plots[[i]]$combined)
          } else {
            gridExtra::grid.arrange(
              export_plots[[i]]$left, 
              export_plots[[i]]$right, 
              ncol = 2,
              top = paste("Variable:", input$y_vars[i]),
              newpage = TRUE
            )
          }
        }
        
        dev.off()
        file.copy(temp_file, file, overwrite = TRUE)
        
      }, finally = {
        # Clean up temporary file
        if (file.exists(temp_file)) {
          unlink(temp_file)
        }
      })
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui = ui, server = server)