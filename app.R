library(shiny)
library(shinydashboard) # layout 
library(ggplot2) # plotting
library(gridExtra)
library(lubridate)  # datetime handling

# Load custom functions
source("app_functions.R")

# Function to get all CSV files from www folder and create friendly names
get_demo_files <- function() {
  www_path <- "www"
  if (!dir.exists(www_path)) {
    return(list())
  }
  
  csv_files <- list.files(www_path, pattern = "\\.csv$", full.names = FALSE)
  
  if (length(csv_files) == 0) {
    return(list())
  }
  
  # Create friendly names by removing prefixes and file extensions
  friendly_names <- gsub("^demo_", "", csv_files)
  friendly_names <- gsub("\\.csv$", "", friendly_names)
  friendly_names <- gsub("_", " ", friendly_names)
  friendly_names <- tools::toTitleCase(friendly_names)
  
  # Create named list for selectInput
  demo_choices <- as.list(csv_files)
  names(demo_choices) <- friendly_names
  
  return(demo_choices)
}

ui <- dashboardPage(
  dashboardHeader(title = "CSV comparison tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    includeCSS("app_styles.css"),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload CSV Files", status = "primary", solidHeader = TRUE, width = 12,
                    # Add option to choose between upload and test files
                    fluidRow(
                      column(12,
                             radioButtons("data_source", "Data Source:",
                                          choices = list(
                                            "Upload your own CSV files" = "upload",
                                            "Use demo test files" = "demo"
                                          ),
                                          selected = "demo"),
                             hr()
                      )
                    ),
                    
                    # Conditional panels based on data source choice
                    conditionalPanel(
                      condition = "input.data_source == 'upload'",
                      fluidRow(
                        column(6,
                               fileInput("csv1", "Upload CSV - Condition 1", accept = ".csv"),
                               textInput("label1", "Label for Condition 1", value = "Condition 1", placeholder = "e.g., Baseline")
                        ),
                        column(6,
                               fileInput("csv2", "Upload CSV - Condition 2", accept = ".csv"),
                               textInput("label2", "Label for Condition 2", value = "Condition 2", placeholder = "e.g., Modified")
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.data_source == 'demo'",
                      fluidRow(
                        column(6,
                               h4("Condition 1"),
                               uiOutput("demo_csv1_selector"),
                               textInput("demo_label1", "Label for Condition 1", value = "Condition 1", placeholder = "e.g., Baseline")
                        ),
                        column(6,
                               h4("Condition 2"),
                               uiOutput("demo_csv2_selector"),
                               textInput("demo_label2", "Label for Condition 2", value = "Condition 2", placeholder = "e.g., Modified")
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 15px; padding: 10px; background-color: #f0f8ff; border-left: 4px solid #2196F3; border-radius: 4px;",
                                   h5(style = "margin-top: 0; color: #1976D2;", icon("info-circle"), " Available Demo Files"),
                                   uiOutput("demo_files_info")
                               )
                        )
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
      
      tabItem(tabName = "export",
              conditionalPanel(
                condition = "output.plots_available",
                fluidRow(
                  box(title = "Export Format", status = "primary", solidHeader = TRUE, width = 12,
                      fluidRow(
                        column(6,
                               h4("Output Format"),
                               radioButtons("export_format", "Choose Export Format:",
                                            choices = list(
                                              "PDF Report" = "pdf",
                                              "PNG Images" = "png"
                                            ),
                                            selected = "pdf"),
                               br(),
                               h4("Plot Style for Export"),
                               radioButtons("export_plot_style", "Export Plot Style:",
                                            choices = list(
                                              "Side-by-side (Separate)" = "separate",
                                              "Combined (Overlay)" = "combined"
                                            ),
                                            selected = "separate")
                        ),
                        column(6,
                               # Conditional panels for format-specific options : PDF & PNG
                               conditionalPanel(
                                 condition = "input.export_format == 'pdf'",
                                 h4("PDF Settings"),
                                 numericInput("pdf_width", "Width (inches):", value = 14, min = 5, max = 20, step = 0.5),
                                 numericInput("pdf_height", "Height per plot (inches):", value = 5, min = 3, max = 10, step = 0.5)
                               ),
                               conditionalPanel(
                                 condition = "input.export_format == 'png'",
                                 h4("PNG Settings"),
                                 numericInput("png_width", "Width (pixels):", value = 1920, min = 800, max = 4000, step = 100),
                                 numericInput("png_height", "Height per plot (pixels):", value = 1080, min = 400, max = 3000, step = 50),
                                 numericInput("png_dpi", "DPI (resolution):", value = 300, min = 72, max = 600, step = 50)
                               )
                        )
                      )
                  )
                ),
                fluidRow(
                  box(title = "Download", status = "success", solidHeader = TRUE, width = 6,
                      conditionalPanel(
                        condition = "input.export_format == 'pdf'",
                        downloadButton("download_pdf", "Download PDF Report", class = "btn-success", icon = icon("file-pdf"))
                      ),
                      conditionalPanel(
                        condition = "input.export_format == 'png'",
                        downloadButton("download_png", "Download PNG Images", class = "btn-success", icon = icon("image"))
                      )
                  ),
                  
                  box(title = "Export Preview", status = "info", solidHeader = TRUE, width = 6,
                      conditionalPanel(
                        condition = "input.export_format == 'pdf'",
                        h4("PDF Report will include:"),
                        tags$ul(
                          tags$li("Title page with metadata"),
                          tags$li("Comparison plots in selected style"),
                          tags$li("Condition labels and averages"),
                          tags$li("High-resolution plots"),
                          tags$li("All selected variables")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.export_format == 'png'",
                        h4("PNG Export will include:"),
                        tags$ul(
                          tags$li("Individual PNG files for each variable"),
                          tags$li("High-resolution images at specified DPI"),
                          tags$li("Files bundled in a ZIP archive"),
                          tags$li("Plots in selected style (separate or combined)")
                        ),
                        br(),
                        h5("File naming:"),
                        tags$ul(
                          tags$li(strong("Combined plots:"), " variable_name_combined.png"),
                          tags$li(strong("Separate plots:"), " variable_name_condition1.png, variable_name_condition2.png")
                        )
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
  # Get available demo files
  demo_files <- get_demo_files()
  
  # Reactive values to track state / progress (CSV submission & plotting)
  values <- reactiveValues(
    setup_confirmed = FALSE,
    plots_generated = FALSE
  )
  
  # Render demo file selectors
  output$demo_csv1_selector <- renderUI({
    if (length(demo_files) == 0) {
      div(
        style = "color: #d9534f; padding: 10px; border: 1px solid #d9534f; border-radius: 4px;",
        icon("exclamation-triangle"),
        " No demo CSV files found in www folder"
      )
    } else {
      selectInput("demo_csv1", "Choose Demo CSV File:", 
                  choices = demo_files,
                  selected = demo_files[[1]])
    }
  })
  
  output$demo_csv2_selector <- renderUI({
    if (length(demo_files) == 0) {
      div(
        style = "color: #d9534f; padding: 10px; border: 1px solid #d9534f; border-radius: 4px;",
        icon("exclamation-triangle"),
        " No demo CSV files found in www folder"
      )
    } else {
      # Set default to second file if available, otherwise first file
      default_selection <- if(length(demo_files) > 1) demo_files[[2]] else demo_files[[1]]
      
      selectInput("demo_csv2", "Choose Demo CSV File:", 
                  choices = demo_files,
                  selected = default_selection)
    }
  })
  
  # Display information about available demo files
  output$demo_files_info <- renderUI({
    if (length(demo_files) == 0) {
      p("No demo files available. Please add CSV files to the www folder.")
    } else {
      div(
        p(paste("Found", length(demo_files), "demo files:")),
        tags$ul(
          lapply(names(demo_files), function(name) {
            tags$li(strong(name), " (", demo_files[[name]], ")")
          })
        ),
        p(style = "margin-bottom: 0; font-size: 0.9em; color: #666;", 
          "Select any two files to compare their data.")
      )
    }
  })
  
  # Update labels when demo files change
  observe({
    if (input$data_source == "demo" && !is.null(input$demo_csv1)) {
      # Extract friendly name for auto-labeling
      friendly_name1 <- names(demo_files)[demo_files == input$demo_csv1]
      if (length(friendly_name1) > 0) {
        updateTextInput(session, "demo_label1", value = friendly_name1)
      }
    }
  })
  
  observe({
    if (input$data_source == "demo" && !is.null(input$demo_csv2)) {
      # Extract friendly name for auto-labeling
      friendly_name2 <- names(demo_files)[demo_files == input$demo_csv2]
      if (length(friendly_name2) > 0) {
        updateTextInput(session, "demo_label2", value = friendly_name2)
      }
    }
  })
  
  # Load CSV files based on data source
  df1 <- reactive({
    if (input$data_source == "upload") {
      load_csv(input$csv1)
    } else {
      # Load demo file
      if (!is.null(input$demo_csv1) && length(demo_files) > 0) {
        demo_file <- list(datapath = file.path("www", input$demo_csv1))
        load_csv(demo_file)
      } else {
        NULL
      }
    }
  })
  
  df2 <- reactive({
    if (input$data_source == "upload") {
      load_csv(input$csv2)
    } else {
      # Load demo file
      if (!is.null(input$demo_csv2) && length(demo_files) > 0) {
        demo_file <- list(datapath = file.path("www", input$demo_csv2))
        load_csv(demo_file)
      } else {
        NULL
      }
    }
  })
  
  # Get labels based on data source
  current_label1 <- reactive({
    if (input$data_source == "upload") {
      input$label1
    } else {
      input$demo_label1
    }
  })
  
  current_label2 <- reactive({
    if (input$data_source == "upload") {
      input$label2
    } else {
      input$demo_label2
    }
  })
  
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
    div(
      selectInput("y_vars", "Y-Axis Variable(s) to Compare", 
                  choices = as.list(y_choices), 
                  multiple = TRUE, 
                  width = "100%"),
      tags$small(class = "text-muted", 
                 style = "color: #666; font-style: italic;",
                 "Maximum 4 variables can be selected for comparison")
    )
  })
  
  # Add validation for maximum 4 variables
  observe({
    if (!is.null(input$y_vars) && length(input$y_vars) > 4) {
      # Keep only the first 4 selections
      updateSelectInput(session, "y_vars", 
                        selected = input$y_vars[1:4])
      showNotification("Maximum 4 variables allowed. Selection limited to first 4 variables.", 
                       type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$confirm_setup, {
    req(input$time_var, input$y_vars)
    
    # Additional validation check
    if (length(input$y_vars) > 4) {
      showNotification("Please select a maximum of 4 variables for comparison.", 
                       type = "error", duration = 5)
      return()
    }
    
    values$setup_confirmed <- TRUE
    showNotification("Setup confirmed! You can now proceed to visualization.", type = "message")
  })
  
  safe_data <- reactive({
    req(df1(), df2(), input$time_var, input$y_vars)
    
    df1_clean <- df1()
    df2_clean <- df2()
    
    # Handle time variable - parse and convert to relative seconds
    time_col1 <- parse_time_column(df1_clean[[input$time_var]])
    time_col2 <- parse_time_column(df2_clean[[input$time_var]])
    
    df1_clean$time_relative <- create_relative_time(time_col1)
    df2_clean$time_relative <- create_relative_time(time_col2)
    
    # Clean Y variables (numeric values comma decimals to points)
    for (var in input$y_vars) {
      df1_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df1_clean[[var]])))
      df2_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df2_clean[[var]])))
    }
    
    list(df1 = df1_clean, df2 = df2_clean)
  })
  
  # Function to create plots for a given style (side-by-side vs combined)
  create_plots_wrapper <- function(plot_style) {
    req(safe_data(), input$time_var, input$y_vars, values$setup_confirmed)
    create_plots(plot_style, safe_data(), input$time_var, input$y_vars, current_label1(), current_label2())
  }
  
  plot_pairs <- reactive({
    create_plots_wrapper(input$plot_style)
  })
  
  observeEvent(input$generate_plots, {
    req(plot_pairs())
    values$plots_generated <- TRUE
    showNotification("Plots generated successfully!", type = "message")
  })
  
  output$plot_panels <- renderUI({
    req(plot_pairs(), values$plots_generated)
    
    if (input$plot_style == "combined") {
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
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plot_combined_", idx)]] <- renderPlot({ plots[[idx]]$combined })
        })
      }
    } else {
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plot_left_", idx)]] <- renderPlot({ plots[[idx]]$left })
          output[[paste0("plot_right_", idx)]] <- renderPlot({ plots[[idx]]$right })
        })
      }
    }
  })
  
  # show which variables are on display
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
  
  # PDF download handler (existing)
  output$download_pdf <- downloadHandler(
    filename = function() paste0("csv_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      # Use the export plot style preference
      export_plots <- create_plots(input$export_plot_style, safe_data(), input$time_var, input$y_vars, current_label1(), current_label2())
      req(export_plots, values$plots_generated)
      
      temp_file <- tempfile(fileext = ".pdf")
      
      tryCatch({
        # Open PDF device
        pdf(temp_file, width = input$pdf_width, height = input$pdf_height, onefile = TRUE)
        
        # Title page generation using base R graphics
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.8, "CSV Comparison Report", 
             cex = 2.5, font = 2, adj = 0.5)
        text(0.5, 0.65, paste("Generated on:", Sys.Date()), 
             cex = 1.4, adj = 0.5)
        text(0.5, 0.55, paste("Conditions:", current_label1(), "vs", current_label2()), 
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
        title(main = "CSV Comparison Report", cex.main = 2.5, line = -2)
        mtext(paste("Generated:", Sys.Date()), side = 1, line = -8, cex = 1.4)
        mtext(paste("Conditions:", current_label1(), "vs", current_label2()), side = 1, line = -6, cex = 1.6)
        
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
  
  # PNG download handler
  output$download_png <- downloadHandler(
    filename = function() paste0("CSV_plots_", Sys.Date(), ".zip"),
    content = function(file) {
      # Use the export plot style preference
      export_plots <- create_plots(input$export_plot_style, safe_data(), input$time_var, input$y_vars, current_label1(), current_label2())
      req(export_plots, values$plots_generated)
      
      # Create temporary directory for PNG files
      temp_dir <- tempdir()
      png_files <- c()
      
      tryCatch({
        # Create PNG files for each variable
        for (i in seq_along(export_plots)) {
          var_name <- make.names(input$y_vars[i])  # Clean variable name for filename
          
          if (input$export_plot_style == "combined") {
            # Combined plot - single PNG per variable
            png_file <- file.path(temp_dir, paste0(var_name, "_combined.png"))
            
            png(png_file, 
                width = input$png_width, 
                height = input$png_height, 
                res = input$png_dpi)
            
            print(export_plots[[i]]$combined)
            dev.off()
            
            png_files <- c(png_files, png_file)
            
          } else {
            # Side-by-side plots - two PNGs per variable
            png_file1 <- file.path(temp_dir, paste0(var_name, "_", make.names(current_label1()), ".png"))
            png_file2 <- file.path(temp_dir, paste0(var_name, "_", make.names(current_label2()), ".png"))
            
            # First condition plot
            png(png_file1, 
                width = input$png_width, 
                height = input$png_height, 
                res = input$png_dpi)
            
            print(export_plots[[i]]$left)
            dev.off()
            
            # Second condition plot
            png(png_file2, 
                width = input$png_width, 
                height = input$png_height, 
                res = input$png_dpi)
            
            print(export_plots[[i]]$right)
            dev.off()
            
            png_files <- c(png_files, png_file1, png_file2)
          }
        }
        
        # Create ZIP file
        zip_file <- tempfile(fileext = ".zip")
        
        # Change to temp directory and create zip
        old_wd <- getwd()
        setwd(temp_dir)
        
        # Get just the filenames for the zip
        zip_filenames <- basename(png_files)
        zip(zip_file, zip_filenames, flags = "-r9X")
        
        setwd(old_wd)
        
        # Copy the zip file to the final destination
        file.copy(zip_file, file, overwrite = TRUE)
        
      }, error = function(e) {
        # If there's an error, try to close any open devices
        if (dev.cur() != 1) dev.off()
        
        # Create a simple error file
        error_file <- tempfile(fileext = ".txt")
        writeLines(paste("Error creating PNG files:", e$message), error_file)
        file.copy(error_file, file, overwrite = TRUE)
        
      }, finally = {
        # Clean up temporary PNG files
        if (length(png_files) > 0) {
          file.remove(png_files[file.exists(png_files)])
        }
      })
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)