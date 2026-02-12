library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(gridExtra)
library(lubridate)
library(luwitemplate)

# Load custom functions
source("app_functions.R")

# --- UI ---
ui <- page_sidebar(
  title = "CSV Comparison Tool",
  theme = my_theme(),
  
  # Vertical navigation sidebar
  
  sidebar = sidebar(
    title = "Navigation",
    open = "always",
    navset_pill_list(
      id = "sidebar_nav",
      nav_panel("Data Upload", value = "upload", icon = icon("upload")),
      nav_panel("Visualization", value = "viz", icon = icon("chart-line")),
      nav_panel("Export", value = "export", icon = icon("download"))
    )
  ),
  
  # --- Main content area ---
  navset_hidden(
    id = "main_content",
    
    # ===================== TAB 1: DATA UPLOAD =====================
    nav_panel_hidden(
      value = "upload",
      layout_columns(
        col_widths = c(7, 5),
        
        # Left column: data source + file selection
        card(
          card_header("Data Source"),
          card_body(
            radioButtons("data_source", NULL,
                         choices = list(
                           "Upload your own CSV files" = "upload",
                           "Use demo test files" = "demo"
                         ),
                         selected = "demo", inline = TRUE
            ),
            hr(),
            
            # Upload mode
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              layout_columns(
                col_widths = c(6, 6),
                div(
                  fileInput("csv1", "CSV - Condition 1", accept = ".csv"),
                  textInput("label1", "Label",
                            value = "Condition 1", placeholder = "e.g., Baseline"
                  )
                ),
                div(
                  fileInput("csv2", "CSV - Condition 2", accept = ".csv"),
                  textInput("label2", "Label",
                            value = "Condition 2", placeholder = "e.g., Modified"
                  )
                )
              )
            ),
            
            # Demo mode
            conditionalPanel(
              condition = "input.data_source == 'demo'",
              layout_columns(
                col_widths = c(6, 6),
                div(
                  uiOutput("demo_csv1_selector"),
                  textInput("demo_label1", "Label",
                            value = "Condition 1", placeholder = "e.g., Baseline"
                  )
                ),
                div(
                  uiOutput("demo_csv2_selector"),
                  textInput("demo_label2", "Label",
                            value = "Condition 2", placeholder = "e.g., Modified"
                  )
                )
              ),
              tags$details(class = "text-muted mt-1",
                           tags$summary(icon("info-circle"), "Demo files info"),
                           uiOutput("demo_files_info")
              )
            )
          )
        ),
        
        # Right column: variable selection
        card(
          card_header("Variable Selection"),
          card_body(
            conditionalPanel(
              condition = "output.files_uploaded",
              uiOutput("time_selector"),
              uiOutput("y_selector"),
              actionButton("confirm_setup", "Confirm Setup",
                           class = "btn-success mt-2 w-100", icon = icon("check")
              )
            ),
            conditionalPanel(
              condition = "!output.files_uploaded",
              p(class = "text-muted fst-italic", "Load data files to select variables.")
            )
          )
        )
      )
    ),
    
    # ===================== TAB 2: VISUALIZATION =====================
    nav_panel_hidden(
      value = "viz",
      
      # When setup is confirmed
      conditionalPanel(
        condition = "output.setup_confirmed",
        layout_columns(
          col_widths = c(4, 8),
          row_heights = "auto",
          
          # Left: config card
          card(
            card_header("Plot Configuration"),
            card_body(
              h4("Selected Variables:"),
              verbatimTextOutput("selected_vars"),
              br(),
              h4("Plot Display Options:"),
              radioButtons("plot_style", "Plot Style:",
                           choices = list(
                             "Side-by-side (Separate)" = "separate",
                             "Combined (Overlay)" = "combined"
                           ),
                           selected = "separate"
              ),
              br(),
              actionButton("generate_plots", "Generate Plots",
                           class = "btn-primary", icon = icon("chart-bar")
              )
            )
          ),
          
          # Right: plot output with variable tabs
          card(
            card_header("Plot Output"),
            card_body(
              conditionalPanel(
                condition = "output.plots_available",
                uiOutput("plot_tabs")
              )
            )
          )
        )
      ),
      
      # When setup is NOT confirmed
      conditionalPanel(
        condition = "!output.setup_confirmed",
        card(
          card_header("Setup Required"),
          card_body(
            h4("Please upload and configure your data files in the Data Upload tab first.")
          )
        )
      )
    ),
    
    # ===================== TAB 3: EXPORT =====================
    nav_panel_hidden(
      value = "export",
      
      # When plots are available
      conditionalPanel(
        condition = "output.plots_available",
        layout_columns(
          col_widths = 12,
          
          # Export format card
          card(
            card_header("Export Format"),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
                div(
                  h4("Output Format"),
                  radioButtons("export_format", "Choose Export Format:",
                               choices = list("PDF Report" = "pdf", "PNG Images" = "png"),
                               selected = "pdf"
                  ),
                  br(),
                  h4("Plot Style for Export"),
                  radioButtons("export_plot_style", "Export Plot Style:",
                               choices = list(
                                 "Side-by-side (Separate)" = "separate",
                                 "Combined (Overlay)" = "combined"
                               ),
                               selected = "separate"
                  )
                ),
                div(
                  conditionalPanel(
                    condition = "input.export_format == 'pdf'",
                    h4("PDF Settings"),
                    numericInput("pdf_width", "Width (inches):", value = 16, min = 5, max = 20, step = 0.5),
                    numericInput("pdf_height", "Height per plot (inches):", value = 9, min = 3, max = 10, step = 0.5)
                  ),
                  conditionalPanel(
                    condition = "input.export_format == 'png'",
                    h4("PNG Settings"),
                    numericInput("png_width", "Width (pixels):", value = 1920, min = 800, max = 4000, step = 100),
                    numericInput("png_height", "Height per plot (pixels):", value = 1080, min = 400, max = 3000, step = 50),
                    numericInput("png_dpi", "DPI (resolution):", value = 200, min = 72, max = 600, step = 50)
                  )
                )
              )
            )
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          # Download card
          card(
            card_header("Download"),
            card_body(
              conditionalPanel(
                condition = "input.export_format == 'pdf'",
                downloadButton("download_pdf", "Download PDF Report",
                               class = "btn-success", icon = icon("file-pdf")
                )
              ),
              conditionalPanel(
                condition = "input.export_format == 'png'",
                downloadButton("download_png", "Download PNG Images",
                               class = "btn-success", icon = icon("image")
                )
              )
            )
          ),
          
          # Preview card
          card(
            card_header("Export Preview"),
            card_body(
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
        )
      ),
      
      # When plots are NOT available
      conditionalPanel(
        condition = "!output.plots_available",
        card(
          card_header("Setup Required"),
          card_body(
            h4("Please generate plots in the Visualization tab before exporting.")
          )
        )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # Sync sidebar nav with main content
  observeEvent(input$sidebar_nav, {
    nav_select("main_content", selected = input$sidebar_nav)
  })
  
  # Get available demo files
  demo_files <- get_demo_files()
  
  # Reactive state tracking
  values <- reactiveValues(
    setup_confirmed = FALSE,
    plots_generated = FALSE,
    data_source_changed = FALSE
  )
  
  # Reset state when data source changes
  observeEvent(input$data_source, {
    values$setup_confirmed <- FALSE
    values$plots_generated <- FALSE
    values$data_source_changed <- TRUE
    updateSelectInput(session, "time_var", selected = character(0))
    updateSelectInput(session, "y_vars", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Demo file selectors
  output$demo_csv1_selector <- renderUI({
    if (length(demo_files) == 0) {
      div(class = "text-danger p-2 border border-danger rounded",
          icon("exclamation-triangle"), " No demo CSV files found in www folder"
      )
    } else {
      selectInput("demo_csv1", "Choose Demo CSV File:",
                  choices = demo_files, selected = demo_files[[1]]
      )
    }
  })
  
  output$demo_csv2_selector <- renderUI({
    if (length(demo_files) == 0) {
      div(class = "text-danger p-2 border border-danger rounded",
          icon("exclamation-triangle"), " No demo CSV files found in www folder"
      )
    } else {
      default_selection <- if (length(demo_files) > 1) demo_files[[2]] else demo_files[[1]]
      selectInput("demo_csv2", "Choose Demo CSV File:",
                  choices = demo_files, selected = default_selection
      )
    }
  })
  
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
        p(class = "text-muted fst-italic mb-0",
          "Select any two files to compare their data."
        )
      )
    }
  })
  
  # Auto-label from demo filenames
  observe({
    if (input$data_source == "demo" && !is.null(input$demo_csv1)) {
      friendly_name1 <- names(demo_files)[demo_files == input$demo_csv1]
      if (length(friendly_name1) > 0) {
        updateTextInput(session, "demo_label1", value = friendly_name1)
      }
    }
  })
  
  observe({
    if (input$data_source == "demo" && !is.null(input$demo_csv2)) {
      friendly_name2 <- names(demo_files)[demo_files == input$demo_csv2]
      if (length(friendly_name2) > 0) {
        updateTextInput(session, "demo_label2", value = friendly_name2)
      }
    }
  })
  
  # Load CSV files
  df1 <- reactive({
    tryCatch({
      if (input$data_source == "upload") {
        if (is.null(input$csv1)) return(NULL)
        load_csv(input$csv1)
      } else {
        if (!is.null(input$demo_csv1) && length(demo_files) > 0) {
          demo_file <- list(datapath = file.path("www", input$demo_csv1))
          load_csv(demo_file)
        } else {
          NULL
        }
      }
    }, error = function(e) {
      showNotification(paste("Error loading CSV 1:", e$message), type = "error")
      return(NULL)
    })
  })
  
  df2 <- reactive({
    tryCatch({
      if (input$data_source == "upload") {
        if (is.null(input$csv2)) return(NULL)
        load_csv(input$csv2)
      } else {
        if (!is.null(input$demo_csv2) && length(demo_files) > 0) {
          demo_file <- list(datapath = file.path("www", input$demo_csv2))
          load_csv(demo_file)
        } else {
          NULL
        }
      }
    }, error = function(e) {
      showNotification(paste("Error loading CSV 2:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Labels
  current_label1 <- reactive({
    if (input$data_source == "upload") {
      if (is.null(input$label1) || input$label1 == "") "Condition 1" else input$label1
    } else {
      if (is.null(input$demo_label1) || input$demo_label1 == "") "Condition 1" else input$demo_label1
    }
  })
  
  current_label2 <- reactive({
    if (input$data_source == "upload") {
      if (is.null(input$label2) || input$label2 == "") "Condition 2" else input$label2
    } else {
      if (is.null(input$demo_label2) || input$demo_label2 == "") "Condition 2" else input$demo_label2
    }
  })
  
  # Shared columns
  shared_columns <- reactive({
    req(df1(), df2())
    tryCatch({
      intersect(names(df1()), names(df2()))
    }, error = function(e) {
      character(0)
    })
  })
  
  output$time_selector <- renderUI({
    columns <- shared_columns()
    if (length(columns) == 0) return(NULL)
    
    # Auto-detect time column by common names
    time_patterns <- c("datetime", "date/time", "date_time", "timestamp",
                       "time", "date", "Zeit", "Datum")
    auto_select <- NULL
    for (pat in time_patterns) {
      match <- columns[tolower(columns) == tolower(pat)]
      if (length(match) > 0) { auto_select <- match[1]; break }
    }
    
    selectInput("time_var", "Time Variable (X-axis)",
                choices = columns, selected = auto_select
    )
  })
  
  output$y_selector <- renderUI({
    req(shared_columns(), input$time_var, df1(), df2())
    y_choices <- setdiff(shared_columns(), input$time_var)
    
    # Filter to columns that are numeric in both datasets
    y_choices <- y_choices[sapply(y_choices, function(col) {
      v1 <- suppressWarnings(as.numeric(gsub(",", ".", df1()[[col]])))
      v2 <- suppressWarnings(as.numeric(gsub(",", ".", df2()[[col]])))
      sum(!is.na(v1)) > 0 && sum(!is.na(v2)) > 0
    })]
    
    if (length(y_choices) == 0) return(NULL)
    div(
      selectInput("y_vars", "Y-Axis Variable(s) to Compare",
                  choices = as.list(y_choices), multiple = TRUE,
                  selected = NULL, width = "100%"
      ),
      tags$small(class = "text-muted fst-italic",
                 "Maximum 4 variables can be selected for comparison"
      )
    )
  })
  
  # Max 4 variable validation
  observe({
    if (!is.null(input$y_vars) && length(input$y_vars) > 4) {
      updateSelectInput(session, "y_vars", selected = input$y_vars[1:4])
      showNotification(
        "Maximum 4 variables allowed. Selection limited to first 4 variables.",
        type = "warning", duration = 3
      )
    }
  })
  
  observeEvent(input$confirm_setup, {
    req(input$time_var, input$y_vars)
    if (length(input$y_vars) > 4) {
      showNotification("Please select a maximum of 4 variables for comparison.",
                       type = "error", duration = 5
      )
      return()
    }
    values$setup_confirmed <- TRUE
    values$plots_generated <- FALSE
    showNotification("Setup confirmed! You can now proceed to visualization.", type = "message")
  })
  
  # Data processing
  safe_data <- reactive({
    req(df1(), df2(), input$time_var, input$y_vars, values$setup_confirmed)
    tryCatch({
      df1_clean <- df1()
      df2_clean <- df2()
      
      if (!input$time_var %in% names(df1_clean) || !input$time_var %in% names(df2_clean)) {
        stop("Time variable not found in one or both datasets")
      }
      for (var in input$y_vars) {
        if (!var %in% names(df1_clean) || !var %in% names(df2_clean)) {
          stop(paste("Variable", var, "not found in one or both datasets"))
        }
      }
      
      time_col1 <- parse_time_column(df1_clean[[input$time_var]])
      time_col2 <- parse_time_column(df2_clean[[input$time_var]])
      df1_clean$time_relative <- create_relative_time(time_col1)
      df2_clean$time_relative <- create_relative_time(time_col2)
      
      for (var in input$y_vars) {
        df1_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df1_clean[[var]])))
        df2_clean[[var]] <- suppressWarnings(as.numeric(gsub(",", ".", df2_clean[[var]])))
      }
      
      list(df1 = df1_clean, df2 = df2_clean)
    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Plot creation wrapper
  create_plots_wrapper <- function(plot_style) {
    req(safe_data(), input$time_var, input$y_vars, values$setup_confirmed)
    tryCatch({
      create_plots(plot_style, safe_data(), input$time_var, input$y_vars,
                   current_label1(), current_label2()
      )
    }, error = function(e) {
      showNotification(paste("Error creating plots:", e$message), type = "error")
      return(NULL)
    })
  }
  
  plot_pairs <- reactive({
    create_plots_wrapper(input$plot_style)
  })
  
  observeEvent(input$generate_plots, {
    plots <- plot_pairs()
    if (!is.null(plots)) {
      values$plots_generated <- TRUE
      showNotification("Plots generated successfully!", type = "message")
    }
  })
  
  # Dynamic tabbed plot output
  output$plot_tabs <- renderUI({
    req(plot_pairs(), values$plots_generated)
    
    tabs <- lapply(seq_along(plot_pairs()), function(i) {
      var_name <- input$y_vars[i]
      if (input$plot_style == "combined") {
        nav_panel(
          var_name,
          plotlyOutput(paste0("plotly_combined_", i), height = "calc(100vh - 280px)")
        )
      } else {
        nav_panel(
          var_name,
          layout_columns(
            col_widths = c(6, 6),
            plotlyOutput(paste0("plotly_left_", i), height = "calc(100vh - 280px)"),
            plotlyOutput(paste0("plotly_right_", i), height = "calc(100vh - 280px)")
          )
        )
      }
    })
    
    do.call(navset_tab, tabs)
  })
  
  # Helper: build a plotly avg annotation with background + border
  make_avg_annotation <- function(avg, color, x_pos, yanchor = "bottom", label_prefix = "avg") {
    list(
      x = x_pos, y = avg,
      text = paste0("<b>", label_prefix, ": ", round(avg, 2), "</b>"),
      showarrow = FALSE,
      xref = "x", yref = "y",
      xanchor = "right", yanchor = yanchor,
      font = list(color = color, size = 12),
      bgcolor = "rgba(255,255,255,0.85)",
      bordercolor = color,
      borderwidth = 1.5,
      borderpad = 4
    )
  }
  
  # Render plotly outputs dynamically
  observe({
    plots <- plot_pairs()
    req(plots, values$plots_generated)
    
    if (input$plot_style == "combined") {
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plotly_combined_", idx)]] <- renderPlotly({
            p <- plots[[idx]]
            if (!is.null(p$combined)) {
              # Dodge: if averages are within 8% of y range, flip one below its line
              closeness <- abs(p$avg1 - p$avg2) / p$y_range
              anchor1 <- "bottom"
              anchor2 <- if (closeness < 0.08) "top" else "bottom"
              
              luwi_ggplotly(p$combined, tooltip = "text") %>%
                plotly::layout(
                  legend = list(y = -0.15, orientation = "h", xanchor = "center", x = 0.5),
                  margin = list(b = 80),
                  annotations = list(
                    make_avg_annotation(p$avg1, p$color1, p$x_max * 0.98, anchor1, p$label1),
                    make_avg_annotation(p$avg2, p$color2, p$x_max * 0.98, anchor2, p$label2)
                  )
                )
            }
          })
        })
      }
    } else {
      for (i in seq_along(plots)) {
        local({
          idx <- i
          output[[paste0("plotly_left_", idx)]] <- renderPlotly({
            p <- plots[[idx]]
            if (!is.null(p$left)) {
              luwi_ggplotly(p$left, tooltip = "text") %>%
                plotly::layout(
                  annotations = list(
                    make_avg_annotation(p$avg1, p$color1, p$x_max * 0.98)
                  )
                )
            }
          })
          output[[paste0("plotly_right_", idx)]] <- renderPlotly({
            p <- plots[[idx]]
            if (!is.null(p$right)) {
              luwi_ggplotly(p$right, tooltip = "text") %>%
                plotly::layout(
                  annotations = list(
                    make_avg_annotation(p$avg2, p$color2, p$x_max * 0.98)
                  )
                )
            }
          })
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
  
  # Conditional output flags
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
  
  # PDF download handler
  output$download_pdf <- downloadHandler(
    filename = function() paste0("csv_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      tryCatch({
        export_plots <- create_plots(input$export_plot_style, safe_data(),
                                     input$time_var, input$y_vars, current_label1(), current_label2(),
                                     for_export = TRUE
        )
        req(export_plots, values$plots_generated)
        
        temp_file <- tempfile(fileext = ".pdf")
        pdf(temp_file, width = input$pdf_width, height = input$pdf_height, onefile = TRUE)
        
        # Title page
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.8, "CSV Comparison Report", cex = 2.5, font = 2, adj = 0.5)
        text(0.5, 0.65, paste("Generated on:", Sys.Date()), cex = 1.4, adj = 0.5)
        text(0.5, 0.55, paste("Conditions:", current_label1(), "vs", current_label2()),
             cex = 1.6, font = 2, adj = 0.5
        )
        text(0.5, 0.45, paste("Variables compared:", length(input$y_vars)), cex = 1.2, adj = 0.5)
        text(0.5, 0.35, paste("Plot style:",
                              if (input$export_plot_style == "combined") "Combined overlay" else "Side-by-side"
        ), cex = 1, adj = 0.5)
        text(0.5, 0.25, paste("Variables:", paste(input$y_vars, collapse = ", ")),
             cex = 1, adj = 0.5
        )
        
        for (i in seq_along(export_plots)) {
          if (input$export_plot_style == "combined") {
            print(export_plots[[i]]$combined)
          } else {
            gridExtra::grid.arrange(
              export_plots[[i]]$left,
              export_plots[[i]]$right,
              ncol = 2,
              top = grid::textGrob(paste("Variable:", input$y_vars[i]),
                                   gp = grid::gpar(fontsize = 16, fontface = "bold")
              ),
              newpage = TRUE
            )
          }
        }
        
        dev.off()
        file.copy(temp_file, file, overwrite = TRUE)
      }, error = function(e) {
        if (dev.cur() != 1) dev.off()
        showNotification(paste("PDF generation error:", e$message), type = "error")
        error_file <- tempfile(fileext = ".txt")
        writeLines(paste("Error generating PDF:", e$message), error_file)
        file.copy(error_file, file, overwrite = TRUE)
      }, finally = {
        if (exists("temp_file") && file.exists(temp_file)) unlink(temp_file)
      })
    },
    contentType = "application/pdf"
  )
  
  # PNG download handler
  output$download_png <- downloadHandler(
    filename = function() paste0("CSV_plots_", Sys.Date(), ".zip"),
    content = function(file) {
      tryCatch({
        export_plots <- create_plots(input$export_plot_style, safe_data(),
                                     input$time_var, input$y_vars, current_label1(), current_label2(),
                                     for_export = TRUE
        )
        req(export_plots, values$plots_generated)
        
        temp_dir <- tempdir()
        png_files <- c()
        
        for (i in seq_along(export_plots)) {
          var_name <- make.names(input$y_vars[i])
          
          if (input$export_plot_style == "combined") {
            png_file <- file.path(temp_dir, paste0(var_name, "_combined.png"))
            png(png_file, width = input$png_width, height = input$png_height, res = input$png_dpi)
            print(export_plots[[i]]$combined)
            dev.off()
            png_files <- c(png_files, png_file)
          } else {
            png_file1 <- file.path(temp_dir, paste0(var_name, "_", make.names(current_label1()), ".png"))
            png_file2 <- file.path(temp_dir, paste0(var_name, "_", make.names(current_label2()), ".png"))
            
            png(png_file1, width = input$png_width, height = input$png_height, res = input$png_dpi)
            print(export_plots[[i]]$left)
            dev.off()
            
            png(png_file2, width = input$png_width, height = input$png_height, res = input$png_dpi)
            print(export_plots[[i]]$right)
            dev.off()
            
            png_files <- c(png_files, png_file1, png_file2)
          }
        }
        
        zip_file <- tempfile(fileext = ".zip")
        old_wd <- getwd()
        setwd(temp_dir)
        zip_filenames <- basename(png_files)
        zip(zip_file, zip_filenames, flags = "-r9X")
        setwd(old_wd)
        file.copy(zip_file, file, overwrite = TRUE)
      }, error = function(e) {
        if (dev.cur() != 1) dev.off()
        error_file <- tempfile(fileext = ".txt")
        writeLines(paste("Error creating PNG files:", e$message), error_file)
        file.copy(error_file, file, overwrite = TRUE)
      }, finally = {
        if (length(png_files) > 0) {
          file.remove(png_files[file.exists(png_files)])
        }
      })
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)