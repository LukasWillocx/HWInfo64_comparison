# Custom functions for HWInfo64 Log Comparison App

#' Load and clean CSV file
#' 
#' Loads a CSV file, handles encoding issues, removes footer rows, and deduplicates columns
#' 
#' @param file File input object from Shiny fileInput
#' @return Cleaned data frame or NULL if loading fails


load_csv <- function(file) {
  if (is.null(file)) return(NULL)
  
  df <- tryCatch({
    read.csv(file$datapath, fileEncoding = "latin1", stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    message("CSV read error: ", e$message)
    return(NULL)
  })
  
  if (nrow(df) < 3) return(NULL)
  
  # Remove footer rows (last 2 rows typically contain summary info)
  df <- df[1:(nrow(df)-2), , drop = FALSE]
  
  # Remove duplicate columns
  df <- df[ , !duplicated(names(df))]
  
  return(df)
}

#' Parse time column with multiple format attempts
#' 
#' Attempts to parse time data using various datetime formats
#' 
#' @param col_data Vector of time data to parse
#' @return Parsed time vector (POSIXct or numeric)

parse_time_column <- function(col_data) {
  # Try different datetime formats
  parsed_time <- tryCatch({
    # First try ISO format with timezone
    as.POSIXct(col_data, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }, error = function(e) {
    tryCatch({
      # Try without timezone info
      as.POSIXct(col_data, format = "%Y-%m-%d %H:%M:%S")
    }, error = function(e2) {
      tryCatch({
        # Try lubridate's flexible parsing
        ymd_hms(col_data, quiet = TRUE)
      }, error = function(e3) {
        # If all else fails, try to parse as numeric (seconds since epoch)
        as.numeric(col_data)
      })
    })
  })
  parsed_time<<-parsed_time
  return(parsed_time)
  
}

#' Create relative time in seconds from start
#' 
#' Converts time data to relative seconds from the first timestamp
#' 
#' @param time_col Parsed time column (POSIXct or numeric)
#' @return Numeric vector of relative time in seconds
create_relative_time <- function(time_col) {
  if (inherits(time_col, "POSIXct")) {
    # Convert to seconds from the first timestamp
    time_series<-as.numeric(difftime(time_col, min(time_col, na.rm = TRUE), units = "secs"))
  } else if (is.numeric(time_col)) {
    # If already numeric, assume it's in appropriate units
    time_series<-time_col - min(time_col, na.rm = TRUE)
  } else {
    # Fallback: create sequence
    time_series<-seq_along(time_col) - 1
  }
  return(time_series)
}

#' Create plots for comparison
#' 
#' Creates either combined overlay plots or separate side-by-side plots
#' 
#' @param plot_style Character: "combined" or "separate"
#' @param safe_data List containing cleaned df1 and df2
#' @param time_var Character: name of time variable
#' @param y_vars Character vector: names of Y variables to plot
#' @param label1 Character: label for condition 1
#' @param label2 Character: label for condition 2
#' @return List of plot objects

create_plots <- function(plot_style, safe_data, time_var, y_vars, label1, label2) {
  time_var <- "time_relative"  # Use processed relative time
  d1 <- safe_data$df1
  d2 <- safe_data$df2
  
  lapply(y_vars, function(var) {
    # Calculate shared axis limits
    y1_vals <- d1[[var]][!is.na(d1[[var]])]
    y2_vals <- d2[[var]][!is.na(d2[[var]])]
    x1_vals <- d1[[time_var]][!is.na(d1[[time_var]])]
    x2_vals <- d2[[time_var]][!is.na(d2[[time_var]])]
    
    # Y-axis: minimum at 0, maximum from both datasets
    y_max <- max(c(y1_vals, y2_vals), na.rm = TRUE)
    y_limits <- c(0, y_max * 1.05)  # Add 5% padding at top
    
    # X-axis: shared range from both datasets (now in seconds)
    x_min <- min(c(x1_vals, x2_vals), na.rm = TRUE)
    x_max <- max(c(x1_vals, x2_vals), na.rm = TRUE)
    x_limits <- c(x_min, x_max)
    
    # Create custom breaks and labels for x-axis
    x_breaks <- pretty(x_limits, n = 8)
    x_labels <- round(x_breaks, 1)
    
    if (plot_style == "combined") {
      # Create combined data frame with safe column names
      combined_data <- rbind(
        data.frame(
          time_x = d1[[time_var]], 
          value_y = d1[[var]], 
          Condition = label1, 
          stringsAsFactors = FALSE
        ),
        data.frame(
          time_x = d2[[time_var]], 
          value_y = d2[[var]], 
          Condition = label2, 
          stringsAsFactors = FALSE
        )
      )
      
      # Remove NA values
      combined_data <- combined_data[!is.na(combined_data$time_x) & !is.na(combined_data$value_y), ]
      
      combined_plot <- ggplot(combined_data, aes(x = time_x, y = value_y, color = Condition)) +
        geom_line(size = 0.5) +
        geom_point(size = 1, alpha = 0.5) +
        labs(title = paste0(var, " — ", label1, " vs ", label2), 
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks, labels = x_labels) +
        scale_color_manual(values = c("#008080", "#FF7F50")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              panel.grid.minor = element_blank(),
              legend.title = element_blank(),
              text = element_text(family = "sans"),
              legend.position = "bottom")
      list(combined = combined_plot)
      
    } else {
      # Create separate plots
      p1 <- ggplot(d1, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = "#008080", size = 0.5) +
        geom_point(color = "#008080", size = 1, alpha = 0.5) +
        labs(title = paste0(var, " — ", label1), 
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks, labels = x_labels) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              panel.grid.minor = element_blank(),
              text = element_text(family = "sans"))
      
      p2 <- ggplot(d2, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = "#FF7F50", size = 0.5) +
        geom_point(color = "#FF7F50", size = 1, alpha = 0.5) +
        labs(title = paste0(var, " — ", label2), 
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = x_breaks, labels = x_labels) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              panel.grid.minor = element_blank(),
              text = element_text(family = "sans"))
      
      list(left = p1, right = p2)
    }
  })
}