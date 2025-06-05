#' Load and clean CSV file
#' 
#' Loads a CSV file, handles encoding issues, removes footer rows, deduplicates columns,
#' and converts "no"/"yes" values to 0/1
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
  
  if (is.null(df) || nrow(df) < 3) return(NULL)
  
  # Remove footer rows (last 2 rows typically contain summary info)
  df <- df[1:(nrow(df)-2), , drop = FALSE]
  
  # Remove duplicate columns
  df <- df[ , !duplicated(names(df))]
  
  # Convert "no"/"yes" to 0/1 in all character columns
  char_cols <- sapply(df, is.character)
  for (col in names(df)[char_cols]) {
    # Convert "no" to 0 and "yes" to 1 (case insensitive)
    df[[col]] <- ifelse(tolower(trimws(df[[col]])) == "no", 0,
                        ifelse(tolower(trimws(df[[col]])) == "yes", 1, df[[col]]))
    
    # Try to convert to numeric if possible
    num_version <- suppressWarnings(as.numeric(df[[col]]))
    if (!all(is.na(num_version))) {
      df[[col]] <- num_version
    }
  }
  
  return(df)
}

#' Parse time column with multiple format attempts and high precision
#' 
#' Attempts to parse time data using various datetime formats, preserving sub-second precision
#' 
#' @param col_data Vector of time data to parse
#' @return Parsed time vector (POSIXct with high precision or numeric)

parse_time_column <- function(col_data) {
  # First, try to detect if this looks like high-precision numeric data
  if (all(grepl("^[0-9.]+$", col_data[!is.na(col_data)][1:min(10, length(col_data))]))) {
    # If it's numeric, preserve full precision
    return(as.numeric(col_data))
  }
  
  # Try different datetime formats with microsecond precision
  parsed_time <- tryCatch({
    # Try ISO format with microseconds
    as.POSIXct(col_data, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  }, error = function(e) {
    tryCatch({
      # Try without timezone info but with microseconds
      as.POSIXct(col_data, format = "%Y-%m-%d %H:%M:%OS")
    }, error = function(e2) {
      tryCatch({
        # Try lubridate's flexible parsing
        ymd_hms(col_data, quiet = TRUE)
      }, error = function(e3) {
        tryCatch({
          # Try parsing as numeric seconds since epoch with full precision
          as.numeric(col_data)
        }, error = function(e4) {
          # If all else fails, create a sequence
          seq_along(col_data) - 1
        })
      })
    })
  })
  
  return(parsed_time)
}

#' Create relative time with high precision (preserving sub-second intervals)
#' 
#' Converts time data to relative seconds from the first timestamp with full decimal precision
#' 
#' @param time_col Parsed time column (POSIXct or numeric)
#' @return Numeric vector of relative time in seconds with decimal precision
create_relative_time <- function(time_col) {
  if (inherits(time_col, "POSIXct")) {
    # Convert to seconds from the first timestamp with full precision
    time_series <- as.numeric(difftime(time_col, min(time_col, na.rm = TRUE), units = "secs"))
  } else if (is.numeric(time_col)) {
    # If already numeric, preserve full precision
    time_series <- time_col - min(time_col, na.rm = TRUE)
  } else {
    # Fallback: create sequence
    time_series <- seq_along(time_col) - 1
  }
  
  return(time_series)
}

#' Create clean time axis breaks and labels
#' 
#' Creates appropriate time breaks with clean, round labels
#' 
#' @param time_values Numeric vector of time values in seconds
#' @param n_breaks Number of desired breaks (default: 8)
#' @return List with breaks and formatted labels
create_time_axis <- function(time_values, n_breaks = 8) {
  time_range <- range(time_values, na.rm = TRUE)
  breaks <- pretty(time_range, n = n_breaks)
  
  # Clean, simple labels - just round numbers
  labels <- round(breaks, 1)
  
  return(list(breaks = breaks, labels = labels))
}

#' Detect sampling interval from time series
#' 
#' Estimates the sampling interval to help with axis formatting
#' 
#' @param time_values Numeric vector of time values
#' @return Estimated sampling interval in seconds
detect_sampling_interval <- function(time_values) {
  if (length(time_values) < 2) return(1.0)
  
  # Calculate differences between consecutive time points
  diffs <- diff(time_values[!is.na(time_values)])
  diffs <- diffs[diffs > 0]  # Remove zero or negative differences
  
  if (length(diffs) == 0) return(1.0)
  
  # Use median to be robust against outliers
  interval <- median(diffs, na.rm = TRUE)
  
  return(interval)
}

#' Create plots for comparison with improved time handling
#' 
#' Creates either combined overlay plots or separate side-by-side plots with high-precision time axis
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
    
    # Calculate averages for display
    avg1 <- mean(y1_vals, na.rm = TRUE)
    avg2 <- mean(y2_vals, na.rm = TRUE)
    
    # X-axis: shared range from both datasets with full precision
    x_min <- min(c(x1_vals, x2_vals), na.rm = TRUE)
    x_max <- max(c(x1_vals, x2_vals), na.rm = TRUE)
    y_max <- max(c(y1_vals, y2_vals), na.rm = TRUE)
    x_limits <- c(x_min, x_max)
    y_limits <- c(0, y_max * 1.05)
    
    # Detect sampling characteristics
    all_time_vals <- c(x1_vals, x2_vals)
    sampling_interval <- detect_sampling_interval(all_time_vals)
    
    # Create clean time axis
    time_axis <- create_time_axis(all_time_vals, n_breaks = 8)
    
    # Add sampling interval info if it's high frequency
    sampling_info <- ""
    if (sampling_interval < 1.0 && sampling_interval >= 0.1) {
      sampling_info <- sprintf(" (≈%.1fs intervals)", sampling_interval)
    } else if (sampling_interval < 0.1) {
      sampling_info <- sprintf(" (≈%.0fms intervals)", sampling_interval * 1000)
    }
    
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
        geom_line(size = 0.5, alpha = 0.8) +
        geom_point(size = 0.8, alpha = 0.4) +
        # Add subtle average lines
        geom_hline(yintercept = avg1, color = "#008080", linetype = "dashed", 
                   alpha = 0.6, size = 0.5) +
        geom_hline(yintercept = avg2, color = "#FF7F50", linetype = "dashed", 
                   alpha = 0.6, size = 0.5) +
        geom_label(data = data.frame(x=x_max*0.95,y=avg1,label=round(avg1,2)),
                  aes(x=x,y=y,label=label),
                  vjust=-0.2,
                  hjust=0,
                  color="#008080")+
        geom_label(data = data.frame(x=x_max*0.87,y=avg2,label=round(avg2,2)),
                  aes(x=x,y=y,label=label),
                  vjust=-0.2,
                  hjust=0,
                  color="#FF7F50")+
        labs(title = paste0(var, " — ", label1, " vs ", label2), 
             subtitle = if(sampling_info != "") paste0(sampling_info) else NULL,
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, 
                           breaks = time_axis$breaks, 
                           labels = time_axis$labels) +
        scale_color_manual(values = c("#008080", "#FF7F50")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"),
              plot.subtitle = element_text(color = "gray60"),
              panel.grid.minor = element_blank(),
              legend.title = element_blank(),
              text = element_text(family = "sans"),
              legend.position = "bottom")
      
      list(combined = combined_plot)
      
    } else {
      # Create separate plots with consistent high-precision time axis
      p1 <- ggplot(d1, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = "#008080", size = 0.5, alpha = 0.8) +
        geom_point(color = "#008080", size = 0.8, alpha = 0.4) +
        # Add subtle average line
        geom_hline(yintercept = avg1, color = "#008080", linetype = "dashed", 
                   alpha = 0.6, size = 0.5) +
        geom_label(data = data.frame(x=x_max*0.96,y=avg1,label=round(avg1,2)),
                  aes(x=x,y=y,label=label),
                  vjust=-0.2,
                  color="#008080")+
        labs(title = paste0(var, " — ", label1), 
             subtitle = if(sampling_info != "") paste0(sampling_info) else NULL,
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, 
                           breaks = time_axis$breaks, 
                           labels = time_axis$labels) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "gray60"),
              panel.grid.minor = element_blank(),
              text = element_text(family = "sans"))
      
      p2 <- ggplot(d2, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = "#FF7F50", size = 0.5, alpha = 0.8) +
        geom_point(color = "#FF7F50", size = 0.8, alpha = 0.4) +
        # Add subtle average line
        geom_hline(yintercept = avg2, color = "#FF7F50", linetype = "dashed", 
                   alpha = 0.6, size = 0.5) +
        geom_label(data = data.frame(x=x_max*0.96,y=avg2,label=round(avg2,2)),
                                    aes(x=x,y=y,label=label),
                                    vjust=-0.2,
                                    color="#FF7F50")+
        labs(title = paste0(var, " — ", label2), 
             subtitle = if(sampling_info != "") paste0(sampling_info) else NULL,
             x = "Time (seconds)", 
             y = var) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, 
                           breaks = time_axis$breaks, 
                           labels = time_axis$labels) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10, color = "gray60"),
              panel.grid.minor = element_blank(),
              text = element_text(family = "sans"))
      
      list(left = p1, right = p2)
    }
  })
}