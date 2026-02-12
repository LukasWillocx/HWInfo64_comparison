#' Load and clean CSV file
#'
#' Loads a CSV file, handles encoding issues, deduplicates columns,
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
  
  # Remove duplicate columns
  df <- df[, !duplicated(names(df))]
  
  # Convert "no"/"yes" to 0/1 in all character columns
  char_cols <- sapply(df, is.character)
  for (col in names(df)[char_cols]) {
    df[[col]] <- ifelse(tolower(trimws(df[[col]])) == "no", 0,
                        ifelse(tolower(trimws(df[[col]])) == "yes", 1, df[[col]])
    )
    num_version <- suppressWarnings(as.numeric(df[[col]]))
    if (!all(is.na(num_version))) {
      df[[col]] <- num_version
    }
  }
  
  return(df)
}

#' Get all CSV files from www folder and create friendly names
get_demo_files <- function() {
  if (!dir.exists("www")) {
    return(list())
  }
  
  csv_files <- list.files(path = "www", pattern = "\\.csv$", full.names = FALSE, ignore.case = TRUE)
  
  if (length(csv_files) == 0) {
    return(list())
  }
  
  friendly_names <- gsub("^demo_", "", csv_files)
  friendly_names <- gsub("\\.csv$", "", friendly_names, ignore.case = TRUE)
  friendly_names <- gsub("_", " ", friendly_names)
  friendly_names <- tools::toTitleCase(friendly_names)
  
  demo_choices <- as.list(csv_files)
  names(demo_choices) <- friendly_names
  
  return(demo_choices)
}


#' Parse time column with multiple format attempts and high precision
#'
#' @param col_data Vector of time data to parse
#' @return Parsed time vector (POSIXct with high precision or numeric)

parse_time_column <- function(col_data) {
  # Pure numeric check
  sample <- col_data[!is.na(col_data)][1:min(10, length(col_data))]
  if (all(grepl("^[0-9.]+$", sample))) {
    return(as.numeric(col_data))
  }
  
  # Helper: try a format, return result only if mostly non-NA
  try_format <- function(data, fmt) {
    result <- suppressWarnings(as.POSIXct(data, format = fmt, tz = "UTC"))
    if (sum(!is.na(result)) > length(result) * 0.5) result else NULL
  }
  
  # Try datetime formats (most specific first)
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",   # 2026-02-12 14:30:05.123
    "%Y-%m-%d %H:%M:%S",    # 2026-02-12 14:30:05
    "%Y-%m-%d %H:%M",       # 2026-02-12 14:30
    "%Y-%m-%dT%H:%M:%S",    # 2026-02-12T14:30:05
    "%Y-%m-%d",              # 2026-02-12
    "%d/%m/%Y %H:%M:%S",    # 12/02/2026 14:30:05
    "%d/%m/%Y",              # 12/02/2026
    "%m/%d/%Y"               # 02/12/2026
  )
  
  for (fmt in formats) {
    result <- try_format(col_data, fmt)
    if (!is.null(result)) return(result)
  }
  
  # Try lubridate flexible parsing
  result <- suppressWarnings(lubridate::ymd_hms(col_data, quiet = TRUE))
  if (sum(!is.na(result)) > length(col_data) * 0.5) return(result)
  
  result <- suppressWarnings(lubridate::ymd(col_data, quiet = TRUE))
  if (sum(!is.na(result)) > length(col_data) * 0.5) return(result)
  
  # Numeric fallback
  num <- suppressWarnings(as.numeric(col_data))
  if (sum(!is.na(num)) > length(col_data) * 0.5) return(num)
  
  # Last resort: index sequence
  seq_along(col_data) - 1
}

#' Format a time value in seconds to a human-readable string
#'
#' @param seconds Numeric vector of time values in seconds
#' @param max_seconds Maximum time value in the dataset (for unit selection)
#' @return Character vector of formatted time strings
format_time_label <- function(seconds, max_seconds = NULL) {
  if (is.null(max_seconds)) max_seconds <- max(seconds, na.rm = TRUE)
  
  sapply(seconds, function(s) {
    if (is.na(s)) return(NA_character_)
    if (max_seconds > 86400 * 2) {
      # Days scale: show "Day X" or "X.Xd"
      d <- s / 86400
      if (d == round(d)) sprintf("Day %d", as.integer(d))
      else sprintf("Day %.1f", d)
    } else if (max_seconds > 3600 * 2) {
      # Hours scale
      h <- floor(s / 3600)
      m <- floor((s %% 3600) / 60)
      sprintf("%dh %02dm", h, m)
    } else if (max_seconds > 120) {
      # Minutes scale
      m <- floor(s / 60)
      sec <- round(s %% 60, 1)
      if (sec == round(sec)) sprintf("%dm %ds", m, as.integer(sec))
      else sprintf("%dm %.1fs", m, sec)
    } else if (max_seconds > 10) {
      # Seconds scale
      sprintf("%.1fs", s)
    } else {
      # Sub-second scale
      sprintf("%.3fs", s)
    }
  })
}

#' Create relative time with high precision
#'
#' @param time_col Parsed time column (POSIXct or numeric)
#' @return Numeric vector of relative time in seconds
create_relative_time <- function(time_col) {
  if (inherits(time_col, "POSIXct")) {
    time_series <- as.numeric(difftime(time_col, min(time_col, na.rm = TRUE), units = "secs"))
  } else if (is.numeric(time_col)) {
    time_series <- time_col - min(time_col, na.rm = TRUE)
  } else {
    time_series <- seq_along(time_col) - 1
  }
  return(time_series)
}

#' Create clean time axis breaks and labels
#'
#' @param time_values Numeric vector of time values in seconds
#' @param n_breaks Number of desired breaks (default: 8)
#' @return List with breaks and formatted labels
create_time_axis <- function(time_values, n_breaks = 8) {
  time_range <- range(time_values, na.rm = TRUE)
  max_time <- max(time_range)
  
  if (max_time > 86400 * 4) {
    unit <- "days"
    divisor <- 86400
    label <- "Time (days)"
    decimals <- 0
  } else if (max_time > 4 * 3600) {
    unit <- "hours"
    divisor <- 3600
    label <- "Time (hours)"
    decimals <- 1
  } else if (max_time > 4 * 60) {
    unit <- "minutes"
    divisor <- 60
    label <- "Time (minutes)"
    decimals <- 1
  } else if (max_time > 10) {
    unit <- "seconds"
    divisor <- 1
    label <- "Time (seconds)"
    decimals <- 1
  } else {
    unit <- "seconds"
    divisor <- 1
    label <- "Time (seconds)"
    decimals <- 2
  }
  
  scaled_time <- time_values / divisor
  breaks <- pretty(scaled_time, n = n_breaks)
  labels <- round(breaks, decimals)
  
  return(list(
    breaks = breaks * divisor,
    labels = labels,
    unit_label = label
  ))
}

#' Detect sampling interval from time series
#'
#' @param time_values Numeric vector of time values
#' @return Estimated sampling interval in seconds
detect_sampling_interval <- function(time_values) {
  if (length(time_values) < 2) return(1.0)
  
  diffs <- diff(time_values[!is.na(time_values)])
  diffs <- diffs[diffs > 0]
  
  if (length(diffs) == 0) return(1.0)
  
  interval <- median(diffs, na.rm = TRUE)
  return(interval)
}

#' Create plots for comparison using luwitemplate styling
#'
#' @param plot_style Character: "combined" or "separate"
#' @param safe_data List containing cleaned df1 and df2
#' @param time_var Character: name of time variable
#' @param y_vars Character vector: names of Y variables to plot
#' @param label1 Character: label for condition 1
#' @param label2 Character: label for condition 2
#' @param for_export Logical: if TRUE, adds avg annotations and branded background
#'   for static PDF/PNG output. If FALSE, keeps transparent bg for plotly.
#' @return List of plot objects

create_plots <- function(plot_style, safe_data, time_var, y_vars, label1, label2, for_export = FALSE) {
  time_var <- "time_relative"
  d1 <- safe_data$df1
  d2 <- safe_data$df2
  
  # Get brand colors for the two conditions
  palette <- luwitemplate::scale_color_luwi_discrete(n = 2)
  color1 <- palette[1]  # primary
  color2 <- palette[2]  # secondary
  
  # For export: use brand background instead of transparent
  theme_colors <- luwitemplate::get_theme_colors()
  export_bg <- if (for_export) theme_colors$body_bg else "transparent"
  
  lapply(y_vars, function(var) {
    # Calculate shared axis limits
    y1_vals <- d1[[var]][!is.na(d1[[var]])]
    y2_vals <- d2[[var]][!is.na(d2[[var]])]
    x1_vals <- d1[[time_var]][!is.na(d1[[time_var]])]
    x2_vals <- d2[[time_var]][!is.na(d2[[time_var]])]
    
    avg1 <- mean(y1_vals, na.rm = TRUE)
    avg2 <- mean(y2_vals, na.rm = TRUE)
    
    x_min <- min(c(x1_vals, x2_vals), na.rm = TRUE)
    x_max <- max(c(x1_vals, x2_vals), na.rm = TRUE)
    y_min <- min(c(y1_vals, y2_vals), na.rm = TRUE)
    y_max <- max(c(y1_vals, y2_vals), na.rm = TRUE)
    x_limits <- c(x_min, x_max)
    
    # Allow negative y values with 5% padding on both sides
    y_pad <- (y_max - y_min) * 0.05
    y_limits <- c(y_min - y_pad, y_max + y_pad)
    
    all_time_vals <- c(x1_vals, x2_vals)
    max_time <- max(all_time_vals, na.rm = TRUE)
    sampling_interval <- detect_sampling_interval(all_time_vals)
    time_axis <- create_time_axis(all_time_vals, n_breaks = 8)
    
    sampling_info <- ""
    if (sampling_interval < 1.0 && sampling_interval >= 0.1) {
      sampling_info <- sprintf(" (\u2248%.1fs intervals)", sampling_interval)
    } else if (sampling_interval < 0.1) {
      sampling_info <- sprintf(" (\u2248%.0fms intervals)", sampling_interval * 1000)
    }
    
    if (plot_style == "combined") {
      combined_data <- rbind(
        data.frame(
          time_x = d1[[time_var]],
          value_y = d1[[var]],
          time_label = format_time_label(d1[[time_var]], max_time),
          Condition = label1,
          stringsAsFactors = FALSE
        ),
        data.frame(
          time_x = d2[[time_var]],
          value_y = d2[[var]],
          time_label = format_time_label(d2[[time_var]], max_time),
          Condition = label2,
          stringsAsFactors = FALSE
        )
      )
      
      combined_data <- combined_data[!is.na(combined_data$time_x) & !is.na(combined_data$value_y), ]
      combined_data$Condition <- factor(combined_data$Condition, levels = c(label1, label2))
      
      combined_plot <- ggplot(combined_data, aes(
        x = time_x, y = value_y, color = Condition
      )) +
        geom_line(linewidth = 0.5, alpha = 0.8)
      
      # text aes only for interactive plotly, not static export
      if (for_export) {
        combined_plot <- combined_plot +
          geom_point(size = 0.8, alpha = 0.4)
      } else {
        combined_plot <- combined_plot +
          geom_point(aes(text = paste0(Condition, "\n", time_label, "\n", var, ": ", round(value_y, 2))),
                     size = 0.8, alpha = 0.4)
      }
      
      combined_plot <- combined_plot +
        labs(
          title = paste0(var, " \u2014 ", label1, " vs ", label2),
          subtitle = if (sampling_info != "") sampling_info else NULL,
          x = time_axis$unit_label,
          y = var,
          color = NULL
        ) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = time_axis$breaks, labels = time_axis$labels) +
        scale_color_manual(values = c(color1, color2)) +
        luwitemplate::theme_luwi() +
        theme(
          legend.title = element_blank(), legend.position = "bottom",
          plot.background = element_rect(fill = export_bg, color = NA),
          panel.background = element_rect(fill = export_bg, color = NA)
        )
      
      # For static export: add hlines and labels directly to ggplot
      if (for_export) {
        closeness <- abs(avg1 - avg2) / (y_limits[2] - y_limits[1])
        vjust1 <- -0.8
        vjust2 <- if (closeness < 0.08) 1.8 else -0.8
        
        combined_plot <- combined_plot +
          geom_hline(yintercept = avg1, color = color1, linetype = "dashed", alpha = 0.6, linewidth = 0.5) +
          geom_hline(yintercept = avg2, color = color2, linetype = "dashed", alpha = 0.6, linewidth = 0.5) +
          annotate("label", x = x_max * 0.98, y = avg1,
                   label = round(avg1, 2),
                   vjust = vjust1, hjust = 1, color = color1, fill = "white",
                   size = 3.5, fontface = "bold", linewidth = 0.5
          ) +
          annotate("label", x = x_max * 0.98, y = avg2,
                   label = round(avg2, 2),
                   vjust = vjust2, hjust = 1, color = color2, fill = "white",
                   size = 3.5, fontface = "bold", linewidth = 0.5
          )
      }
      
      list(combined = combined_plot,
           avg1 = avg1, avg2 = avg2, color1 = color1, color2 = color2,
           x_min = x_min, x_max = x_max, y_range = y_limits[2] - y_limits[1],
           label1 = label1, label2 = label2
      )
      
    } else {
      # Separate side-by-side plots
      d1$time_label <- format_time_label(d1[[time_var]], max_time)
      d2$time_label <- format_time_label(d2[[time_var]], max_time)
      
      p1 <- ggplot(d1, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = color1, linewidth = 0.5, alpha = 0.8)
      
      p2 <- ggplot(d2, aes(x = .data[["time_relative"]], y = .data[[var]])) +
        geom_line(color = color2, linewidth = 0.5, alpha = 0.8)
      
      if (for_export) {
        p1 <- p1 + geom_point(color = color1, size = 0.8, alpha = 0.4)
        p2 <- p2 + geom_point(color = color2, size = 0.8, alpha = 0.4)
      } else {
        p1 <- p1 + geom_point(aes(text = paste0(time_label, "\n", var, ": ", round(.data[[var]], 2))),
                              color = color1, size = 0.8, alpha = 0.4)
        p2 <- p2 + geom_point(aes(text = paste0(time_label, "\n", var, ": ", round(.data[[var]], 2))),
                              color = color2, size = 0.8, alpha = 0.4)
      }
      
      p1 <- p1 +
        geom_hline(yintercept = avg1, color = color1, linetype = "dashed", alpha = 0.6, linewidth = 0.5) +
        labs(
          title = paste0(var, " \u2014 ", label1),
          subtitle = if (sampling_info != "") sampling_info else NULL,
          x = time_axis$unit_label,
          y = var
        ) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = time_axis$breaks, labels = time_axis$labels) +
        luwitemplate::theme_luwi() +
        theme(
          plot.background = element_rect(fill = export_bg, color = NA),
          panel.background = element_rect(fill = export_bg, color = NA)
        )
      
      p2 <- p2 +
        geom_hline(yintercept = avg2, color = color2, linetype = "dashed", alpha = 0.6, linewidth = 0.5) +
        labs(
          title = paste0(var, " \u2014 ", label2),
          subtitle = if (sampling_info != "") sampling_info else NULL,
          x = time_axis$unit_label,
          y = var
        ) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = x_limits, breaks = time_axis$breaks, labels = time_axis$labels) +
        luwitemplate::theme_luwi() +
        theme(
          plot.background = element_rect(fill = export_bg, color = NA),
          panel.background = element_rect(fill = export_bg, color = NA)
        )
      
      if (for_export) {
        p1 <- p1 + annotate("label", x = x_max * 0.98, y = avg1,
                            label = round(avg1, 2),
                            vjust = -0.8, hjust = 1, color = color1, fill = "white",
                            size = 3.5, fontface = "bold", linewidth = 0.5
        )
        p2 <- p2 + annotate("label", x = x_max * 0.98, y = avg2,
                            label = round(avg2, 2),
                            vjust = -0.8, hjust = 1, color = color2, fill = "white",
                            size = 3.5, fontface = "bold", linewidth = 0.5
        )
      }
      
      list(left = p1, right = p2,
           avg1 = avg1, avg2 = avg2, color1 = color1, color2 = color2,
           x_min = x_min, x_max = x_max, y_range = y_limits[2] - y_limits[1]
      )
    }
  })
}