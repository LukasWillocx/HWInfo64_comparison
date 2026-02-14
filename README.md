# CSV Comparison Tool

A Shiny app for visually comparing time-series data across two CSV files. Upload two datasets (or use built-in demos), select shared variables, and generate interactive side-by-side or overlay plots with automatic averaging and branded styling.

## What It Does

- Compares up to 4 numeric variables between two CSV files on a shared time axis
- Auto-detects time columns and parses multiple datetime formats (ISO, EU, US, sub-second precision)
- Handles messy data: deduplicates columns, converts yes/no to 1/0, and coerces numeric strings with comma decimals
- Supports two plot modes: **side-by-side** (separate panels per condition) and **combined** (overlay with color-coded conditions)
- Displays per-condition averages as dashed reference lines with labeled values
- Interactive plotly charts with hover tooltips, zoom, pan, and auto-scaled time axes with human-readable labels (seconds, minutes, hours, days)
- Exports to **PDF** (multi-page report with branded title page, metadata, and all selected variables) or **PNG** (high-res individual plots bundled in a ZIP archive)
- Dark mode toggle with full theme support — UI components, plots, and exports all adapt

## Quick Start

1. Launch the app and choose between uploading your own CSVs or selecting from demo files in `www/`
2. Assign labels to each condition (auto-populated from demo filenames)
3. Pick a time variable for the X-axis and up to 4 numeric Y-axis variables
4. Confirm setup, switch to the Visualization tab, and generate plots
5. Optionally switch plot styles, then export from the Export tab with custom dimensions and DPI

## Demo Files

Place any CSV files in the `www/` folder. The app auto-discovers them, strips `demo_` prefixes, and creates human-friendly labels. Select any two files with overlapping column names to compare.

## Requirements

`shiny`, `bslib`, `ggplot2`, `plotly`, `gridExtra`, `lubridate`, `luwitemplate`

For PNG export on Linux: `sudo apt install zip`
