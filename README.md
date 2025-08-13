# Baseball Performance Analysis

This project automates the generation of **interactive baseball pitcher reports** using R, R Markdown, and advanced visualization libraries. It is designed for analyzing game-by-game pitching performance, producing **HTML reports** that include statistical summaries, strike zone visualizations, and interactive heatmaps.

## Overview

The workflow uses a single dataset of pitch-level tracking data (CSV format) to create:
- **Umpire Reports**: Visual strike zone plots and pitch call breakdown tables.
- **Pitcher Summary Tables**: Base and extended statistics (velocity, spin, break, approach angles, etc.).
- **Hit Tables**: Batted ball details (hit type, result, launch angle, exit velocity).
- **Interactive Plate Heatmaps**: Pitch-by-pitch visualizations using Plotly.
- **Full-Game Reports**: Combined outputs in a clean, R Markdown-generated HTML file.

The analysis is fully automated — running one command or knitting the R Markdown file will generate a complete report for the selected game.

## File Structure

- **`Baseball_Functions.R`**  
  Contains reusable helper functions for data cleaning, statistical summaries, GT table formatting, and visualizations.

- **`Game_Report_Template.Rmd`**  
  R Markdown template that calls the helper functions to produce a complete pitcher report in HTML format.

- **`data/`** *(optional)*  
  Directory for storing raw game CSV files (pitch-level tracking data).

## Requirements

Install the following R packages before running:

    install.packages(c(
      "tidyverse", "gt", "gtExtras", "ggplot2",
      "plotly", "htmltools", "knitr", "readr"
    ))

## Usage

### 1. Prepare Your Data
- Save the pitch-level tracking CSV for your game into the `data/` folder (or any accessible location).
- Ensure the dataset contains the required columns:
  - Pitcher name
  - Pitch call type
  - Plate location coordinates (`PlateLocSide`, `PlateLocHeight`)
  - Pitch characteristics (e.g., `RelSpeed`, `SpinRate`, `InducedVertBreak`)

### 2. Generate a Report

**Option A — Using R Markdown**
1. Open `Game_Report_Template.Rmd` in RStudio.
2. Set the file path to your game CSV in the YAML or setup chunk.
3. Click **Knit to HTML** — this will generate the complete interactive report.

**Option B — Using R Console**

    # Load functions
    source("Baseball_Functions.R")

    # Run report generator
    generate_full_report("data/my_game.csv")

### 3. Output
- **Umpire Report**: Strike zone plot + called ball/strike table.
- **Pitcher Statistics**: Base and extended stats for each pitcher.
- **Hit Tables**: Batted ball type, result, launch angle, exit velocity.
- **Interactive Heatmaps**: Clickable pitch location visualizations.

When knitting to HTML, all sections are compiled into one self-contained, interactive file.

## Notes
- Works best with pitch tracking datasets containing detailed pitch location and movement data.
- Built for modularity — all plotting and table functions can be reused in other analysis scripts.
- The `Baseball_Functions.R` script can be integrated into other R projects for customized baseball data analysis.

## View Example Report

You can view an example HTML report here:  
[**View Example Report**](https://eliswagel.github.io/USF-Pitching-Analysis/Game_Report_Template.html)



