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

You can generate the report from either the R Markdown template or the R console.

### Quick start (recommended)
1) **Get data**  
   - Use our sample: [`data/20240512-BenedettiDiamond-1_unverified.csv`](data/20240512-BenedettiDiamond-1_unverified.csv)  
   - Or use your own CSV of pitching data tha you want analyzed

2) **Open the template**  
   - Open `Game_Report_Template.Rmd` in RStudio.  
   - In the setup chunk, set the path to your CSV, for example:
     ```r
     data_path <- "data/20240512-BenedettiDiamond-1_unverified.csv"  # or your own file
     ```

3) **Knit**  
   - Click **Knit** → **Knit to HTML**.  
   - You’ll get a single interactive HTML file with:
     - Umpire strike-zone plot + called ball/strike summary  
     - Pitcher stats  
     - Hit Pitch tables  
     - Interactive pitch-location heatmaps

## View Example Report

A sample CSV is provided so you can try the analysis without needing your own data.  
[📥 Download Sample CSV](data/20240512-BenedettiDiamond-1_unverified.csv)

You can view an example HTML report here:  
[**View Example Report**](https://eliswagel.github.io/USF-Pitching-Analysis/Game_Report_Template.html)



