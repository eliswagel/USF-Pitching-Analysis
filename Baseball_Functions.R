# ================================
# Libraries
# ================================
library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)
library(plotly)
library(htmltools)

# ================================
# Helpers (formatting, theming, palettes)
# ================================

#' Format number as percentage
#'
#' Formats a numeric value or a ratio as a percentage string with a fixed number of digits.
#'
#' Parameters
#' ----------
#' num : numeric
#'     Numeric value to format.
#' den : numeric, optional
#'     Denominator; if provided, `num/den` is used as the ratio.
#' digits : integer, default=1
#'     Number of decimal places to include.
#'
#' Returns
#' -------
#' character
#'     A percentage string (e.g., "42.0%").
#'
#' Examples
#' --------
#' percent_fmt(0.42)
#' percent_fmt(42, 100, digits = 0)
percent_fmt <- function(num, den = NULL, digits = 1) {
  if (!is.null(den)) num <- num / den
  sprintf(paste0("%.", digits, "f%%"), 100 * num)
}

#' Convert outs to innings-pitched
#'
#' Converts a total number of outs into baseball innings-pitched notation.
#'
#' Parameters
#' ----------
#' outs : integer
#'     Total outs recorded.
#'
#' Returns
#' -------
#' character
#'     Innings-pitched string (e.g., 8 → "2.2").
#'
#' Examples
#' --------
#' ip_from_outs(8)  # "2.2"
ip_from_outs <- function(outs) sprintf("%d.%d", outs %/% 3, outs %% 3)

# Shared pitch color palette (names must match TaggedPitchType values)
pitch_palette <- c(
  "Splitter"          = "#FFFFCC",
  "Curveball"         = "orange",
  "Cutter"            = "green",
  "ChangeUp"          = "purple",
  "Slider"            = "red",
  "Fastball"          = "blue",
  "Sinker"            = "pink",
  "FourSeamFastball"  = "lightgreen"
)

# Readable text color on top of the fills above
pitch_text_color <- c(
  "Splitter"          = "black",
  "Curveball"         = "black",
  "Cutter"            = "black",
  "ChangeUp"          = "white",
  "Slider"            = "white",
  "Fastball"          = "white",
  "Sinker"            = "black",
  "FourSeamFastball"  = "black"
)

#' Base GT theme
#'
#' Applies a consistent table theme to a `gt` object: dividers, header alignment,
#' column label styling, and body text styling.
#'
#' Parameters
#' ----------
#' gt_tbl : gt_tbl
#'     A `gt` table object.
#'
#' Returns
#' -------
#' gt_tbl
#'     The styled `gt` table.
#'
#' Examples
#' --------
#' # gt_theme_base(gt(cars))
gt_theme_base <- function(gt_tbl) {
  gt_tbl %>%
    gt::tab_options(
        table.align = "left"  # ensure the whole gt table is left-aligned in HTML
   ) %>%
    gtExtras::gt_add_divider(columns = tidyselect::everything(),
                             style = "solid", weight = gt::px(1.5)) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("top","bottom"),
                               color = "gray", style = "solid",
                               weight = gt::px(1.5)),
      locations = gt::cells_body()
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::tab_style(
      locations = gt::cells_column_labels(columns = tidyselect::everything()),
      style = list(
        gt::cell_fill(color = "#D3D3D3"),
        gt::cell_text(weight = "bold", align = "center", size = 22)
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = 20, align = "center", weight = "bold"),
      locations = gt::cells_body()
    )
}

#' Apply pitch palette to GT table
#'
#' Colors a categorical column (default: `TaggedPitchType`) in a `gt` table
#' according to predefined fill and text palettes.
#'
#' Parameters
#' ----------
#' gt_tbl : gt_tbl
#'     A `gt` table object.
#' data : data.frame
#'     The underlying data used by the table (for determining categories present).
#' col : character, default="TaggedPitchType"
#'     Name of the categorical column to style.
#' fill_pal : named character, optional
#'     Named color vector mapping category → fill color.
#' text_pal : named character, optional
#'     Named color vector mapping category → text color.
#'
#' Returns
#' -------
#' gt_tbl
#'     The `gt` table with per-value cell styling applied.
#'
#' Examples
#' --------
#' # gt_apply_pitch_palette(gt(tbl), tbl, "TaggedPitchType")
gt_apply_pitch_palette <- function(gt_tbl, data, col = "TaggedPitchType",
                                   fill_pal = pitch_palette,
                                   text_pal = pitch_text_color) {
  vals <- sort(unique(data[[col]]))
  to_color <- intersect(names(fill_pal), vals)
  out <- gt_tbl
  for (nm in to_color) {
    out <- out %>%
      gt::tab_style(
        locations = gt::cells_body(columns = !!rlang::sym(col),
                                   rows = !!rlang::sym(col) == nm),
        style = list(
          gt::cell_fill(color = fill_pal[[nm]]),
          gt::cell_text(color = text_pal[[nm]], weight = "bold")
        )
      )
  }
  out
}

#' Normalize labels for hit tables
#'
#' Rewrites certain categorical labels to more readable forms for display.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Input table containing `TaggedHitType` and `PlayResult`.
#'
#' Returns
#' -------
#' data.frame
#'     A mutated copy with human-friendly labels.
#'
#' Examples
#' --------
#' # normalize_labels(df)
normalize_labels <- function(df) {
  df %>%
    mutate(
      TaggedHitType = recode(TaggedHitType,
        LineDrive  = "Line Drive",
        FlyBall    = "Fly Ball",
        GroundBall = "Ground Ball",
        Popup      = "Pop Up",
        .default   = TaggedHitType
      ),
      PlayResult = recode(PlayResult,
        HomeRun         = "Home Run",
        FieldersChoice  = "Fielders Choice",
        .default        = PlayResult
      )
    )
}

# ================================
# Small utilities
# ================================

#' Round selected numeric columns
#'
#' Rounds location, angle, and exit velocity columns to two decimals.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Input data with `PlateLocHeight`, `PlateLocSide`, `Angle`, `ExitSpeed`.
#'
#' Returns
#' -------
#' data.frame
#'     Data with selected columns rounded.
rounded_dataset <- function(df) {
  df %>%
    mutate(
      PlateLocHeight = round(PlateLocHeight, 2),
      PlateLocSide   = round(PlateLocSide, 2),
      Angle          = round(Angle, 2),
      ExitSpeed      = round(ExitSpeed, 2)
    )
}

#' Get USF pitchers
#'
#' Pulls distinct pitcher names for the team code "SAN_DON".
#'
#' Parameters
#' ----------
#' data : data.frame
#'     Full pitch dataset with `Pitcher` and `PitcherTeam`.
#'
#' Returns
#' -------
#' character
#'     Vector of unique pitcher names.
get_usf_pitchers <- function(data) {
  data %>%
    filter(PitcherTeam == "SAN_DON") %>%
    distinct(Pitcher) %>%
    pull(Pitcher)
}

#' Get opposing pitchers
#'
#' Pulls distinct pitcher names where team is not "SAN_DON".
#'
#' Parameters
#' ----------
#' data : data.frame
#'     Full pitch dataset with `Pitcher` and `PitcherTeam`.
#'
#' Returns
#' -------
#' character
#'     Vector of unique opposing pitcher names.
get_opposing_pitchers <- function(data) {
  data %>%
    filter(PitcherTeam != "SAN_DON") %>%
    distinct(Pitcher) %>%
    pull(Pitcher)
}

# ================================
# Umpire report (plot + table + wrapper)
# ================================

#' Umpire location scatter
#'
#' Produces a pitch-location scatter plot colored by `PitchCall`, with a drawn strike zone.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Pitch data (expects `PlateLocSide`, `PlateLocHeight`, `PitchCall`).
#' title : character
#'     Plot title.
#'
#' Returns
#' -------
#' ggplot
#'     A ggplot object.
Umpire_Report <- function(df, title) {
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
    geom_point(size = 2.1) +
    annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4,
             fill = NA, color = "black", alpha = 0.1) +
    ylim(0, 5) + xlim(-2.7, 2.7) +
    theme_bw() +
    labs(title = title, x = "Plate Loc Side", y = "Plate Loc Height", color = "Pitch Call") +
    theme(legend.key = element_blank())
}

#' Umpire statistics table
#'
#' Summarizes pitch counts, frequencies, and called balls/strikes per `TaggedPitchType`.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Pitch data filtered to called balls/strikes as needed.
#'
#' Returns
#' -------
#' gt_tbl
#'     Styled `gt` table of umpire statistics.
Umpire_Report_Statistics <- function(df) {
  tbl_data <- df %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Total        = n(),
      Frequency    = percent_fmt(Total, nrow(df)),
      CalledBalls  = sum(PitchCall == "BallCalled",  na.rm = TRUE),
      CalledStrikes= sum(PitchCall == "StrikeCalled", na.rm = TRUE),
      .groups = "drop"
    )

  tbl_data %>%
    gt() %>%
    gt_theme_base() %>%
    cols_label(
      TaggedPitchType = "Pitch",
      CalledBalls     = "Called Balls",
      CalledStrikes   = "Called Strikes"
    ) %>%
    gt_apply_pitch_palette(data = tbl_data, col = "TaggedPitchType")
}

#' Build a single umpire summary bundle
#'
#' Filters by pitcher set, restricts to called pitches, and returns the data,
#' the plot, and the summary table.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full pitch dataset.
#' pitcher_names : character
#'     Vector of pitcher names to include.
#' report_title : character
#'     Title for the plot.
#'
#' Returns
#' -------
#' list
#'     List with `umpire_df`, `report_plot`, `summary_table`.
generate_umpire_summary <- function(df, pitcher_names, report_title) {
  umpire_df <- df %>%
    filter(Pitcher %in% pitcher_names) %>%
    filter(PitchCall %in% c("StrikeCalled", "BallCalled"))

  list(
    umpire_df    = umpire_df,
    report_plot  = Umpire_Report(umpire_df, report_title),
    summary_table= Umpire_Report_Statistics(umpire_df)
  )
}

#' Generate and (optionally) print USF vs Opponent umpire summaries
#'
#' Creates plot+table for USF pitchers and for opponents, prints them if `print=TRUE`,
#' and returns all objects invisibly.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full pitch dataset.
#' print : logical, default=TRUE
#'     Whether to cat headers and print the plot/table objects.
#'
#' Returns
#' -------
#' invisible(list)
#'     `usf_plot`, `usf_table`, `opponent_plot`, `opponent_table`.
generate_umpire_summaries <- function(df, print = TRUE) {
  # 1) Build pitcher lists
  usf_names      <- get_usf_pitchers(df)
  opposing_names <- get_opposing_pitchers(df)

  # 2) Compute summaries
  usf_sum      <- generate_umpire_summary(df, usf_names,      "USF Umpire Report")
  opp_sum      <- generate_umpire_summary(df, opposing_names, "Opponent Umpire Report")

  # 3) Auto-print (plots + gt tables) with simple headers
  if (isTRUE(print)) {
    cat("\n# USF Umpire Report\n\n")
    print(usf_sum$report_plot)
    print(usf_sum$summary_table)

    cat("\n\n# Opponent Umpire Report\n\n")
    print(opp_sum$report_plot)
    print(opp_sum$summary_table)
  }

  # 4) Return everything (invisibly) for further use
  invisible(list(
    usf_plot       = usf_sum$report_plot,
    usf_table      = usf_sum$summary_table,
    opponent_plot  = opp_sum$report_plot,
    opponent_table = opp_sum$summary_table
  ))
}

# ================================
# Pitcher summary tables (base + extended) and wrapper
# ================================

#' Base statistics summary table
#'
#' Produces a compact table of pitching results (IP, PA, ER, H, K, BB, HBP, XBH, Strike%).
#'
#' Parameters
#' ----------
#' pitcher_name : character
#'     Pitcher to summarize (labeling only; data should already be filtered if needed).
#' df : data.frame
#'     Data filtered to the pitcher, containing required columns.
#'
#' Returns
#' -------
#' gt_tbl
#'     Styled table of basic statistics.
statistics_summary_table <- function(pitcher_name, df) {
  df %>%
    summarise(
      IP = { outs <- sum(OutsOnPlay, na.rm = TRUE); ip_from_outs(outs) },
      PA = n_distinct(paste(Inning, PAofInning, sep = "_")),
      ER = sum(RunsScored, na.rm = TRUE),
      H  = sum(PlayResult %in% c("Single","Double","Triple","HomeRun"), na.rm = TRUE),
      K  = sum(KorBB == "Strikeout", na.rm = TRUE),
      BB = sum(KorBB == "Walk", na.rm = TRUE),
      HBP= sum(PitchCall == "HitByPitch", na.rm = TRUE),
      XBH= sum(PlayResult %in% c("Double","Triple","HomeRun"), na.rm = TRUE),
      Strikes = percent_fmt(mean(PitchCall %in% c("StrikeSwinging","StrikeCalled"), na.rm = TRUE))
    ) %>%
    gt() %>%
    gt_theme_base() %>%
    cols_width(
      IP ~ px(80), PA ~ px(80), ER ~ px(80), H ~ px(80),
      K ~ px(80), BB ~ px(80), HBP ~ px(80), XBH ~ px(80), Strikes ~ px(80)
    )
}

#' Extended pitch characteristics table
#'
#' Aggregates pitch characteristics by `TaggedPitchType` (velo, IVB, HB, spin, release angles,
#' extension, approach angles) and derived metrics (Whiff, Chase, Zone%).
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Data for a single pitcher (recommended).
#' pitcher_name : character
#'     Pitcher name (for labeling only).
#'
#' Returns
#' -------
#' gt_tbl
#'     Styled table with extended statistics.
extended_statistics_table <- function(df, pitcher_name) {
  req <- c(
    "RelSpeed","VertRelAngle","HorzRelAngle","SpinRate","SpinAxis","Tilt",
    "RelHeight","RelSide","Extension","VertBreak","InducedVertBreak","HorzBreak",
    "PlateLocHeight","PlateLocSide","ZoneSpeed","VertApprAngle","HorzApprAngle"
  )

  tbl_data <- df %>%
    filter(if_all(all_of(req), ~ !is.na(.x))) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Count     = n(),
      Frequency = percent_fmt(Count, nrow(df)),
      Velocity  = round(mean(RelSpeed), 1),
      IVB       = round(mean(InducedVertBreak), 1),
      HB        = round(mean(HorzBreak), 1),
      Spin      = round(mean(SpinRate), 1),
      VAA       = round(mean(VertApprAngle), 1),
      HAA       = round(mean(HorzApprAngle), 1),
      vRel      = round(mean(VertRelAngle), 1),
      hRel      = round(mean(HorzRelAngle), 1),
      Ext       = round(mean(Extension), 1),
      Whiff = percent_fmt(
        sum(PitchCall == "StrikeSwinging"),
        sum(PitchCall %in% c("FoulBallNotFieldable","InPlay","StrikeSwinging"))
      ),
      Chase = percent_fmt(
        sum(PitchCall %in% c("InPlay","StrikeSwinging","FoulBallNotFieldable") &
              (PlateLocSide > 1 | PlateLocSide < -1 | PlateLocHeight > 3.4 | PlateLocHeight < 1.6)),
        sum(PlateLocSide > 1 | PlateLocSide < -1 | PlateLocHeight > 3.4 | PlateLocHeight < 1.6)
      ),
      Zone  = percent_fmt(mean(PlateLocSide > -1 & PlateLocSide < 1 &
                               PlateLocHeight < 3.4 & PlateLocHeight > 1.6)),
      .groups = "drop"
    )

  tbl_data %>%
    gt() %>%
    gt_theme_base() %>%
    cols_label(TaggedPitchType = "Pitch") %>%
    gt_apply_pitch_palette(data = tbl_data, col = "TaggedPitchType")
}

#' Bundle pitcher summary tables
#'
#' Returns both the base and extended `gt` summary tables for a given pitcher.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full dataset.
#' pitcher_name : character
#'     Name of pitcher to summarize.
#'
#' Returns
#' -------
#' list
#'     `base_summary_table`, `extended_summary_table`.
generate_pitcher_summary <- function(df, pitcher_name) {
  pitcher_df <- df %>% filter(Pitcher == pitcher_name)
  list(
    base_summary_table     = statistics_summary_table(pitcher_name, pitcher_df),
    extended_summary_table = extended_statistics_table(pitcher_df, pitcher_name)
  )
}

#' Print summary stats (per pitcher) and hit tables
#'
#' Iterates over USF pitchers and prints: base table, extended table, and hit table.
#' Designed for Rmd/Viewer output.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full dataset.
#'
#' Returns
#' -------
#' invisible(NULL)
#'     Called for side effects (printed output).
generate_summary_statistics_tables <- function(df) {
  library(knitr)
  pitcher_list <- get_usf_pitchers(df)

  for (pitcher in pitcher_list) {
    cat("## Pitcher:", pitcher, "\n\n")

    cat("### Overview\n\n")
    print(generate_pitcher_summary(df, pitcher)$base_summary_table)

    cat("\n### Summary Statistics\n\n")
    print(generate_pitcher_summary(df, pitcher)$extended_summary_table)

    cat("\n### Hit Pitches\n\n")
    print(generate_formatted_hit_table(df, pitcher))

    cat("\n---\n\n")
  }
}

# ================================
# Hit table (formatted) using helpers
# ================================

#' Formatted table of balls in play
#'
#' Shows pitch type, hit type, result, angle, and exit speed for a pitcher’s balls in play.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full dataset.
#' pitcher_name : character
#'     Name of pitcher to filter on.
#'
#' Returns
#' -------
#' gt_tbl
#'     Styled table with palette applied to pitch type column.
generate_formatted_hit_table <- function(df, pitcher_name) {
  tbl_data <- df %>%
    filter(Pitcher == pitcher_name, PitchCall == "InPlay") %>%
    normalize_labels() %>%
    select(TaggedPitchType, TaggedHitType, PlayResult, Angle, ExitSpeed) %>%
    mutate(Angle = round(Angle, 2), ExitSpeed = round(ExitSpeed, 2))

  tbl_data %>%
    gt() %>%
    gt_theme_base() %>%
    cols_label(
      TaggedPitchType = "Pitch",
      TaggedHitType   = "Hit Type",
      PlayResult      = "Play Result",
      ExitSpeed       = "Exit Speed"
    ) %>%
    gt_apply_pitch_palette(data = tbl_data, col = "TaggedPitchType")
}

# ================================
# Interactive plate heatmaps (plotly) per pitch type + per pitcher
# ================================

#' Interactive plate heatmaps (per pitch type)
#'
#' Builds plotly-converted ggplots of plate locations, one per pitch type for a pitcher.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full dataset.
#' pitcher_name : character
#'     Pitcher to visualize.
#'
#' Returns
#' -------
#' htmltools::tagList
#'     A list of plotly widgets keyed by pitch type.
interactive_heatmaps <- function(df, pitcher_name) {
  pitcher_df  <- filter(df, Pitcher == pitcher_name)
  pitch_types <- unique(pitcher_df$TaggedPitchType)
  plots <- vector("list", length(pitch_types)); names(plots) <- pitch_types

  for (pitch in pitch_types) {
    data_filtered <- filter(pitcher_df, TaggedPitchType == pitch)

    p_plot <- ggplot(data_filtered, aes(
      x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType,
      text = paste("Pitch Call:", PitchCall,
                   "<br>Play Result:", PlayResult,
                   "<br>Exit Speed:", ExitSpeed,
                   "<br>Angle:", Angle)
    )) +
      annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black", alpha = 1) +
      geom_point(size = 2.8) +
      scale_color_manual(values = pitch_palette) +
      ylim(0, 5) + xlim(-2.7, 2.7) +
      theme_bw() +
      labs(title = pitch, x = "Plate Loc Side", y = "Plate Loc Height", color = "Pitch Type") +
      theme(legend.key = element_blank())

    plots[[pitch]] <- ggplotly(p_plot, width = 630, height = 570) %>%
      layout(hoverlabel = list(bgcolor = "black"))
  }

  tagList(plots)
}

#' Render interactive heatmaps for all USF pitchers
#'
#' Iterates through USF pitchers and renders labeled sections of interactive heatmaps.
#'
#' Parameters
#' ----------
#' df : data.frame
#'     Full dataset.
#'
#' Returns
#' -------
#' htmltools::browsable
#'     A browsable HTML fragment containing all sections.
generate_interactive_heatmaps <- function(df) {
  pitchers <- get_usf_pitchers(df)

  sections <- lapply(pitchers, function(pitcher) {
    tagList(
      tags$h2(paste("Pitcher:", pitcher)),
      interactive_heatmaps(df, pitcher)
    )
  })

  browsable(tagList(sections))
}

# ================================
# One-shot report runner
# ================================

#' Generate the full report from a CSV path
#'
#' Sources your helper script, reads the CSV, and runs the three main report
#' generators (umpire summaries, summary statistic tables, interactive heatmaps).
#'
#' Parameters
#' ----------
#' file_path : character
#'     Absolute or relative path to the CSV file.
#'
#' Side Effects
#' ------------
#' Prints plots and tables to the active R Markdown or Viewer context.
#'
#' Returns
#' -------
#' invisible(NULL)
#'     Called for side effects.
generate_full_report <- function(file_path) {
  # Load your baseball functions
  source("~/Desktop/USF/Baseball_Functions.R")
  
  # Read CSV into a dataframe
  df <- readr::read_csv(file_path)
  
  # Run the three report functions
  generate_umpire_summaries(df)
  generate_summary_statistics_tables(df)
  generate_interactive_heatmaps(df)
}

