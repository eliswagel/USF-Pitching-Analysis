library(tidyverse)
library(gt)
library(ggrepel)
library(plotly)
library(gtExtras)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmltools)

colors <- c(
  ChangeUp = "purple",
  Splitter = "yellow",
  Curveball = "orange",
  Cutter = "darkgreen",
  FourSeamFastBall = "#8B0000",
  Slider = "red",
  Sinker = "#483D8B",
  Fastball = "blue"
)


generate_formatted_hit_table <- function(df, pitcher_name) {
  pitcher_df <- df %>% filter(Pitcher == pitcher_name)

  pitcher_df %>%
    mutate(
      TaggedHitType = case_when(
        TaggedHitType == "LineDrive" ~ "Line Drive",
        TaggedHitType == "FlyBall" ~ "Fly Ball",
        TaggedHitType == "GroundBall" ~ "Ground Ball",
        TaggedHitType == "Popup" ~ "Pop Up",
        TRUE ~ TaggedHitType
      ),
      PlayResult = case_when(
        PlayResult == "HomeRun" ~ "Home Run",
        PlayResult == "FieldersChoice" ~ "Fielders Choice",
        TRUE ~ PlayResult
      )
    ) %>%
    filter(PitchCall == "InPlay") %>%
    select(TaggedPitchType, TaggedHitType, PlayResult, Angle, ExitSpeed) %>%
    mutate(
      Angle = round(Angle, 2),
      ExitSpeed = round(ExitSpeed, 2)
    ) %>%
    gt() %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      TaggedPitchType = "Pitch",
      TaggedHitType = "Hit Type",
      PlayResult = "Play Result",
      ExitSpeed = "Exit Speed"
    ) %>%
    gt_add_divider(columns = c("TaggedPitchType", "TaggedHitType", "PlayResult", "Angle", "ExitSpeed"),
                   style = "solid", weight = px(0.5)) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "gray", style = "solid", weight = px(0.5)),
      locations = cells_body()
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c("TaggedPitchType", "TaggedHitType", "PlayResult", "ExitSpeed", "Angle")),
      style = list(cell_fill(color = "#D3D3D3"),
                   cell_text(weight = "bold", align = "center", size = 22))
    ) %>%
    tab_style(style = cell_text(size = 20, align = "center"), locations = cells_body()) %>%
    tab_style(locations = cells_body(columns = c("Angle", "ExitSpeed", "TaggedHitType", "PlayResult")),
              style = list(cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Splitter"),
              style = list(cell_fill(color = "#FFFFCC"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Curveball"),
              style = list(cell_fill(color = "orange"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Cutter"),
              style = list(cell_fill(color = "green"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "ChangeUp"),
              style = list(cell_fill(color = "purple"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Slider"),
              style = list(cell_fill(color = "red"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Fastball"),
              style = list(cell_fill(color = "blue"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Sinker"),
              style = list(cell_fill(color = "pink"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "FourSeamFastBall"),
              style = list(cell_fill(color = "lightgreen"), cell_text(color = "black", weight = "bold")))
}


rounded_dataset <- function(df) {df %>% 
    mutate(
      PlateLocHeight = round(PlateLocHeight, 2),
      PlateLocSide = round(PlateLocSide, 2),
      Angle = round(Angle, 2),
      ExitSpeed = round(ExitSpeed, 2))}

filtered_data <- function(df, name) {df %>% filter(Pitcher == name)}

get_usf_pitchers <- function(data) {
  data %>%
    filter(PitcherTeam == "SAN_DON") %>%
    distinct(Pitcher) %>%
    pull(Pitcher)
}

get_opposing_pitchers <- function(data) {
  data %>%
    filter(PitcherTeam != "SAN_DON") %>%
    distinct(Pitcher) %>%
    pull(Pitcher)
}

# Add new function to generate pitcher summary tables
generate_pitcher_summary <- function(df, pitcher_name) {
  pitcher_df <- df %>% filter(Pitcher == pitcher_name)

  list(
    summary_1 = Summary_1(pitcher_name, pitcher_df),
    summary_table = Summary_Table_Updated(pitcher_df, pitcher_name)
  )
}

Summary_1 = function(Pitcher_Name, df) {
  df %>%
    summarise(
      IP  = {
          outs <- sum(OutsOnPlay, na.rm = TRUE)
          sprintf("%d.%d", outs %/% 3, outs %% 3)
        },
      PA = n_distinct(paste(df$Inning, df$PAofInning, sep = "_")),
      ER = sum(df$RunsScored),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      K = sum(KorBB == "Strikeout"),
      BB = sum(KorBB == "Walk"),
      HBP = sum(PitchCall == "HitByPitch"),
      XBH = sum(PlayResult %in% c("Double", "Triple", "HomeRun")),
      Strikes = sprintf("%.1f%%", (sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled")) / n()) * 100)
    ) %>%
    gt() %>%
    tab_style(
      locations = cells_column_labels(columns = c("IP", "PA", "ER", "H", "K", "BB", "HBP", "XBH", "Strikes")),
      style = list(cell_fill(color = "#D3D3D3"), cell_text(weight = "bold", align = "center", size = "22"))
    ) %>%
    tab_style(style = cell_text(size = 20, align = "center"), locations = cells_body()) %>%
    gt_add_divider(columns = c("IP", "PA", "ER", "H", "K", "BB", "HBP", "XBH", "Strikes"), style = "solid", weight = px(1.5)) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "gray", style = "solid", weight = px(1.5)),
      locations = cells_body()
    ) %>%
    opt_align_table_header(align = "left") %>%
    tab_style(
      locations = cells_column_labels(columns = c("IP", "PA", "ER", "H", "K", "BB", "HBP", "XBH", "Strikes")),
      style = list(cell_fill(color = "#D3D3D3"), cell_text(weight = "bold"))
    ) %>%
    cols_width(
      IP ~ px(80), PA ~ px(80), ER ~ px(80), H ~ px(80),
      K ~ px(80), BB ~ px(80), HBP ~ px(80), XBH ~ px(80),
      Strikes ~ px(80)
    )
}

Summary_Table_Updated = function(df, pitcher_name) {
  df %>%
    filter(
      !is.na(RelSpeed) & !is.na(VertRelAngle) & !is.na(HorzRelAngle) &
      !is.na(SpinRate) & !is.na(SpinAxis) & !is.na(Tilt) &
      !is.na(RelHeight) & !is.na(RelSide) & !is.na(Extension) &
      !is.na(VertBreak) & !is.na(InducedVertBreak) & !is.na(HorzBreak) &
      !is.na(PlateLocHeight) & !is.na(PlateLocSide) & !is.na(ZoneSpeed) &
      !is.na(VertApprAngle) & !is.na(HorzApprAngle)
    ) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Count = n(),
      Frequency = sprintf("%.1f%%", Count / nrow(df) * 100),
      Velocity = round(mean(RelSpeed, na.rm = TRUE), 1),
      IVB = round(mean(InducedVertBreak), 1),
      HB = round(mean(HorzBreak), 1),
      Spin = round(mean(SpinRate), 1),
      VAA = round(mean(VertApprAngle), 1),
      HAA = round(mean(HorzApprAngle), 1),
      vRel = round(mean(VertRelAngle), 1),
      hRel = round(mean(HorzRelAngle), 1),
      Ext = round(mean(Extension), 1),
      Whiff = sprintf("%.1f%%", (sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("FoulBallNotFieldable", "InPlay", "StrikeSwinging"))) * 100),
      Chase = sprintf("%.1f%%", (sum(PitchCall %in% c("InPlay", "StrikeSwinging", "FoulBallNotFieldable") &
                                       (PlateLocSide > 1 | PlateLocSide < -1 | PlateLocHeight > 3.4 | PlateLocHeight < 1.6)) /
                                   sum(PlateLocSide > 1 | PlateLocSide < -1 | PlateLocHeight > 3.4 | PlateLocHeight < 1.6)) * 100),
      Zone = sprintf("%.1f%%", (sum(PlateLocSide > -1 & PlateLocSide < 1 & PlateLocHeight < 3.4 & PlateLocHeight > 1.6) / n()) * 100)
    ) %>%
    gt() %>%
    opt_align_table_header(align = "left") %>%
    cols_label(TaggedPitchType = "Pitch") %>%
    gt_add_divider(columns = c("TaggedPitchType", "Count", "Velocity", "Frequency", "IVB", "HB", "Spin", "VAA","HAA", "vRel", "hRel", "Ext", "Whiff", "Chase", "Zone"), style = "solid", weight = px(1.5)) %>%
    tab_style(
      style = cell_borders(sides = c("top", "bottom"), color = "gray", style = "solid", weight = px(1.5)),
      locations = cells_body()
    ) %>%
    tab_style(
      locations = cells_column_labels(columns = c("TaggedPitchType", "Count", "Frequency", "Velocity", "IVB", "HB", "Spin", "VAA", "HAA", "vRel", "hRel", "Ext", "Whiff", "Chase", "Zone")),
      style = list(cell_fill(color = "#D3D3D3"), cell_text(weight = "bold", align = "center", size = 22))
    ) %>%
    tab_style(style = cell_text(size = 20, align = "center"), locations = cells_body()) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Splitter"), style = list(cell_fill(color = "#FFFFCC"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Curveball"), style = list(cell_fill(color = "orange"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Cutter"), style = list(cell_fill(color = "green"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "ChangeUp"), style = list(cell_fill(color = "purple"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Slider"), style = list(cell_fill(color = "red"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Fastball"), style = list(cell_fill(color = "blue"), cell_text(color = "white", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Sinker"), style = list(cell_fill(color = "pink"), cell_text(color = "black", weight = "bold"))) %>%
    tab_style(locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "FourSeamFastBall"), style = list(cell_fill(color = "lightgreen"), cell_text(color = "black", weight = "bold"))) %>%
    cols_width(
      Count ~ px(100), Frequency ~ px(100), Velocity ~ px(100), IVB ~ px(100), HB ~ px(100), Spin ~ px(100),
      VAA ~ px(100), HAA ~ px(100), vRel ~ px(100), hRel ~ px(100), Ext ~ px(100),
      Whiff ~ px(100), Chase ~ px(100), Zone ~ px(100)
    )
}
  

Heat_Map <- function(df, title) {
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, label = PitchNo)) + 
    geom_point(size = 1.8) +
    geom_text(data = subset(df, PitchCall == "InPlay"), aes(label = PitchNo), size = 2.7, nudge_y = 0.2) +  
    annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4, 
             fill = NA, color = "black", alpha = 0.1) +  
    ylim(0, 5) +  
    xlim(-2.7, 2.7) +  
    theme_bw() +  
    labs(
      title = title,
      x = "Plate Loc Side",
      y = "Plate Loc Height",
      color = "Pitch Type"
    ) +  
    theme(legend.key = element_blank()) +  
    scale_color_manual(values = colors) +  
    facet_wrap(~TaggedPitchType)  
}


generate_umpire_summary <- function(df, pitcher_names, report_title) {
  umpire_df <- df %>%
    filter(Pitcher %in% pitcher_names) %>%
    filter(PitchCall %in% c("StrikeCalled", "BallCalled"))
  
  report_plot <- Umpire_Report(umpire_df, report_title)
  summary_table <- Umpire_Report_Summary_Table(umpire_df)
  
  list(
    umpire_df = umpire_df,
    report_plot = report_plot,
    summary_table = summary_table
  )
}

Umpire_Report <- function(df, title) {
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall))+
    geom_point(size = 2.1) +
    annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4,
             fill = NA, color = "black", alpha = 0.1) +
    ylim(0, 5) +
    xlim(-2.7, 2.7) +
    theme_bw() +
    labs(
      title = title,
      x = "Plate Loc Side",
      y = "Plate Loc Height",
      color = "Pitch Call"
    ) +
    theme(legend.key = element_blank())}

Umpire_Report_Summary_Table = function(df) {
  df %>%
    group_by(TaggedPitchType) %>%
    summarise(
      Total = n(),  # Counts the total number of pitches for each TaggedPitchType
      Frequency = sprintf("%.1f%%", Total / nrow(df) * 100),
      CalledBalls = sum(PitchCall %in% c("BallCalled"), na.rm = TRUE),
      CalledStrikes = sum(PitchCall %in% c("StrikeCalled"), na.rm = TRUE)
    ) %>%
    gt() %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      TaggedPitchType = "Pitch",
      CalledBalls = "Called Balls",
      CalledStrikes = "Called Strikes"
    ) %>%
    gt_add_divider(columns = c( "TaggedPitchType", "Total", "Frequency","CalledBalls", "CalledStrikes"), style = "solid", weight = px(1.5)) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "gray",
        style = "solid",
        weight = px(1.5)),
      locations = cells_body()) %>%
    tab_style(
      locations = cells_column_labels(columns = c( "TaggedPitchType","Total", "Frequency","CalledBalls", "CalledStrikes")),
      style = list(cell_fill(color = "#D3D3D3"), cell_text(weight = "bold", align = "center", size = 22))
    ) %>%
    tab_style(
      style= cell_text(size = 20, align = "center"),
      locations = cells_body()
    ) %>%
    tab_style(
      locations = cells_body(columns = c("Total", "CalledBalls", "Frequency", "CalledStrikes")),
      style = list(cell_text(color = "black", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Splitter"),
      style = list(cell_fill(color = "#FFFFCC"), cell_text(color = "black", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Curveball"),
      style = list(cell_fill(color = "orange"), cell_text(color = "black", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Cutter"),
      style = list(cell_fill(color = "green"), cell_text(color = "black", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "ChangeUp"),
      style = list(cell_fill(color = "purple"), cell_text(color = "white", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Slider"),
      style = list(cell_fill(color = "red"), cell_text(color = "white", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Fastball"),
      style = list(cell_fill(color = "blue"), cell_text(color = "white", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "Sinker"),
      style = list(cell_fill(color = "pink"), cell_text(color = "black", weight = "bold"))
    ) %>%
    tab_style(
      locations = cells_body(columns = "TaggedPitchType", rows = TaggedPitchType == "FourSeamFastBall"),
      style = list(cell_fill(color = "lightgreen"), cell_text(color = "black", weight = "bold"))
    )
}

generate_usf_pitcher_report <- function(df) {
  library(knitr)
  
  # Get pitcher list from the dataset
  pitcher_list <- get_usf_pitchers(df)
  
  for (pitcher in pitcher_list) {
    # Pitcher section title
    cat("## Pitcher:", pitcher, "\n\n")
    
    # Overview
    cat("### Overview\n\n")
    print(generate_pitcher_summary(df, pitcher)$summary_1)
    
    # Summary Statistics
    cat("\n### Summary Statistics\n\n")
    print(generate_pitcher_summary(df, pitcher)$summary_table)
    
    # Hit Pitches
    cat("\n### Hit Pitches\n\n")
    print(generate_formatted_hit_table(df, pitcher))
    
    # Horizontal rule
    cat("\n---\n\n")
  }
}

interactive_heatmaps_updated <- function(df, pitcher_name) {
  # Manually defined pitch colors based on your styling
  pitch_colors <- c(
    "Splitter" = "#FFFFCC",
    "Curveball" = "orange",
    "Cutter" = "green",
    "ChangeUp" = "purple",
    "Slider" = "red",
    "Fastball" = "blue",
    "Sinker" = "pink",
    "FourSeamFastball" = "lightgreen"
  )

  pitcher_df <- df %>% filter(Pitcher == pitcher_name)
  pitch_types <- unique(pitcher_df$TaggedPitchType)
  plots <- list()

  for (pitch in pitch_types) {
    data_filtered <- pitcher_df %>% filter(TaggedPitchType == pitch)

    p_plot <- ggplot(data_filtered, aes(
      x = PlateLocSide,
      y = PlateLocHeight,
      color = TaggedPitchType,
      text = paste(
        "Pitch Call:", PitchCall, "<br>",
        "Play Result:", PlayResult, "<br>",
        "Exit Speed:", ExitSpeed, "<br>",
        "Angle:", Angle
      )
    )) +
      annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black", alpha = 1) +
      geom_point(size = 2.8) +
      scale_color_manual(values = pitch_colors) +
      ylim(0, 5) + xlim(-2.7, 2.7) +
      theme_bw() +
      labs(
        title = pitch, 
        x = "Plate Loc Side", y = "Plate Loc Height",
        color = "Pitch Type"
      ) +
      theme(legend.key = element_blank())

    plots[[pitch]] <- ggplotly(p_plot, width = 700, height = 800) %>%
      layout(hoverlabel = list(bgcolor = "black"))
  }

  tagList(plots)
}
