library(hrbrthemes)
library(extrafont)
windowsFonts("Arial Narrow" = windowsFont("Arial Narrow"))

# FIGURE PREFERENCES

colors_focal <- c(`TRUE` = "darkorange3", `FALSE` = "grey75")


#https://coolors.co/fff8f0-95190c-006494-51355a-2a0c4e
colors_pty <- c(`-1` = "#006494", `0` = "#51355A", `1` = "#95190C")
colors_pty_fct <- c(`Democrat` = "#4472C4", `Independent/Other` = "grey50", `Republican` = "#ED7D31")
agree_cols <- c("#440154FF", "#46337EFF", "#4AC16DFF", "#9FDA3AFF")


# LABELS ---------------------

lab_assignment <- "Data Projects"

labs_media <- c("ads" = "Ads Project", "news" = "News Project")

labs_course_code <- c(
  "POS3713-0002.sp21"  = "Research Methods, Spring 2021",
  "POS3931-0001.fa21"  = "Social Influence, Fall 2021",
  "POS4235-0001.fa21"  = "Media & Politics, Fall 2021",
  "POS4235-0001.sp21"  = "Media & Politics, Spring 2021"
)

# GGPLOT THEMES ---------------
box_color <- "grey92"
col_lines = "grey33"


theme_paper <- function(..., center = TRUE,
                        base_size = 12,
                        axis_title_size = 10,
                        grid = TRUE
                        ){

  # center axis titles?
  atj <- ifelse(center, "cc", "rt")

  grid_axis_col <- "grey90"

  theme_ipsum(
    base_size = base_size,
    plot_title_size = 14,
    plot_title_face = "bold",
    plot_title_margin = 10,
    subtitle_size = 12,
    subtitle_face = "plain",
    subtitle_margin = 15,
    strip_text_size = 13,
    caption_size = 10,
    caption_face = "plain",
    caption_margin = 10,
    axis_title_size = axis_title_size,
    axis_title_face = "plain",
    strip_text_face = "bold",
              axis_title_just = atj,
              plot_margin = margin(5, 5, 5, 5),
              grid_col = grid_axis_col,
              axis_col = grid_axis_col,
              grid = grid, axis = FALSE, ticks = FALSE,
              ...)
}
