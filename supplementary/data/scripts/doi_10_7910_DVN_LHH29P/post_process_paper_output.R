# Package setup
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org"); library(pacman)
pacman::p_load(yaml, dplyr, testthat, argparse, stringr, here, tidytable, glue)

# Working directory and command line argument setup
here::i_am("generate_tables_and_figures/post_process_paper_output/src/post_process_paper_output.R")
source(here("R", "project_functions.R"))
cl_args <- parse_make_args(c(
  "CONFIG_FILE",
  "PAPER_OUT_DIR"
))

# ---

RAW_PAPER_OUT_DIR = cl_args$PAPER_OUT_DIR
FINAL_PAPER_OUT_DIR = gsub('/raw', '/final', RAW_PAPER_OUT_DIR)
config <- read_yaml(cl_args$CONFIG_FILE)


figure_files = list.files(RAW_PAPER_OUT_DIR, pattern='.jpg|.png|.pdf')
for (figure_file in figure_files) {

  original_figure_file = glue("{RAW_PAPER_OUT_DIR}/{figure_file}")
  final_figure_file = glue("{FINAL_PAPER_OUT_DIR}/{gsub('_up_to_date', '', figure_file)}")

  file.copy(from = original_figure_file, to = final_figure_file, overwrite = T)
}



tex_files = list.files(RAW_PAPER_OUT_DIR, pattern='.tex')
for (tex_file in tex_files) {

  table = gsub('_up_to_date.tex', '', tex_file)

  line_spacing = config$default_line_spacing
  if (table %in% names(config$line_spacing_lookup)) {
    line_spacing = config$line_spacing_lookup[[table]]
  }

  tex_file_lines <- readLines(glue("{RAW_PAPER_OUT_DIR}/{tex_file}"))
  tex_file_lines <- gsub("Total Social Cost of Crime", "\\\\textbf{Total Social Cost of Crime}", tex_file_lines)
  tex_file_lines <- gsub("Predicted Involvement in a Violent Gun Crime \\(Risk Score\\)",
                         "Predicted Involvement in a Violent \\\\\\\\ \\\\quad \\\\quad Gun Crime \\(Risk Score\\)",
                         tex_file_lines)
  first_line = tex_file_lines[1]

  if (!grepl('selectfont', first_line)) {
    print(glue("Unable to automatically update line spacing for {tex_file}."))
  }

  pattern = '\\}\\{[0-9.]+\\}'
  replacement = paste0('}{', line_spacing, '}')

  fixed_first_line = gsub(pattern, replacement, first_line)

  fixed_tex_file_lines = c(fixed_first_line, tex_file_lines[-1])

  final_tex_filepath = glue("{FINAL_PAPER_OUT_DIR}/{table}.tex")
  writeLines(fixed_tex_file_lines, final_tex_filepath)

}

print("DONE")
