# preparation -------------------------------------------------------------

# unzip "03_replication_files.zip"
# processing time: less than 1 second
unzip("03_replication_files.zip")


# general setup
# processing time: 9.3 seconds
source(
  "03_replication_files/01_preparation/01_setup.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# clean raw data from public whip
# processing time: 52.9 seconds
source(
  "03_replication_files/01_preparation/02_clean_public_whip.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# clean raw data from british election study
# processing time: 7.6 minutes
source(
  "03_replication_files/01_preparation/03_clean_bes_ip.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# import clean datasets
# processing time: 3.5 seconds
source(
  "03_replication_files/01_preparation/04_import_datasets.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# analysis: main text -----------------------------------------------------

# create table 1
# processing time: 3.7 seconds
source(
  "03_replication_files/03_analysis/01_table_1.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure 1
# processing time: 4.8 hours
source(
  "03_replication_files/03_analysis/02_figure_1.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure 2
# processing time: 1.1 minutes
source(
  "03_replication_files/03_analysis/03_figure_2.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# analysis: appendix ------------------------------------------------------


# create table a1
# processing time: 2.7 seconds
source(
  "03_replication_files/03_analysis/04_table_a1.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure a1
# processing time: 1 second
source(
  "03_replication_files/03_analysis/05_figure_a1.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure a2
# processing time: 0.5 seconds
source(
  "03_replication_files/03_analysis/06_figure_a2.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure a3
# processing time: 0.6 seconds
source(
  "03_replication_files/03_analysis/07_figure_a3.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure a4
# processing time: 0.9 seconds
source(
  "03_replication_files/03_analysis/08_figure_a4.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a2
# processing time: 1.3 seconds
source(
  "03_replication_files/03_analysis/09_table_a2.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a3
# processing time: 1.3 seconds
source(
  "03_replication_files/03_analysis/10_table_a3.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a4
# processing time: 3.5 seconds
source(
  "03_replication_files/03_analysis/11_table_a4.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a5
# processing time: 4.9 seconds
source(
  "03_replication_files/03_analysis/12_table_a5.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a6
# processing time: 4.7 seconds
source(
  "03_replication_files/03_analysis/13_table_a6.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a7
# processing time: 18.4 seconds
source(
  "03_replication_files/03_analysis/14_table_a7.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a8
# processing time: 2.8 seconds
source(
  "03_replication_files/03_analysis/15_table_a8.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a9
# processing time: 3 seconds
source(
  "03_replication_files/03_analysis/16_table_a9.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a10
# processing time: 3.5 seconds
source(
  "03_replication_files/03_analysis/17_table_a10.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create figure a5
# processing time: 13.5 seconds
source(
  "03_replication_files/03_analysis/18_figure_a5.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# create table a11
# processing time: 2.5 seconds
source(
  "03_replication_files/03_analysis/19_table_a11.R",
  echo = TRUE,
  max.deparse.length = Inf
)

# estimate statistical claims in the text
# processing time: less than 1 second
source(
  "03_replication_files/03_analysis/20_intext_claims.R",
  echo = TRUE,
  max.deparse.length = Inf
)


# end: 02_main_script.R ---------------------------------------------------