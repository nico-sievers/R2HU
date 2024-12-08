% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R2HUconcatenateFlowdata.R
\name{R2HUconcatenateFlowdata}
\alias{R2HUconcatenateFlowdata}
\title{Function to concatenate raw Flow Cytometry data into the standard KOSMOS data sheet layout}
\usage{
R2HUconcatenateFlowdata(
  working_directory = "./",
  rawfile_folder = "export/set-statistics/",
  rawfile_identifier = "set_statistics_",
  include_easyclus_metadata = FALSE,
  easyclus_metadata_filename = "KOSMOS_Kiel_spring_2024_metadata-cleaned-EasyClus.csv",
  writetofile = TRUE,
  concatenatedfile_name = "concatenated_clean_data.csv",
  writetoexcel = FALSE,
  excelfile_name = "KOSMOS_Kiel_spring_2024_FlowCytometry_Sievers_R.xlsx",
  excelfile_sheet = "Main table",
  apply_chlacalibration = FALSE,
  chlacalfile_name = "KOSMOS_Kiel_spring_2024_Chlorophyll_a_25_03_2024_JT.xlsx",
  chlacalfile_sheet = "Main table",
  exclude_day_from_biomass = NULL,
  setsinsupergroups = FALSE,
  calculate_percent_change = FALSE,
  baseline_days = 1:3,
  percent_change_vars = c("Concentration [n/µl]", "ChlaProxyRaw", "FLredpercell"),
  apply_lengthcalibration = FALSE
)
}
\arguments{
\item{working_directory, rawfile_folder, rawfile_identifier, writetofile, concatenatedfile_name, writetoexcel, excelfile_name, excelfile_sheet, apply_chlacalibration, setsinsupergroups, apply_lengthcalibration}{\code{(to be updated)}}
}
\value{
Yields a clean data frame of the standard layout and writes to files if selected.
}
\description{
\code{-BETA-}   Ask Nico if you would like to use this function.
}
