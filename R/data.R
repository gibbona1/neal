#' This is data to be included in my package
#'
#' @name bto_codes
#' @docType data
#' @description
#' The British Trust for Ornithology has a list of 2-letter species codes for over 250 bird species. 
#' The dataset acts as a lookup table, matching species names with their corresponding BTO codes.
#' @keywords data
#' @references \url{ https://www.bto.org/sites/default/files/u16/downloads/forms_instructions/bto_bird_species_codes.pdf}
NULL
#' This is data to be included in my package
#'
#' @name location_list
#' @docType data
#' @description
#' This has a list of recorders that were deployed along with accompanying metadata about the sites studied. 
#' If the current audio file in the above format matches a recorder in the list, 
#' the following columns of the file are passed to the metadata tab:
#' \itemize{
#'    \item recorder name: prefix for audio file names recorded by this device
#'    \item lat: latitude of the recorder for the study period, in decimal degrees
#'    \item long: longitude of the recorder for the study period, in decimal degrees
#'    \item location name: name of the study site
#'    \item location county: county of study site
#'    \item habitat type: primary habitat type of the study site
#'    \item dist to coastline: approximate distance to the nearest coastline in kilometres
#' }
#' If any of this information is unavailable, the column name in the metadata panel will still appear but the
#' body of text will be blank. Extra columns can be added to the file, where they will be printed verbatim to
#' the metadata panel.
#' @keywords data
NULL
#' This is data to be included in my package
#'
#' @name species_list
#' @docType data
#' @description
#' NEAL allows for multiple species lists to be included, all of which are stored in species_list.csv.
#' The file has one column for each site being studied, with the first entry of each column being the site
#' name. Columns can have different numbers of rows (species), i.e. all those one expects to find at the
#' given site. The species names can be stored in the column in any order as they are sorted alphabetically at
#' run time. Columns can be appended to it by uploading via a widget in the Configuration tab. Each of the
#' columns in the uploaded data not already present in the species data are appended.
#' @keywords data
NULL
