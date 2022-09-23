library(httr)
library(jsonlite)
library(tidyr)
#' Title
#'
#' @param url_path 
#'
#' @return
#' @export
#' @import httr
#' @import tidyr
#' @import jsonlite
#' @examples
get_kolda_data <- function(url_path){
  # base_url <- "https://api.kolada.se/v2/data/kpi/n60026/year/2020,2019,2018" #Total Monthly salary
  base_url <- "https://api.kolada.se/v2/data/"
  final_url <- paste(base_url, url_path, sep = "")
  
  #Call the Request
  kolada_response <- GET(final_url)

  #Check if the response is of the JSON type
  if(http_type(kolada_response) != "application/json")
    stop("Response not in JSON")
  
  #Parse the response
  parsed <- jsonlite::fromJSON(content(kolada_response, "text"),
                               simplifyVector = FALSE)
  
  #Check if the response is good
  if(http_error(kolada_response)){
    stop(paste("The request errored out with the status:",
               kolada_response$status_code, 
               ".\n Error Message:", parsed$message))
  }
  
  api_char <- base::rawToChar(kolada_response$content)
  api_JSON <- jsonlite::fromJSON(api_char)
  #Unnest the values list which contains data frames
  output_tibble <- unnest(as_tibble(api_JSON$values),cols = c('values'))
  return(as.data.frame(output_tibble))
}

get_kolda_data('kpi/n60026/year/2020,2019,2018')
