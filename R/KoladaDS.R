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
  final_df <- as.data.frame(output_tibble)
  final_df <- cbind(final_df[,1:5], value = as.integer(final_df[,7]))
  
  #Get the municipality master data
  kolada_response <- GET('https://api.kolada.se/v2/municipality')
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
  
  api_char <- base::rawToChar(as.raw(strtoi(kolada_response$content, base = 16L)))
  Encoding(api_char) <- "UTF-8"
  api_JSON <- jsonlite::fromJSON(api_char)
  mun_master_df <- as.data.frame(api_JSON$values)
  
  final_kolda_lst <- list("MunMaster" = mun_master_df, "FinalData" = final_df)
  return(final_kolda_lst)
}

get_kolda_data('kpi/n60026/year/2020,2019,2018')
