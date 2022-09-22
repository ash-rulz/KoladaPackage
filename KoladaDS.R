library(httr)

#Create the URL
# base_url <- "https://api.kolada.se/v2/data/kpi/N00951/municipality/0580/year/2020,2019,2018?format=json"
base_url <- "https://api.kolada.se/v2/data/kpi/N00951/municipality/0580/year/2020,2019,2018"
# base_url <- "https://api.kolada.se/v2/data/kpi/N00923/year/2010?format=json"
#Call the Request
kolada_response <- GET(base_url)

#Check if the response is of the right type
if(http_type(kolada_response) != "application/json"){
  print("Response not in JSON")
}else{
  print("Response in JSON")
}

#Parse the response
#library(jsonlite)
parsed <- jsonlite::fromJSON(content(kolada_response, "text"),
                   simplifyVector = FALSE)
api_char <- base::rawToChar(kolada_response$content)
api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
api_JSON

str(parsed)
 
#values is the nested list. Convert that to a data frame via rbind
as.data.frame(do.call(rbind, parsed["values"]))


