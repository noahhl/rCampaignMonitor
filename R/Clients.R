GetClients <- function(api_key, format="json") {
  url <- paste("http://api.createsend.com/api/v3/clients.", format, sep="")
  return(GetData(url, api_key, format))
}