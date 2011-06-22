GetData <- function(url, api_key, format) {
  res <- getURL(url, userpwd=api_key)
  if(tolower(format) == "json") {
    res <- fromJSON(res)
  } else if(tolower(format) == "xml") {
    res <- XMLToList(res)
  }
  return(res)
}
