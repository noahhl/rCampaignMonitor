GetLists <- function(ClientID, api_key, format="json") {
  url = paste("http://api.createsend.com/api/v3/clients/", ClientID, "/lists.", format, sep="")
  return(GetData(url, api_key, format))  
}

GetListInfo <- function(listid, api_key, format="json") {
  url = paste("http://api.createsend.com/api/v3/lists/", listid, "/stats.", format, sep="")
  return(GetData(url, api_key, format))
}

