ListSegments <- function(account, api_key) {
  segments <- fromJSON(getURL(paste("http://api.createsend.com/api/v3/lists/",account,"/segments.json", sep=""), userpwd=api_key))  
  return(segments)
}




GetSegmentMembers <- function(segment, since="2000-01-01", limit="ALL", order_by="email", order_dir="asc", format="json", api_key=key) {
  if(tolower(limit)=="all")
    limit <- 999999999999999
  SubsToGet <- GetSegmentInfo(segment, format, api_key)$ActiveSubscribers
  page <- 1 ; pagesize <- min(1000, max(10,limit, SubsToGet)) ; results <- c()
  while(min(SubsToGet, limit) >= (page-1)*pagesize) {
    url=paste("http://api.createsend.com/api/v3/segments/",segment, "/active.", format, "?date=", since, "&page=",page, "&pagesize=", pagesize, "&orderfield=", order_by, "&orderdirection=", order_dir, sep="")
    results <- c(results, fromJSON(getURL(url, userpwd=api_key))$Results)
    page <- page+1

  }
  return(results[1:min(limit, length(results))])
}

GetSegmentInfo <- function(segment, format="json", api_key=key) {
  url = paste("http://api.createsend.com/api/v3/segments/", segment, ".", format, sep="")
  res <- getURL(url, userpwd=api_key)
  if(format == "json") {
    res <- fromJSON(res)
  }
  return(res)
}
