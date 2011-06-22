GetActiveSubscribers <- function(listid, api_key, since="2000-01-01", limit="ALL", order_by="email", order_dir="asc", format="json", verbose=FALSE) {
  if(tolower(limit)=="all")
    limit <- 999999999999999
  SubsToGet <- GetListInfo(listid, api_key, format)$TotalActiveSubscribers
  page <- 1 ; pagesize <- min(1000, max(10,limit, SubsToGet)) ; results <- c()
  while(min(SubsToGet, limit) >= (page-1)*pagesize) {
    url=paste("http://api.createsend.com/api/v3/lists/",listid, "/active.", format, "?date=", since, "&page=",page, "&pagesize=", pagesize, "&orderfield=", order_by, "&orderdirection=", order_dir, sep="")
    results <- c(results, fromJSON(getURL(url, userpwd=api_key))$Results)
    page <- page+1
    if(verbose)
      cat("Page:", page, "Number:", length(results))
  }
  return(results[1:min(limit, length(results))])
}


BulkAddCustomField <- function(subscribers, fieldName, destinationList, api_key, verbose=FALSE) {
  s <- list()
  for(i in 1:nrow(subscribers)) {
    CustomFields <- list()
    CustomFields[[1]] <- c(Key=fieldName, Value=subscribers$Value[i])
    s[[i]] <- list(EmailAddress=subscribers$EmailAddress[i], CustomFields=CustomFields)
  }
  a <- list(Subscribers=s, Resubscribe=FALSE)

  reader <- basicTextGatherer()
  curlPerform(url=paste("http://api.createsend.com/api/v3/subscribers/", destinationList, "/import.json", sep=""), postfields=toJSON(a), writefunction=reader$update, userpwd=api_key, httpheader="Content-type: application/json")
  if(verbose)
    print(fromJSON(reader$value()))
}