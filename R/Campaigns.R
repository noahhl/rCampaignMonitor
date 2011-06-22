GetCampaignRecipients <- function(campaign, api_key, limit="ALL", order_by="email", order_dir="asc", format="json") {
  if(tolower(limit)=="all")
    limit <- 999999999999999
  SubsToGet <- GetCampaignInfo(campaign, api_key, format)$Recipients
  page <- 1 ; pagesize <- min(1000, max(10,limit, SubsToGet)) ; results <- c()
  while(min(SubsToGet, limit) >= (page-1)*pagesize) {
    url=paste("http://api.createsend.com/api/v3/campaigns/",campaign, "/recipients.", format, "?page=",page, "&pagesize=", pagesize, "&orderfield=", order_by, "&orderdirection=", order_dir, sep="")
    results <- c(results, GetData(url, api_key, format)$Results)
    page <- page+1
  }
  return(results[1:min(limit, length(results))])
}


GetCampaignInfo <- function(campaign, api_key, format="json") {
  url = paste("http://api.createsend.com/api/v3/campaigns/", campaign, "/summary.", format, sep="")
  return(GetData(url, api_key, format))
}

GetSentCampaigns <- function(client, api_key, format="json") {
  url = paste("http://api.createsend.com/api/v3/clients/", client, "/campaigns.", format, sep="")
  return(GetData(url, api_key, format))
}

GetDraftCampaigns <- function(client, api_key, format="json") {
  url = paste("http://api.createsend.com/api/v3/clients/", client, "/drafts.", format, sep="")
  return(GetData(url, api_key, format))
}