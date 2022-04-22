library(dplyr)
library(httr)


baidu_json_scrape <- function(area, baidu_base = "https://voice.baidu.com/newpneumonia/getv2") {
  # --- Handle User Agent spoofing
  user_agents <- c(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.132 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:73.0) Gecko/20100101 Firefox/73.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.0.5 Safari/605.1.15",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36 Edge/16.16299",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:63.0) Gecko/20100101 Firefox/63.0"
  )

  # Randomly select a user agent for this session
  user_agent <- sample(user_agents, 1, TRUE)

  # Change the user agent and save the old one so we can revert on close
  httr::user_agent(user_agent)

  query_url <- sprintf("%s?from=mola-virus&stage=publish&target=trendCity&area=%s", baidu_base, area)

  res <- GET(query_url)

  parsed_content <- content(res, as = "parsed", simplifyVector = TRUE, encoding = "UTF-8")

  if (res$status_code != 200) {
    # Early-return if we encountered an error
    warning(sprintf("%s: Returned status %d", area, res$status_code))

    return(NULL)
  }

  return(parsed_content)
}

baidu_json_parse <- function(json_data) {
  time_series_data <- setNames(
    json_data[["data"]][["trend"]][["list"]][[1]][["data"]],
    json_data[["data"]][["trend"]][["list"]][[1]][["name"]]
    )

  date_stamps <- unlist(json_data[["data"]][["trend"]][["updateDate"]])

  area_name <- json_data[["data"]][["name"]]

  
  out <- tibble(
    area_zh = area_name,
    date = date_stamps,
    !!!time_series_data
  )

  return(out)
}

# Write a wrapper around the function
# that adds debounce
debouce_scrape <- function(city, time = 2) {
  Sys.sleep(time)

  out <- baidu_json_scrape(city)

  return(out)
}

# Write a handler for the case where this doesn't work
# and just return NULL and throw a warning
attempt_parse <- function(data) {
  
  out <- tryCatch(
    baidu_json_parse(data),
    error = function(e) {
      warning(as.character(e))
      return(NULL)
    })
  
  return(out)
}