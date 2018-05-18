#####

library(rvest)
library(data.table)

### 2018 ###

getCheapestPrice <- function(dates = c("2018-07-14", "2018-07-26"), session = NULL) {
#  url <- "https://book.austrian.com/app/fb.fly?action=avail&pos=BE&l=en&origin=BRU&destin=KRR&mode=date&numadt=2&numchd=2&numinf=0&day0=25&month0=07&year0=2018&day1=06&month1=08&year1=2018&journey=2"
  baseUrl <- "https://book.austrian.com/app/fb.fly"
  url <- paste0(
      "https://book.austrian.com/app/fb.fly?action=avail&pos=BE&l=en&origin=BRU&destin=KRR&numadt=2&numchd=2&numinf=0&journey=2&mode=date",
      "&day0=", mday(dates[[1]]), "&month0=", month(dates[[1]]), "&year0=", year(dates[[1]]), 
      "&day1=", mday(dates[[2]]), "&month1=", month(dates[[2]]), "&year1=", year(dates[[2]])      
  )
  session <- if (!is.null(session)) jump_to(session, url) else 
        html_session(baseUrl) %>% jump_to(url)
  
  doc <- read_html(session)
  
  outb <- doc %>% 
      html_node("fieldset.outbound div.cheapest_flight_container + a > h5.tariff_name > span.default_content > strong") %>% 
      html_text(trim = TRUE) %>% as.numeric
  inb <- doc %>% 
      html_node("fieldset.inbound div.cheapest_flight_container + a > h5.tariff_name > span.default_content > strong") %>% 
      html_text(trim = TRUE) %>% as.numeric
  
  estPrice <- round((inb+outb)*3.8)

  flightInfo <- getFlightInfo(doc = doc)

  out <- list(price = estPrice, dates = dates, url = url, flights = flightInfo) # session = session)
  class(out) <- "priceDoc"
  out
}

print.priceDoc <- function(x, ...) {
  cat(paste(months(as.Date(x$dates)), mday(x$dates), collapse = " \u2014 "), ": ", x$price, sep = "")
}

getPriceDF <- function(datesDF) {
  session <- html_session("https://book.austrian.com/app/fb.fly")
  
  allRes <- list()
  for (iRow in seq_len(nrow(datesDF))) {
    res <- getCheapestPrice(dates = unname(unlist(datesDF[iRow, ])), session = session)
    allRes[[iRow]] <- res
    cat(iRow, ": ")
    print(res)
    cat("\n")
    saveRDS(allRes, "allres.rds") # in case it breaks, still keep what was saved
    Sys.sleep(runif(1)*5)
  }  
  
  list(
      df = cbind(datesDF, price = sapply(allRes, `[[`, "price")), 
      res = allRes
  )
  
}

getFlightInfo <- function(res = NULL, doc = NULL, session = NULL) {
  if (!is.null(doc)) {
    # continue
  } else if (!is.null(res)) {
    doc <- read_html(res$session)
  } else if (!is.null(session)) {
    doc <- read_html(session)
  } else 
    return(NULL)
  
  cbind(
      leg = doc %>% html_nodes("fieldset > div.row  div.flight_info div.flight_time_block") %>% 
          html_text(trim = TRUE) %>% gsub("\\s{2,}", " ", .) %>% 
          gsub("BRU KRR", ">", .) %>% gsub("KRR BRU", "<", .),
      stop = doc %>% html_nodes("fieldset > div.row  div.flight_info div.stop_block p.small") %>% html_text,
      price = doc %>% html_nodes("fieldset > div.row  div.flight_info div.flight_economy strong")%>%html_text(trim = TRUE)
  )
}

