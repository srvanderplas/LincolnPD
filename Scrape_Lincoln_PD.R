library(tidyverse)
library(rvest)
library(lubridate)


base_url <- "https://www.lincoln.ne.gov/city/police/stats/acc.htm"

get_reports_date <- function(date) {
  search_date <- format.Date(date, "%m-%d-%Y") %>%
    str_remove("^0")

  body_str <- paste0("CGI=DISK0%3A%5B020020.WWW%5DACCDESK.COM&rky=&date=", search_date)
  res <- httr::POST(
    url = "HTTPS://CJIS.LINCOLN.NE.GOV/HTBIN/CGI.COM",
    config = httr::add_headers(.headers = c(Host="cjis.lincoln.ne.gov",
                                            `User-Agent` = "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0",
                                            Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                            `Accept-Language` = "en-US,en;q=0.5",
                                            `Accept-Encoding` = "gzip, deflate, br",
                                            `Content-Type` = "application/x-www-form-urlencoded",
                                            `Content-Length` = nchar(body_str),
                                            Origin = "https://www.lincoln.ne.gov",
                                            DNT = 1,
                                            Connection = "keep-alive",
                                            Referer = "https://www.lincoln.ne.gov/city/police/stats/acc.htm",
                                            `Upgrade-Insecure-Requests` = 1)),
    body = body_str)

  res_html <- read_html(res$content)
  tab <- html_node(res_html, "table")

  # Get rid of span column
  colspan <- tab %>% xml_find_all(xpath = "//*[@colspan='6']")
  xml_remove(colspan)

  # Get rid of empty rows
  empty_nodes <- html_nodes(tab, xpath = "//tr[normalize-space(.)='']")
  xml_remove(empty_nodes)

  # Fix tickets
  tickets <- html_nodes(tab, xpath = "//input[@type='SUBMIT']")
  values <- html_attr(tickets, "value")
  xml_set_text(tickets, values)

  html_table(tab) %>%
    set_names(make.names(names(.)) %>% str_remove("\\W$")) %>%
    mutate(Time = paste(Date, sprintf("%04d", Time)) %>%
             str_replace(" 9999", " 0000") %>%
             mdy_hm,
           Date = mdy(Date, tz = "America/Chicago"),
           Accident.Type = str_replace(Accident.Type, "^\\s{0,}$", NA_character_),
           Tickets = str_replace(Tickets, "None", NA_character_))
}

get_tickets <- function(case) {

  body_str <- paste0("CGI=DISK0%3A%5B020020.WWW%5DACCDESK.COM&TICK=+", str_trim(case), "+")

  res <- httr::POST(
    url = "HTTPS://CJIS.LINCOLN.NE.GOV/HTBIN/CGI.COM",
    config = httr::add_headers(.headers = c(Host="cjis.lincoln.ne.gov",
                                            `User-Agent` = "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0",
                                            Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                                            `Accept-Language` = "en-US,en;q=0.5",
                                            `Accept-Encoding` = "gzip, deflate, br",
                                            `Content-Type` = "application/x-www-form-urlencoded",
                                            `Content-Length` = nchar(body_str),
                                            Origin = "https://www.lincoln.ne.gov",
                                            DNT = 1,
                                            Connection = "keep-alive",
                                            Referer = "https://www.lincoln.ne.gov/city/police/stats/acc.htm",
                                            `Upgrade-Insecure-Requests` = 1)),
    body = body_str)

  get_person <- function(node) {
    node %>%
      xml_parent() %>% html_children() %>% `[`(2) %>% html_text()
  }

  get_ticket <- function(node) {
    num <- node %>% html_text() %>% str_trim()
    offense_date <- node %>% xml_parent() %>% html_children() %>% `[`(2) %>% html_text() %>% str_trim() %>% mdy()
    arraignment <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("th:contains('Arraignment')") %>% xml_parent() %>% xml_text() %>% str_trim() %>% str_remove("Arraignment date/time: ")
    charge_num <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("td:contains('Charge#')") %>% xml_text() %>% str_trim() %>% str_extract("\\d{1,}")
    citations <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("tr:contains('Cited for:')") %>% xml_find_all("td[4]") %>% xml_text() %>% str_trim()
    charges <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("tr:contains('Charged with:')") %>% xml_find_all("td[3]") %>% xml_text() %>% str_trim()
    amendments <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("tr:contains('Amended to:')") %>% xml_find_all("td[3]") %>% xml_text() %>% str_trim()
    disposition <- node %>% xml_parent() %>% xml_parent() %>% xml_nodes("tr:contains('DISPOSITION:')") %>% xml_text() %>% str_remove("DISPOSITION:") %>% str_trim()

    tibble(ticket_num = num, offense_date = offense_date, arraignment = arraignment,
           charge = charge_num, charge_desc = charges,
           citations = citations, amendments = amendments, disposition = disposition)
  }

  res_html <- read_html(res$content)
  tab <- html_node(res_html, "table")
  person <- html_nodes(tab, css = "th:contains('Person cited:')") %>% purrr::map_chr(get_person)
  ticket <- html_nodes(tab, css = "td:contains('LB')") %>% purrr::map(get_ticket)

  tibble(person = person, ticket = ticket)
}

days <- seq.Date(ymd("2017-1-1"), Sys.Date(), by = "day")

get_reports_safely <- safely(get_reports_date)
traffic_violations <- purrr::map(days, get_reports_safely)

traffic_violations_df <- map_df(traffic_violations, "result")
traffic_violations_df %>% group_by(Date) %>% count %>% ggplot(aes(x = Date, y = n)) + geom_line() + scale_y_continuous("Number of Traffic Accidents in Lincoln")

traffic_violations2 <- purrr::map(seq.Date(ymd("2017-1-1", "2018-12-31"), by = "day"), get_reports_safely) %>% map_df("result")

traffic_violations_df2 <- map_df(traffic_violations2, "result")
traffic_violations <- bind_rows(traffic_violations_df2, traffic_violations_df)

traffic_violations %>% group_by(Date) %>% count %>% ggplot(aes(x = Date, y = n)) + geom_line() + scale_y_continuous("Number of Traffic Accidents in Lincoln")

write_csv(traffic_violations, "2017-present.csv")
