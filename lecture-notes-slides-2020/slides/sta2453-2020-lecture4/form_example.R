

library(rvest)
library(dplyr)


url = "https://ca.indeed.com/"

html <- url %>% 
  html_session()

form_blank <- url %>% 
  read_html() %>% 
  html_form()
  
form_filled = form_blank[[1]] %>%
  set_values(
    "q" = "writing",
    "l" = "Edmonton, AB") 

submitted <- html %>% 
  submit_form(form_filled)


submitted %>% 
  html_nodes(".title") %>% 
  #html_text() %>% 
  rvest::html_attr("title")

url <- 'https://ca.indeed.com/'

//tagname[@attribute = “value“]

job_title <- submitted %>% 
  rvest::html_nodes("div") %>%
  rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>%
  rvest::html_attr("title")

submitted %>% 
  html_nodes('.title.jobtitle turnstileLink')


form_url <- url %>% 
  html_session()

form_blank <-  form_url %>% 
  read_html() %>% 
  html_form()


str(form_blank)

form_filled <- form_blank[[1]] %>%
  set_values(
    "input" = "Paignton",
    "date" = "22.11.17",
    "time" = "19:58") 
submitted = submit_form(html,form_filled)

jobs <- submitted %>% 
  rvest::html_nodes("div") %>%
  rvest::html_nodes(xpath = '//*[@data-tn-component = "organicJob"]')

jobs %>% 
  rvest::html_attrs('')

jobs %>%
  rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>% 
  html_attrs()


submitted %>% 
  rvest::html_nodes('[class="location"]') %>%
  rvest::html_attr("title")



submitted %>% 
  rvest::html_nodes(".location") %>%
  rvest::html_text()

submitted %>% 
  rvest::html_nodes(".company") %>%
  rvest::html_text() %>%
  stringi::stri_trim_both()


# job links
submitted %>% 
  rvest::html_nodes("div") %>%
  rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
  rvest::html_attr("href")

# get links CSS selectors
submitted %>% 
  rvest::html_nodes('[data-tn-element="jobTitle"]') %>%
  rvest::html_attr("href")


# job description
# get job description xpath
submitted %>%
  rvest::html_nodes("span")  %>% 
  rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()

# get job description CSS selector
submitted %>%
  rvest::html_nodes('.summary') %>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()


submitted_url <- submitted$url
big_number <- '&start=30000'
full_url <- paste0(submitted_url, big_number)

pages <- full_url %>% 
  read_html() %>% 
  html_nodes("#resultsCol > nav > div > ul> li") %>% 
  html_text()

max_page <- as.numeric(pages[length(pages)])*10






page_result_start <- 10 # starting page 
page_result_end <- max_page # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)

full_df <- data.frame()
for(i in seq_along(page_results)) {
  
  first_page_url <- submitted_url
  url <- paste0(first_page_url, "&start=", page_results[i])
  page <- xml2::read_html(url)
  # Sys.sleep pauses R for two seconds before it resumes
  # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  #get the job title
  job_title <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
    rvest::html_attr("title")
  
  #get the company name
  company_name <- page %>% 
    rvest::html_nodes("span")  %>% 
    rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
    rvest::html_text() %>%
    stringi::stri_trim_both() -> company.name 
  
  
  #get job location
  job_location <- page %>% 
    rvest::html_nodes("span") %>% 
    rvest::html_nodes(xpath = '//*[@class="location"]')%>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()
  
  # get links
  links <- page %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
    rvest::html_attr("href")
  
  job_description <- c()
  for(i in seq_along(links)) {
    
    url <- paste0("https://ca.indeed.com/", links[i])
    page <- xml2::read_html(url)
    
    job_description[[i]] <- page %>%
      rvest::html_nodes("span")  %>% 
      rvest::html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
  }
  df <- data.frame(job_title, company_name, job_location, job_description)
  full_df <- rbind(full_df, df)
}

df_Vancouver <- full_df %>%
  dplyr::distinct() %>%
  dplyr::mutate(city = "Vancouver")

df_Montreal <- full_df %>%
  dplyr::distinct() %>%
  dplyr::mutate(city = "Montreal")

df_Toronto <- full_df %>%
  dplyr::distinct() %>%
  dplyr::mutate(city = "Toronto")

df_Canada <- rbind(df_Vancouver, df_Toronto, df_Montreal)
write.csv(df_Canada, "df_Canada.csv")

# some cleaning
df_Canada$job_description <- gsub("[\r\n]", "", df_Canada$job_description)

df_Canada <- read.csv(here::here("df_Canada.csv"))

