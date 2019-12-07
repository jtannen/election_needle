library(tidyverse)
select <- dplyr::select

source("needle_util.R")

#####################
## SOURCE SPECIFIC
#####################

get_download_link <- function(){
  # results_site <- readLines("http://phillyelectionresults.com/ExportFiles.html")
  # link_grep <- "http://getphillyelectionresults\\.com/export/PRECINCT_[0-9]+_H[0-9]+_M[0-9]+_S[0-9]+\\.txt"
  # link_line <- grep(link_grep, results_site, value = TRUE)
  # link <- gsub('.*href=\\\"(.*)\\\" download.*', "\\1", link_line)
  # filename <- gsub("http://getphillyelectionresults.com/export/(.*)", "\\1", link)
  link <- "https://files7.philadelphiavotes.com/election-results/2019_GENERAL/enr/division-level-results.csv"  
  filename <- sprintf("division_level_results_%s.csv", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"))
  return(list(link=link, filename=filename))  
}

read_download <- function(file){
  colnames <- read_csv(file, n_max = 1, col_names=F) %>% unlist()
  candidates <- read_csv(file, n_max = 1, skip=1, col_names=F) %>% unlist()
  
  df <- read_csv(file, skip=2, col_names=F) %>%
    rename(warddiv=X3) %>%
    select(-X1, -X2) %>%
    pivot_longer(values_to="votes", names_to="key", -warddiv) %>%
    filter(!is.na(votes)) %>% ##only removes X119
    mutate(key = asnum(gsub("^X", "", key))) %>%
    mutate(office = colnames[key], candidate=candidates[key]) %>%
    mutate(
      party = gsub("(.*) \\((.*)\\)$", "\\2", candidate),
      candidate = gsub("(.*) \\(.*\\)$", "\\1", candidate),
      office = ifelse(office == "COUNCIL AT-LARGE", "COUNCIL AT LARGE", office),
      office = ifelse(candidate %in% c('JUDY MOORE', "BRIAN O'NEILL"), "DISTRICT COUNCIL-10TH DISTRICT", office)
    ) %>% 
    select(warddiv, office, candidate, party, votes) %>%
    filter(warddiv != "COUNTY TOTALS")

  return(df)
}


##################
## Download/Load
##################
get_destination_file <- function(){
  sprintf("raw_data/results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

download_election_results <- function(verbose=TRUE) {
  link_list <- get_download_link()
  attach(link_list)
  
  if(filename %in% list.files("raw_data")){
    if(verbose) print(sprintf("%s already downloaded.", filename))
    return(NULL)
  }
  
  download.file(link, get_destination_file())
  
  if(verbose) print(sprintf("%s downloaded.", destfile))
  return(destfile)
}


load_data <- function(file){
  df <- read_download(file)
  df %<>% 
    filter_divs_and_offices() %>%
    format_columns()
  
  return(df)
}


format_candidate_name <- function(x){
  x <- gsub("^[0-9]+\\-", "", x)
  x <- gsub("(.*)\\,.*", "\\1", x)
  x <- format_name(x)
  # x <- gsub("(\\B)([A-Z]+\\b)", "\\U\\1\\L\\2", x, perl = TRUE)
  x <- ifelse(x == "Diberardinis", "DiBerardinis", x)
  return(x)
}

filter_divs_and_offices <- function(df){
   returns <- df %>% 
     group_by(warddiv) %>%
     summarise(sum_votes = sum(votes)) %>%
     group_by() %>%
     filter(sum_votes > 0)
   
   df <- df %>% inner_join(returns %>% dplyr::select(warddiv))
   
   if(nrow(df) == 0) stop("No data yet!")
   
   df <- df %>% 
     filter(office %in% CONFIG$offices) %>%
     filter(!candidate %in% c("Write-In", "Write In"))
   
  return(df)     
} 

format_columns <- function(df) {
  df <- df %>% 
    mutate(candidate = format_candidate_name(candidate)) %>%
    group_by(warddiv, office) %>% 
    mutate(pvote = votes / sum(votes)) %>%
    group_by()
  return(df)
}
