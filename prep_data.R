library(tidyverse)
library(MASS)
library(sf)

select <- dplyr::select

source("needle_util.R")
source("svd.R")

SVD_METHOD = "shrinkage"

ELECTION <- "20191105"
CONFIG <- get_config(ELECTION)
USE_LOG <- CONFIG$use_log

set.seed(215)

df_past <- readRDS("../../data/processed_data/df_major_2019-10-17.Rds") %>%
  unite("election", year, election) %>%
  mutate(warddiv = paste0(substr(warddiv,1,2),"-",substr(warddiv,3,4))) %>%
  filter(candidate != "Write In")


divs <- st_read("../../data/gis/201911/Political_Divisions.shp") %>%
  mutate(
    precinct = paste0(
      substr(DIVISION_N,1,2), "-", substr(DIVISION_N,3,4)
    )
  )

setClass(
  "needleSVDs",
  slots=c(
    pvote_svd="SVDParams",
    turnout_svds="list",
    log="logical"
  )  
)

needleSVDs <- function(  
  pvote_svd,
  turnout_svds,
  log=USE_LOG
){
  new(
    "needleSVDs",
    pvote_svd=pvote_svd,
    turnout_svds=turnout_svds,
    log=log
  )
}


get_turnout_svd <- function(election_type, party_grep=NULL, verbose=TRUE){
  if(!is.null(party_grep)){
    df_party <- df_past %>% filter(grepl(party_grep, party))
  } else {
    df_party <- df_past
  }
  df_party <- df_party %>% filter(grepl(paste0(election_type, "$"), election))
  turnout_party <- df_party %>% 
    filter(is_primary_office) %>%
    group_by(warddiv, election) %>%
    summarise(votes = sum(votes)) %>%
    group_by() %>%
    mutate(target = {if(USE_LOG) log(votes + 1) else votes})
  
  turnout_wide <- turnout_party %>%
    select(warddiv, election, target) %>%
    spread(election, target, fill = 0)
  
  turnout_wide_mat <- as.matrix(turnout_wide %>% select(-warddiv))
  row.names(turnout_wide_mat) <- turnout_wide$warddiv
  
  svd <- get_svd(turnout_wide_mat, verbose=TRUE, method=SVD_METHOD)

  return(svd)
}

# if(CONFIG$is_primary){
#   turnout_cov_rep <- get_turnout_svd("primary", "^REP")
#   turnout_cov_dem <- get_turnout_svd("primary", "^DEM")
# } else {
#   turnout_cov_all <- get_turnout_svd("general")
# }

get_pvote_svd <- function(df_past, use_party="^DEM", use_primary=TRUE, use_general=TRUE, use_log=USE_LOG){
  df_pvote <- df_past %>% 
    filter(candidate != "Write In") %>%
    filter(election == "general" | use_primary) %>%
    filter(election == "primary" | use_general) %>%
    filter(grepl(use_party, party, ignore.case=TRUE))
  
  
  df_pvote <- df_pvote %>%
    group_by(election, office, district, warddiv) %>%
    mutate(pvote = votes / sum(votes)) %>%
    mutate(target = {if(use_log) log(pvote + 0.001) else pvote}) %>%
    group_by()
  
  n_cand <- df_pvote %>% 
    select(election, office, district, candidate) %>%
    unique() %>%
    group_by(election, office, district) %>%
    summarise(n_cand = n()) %>%
    mutate(
      prior_mean = {if(use_log) log(1/n_cand) else 1/n_cand}
    ) %>%
    ungroup()
  
  df_pvote <- df_pvote %>%
    left_join(n_cand) %>%
    mutate(target_demean = target - prior_mean)
  
  pvote_wide <- df_pvote %>% 
    mutate(office=paste0(office, ifelse(is.na(district), "", district))) %>%
    unite("key", candidate, office, election) %>%
    select(warddiv, key, target_demean) %>%
    spread(key, target_demean, fill=0)
  
  pvote_mat <- as.matrix(pvote_wide %>% select(-warddiv))
  
  rownames(pvote_mat) <- pvote_wide$warddiv
  return(
    ## column_means = 0 since I subtracted 1/n_cand
    get_svd(
      pvote_mat, 
      n_svd=5, 
      known_column_means=0, 
      verbose=TRUE,
      method=SVD_METHOD
    )
  ) 
}

# pvote_svd <- get_pvote_svd(df_past)
# 
# if(CONFIG$is_primary){
#   needle_params <- needleSVDs(
#     pvote_svd=pvote_svd,
#     turnout_svds=list(
#       "rep" = turnout_cov_rep,
#       "dem" = turnout_cov_dem
#     ),
#     log=USE_LOG
#   )
# } else {
#   needle_params <- needleSVDs(
#     pvote_svd=pvote_svd,
#     turnout_svds=list(
#       "general" = turnout_cov_all
#     ),
#     log=USE_LOG
#   )
# }

#######################
## PLOTS
#######################

map_precinct_score <- function(svd, col, precinct_sf, adj_area=TRUE){
  if(!is(svd, "SVDParams")) stop("params must be of class SVDParams")
  
  precinct_sf$area <- as.numeric(st_area(precinct_sf))
  
  if(adj_area){
    if(svd@log){
      adj_fe <- function(fe, area) fe - log(area)
    } else {
      adj_fe <- function(fe, area) fe / area
    }
  } else {
    adj_fe <- function(x, ...) x
  }
  
  ggplot(
    precinct_sf %>% 
      left_join(svd@row_scores, by=c("precinct"="row"))
  ) +
    geom_sf(
      aes(fill = adj_fe(!!sym(col), area)),
      color= NA
    ) +
    scale_fill_viridis_c("Score")+
    theme_map_sixtysix() 
} 

map_precinct_fe <- function(svd, precinct_sf, adj_area) {
  map_precinct_score(svd, "mean", precinct_sf, adj_area) +
    scale_fill_viridis_c("Mean")
}
map_precinct_dim <- function(svd, k, precinct_sf){
  map_precinct_score(svd, paste0("score.",k), precinct_sf, adj_area=FALSE) +
    scale_fill_gradient2(
      paste("Score, Dimension", k), 
      midpoint = 0
    )
}

plot_election_score <- function(svd, col){
  if(!is(svd, "SVDParams")) stop("svd must be of class SVDParams")
  
  election_df <- svd@col_scores %>%
    mutate(
      year = asnum(substr(col, 1, 4)),
      etype = substr(col, 6, nchar(as.character(col)))
    )
  
  ggplot(
    election_df,
    aes(x=year, y=!!sym(col))
  ) +
    geom_line(
      aes(group=year %% 4),
      color= strong_green
    ) +
    geom_point(
      color = strong_green,
      size = 2
    ) +
    facet_grid(etype ~ .) +
    xlab("") +
    theme_sixtysix() +
    ggtitle("election scores", "Grouped by 4 election cycle")
} 

plot_election_fe <- function(svd) plot_election_score(svd, "mean")
plot_election_dim <- function(svd, k) plot_election_score(svd, paste0("score.", k))

diagnostics <- function(needle_params, precinct_sf, config){
  print("Plotting Diagnostics...")
  
  pause <- function() invisible(readline(prompt = "Press <Enter> to continue..."))
  
  pvote_svd <- needle_params@pvote_svd
  
  print(
    map_precinct_fe(pvote_svd, divs, adj_area=FALSE) +
      ggtitle("Precinct means of pvote")
  )
  pause()
  
  for(k in 1:(ncol(pvote_svd@row_scores)-2)){
    print(
      map_precinct_dim(pvote_svd, k, precinct_sf) +
        ggtitle(sprintf("pvote Dimension %s", k))
    )
    pause()
  }
  
  for(turnout_svd_name in names(needle_params@turnout_svds)){
    turnout_svd <- needle_params@turnout_svds[[turnout_svd_name]]
    
    print(
      map_precinct_fe(turnout_svd, divs, adj_area=FALSE) +
        ggtitle(sprintf("Precinct means of turnout %s", turnout_svd_name))
    )
    pause()
    print(
      plot_election_fe(turnout_svd) +
        ggtitle(sprintf("Turnout %s FE", turnout_svd_name))
    )
    pause()
    
    for(k in 1:(ncol(turnout_svd@row_scores)-2)){
      print(
        map_precinct_dim(turnout_svd, k, divs) +
          ggtitle(sprintf("Dim %s, %s", k, turnout_svd_name))
      )
      pause()
      print(
        plot_election_dim(turnout_svd,k) +
          ggtitle(sprintf("Turnout %s Dimension %s", turnout_svd_name, k))
      )
      pause()
    }
  }
}


if(FALSE){
  pvote_svd <- get_pvote_svd(df_past)

  if(CONFIG$is_primary){
    turnout_svds=list(
      "rep" = get_turnout_svd("primary", "^REP"),
      "dem" = get_turnout_svd("primary", "^DEM")
    )
  } else {
    turnout_svds=list(
      "general" = get_turnout_svd("general")
    )
  }
  
  needle_params <- needleSVDs(
    pvote_svd=pvote_svd,
    turnout_svds=turnout_svds,
    log=USE_LOG
  )
  
  diagnostics(needle_params, divs, CONFIG)
  
  saveRDS(
    needle_params,
    file=sprintf(
      "outputs/needleparams_%s_log%s.Rds",
      ELECTION,
      USE_LOG
    )
  )
}

