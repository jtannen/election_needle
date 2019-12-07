library(tidyverse)
library(MASS)
library(sf)

# setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")
select <- dplyr::select

source("needle_util.R")
source("svd_for_turnout_and_pvote.R", chdir=TRUE)

SVD_METHOD = "svd"
# SVD_METHOD = "shrinkage"

ELECTION <- "20191105"

CONFIG <- get_config(ELECTION)
DIV_PATH <- CONFIG$div_path
USE_LOG <- CONFIG$use_log

calc_params <- function(
  method=SVD_METHOD,
  is_primary=CONFIG$is_primary,
  use_log=USE_LOG,
  show_diagnostics=TRUE,
  df_past_path=most_recent_file("../../data/processed_data/df_major_")
){
  df_past <- readRDS(df_past_path) %>%
    unite("election", year, election) %>%
    mutate(warddiv = pretty_div(warddiv)) %>%
    filter(candidate != "Write In")
  
  
  divs <- st_read(DIV_PATH) %>%
    mutate(
      warddiv = pretty_div(DIVISION_N)
    )
  
  setClass(
    "needleSVDs",
    slots=c(
      pvote_svd="SVDParams",
      turnout_svd="SVDParams",
      log="logical"
    )  
  )
  
  needleSVDs <- function(  
    pvote_svd,
    turnout_svd,
    log=use_log
  ){
    new(
      "needleSVDs",
      pvote_svd=pvote_svd,
      turnout_svd=turnout_svd,
      log=log
    )
  }
  
  ###################
  ## CALCULATE
  ##################
  
  if(is_primary){
    dem_params <- needleSVDs(
      pvote_svd = get_pvote_svd(df_past, primary_party_regex = "^DEM"),
      turnout_svd=get_turnout_svd(df_past, "primary", "^DEM"),
      log=use_log
    )
    rep_params <- needleSVDs(
      pvote_svd = get_pvote_svd(df_past, primary_party_regex = "^REP"),
      turnout_svd=get_turnout_svd(df_past, "primary", "^REP"),
      log=use_log
    )
    needle_params <- list(dem=dem_params, rep=rep_params)
  } else {
    general_params <- needleSVDs(
      pvote_svd=get_pvote_svd(df_past, primary_party_regex = "^DEM"),
      turnout_svd=get_turnout_svd(df_past, "general"),
      log=use_log
    )
    needle_params <- list(general=general_params)
  }

  if(show_diagnostics){
    for(params_name in names(needle_params)){
      print(sprintf("Diagnostics for %s", params_name))
      diagnostics(needle_params[[params_name]], divs)
    }
  }
  
  return(needle_params)
}

if(FALSE){
  set.seed(215)
  needle_params <- calc_params()
  saveRDS(
    needle_params,
    file=sprintf(
      "outputs/needleparams_%s_log%s_%s.Rds",
      ELECTION, USE_LOG, SVD_METHOD
    )
  )
}

