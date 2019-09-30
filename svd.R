library(tidyverse)
library(MASS)
library(sf)

select <- dplyr::select

source("../../admin_scripts/util.R")

USE_LOG <- TRUE

set.seed(215)

df_past <- safe_load("../../data/processed_data/df_major_2019_05_14.Rda") %>%
  unite("election", year, election) %>%
  unite("warddiv", WARD19, DIV19, sep="-") 

# df_past <- df_past %>% filter(election >= "2012")

get_turnout_past <- function(party_grep){
  df_party <- df_past %>% filter(grepl(party_grep, PARTY))
  turnout_party <- df_party %>% 
    filter(is_primary_office) %>%
    group_by(warddiv, election) %>%
    summarise(VOTES = sum(VOTES)) %>%
    group_by() %>%
    mutate(log_turnout = log(VOTES + 1)) %>%
    mutate(target = {if(USE_LOG) log_turnout else VOTES})
  
  turnout_wide <- turnout_party %>%
    select(warddiv, election, target) %>%
    spread(election, target, fill = 0)
  
  turnout_wide_mat <- as.matrix(turnout_wide %>% select(-warddiv))
  row.names(turnout_wide_mat) <- turnout_wide$warddiv
  return(get_svd(turnout_wide_mat))
}

get_svd <- function(wide_mat, n_svd=3, winsorize=TRUE){
  row_means <- rowMeans(wide_mat)
  wide_mat_demeaned <- sweep(wide_mat, row_means, MARGIN=1)
  
  svd <- svd(wide_mat_demeaned, n_svd, n_svd)
  
  ## winsorize the svd
  if(winsorize){
    for(i in 1:n_svd){
      threshold <- quantile(abs(svd$u[,i]), 0.9995)
      svd$u[,i] <- sign(svd$u[,i]) * pmin(abs(svd$u[,i]), threshold)
    }
  }
  
  fitted <- svd$u %*% diag(svd$d[1:n_svd]) %*% t(svd$v)
  
  print("Fitted vs True values, check for similarity:")
  print("Fitted:")
  print(fitted[1:6, 1:6])
  print("True:")
  print({d <- wide_mat_demeaned; colnames(d) <- NULL; d[1:6, 1:6]})
  
  print("Calculating covariances")
  row_cov <- svd$u %*% diag(svd$d[1:n_svd]) %*% cov(svd$v) %*% diag(svd$d[1:n_svd]) %*% t(svd$u)
  diag(row_cov) <- diag(row_cov) + var(as.vector(fitted - wide_mat_demeaned))
  row.names(row_cov) <- row.names(wide_mat)
  
  row_scores <- data.frame(
    row = row.names(wide_mat),
    score = svd$u,
    mean = row_means
  )
  
  col_scores <- data.frame(
    col = colnames(wide_mat),
    score = svd$v
  )
  
  return(list(
    row_cov=row_cov,
    row_scores=row_scores, 
    col_scores=col_scores, 
    svd_d = svd$d
  ))
}  

turnout_cov_rep <- get_turnout_past("^REP")
turnout_cov_dem <- get_turnout_past("^DEM")
turnout_cov_all <- get_turnout_past(".*")


get_pvote_svd <- function(df_past, party="DEM", use_primary=TRUE, use_general=TRUE, use_log=USE_LOG){
  df_pvote <- df_past %>% 
    filter(CANDIDATE != "Write In") %>%
    filter(election == "general" | use_primary) %>%
    filter(election == "primary" | use_general) %>%
    filter(grepl("DEM", PARTY, ignore.case=TRUE))


  df_pvote <- df_pvote %>%
    group_by(election, OFFICE, warddiv) %>%
    mutate(pvote = VOTES / sum(VOTES)) %>%
    mutate(target = {if(use_log) log(pvote + 0.001) else pvote}) %>%
    group_by()
  
  n_cand <- df_pvote %>% 
    select(election, OFFICE, CANDIDATE) %>%
    unique() %>%
    group_by(election, OFFICE) %>%
    summarise(n_cand = n()) %>%
    mutate(
      prior_mean = {if(use_log) log(1/n_cand) else 1/n_cand}
    )

  df_pvote <- df_pvote %>%
    left_join(n_cand) %>%
    mutate(target_demean = target - prior_mean)
  
  pvote_wide <- df_pvote %>% 
    unite("key", CANDIDATE, OFFICE, election) %>%
    select(warddiv, key, target_demean) %>%
    spread(key, target_demean, fill=0)
  
  pvote_mat <- as.matrix(pvote_wide %>% select(-warddiv))
  
  rownames(pvote_mat) <- pvote_wide$warddiv
  return(get_svd(pvote_mat, n_svd=5))
}

pvote_svd <- get_pvote_svd(df_past)

save(
  pvote_svd,
  turnout_cov_dem, 
  turnout_cov_rep, 
  turnout_cov_all,
  file=paste0("outputs/saved_covars_log",USE_LOG,".Rda")
)
