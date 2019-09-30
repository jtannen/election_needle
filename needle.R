library(tidyverse)
library(MASS)
library(sf)
select <- dplyr::select

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")
source("../../admin_scripts/util.R")

if(!exists("USE_SAVED_COVARIANCE")) USE_SAVED_COVARIANCE <- TRUE
if(!exists("USE_MAPS")) USE_MAPS <- TRUE
if(!exists("should_download")) should_download <- TRUE
if(!exists("USE_LOG")) USE_LOG <- TRUE
if(!exists("IS_PRIMARY")) IS_PRIMARY <- FALSE

set.seed(215)



## UPDATE ON ELECTION DAY
if(IS_PRIMARY){
  USE_OFFICES <- c(
    "CITY COMMISSIONERS-DEM",
    "COUNCIL AT LARGE-DEM",
    "DISTRICT COUNCIL-1ST DISTRICT-DEM",
    "DISTRICT COUNCIL-2ND DISTRICT-DEM",
    "DISTRICT COUNCIL-7TH DISTRICT-DEM",
    "MAYOR-DEM"
  )
} else {
  USE_OFFICES <- c(
    "CITY COMMISSIONERS",
    "COUNCIL AT LARGE",
    "DISTRICT COUNCIL-10TH DISTRICT",
    "MAYOR"
  )
}
##################
## Load download
##################

asnum <- function(x) as.numeric(as.character(x))

format_name <- function(x){
  x <- gsub("^[0-9]+\\-", "", x)
  x <- gsub("(.*)\\,.*", "\\1", x)
  x <-  gsub("(\\B)([A-Z]+\\b)", "\\U\\1\\L\\2", x, perl = TRUE)
  x <- ifelse(x == "Almiron", "Almiron", x)
  x <- ifelse(x == "Diberardinis", "DiBerardinis", x)
}

if(should_download){
  results_site <- readLines("http://phillyelectionresults.com/ExportFiles.html")
  link_grep <- "http://getphillyelectionresults\\.com/export/PRECINCT_[0-9]+_H[0-9]+_M[0-9]+_S[0-9]+\\.txt"
  link_line <- grep(link_grep, results_site, value = TRUE)
  link <- gsub('.*href=\\\"(.*)\\\" download.*', "\\1", link_line)
  filename <- gsub("http://getphillyelectionresults.com/export/(.*)", "\\1", link)
  download.file(
    link, 
    destfile = paste0("raw_data/", filename)
  )
}

load_data <- function(file){
  df <- read_delim(
    paste0("raw_data/", file),
    delim = "@"
  ) %>%
    rename(
      office = `Office_Prop Name`,
      candidate = Tape_Text,
      warddiv = Precinct_Name
    )
  
  df <- df[-(nrow(df) - 0:1),]
  
  returns <- df %>% 
    group_by(warddiv) %>%
    summarise(sum_votes = sum(Vote_Count)) %>%
    group_by() %>%
    filter(sum_votes > 0)
  
  df <- inner_join(df, returns %>% dplyr::select(warddiv))
  
  if(nrow(df) == 0) stop("No data yet!")
  
  df <- df %>% 
    filter(office %in% USE_OFFICES) %>%
    filter(candidate != "Write In")
  
  df <- df %>% 
    mutate(candidate = format_name(candidate)) %>%
    group_by(warddiv, office) %>% 
    mutate(pvote = Vote_Count / sum(Vote_Count)) %>%
    group_by()
  
  return(df)
}

if(FALSE){
  files <- list.files("raw_data")
  file <- max(files)
  df <- load_data(file)
}

if(FALSE){
  df <- load_data("PRECINCT_2019521_H20_M38_S25.txt")
}


divs <- st_read("../../data/gis/201911/Political_Divisions.shp")
divs <- st_transform(divs, 2272) %>%
  rename(
    warddiv = DIVISION_N
  ) %>%
  arrange(warddiv)

wards <- st_read("../../data/gis/2019/Political_Wards.shp") %>%
  mutate(ward=sprintf("%02d", asnum(WARD_NUM))) %>%
  st_transform(2272)

phila_whole <- st_union(wards)

plot_dim <- function(u){
  ggplot(
    divs %>% mutate(u = !!u)
  ) + geom_sf(aes(fill=u), color = NA) +
    scale_fill_viridis_c()
}

## test on past data
if(FALSE){
  use_past_offices <- c(
    "CITY COMMISSIONERS",
    "COUNCIL AT LARGE",
    "JUDGE OF THE COURT OF COMMON PLEAS",
    "MAYOR",
    "SHERIFF"
  )
  
  df <- readRDS("../../data/processed_data/df_all_2019_09_19.Rds") %>%
    group_by() %>%
    filter(
      election == "general",
      year == '2015',
      !grepl("WRITE IN", candidate, ignore.case = TRUE)
    ) %>%
    unite("election", year, election) %>%
    mutate(
      office = paste0(
        office, 
        ifelse(!is.na(district), paste0("-",district),"")
      )
    ) %>%
    filter(office %in% use_past_offices) %>%
    group_by(warddiv, office, candidate) %>%
    summarise(Vote_Count=sum(votes)) %>%
    ungroup()
    
  
  sample_divs <- sample(unique(df$warddiv), 10)
  df <- df %>% filter(warddiv %in% sample_divs)
  
  # df <- df %>% filter(substr(warddiv, 1, 2) == "01")
}

if(!USE_SAVED_COVARIANCE){
  df_past <- readRDS("../../data/processed_data/df_major_2019_09_19.Rds") %>%
    unite("election", year, election) %>%
    filter(
      !office %in% c(
        "REPRESENTATIVE IN THE GENERAL ASSEMBLY",
        "SENATOR IN THE GENERAL ASSEMBLY",
        "REPRESENTATIVE IN CONGRESS",
        "DISTRICT COUNCIL"
      ),
      !grepl("^(JUDGE|JUSTICE)", office)
    )

  ##################
  ## Turnout
  ##################
  
  get_turnout_past <- function(party_grep=NA){
    if(IS_PRIMARY){
      df_filtered <- df_past %>% 
        filter(
          grepl(party_grep, party),
          grepl("^[0-9]{4}_primary", election)
        )
    } else {
      df_filtered <- df_past
    }
    
    turnout <- df_filtered %>% 
      filter(is_primary_office) %>%
      group_by(warddiv, election) %>%
      summarise(votes = sum(votes)) %>%
      group_by() %>%
      mutate(log_turnout = log(votes + 1)) %>%
      mutate(target = {if(USE_LOG) log_turnout else votes})
    
    turnout_wide <- turnout %>%
      select(warddiv, election, target) %>%
      spread(election, target, fill = 0)
    
    turnout_means <- rowMeans(turnout_wide %>% select(-warddiv))
    names(turnout_means) <- turnout_wide$warddiv
    
    turnout_mat <- as.matrix(turnout_wide %>% select(-warddiv)) %>%
      sweep(turnout_means, MARGIN=1)
    
    n_svd <- 3
    svd <- svd(turnout_mat, n_svd, n_svd)
    
    ## winsorize the svd
    for(i in 1:n_svd){
      threshold <- quantile(abs(svd$u[,i]), 0.99)
      svd$u[,i] <- sign(svd$u[,i]) * pmin(abs(svd$u[,i]), threshold)
    }
    
    for(i in 1:n_svd){
      print(plot_dim(svd$u[,i]))
      readline("Press Enter")
    }
    
    true_mat <- turnout_mat
    
    fitted <- svd$u %*% diag(svd$d[1:n_svd]) %*% t(svd$v)
    
    print("Fitted vs True values, check for similarity:")
    print("Fitted:")
    print(fitted[1:6, 1:6])
    print("True:")
    print(true_mat[1:6, 1:6])
    
    print("Calculating covariances")
    turnout_cov <- svd$u %*% diag(svd$d[1:n_svd]) %*% cov(svd$v) %*% diag(svd$d[1:n_svd]) %*% t(svd$u)
    diag(turnout_cov) <- diag(turnout_cov) + var(as.vector(fitted - true_mat))
    row.names(turnout_cov) <- turnout_wide$warddiv
    
    
    
    return(list(turnout_cov=turnout_cov, turnout_means=turnout_means))
  }  
  
  if(IS_PRIMARY){
    turnout_cov_rep <- get_turnout_past(party_grep = "^REP")
    turnout_cov_dem <- get_turnout_past(party_grep = "^DEM")
  } else {
    turnout_cov_general <- get_turnout_past()
  }
  
  #################################
  ## pvote
  #################################
  
  df_pvote <- df_past %>%
    filter(candidate != "Write In")
  
  if(IS_PRIMARY){
    df_pvote <- df_pvote %>% filter(grepl("primary", election))
    df_pvote_dem <- df_pvote %>% filter(grepl("DEM", party, ignore.case=TRUE))
    df_pvote_rep <- df_pvote %>% filter(grepl("REP", party, ignore.case=TRUE))
    pvote_list <- list(rep = df_pvote_rep, dem = df_pvote_dem)
  } else {
    pvote_list <- list(general=df_pvote)
  }
  
  table(pvote_list[[1]]$election)
  
  cov_list <- list()
  PLOT_DIMS <- TRUE
  # if(IS_PRIMARY) stop("Need to implement primaries using df_pvote_dem")
  for(pvote_name in names(pvote_list)){
    print("pvote Covariance"); print(pvote_name)
    df_pvote <- pvote_list[[pvote_name]]
    df_pvote <- df_pvote %>%
      group_by(election, office, warddiv) %>%
      mutate(pvote = votes / sum(votes)) %>%
      ungroup() %>%
      mutate(target = {if(USE_LOG) log(pvote + 0.001) else pvote}) 
  
    n_cand <- df_pvote %>% 
      select(election, office, candidate) %>%
      unique() %>%
      group_by(election, office) %>%
      summarise(ncand = n()) %>%
      mutate(
        prior_mean = {if(USE_LOG) log(1/ncand) else 1/ncand}
      )
  
    df_pvote <- df_pvote %>%
      left_join(n_cand) %>%
      mutate(target_demean = target - prior_mean)
  
    pvote_wide <- df_pvote %>% 
      unite("key", candidate, office, election) %>%
      select(warddiv, key, target_demean) %>%
      spread(key, target_demean, fill=0)
  
    pvote_mat <- as.matrix(pvote_wide %>% select(-warddiv))
    rownames(pvote_mat) <- pvote_wide$warddiv
    
    n_svd <- 5
    svd <- svd(pvote_mat, n_svd, n_svd)
  
    ## winsorize the svd
    for(i in 1:n_svd){
      threshold <- quantile(abs(svd$u[,i]), 0.9995)
      svd$u[,i] <- sign(svd$u[,i]) * pmin(abs(svd$u[,i]), threshold)
      if(PLOT_DIMS) {
        print(plot_dim(svd$u[,i]))
        readline("Press Enter")
      }
    }
    
    true_mat <- pvote_mat
    
    fitted <- svd$u %*% diag(svd$d[1:n_svd]) %*% t(svd$v)
    
    print("Fitted vs True values, check for similarity:")
    print("Fitted:")
    print(fitted[1:6, 1:6])
    print("True:")
    print({x <- true_mat[1:6, 1:6]; colnames(x) <- NULL; x})
    
    print("Calculate ward-level variance")
    resid <- (true_mat - fitted) %>%
      as.data.frame() %>%
      mutate(warddiv = pvote_wide$warddiv) %>%
      gather("candidate", "value", -warddiv) 
    
    ward_resid <- resid %>%
      mutate(ward=substr(warddiv,1,2)) %>%
      group_by(ward, candidate) %>%
      summarise(value = mean(value)) 
    
    resid_var <- var(as.vector(fitted - true_mat))
    print("Total Resid Var:"); print(resid_var)
    
    ward_var <- var(ward_resid$value)
    print("Ward Variance:"); print(ward_var)
    
    print("Calculating covariances")
    pvote_cov <- svd$u %*% diag(svd$d[1:n_svd]) %*% cov(svd$v) %*% diag(svd$d[1:n_svd]) %*% t(svd$u)
    for(ward in 1:66){
      rows <- which(substr(pvote_wide$warddiv, 1, 2) == sprintf("%02d", ward))
      pvote_cov[rows, rows] <- pvote_cov[rows, rows] + ward_var 
    }
    diag(pvote_cov) <- diag(pvote_cov) + resid_var - ward_var
  
    row.names(pvote_cov) <- pvote_wide$warddiv
    
    cov_list[[pvote_name]] <- pvote_cov
  }
  
  if(IS_PRIMARY) {
    cov_list[["turnout_cov_dem"]] <- turnout_cov_dem
    cov_list[["turnout_cov_rep"]] <- turnout_cov_rep
  } else {
    cov_list[["turnout_cov_general"]] <- turnout_cov_general
  }
  
  save(list = c(
    "cov_list",
    "svd"
    ), 
    file=sprintf("saved_covars_log%s_primary%s.Rda", USE_LOG, IS_PRIMARY)
  ) 

  #########################
  ## Get Council Districts
  #########################
  print("Getting Council Districts")
  council <- st_read("../../data/gis/city_council/Council_Districts_2016.shp")
  council <- st_transform(council, 2272)
  div_centroids <- st_centroid(divs)
  divs_to_council <- st_covered_by(div_centroids, council)
  divs_to_council <- unlist(divs_to_council)
  if(length(divs_to_council) != nrow(divs)) stop("Too many matches")
  divs_to_council <- data.frame(
    warddiv = divs$warddiv,
    council = as.character(council$DISTRICT)[divs_to_council]
  ) 
  save(divs_to_council, file="divs_to_council.Rda")
    
} else {
  load(sprintf("saved_covars_log%s_primary%s.Rda", USE_LOG, IS_PRIMARY))
  load("divs_to_council.Rda")
}

#####################
## predict turnout
#####################
pretty_div <- function(x) paste(substr(x,1,2), substr(x,3,4), sep="-")
if(IS_PRIMARY){
  warddiv_order <- names(turnout_cov_dem$turnout_means)
} else {
  warddiv_order <- names(turnout_cov_general$turnout_means)
} 

n_boot <- 400

mvrnorm_chol <- function(n, mu, sigma_chol){
  sample_raw <- matrix(rnorm(n * length(mu)), length(mu), n)
  sample <- t(sigma_chol) %*% sample_raw + matrix(mu, nrow=length(mu), ncol=n)
  rownames(sample) <- rownames(sigma_chol)
  return(t(sample))
}

get_turnout <- function(df, turnout_office, turnout_cov_list){
  divs_with_data <- unique(df$warddiv)
  a_rows <- match(divs_with_data, warddiv_order)
  c_rows <- seq_along(turnout_cov_list$turnout_means)[-a_rows]
  
  turnout_cov <- turnout_cov_list$turnout_cov
  turnout_means <- turnout_cov_list$turnout_means
  observed_turnout <- df %>% filter(office == turnout_office) %>%
    group_by(warddiv) %>%
    summarise(turnout = sum(Vote_Count)) %>%
    mutate(target_obs = {if(USE_LOG) log(turnout + 1) else turnout})
  
  ## Assume distribution is normal with covariance [A, B], [B', C]
  A <- turnout_cov[a_rows, a_rows]
  B <- turnout_cov[c_rows, a_rows]
  C <- turnout_cov[c_rows, c_rows]
  
  a <- observed_turnout$target_obs
  A_inv <- solve(A)
  c_mean <- turnout_means[c_rows] + B %*% A_inv %*% (a - turnout_means[a_rows])
  
  c_sigma <- C - B %*% A_inv %*% t(B)
  
  if(nrow(c_sigma) > 0){
    turnout_sim <- mvrnorm_chol(n=n_boot, mu=c_mean, sigma_chol=chol(c_sigma))
    turnout_sim <- as.data.frame(turnout_sim) %>% 
      mutate(sim = 1:n_boot) %>%
      gather("warddiv", "target", -sim) %>%
      mutate(turnout = {if(USE_LOG) exp(target) else target})
  } else {
    turnout_sim <- data.frame(sim = integer(0), warddiv = character(0), turnout = numeric(0))
  }

  return(turnout_sim)
}

get_needle_for_office <- function(
  df,
  use_office,
  office_name,
  turnout_sim,
  n_winners,
  consider_divs=warddiv_order
){

  divs_with_data <- unique(df$warddiv)
  a_rows <- match(divs_with_data, warddiv_order)
  c_rows <- seq_along(warddiv_order)[-a_rows]
  
  df_office <- df %>% 
    filter(office == use_office) %>%
    filter(warddiv %in% consider_divs) %>%
    group_by(warddiv) %>%
    mutate(pvote = Vote_Count/sum(Vote_Count)) %>%
    group_by() %>%
    mutate(
      target = {if(USE_LOG) log(pvote+0.001) else pvote},
      candidate=format_name(candidate)
    )

  candidates <- unique(df_office$candidate)
  ncand <- length(candidates)
  
  if(ncand == 0){ print(paste0(office_name, ": NO RETURNS YET")); return()}
  
  a_rows_i <- a_rows[warddiv_order[a_rows] %in% consider_divs]
  c_rows_i <- c_rows[warddiv_order[c_rows] %in% consider_divs]
  
  ## Assume distribution is normal with covariance [A, B], [B', C]
  A <- pvote_cov[a_rows_i, a_rows_i]
  B <- pvote_cov[c_rows_i, a_rows_i]
  C <- pvote_cov[c_rows_i, c_rows_i]
  
  if(length(a_rows_i) > 0){
    A_inv <- solve(A)
  } else {
    A_inv <- A
  }
  
  c_sigma <- C - B %*% A_inv %*% t(B)
  
  if(length(a_rows_i) <= 1) rownames(c_sigma) <- names(B) ## otherwise it gets inherited
  
  if(USE_LOG) prior_mean <- log(1/ncand) else prior_mean <- 1/ncand
  
  if(length(c_rows_i) > 0){
    chol_c_sigma <- chol(c_sigma)
    
    df_list = vector(mode="list", length = ncand)
    for(cand in candidates){
      observed_pvote <- df_office %>%
        filter(candidate == cand)
      
      if(any(observed_pvote$warddiv != warddiv_order[a_rows_i])) stop("Wards out of order")
      a <- observed_pvote$target
      c_mean <- prior_mean + B %*% A_inv %*% (a - prior_mean)
      
      pvote_cand_sample <- mvrnorm_chol(n=n_boot, mu=c_mean, sigma_chol=chol_c_sigma)
      pvote_cand_sample <- as.data.frame(pvote_cand_sample) %>% 
        mutate(sim = 1:n_boot) %>%
        gather("warddiv", "target", -sim) %>%
        mutate(candidate=cand)
      df_list[[cand]] <- pvote_cand_sample
    }
    
    office_sim <- bind_rows(df_list) %>%
      mutate(pvote = {if(USE_LOG) exp(target) else pmax(target,0)}) %>%
      group_by(warddiv, sim) %>%
      mutate(pvote = pvote / sum(pvote)) %>%
      group_by()
    
    office_sim_total <- office_sim %>%
      left_join(turnout_sim %>% select(-target)) %>%
      group_by(candidate, sim) %>%
      summarise(votes_sim = sum(pvote * turnout)) %>%
      left_join(df_office %>% group_by(candidate) %>% summarise(votes_obs = sum(Vote_Count)))
    
  } else {
    office_sim <- data.frame()
    office_sim_total <- df_office %>% 
      group_by(candidate) %>% 
      summarise(votes_obs = sum(Vote_Count, na.rm=TRUE)) %>%
      mutate(sim = 1, votes_sim=0)
  }
  
  office_sim_total <- office_sim_total %>% 
    group_by(sim) %>%
    mutate(
      pvote = (votes_sim + votes_obs) / sum(votes_sim + votes_obs),
      is_winner = rank(desc(pvote)) <= n_winners
    ) %>%
    group_by()
  
  winners_sim <- office_sim_total %>% 
    group_by(candidate) %>%
    summarise(
      pvote = mean(pvote),
      pwin = mean(is_winner)
    ) %>%
    mutate(x = -cos(pwin * pi), y = sin(pwin*pi))
  
  cand_order <- winners_sim %>% arrange(desc(pvote)) %>%
    with(candidate)
  
  winners_sim$candidate <- factor(
    format_name(winners_sim$candidate),
    levels = cand_order
  )
    
  needle_facet <- ggplot(
    winners_sim
  ) +
    geom_segment(
      x=0, y=0,
      aes(xend=x, yend=y), 
      arrow = arrow(length = unit(0.1, "inches")),
      size=2, color="grey30"
    ) +
    geom_point(x=0,y=0,color="grey30", size=4) +
    facet_wrap(~candidate, ncol=3) +
    scale_x_continuous(
      "", 
      breaks = c(0), 
      labels =c(""), 
      limits=c(-1,1)
    ) + 
    scale_y_continuous("", breaks = FALSE, minor_breaks = FALSE, labels = "", limits=c(0,1)) + 
    geom_text(
      aes(label = paste0(round(100*pwin),"%")), 
      size = 8,
      x=-1, y=1, hjust=0, vjust=1,
      fontface="bold",
      color=strong_purple
    ) +
    geom_text(
      aes(label = paste0("(", round(100*pvote),"% of vote)")), 
      x=-1, y=0.5, hjust=0, vjust=0,
      # fontface="bold",
      color="grey50"
    ) +
    coord_fixed() +
    theme_sixtysix() %+replace% 
    theme(
      strip.text = element_text(face="bold", size = 20)
    )
    # ggtitle(sprintf("Probability of winning %s", office_name))
  if(USE_MAPS){
    div_sim <- df_office %>%
      group_by(warddiv) %>%
      mutate(
        pvote = Vote_Count / sum(Vote_Count),
        candidate = format_name(candidate)
      ) %>%
      mutate(candidate = factor(candidate, levels = cand_order))
    
    limits <- st_bbox(divs %>% filter(warddiv %in% consider_divs))
    
    ndivs <- length(unique(df_office$warddiv))
    if(ndivs > 200){
      ggmap <- wards %>%
        inner_join(
          div_sim %>% mutate(ward = substr(warddiv,1,2)) %>%
            group_by(warddiv) %>%
            mutate(turnout = sum(Vote_Count)) %>%
            group_by(ward, candidate) %>%
            summarise(pvote = weighted.mean(pvote, w=turnout))
        )
    } else {
      ggmap <- divs %>% 
        inner_join(div_sim)
    }
    
    map_facet <- ggplot(
      ggmap
    ) + 
      geom_sf(data=phila_whole, fill="grey70", color=NA) +
      geom_sf(aes(fill = 100 * pvote), color = NA) +
      facet_wrap(~candidate, ncol=3) +
      expand_limits(fill=0) +
      scale_fill_viridis_c("% of Vote") +
      theme_map_sixtysix() %+replace%
      theme(
        legend.position = "right",
        strip.text = element_text(face="bold", size = 20)
      )+
      xlim(limits[c("xmin", "xmax")]) +
      ylim(limits[c("ymin", "ymax")])
  } else {
    map_facet <- ggplot()
  }  
  
  return(list(
    needle=needle_facet,
    map=map_facet,
    office_sim=office_sim,
    office_sim_total=office_sim_total,
    winners_sim=winners_sim
  ))
}

if(FALSE){
  if(IS_PRIMARY){
    mayor_office <- "MAYOR-DEM" 
    pred_office <- "COUNCIL AT LARGE-DEM"
    turnout_cov <- cov_list$turnout_cov_dem
  } else {
    mayor_office <- "MAYOR"
    pred_office <- "COUNCIL AT LARGE"
    turnout_cov <- cov_list$turnout_cov_general
  }
  
  turnout_sim <- get_turnout(df, mayor_office, turnout_cov)
  
  if(IS_PRIMARY) n_council_winners <- 5 else n_council_winners <- 7
  
  council_at_large_plot <- get_needle_for_office(
    df,
    use_office=pred_office,
    office_name="Council At Large",
    turnout_sim = turnout_sim,
    n_winners=n_council_winners
  )
  council_at_large_plot[["needle"]] %>% print()
}

