library(corpcor)

library(tidyverse)
library(MASS)
library(sf)
library(memoise)

select <- dplyr::select
source("../../admin_scripts/util.R")

setClass(
  "SVDParams",
  slots=c(
    row_cov="matrix",
    row_scores="data.frame", 
    col_scores="data.frame", 
    svd_d = "numeric",
    log="logical"
  )  
)
SVDParams <- function(
  row_cov,
  row_scores, 
  col_scores, 
  svd_d
){
  new(
    "SVDParams",
    row_cov=row_cov,
    row_scores=row_scores, 
    col_scores=col_scores, 
    svd_d = svd_d
  )
}

get_svd <- function(
  wide_mat, 
  n_svd=3,
  winsorize=TRUE,
  get_group_from_rowname=function(warddiv) substr(warddiv,1,2),
  known_column_means=NULL,  ## Priors for mean. If not null, use fixed effects.
  verbose=FALSE,
  method="svd"
){
  row_means <- rowMeans(wide_mat)
  wide_mat_demeaned <- sweep(wide_mat, row_means, MARGIN=1)
  
  if(is.null(known_column_means)){
    col_means <- colMeans(wide_mat_demeaned)
  } else {
    col_means <- known_column_means
  }
  
  wide_mat_demeaned <- sweep(wide_mat_demeaned, col_means, MARGIN=2)
  
  if(winsorize){
    threshold <- quantile(abs(wide_mat_demeaned), 0.995)
    replace <- abs(wide_mat_demeaned) > threshold
    wide_mat_demeaned[replace] <- sign(wide_mat_demeaned[replace]) * threshold
  }
  
  if(method == "shrinkage"){
    cov_list <- get_shrinkage_cov(wide_mat_demeaned, row_means, col_means, n_svd)
  } else if (method == "svd"){
    cov_list <- get_svd_cov(wide_mat_demeaned, row_means, col_means, n_svd, get_group_from_rowname)
  }
  
  row_cov <- cov_list$row_cov
  row_scores <- cov_list$row_scores 
  col_scores <- cov_list$col_scores 
  svd_d <- cov_list$svd_d
  
  row.names(row_cov) <- row.names(wide_mat)
  
  row_score_mat <- as.matrix(row_scores %>% select(starts_with("score.")))
  col_score_mat <- as.matrix(col_scores %>% select(starts_with("score.")))
  
  fitted <- row_score_mat %*% diag(svd_d) %*% t(col_score_mat)
  
  print("Fitted vs True values, check for similarity:")
  print("Fitted:")
  print(fitted[1:6, 1:6])
  print("True:")
  print({d <- wide_mat_demeaned; colnames(d) <- NULL; d[1:6, 1:6]})
  
  if(verbose){
    obs_cov <- cov(t(wide_mat_demeaned))
    lower_left <- row(obs_cov) > col(obs_cov)
    max_plot <- 10000
    if(sum(lower_left) > max_plot){
      subsample <- sample.int(sum(lower_left), max_plot)
    } else subsample <- rep(TRUE, sum(lower_left))
    
    obs_vec <- as.vector(obs_cov[lower_left])[subsample]
    shrink_vec <- as.vector(row_cov[lower_left])[subsample]
    
    p <- ggplot(
      data.frame(obs=obs_vec, shrink=shrink_vec),
      aes(x=obs,y=shrink)
    ) +
      geom_abline(slope=1)+
      geom_point(alpha = 0.3) 
    print(p)
  }
  
  return(SVDParams(
    row_cov=row_cov,
    row_scores=row_scores, 
    col_scores=col_scores, 
    svd_d = svd_d
  ))
}  

## hacky way to make types case
shrinkage_cov_as_matrix <- function(shrinkage){
  k <- nrow(shrinkage)
  if(!k == ncol(shrinkage)) stop("shrinkage isn't square. It should be.")
  return(shrinkage[1:k, 1:k])
}

get_shrinkage_cov <- function(
  wide_mat_demeaned, 
  row_means, 
  col_means, 
  n_svd, 
  ...
){
  row_cov <- cov.shrink(t(wide_mat_demeaned))
  row_cov <- shrinkage_cov_as_matrix(row_cov)
  e <- eigen(row_cov, symmetric=TRUE)
  
  svd_d <- e$values[1:n_svd]
  
  row_scores <- data.frame(
    row = row.names(wide_mat_demeaned),
    score = e$vectors[,1:n_svd],
    mean = row_means
  )
  
  ## col scores are dot product between row_scores and obs
  col_score_mat <- t(wide_mat_demeaned) %*% e$vectors[, 1:n_svd] %*% diag(1/svd_d)
  col_scores <- data.frame(
    col = colnames(wide_mat_demeaned),
    score = col_score_mat,
    mean = col_means
  )
  
  return(
    list(
      row_cov=row_cov,
      row_scores=row_scores,
      col_scores=col_scores,
      svd_d=svd_d
    )
  )
}


get_svd_cov <- function(
  wide_mat_demeaned, 
  row_means, 
  col_means, 
  n_svd, 
  get_group_from_rowname, 
  ...
){
  svd <- svd(wide_mat_demeaned, n_svd, n_svd)
  
  svd_d <- svd$d[1:n_svd]
  
  fitted <- svd$u %*% diag(svd_d) %*% t(svd$v)
  resid <- (wide_mat_demeaned - fitted) %>%
    as.data.frame() %>%
    rownames_to_column("row") %>%
    gather("column", "residual", -row) 
  
  print("Calculate group variance")
  group_resid <- resid %>%
    mutate(group=get_group_from_rowname(row)) %>%
    group_by(group, column) %>%
    summarise(mean_resid = mean(residual)) 
  
  resid_var <- var(as.vector(fitted - wide_mat_demeaned))
  group_var <- var(group_resid$mean_resid)
  
  print("Total Resid Var:"); print(resid_var)
  print("Group Variance:"); print(group_var)
  
  
  print("Calculating covariances")
  row_cov <- svd$u %*% diag(svd_d) %*% cov(svd$v) %*% diag(svd_d) %*% t(svd$u)
  for(group in unique(group_resid$group)){
    rows <- which(
      get_group_from_rowname(row.names(wide_mat_demeaned)) == group
    )
    row_cov[rows, rows] <- row_cov[rows, rows] + group_var 
  }
  
  diag(row_cov) <- diag(row_cov) + resid_var - group_var
  row_scores <- data.frame(
    row = row.names(wide_mat_demeaned),
    score = svd$u,
    mean = row_means
  )
  
  col_scores <- data.frame(
    col = colnames(wide_mat_demeaned),
    score = svd$v,
    mean = col_means
  )
  
  return(
    list(
      row_cov=row_cov,
      row_scores=row_scores,
      col_scores=col_scores,
      svd_d=svd_d
    )
  )
}

#################
## Sample from posterior
#################

mvrnorm_chol <- function(n, mu, sigma_chol){
  sample_raw <- matrix(rnorm(n * length(mu)), length(mu), n)
  sample <- t(sigma_chol) %*% sample_raw + matrix(mu, nrow=length(mu), ncol=n)
  rownames(sample) <- rownames(sigma_chol)
  return(t(sample))
}

memoised_chol <- memoise(chol)

posterior_sample <- function(
  svd, 
  obs, 
  obs_id, 
  column_mean,  ## prior for column_mean
  filter_to_ids=NULL,
  n_sim=400,
  verbose=FALSE
){
  
  vprint <- function(x) if(verbose) print(x) else NULL
  
  row_cov <- svd@row_cov
  row_scores <- svd@row_scores
  
  if(!is.null(filter_to_ids)){
    use_rows <- row.names(row_cov) %in% filter_to_ids
    row_cov <- row_cov[use_rows, use_rows]
    row_scores <- row_scores %>% filter(row %in% filter_to_ids)
    use_obs <- obs_id %in% filter_to_ids
    obs <- obs[use_obs]
    obs_id <- obs_id[use_obs]
  }
  
  obs <- obs - column_mean
  
  row_means <- row_scores$mean
  row_ids <- row_scores$row
  
  vprint("Calculating Matrices")
  
  a_rows <- match(obs_id, row_ids)
  c_rows <- seq_along(row_means)[-a_rows]
  
  ## Assume distribution is normal with covariance [A, B], [B', C]
  A <- row_cov[a_rows, a_rows]
  B <- row_cov[c_rows, a_rows]
  C <- row_cov[c_rows, c_rows]
  
  a <- obs
  
  vprint("Calculating A inverse")
  vprint(sprintf("nrow(A) = %s", nrow(A)))
  
  if(length(a_rows) > 0){
    A_inv <- solve(A)
  } else {
    A_inv <- A
  }
  
  prior_mean <- row_means + column_mean
  
  c_mean <- prior_mean[c_rows] + B %*% A_inv %*% (a - prior_mean[a_rows])
  
  c_sigma <- C - B %*% A_inv %*% t(B)
  if(length(a_rows) <= 1) rownames(c_sigma) <- names(B) ## otherwise it gets inherited
  
  vprint("Calculating Cholesky")
  chol_c_sigma <- memoised_chol(c_sigma)
  
  vprint("MVRNorm")
  if(nrow(c_sigma) > 0){
    sim <- mvrnorm_chol(n=n_sim, mu=c_mean, sigma_chol=chol_c_sigma)
    sim <- as.data.frame(sim) %>% 
      mutate(sim = 1:n_sim) %>%
      gather("row", "value", -sim)
  } else {
    sim <- data.frame(sim = integer(0), row = character(0), value = numeric(0))
  }
  
  vprint("Processing Simulations")
  
  obs_rep <- bind_rows(
    rep(
      list(data.frame(row=obs_id, value=obs)),
      n_sim
    ),
    .id="sim"
  ) %>% mutate(simulated=FALSE)
  
  sim <- sim %>%
    mutate(simulated=TRUE, sim=as.character(sim)) %>%
    bind_rows(obs_rep)
  
  return(sim)
}
