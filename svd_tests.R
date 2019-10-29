setwd("C://Users/Jonathan Tannen/Dropbox/sixty_six/posts/election_night_needle/")

library(MASS)
source("cov_shrinkage.R")

set.seed(215)

n_row <- 1000
n_svd <- 5
n_obs <- 100

## Method 1
# A <- matrix(rnorm(n_row^2), n_row, n_row)
# A <- A / rowSums(abs(A))
# true_cov <- A %*% t(A)

## Method 2: Block diag
true_cov <- matrix(0, n_row, n_row)
for(i in 1:(n_row/10)){
  use_rows <- which(ceiling((1:n_row)/10) == i)
  true_cov[use_rows, use_rows] <- 1/i
}
diag(true_cov) <- diag(true_cov) + 0.1

upper_right <- row(true_cov) < col(true_cov)
lower_left <- row(true_cov) > col(true_cov)

## Method 3: lower traingular
# true_cov <- diag(1:n_row)
# true_cov[upper_right] <- row(true_cov)[upper_right]
# true_cov[lower_left] <- col(true_cov)[lower_left]

obs <- mvrnorm(n=n_obs, mu=rep(0, n_row), Sigma=true_cov)

obs_cov <- cov(obs)
obs_cov[1:3, 1:3]
true_cov[1:3, 1:3]

## Just checks if the mvrnorm worked
x <- as.vector(true_cov[lower_left])
y <- as.vector(obs_cov[lower_left])
subsample <- sample.int(length(x), 1000)
mean(sign(y) * (y - x))


ggplot(
  data.frame(x=x[subsample], y=y[subsample]),
  aes(x=x, y=y)
) + geom_point() + geom_abline(slope=1) + 
  coord_fixed() + theme_minimal()

colnames(obs) <- sprintf("%04d", 1:n_row)
row.names(obs) <- sprintf("%04d", 1:n_obs)

svd <- get_svd(t(obs), n_svd=n_svd)

svd@row_cov[1:3, 1:3]
obs_cov[1:3, 1:3]
true_cov[1:3, 1:3]

y <- as.vector(svd@row_cov[lower_left])

ggplot(
  data.frame(x=x[subsample], y=y[subsample]),
  aes(x=x, y=y)
) + geom_point() + geom_abline(slope=1) + 
  coord_fixed() + theme_minimal()

mean(sign(y) * (y - x))
