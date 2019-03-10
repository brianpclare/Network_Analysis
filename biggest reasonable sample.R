library(tidyverse)
library(magrittr)
library(data.table)


# chunk_size <- 216

# data <- fread("all_edges_real.csv",
#               colClasses = c("character", "character", "numeric"))
# 
# names <- union(data$node_1, data$node_2) %>%
# unique()
# 
# degrees1 <- data %>% filter(connection == 1) %>% group_by(node_1) %>%
#   tally() %>% rename(node = node_1)
# degrees2 <- data %>% filter(connection == 1) %>% group_by(node_2) %>%
#   tally() %>% rename(node = node_2)
# degrees <- bind_rows(degrees1, degrees2) %>% group_by(node) %>%
#   summarize(degree = sum(n)) %>% arrange(desc(degree))
# 
# dense_subset <- degrees %>% top_n(1200, degree)
# 
# names <- degrees$node %>% unlist()
# 
# data %<>% filter(node_1 %in% names) %>% filter(node_2 %in% names)
# fwrite(data, "dense_subset.csv")

data <- fread("dense_subset.csv", sep = ",", 
              colClasses = c("character", "character", "numeric"))

names <- union(data$node_1, data$node_2) %>%
  unique()

n <- length(names)
num_rows <- n * (n - 1) / 2
chunk_size <- 643


betas <- matrix(0, nrow = n, ncol = 1)
rownames(betas) <- names
pi_vec <- rep(0.5, chunk_size)

make_preds <- function(sample, chunk_size){
  x <- rep(0, chunk_size)
  for(i in 1:chunk_size){
    bl <- ncol(betas)
    x[i] <- 1/(1 + exp(-(betas[rownames(betas) == sample[[i, 1]], bl] +
                           betas[rownames(betas) == sample[[i, 2]], bl])))
    
  }
  return(unlist(x))
}

chunk_calc <- function(index, chunk_size, sample){
  Z <- matrix(0, nrow = chunk_size, ncol = n)
  colnames(Z) <- names
  Y <- rep(0, chunk_size)
  
  for (i in 1:chunk_size) {
    Z[[i, sample[[i, 1]]]] <- 1
    Z[[i, sample[[i, 2]]]] <- 1
    Y[i] <- sample[[i, 3]]
  }
  pi_vec <- make_preds(sample, chunk_size)
  W <- (pi_vec) * (1 - pi_vec)
  
  ZYPI <- crossprod(Z, (as.numeric(Y) - pi_vec))
  ZTWZ <- crossprod(Z, (W*Z))
  
  return(list(ZTWZ, ZYPI))
}

do_an_iteration <- function(chunk_size){
  index <- 0
  ztwz <- matrix(0, nrow = n, ncol = n)
  zypi <- rep(0, n)
  
  while(TRUE){
    df <- data[(index + 1):(index + chunk_size), ] %>% as.matrix()
    new_stuff <- chunk_calc(index, nrow(df), df)
    ztwz <- ztwz + new_stuff[[1]]
    zypi <- zypi + new_stuff[[2]]
    index <- index + chunk_size
    if(index >= num_rows){
      break
    }
  }
  
  bl <- ncol(betas)
  
  C <- chol(ztwz)
  
  betas <<- betas %>% cbind(betas[, bl] +
                              backsolve(C, backsolve(C, zypi,
                                                     transpose = TRUE)) )
}

do_an_iteration(chunk_size)

for(i in 2:10){
  cat("Iteration #", i, " started at ", Sys.time(), sep = "")
  do_an_iteration(chunk_size)
  cat("Iteration #", i, " ended at ", Sys.time(), sep = "")
}

data$preds <- make_preds(data, chunk_size = num_rows)
fwrite(data, "top_1287_with_preds.csv")
write_rds(betas, "top_1287_betas.rds")


data <- read_csv("top_1287_with_preds.csv")
degrees1 <- data %>% filter(connection == 1) %>% group_by(node_1) %>%
  tally() %>% rename(node = node_1)
degrees2 <- data %>% filter(connection == 1) %>% group_by(node_2) %>%
  tally() %>% rename(node = node_2)
degrees <- bind_rows(degrees1, degrees2) %>% group_by(node) %>%
  summarize(degree = sum(n)) %>% arrange(desc(degree))
betas <- read_rds("top_1287_betas.rds")
beta_df <- tibble(node = rownames(betas), beta = betas[, 11])
plot_ref <- inner_join(beta_df, degrees)
fwrite(plot_ref, "betas_degrees.csv")
