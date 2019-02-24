library(arm)
library(tidyverse)
library(magrittr)
library(data.table)


# sample <- read_csv("public_figure_edges.csv", col_types = "nn") %>% filter(node_1 != node_2)
# 
# high <- map2(sample$node_1, sample$node_2, max) %>% flatten_dbl()
# low <- map2(sample$node_1, sample$node_2, min) %>% flatten_dbl()
# sample$node_1 <- low %>% as.character()
# sample$node_2 <- high %>% as.character()
# rm(high, low)
# fwrite(sample, "cleaned up data.csv")
sample <- read_csv("cleaned up data.csv", col_types = "cc")


sample$connection <- 1

part_sample <- sample %>% filter(connection == 1) %>% 
  select(-connection)

names <- unique(
  union(part_sample$node_1, part_sample$node_2)
)

set.seed(4)
names <- sample(names, 1000)


n <- length(names)
num_rows <- n * (n - 1) / 2

# make only chunk_length rows in each dataframe
chunk_length = 500

# sample %<>% filter(node_1 %in% names) %>% filter(node_2 %in% names)
# 
# all_edges <- crossing(node_1 = names, node_2 = names)
# all_edges %<>% filter(as.numeric(node_1) < as.numeric(node_2))
# all_edges <- full_join(all_edges, sample)
# all_edges$connection <- ifelse(is.na(all_edges$connection), 0, 1)
fwrite(all_edges, "1k_test_edges.csv")
write_rds(all_edges, "rds_all_edges_1ktest")

rm(sample, part_sample)



index <- 0
if(index + chunk_length >= num_rows){
  chunk_length <<- num_rows - index
}

while(index < num_rows) {
  
  
  
  
  index <- index + chunk_length
}

# names needs to be outside
# all_edges is read chunk_length rows at a time inside (with colname fixing)

read_design_matrix <- function(index, chunk_length){

    wide <- data.table(connection = rep(0, chunk_length))
  
    for (j in names) {
      set(wide, j = j, value = 0)
    }
  
    all_edges <- fread("1k_test_edges.csv", 
                       colClasses = list(character = 1:2),
                       skip = index,
                       nrows = chunk_length)
    colnames(all_edges) <- c("node_1", "node_2", "connection")
    
    for (i in 1:chunk_length) {
      wide[[.subset2(all_edges, 1)[index + i]]][[i]] <- 1
      wide[[.subset2(all_edges, 2)[index + i]]][[i]] <- 1
      wide$connection[i] <- .subset2(all_edges, 3)[index + i]
    }
    
    print(index)

  return(wide)
  
}


make.data<-function(chunksize,...){
  conn<-NULL
  function(reset=FALSE){
    if(reset){
      if(!is.null(conn)) close(conn)
      conn<<-url(urlname,open="r")
    } else{
      rval<-read_design_matrix(conn, nrows=chunksize,...)
      if (nrow(rval)==0) {
        close(conn)
        conn<<-NULL
        rval<-NULL
      }
      return(rval)
    }
  }
}




########################################################


# wide <- fread("1k_test.csv")

##### move this stuff over to a new GLM script

# not looking for wide, new dataframe now. we'll worry about this later
test_model <- glm(data = wide, connection ~ 0 + ., 
                  family = binomial(link = "logit"))
summary(test_model)

preds <- predict(test_model, wide) %>% invlogit()
preds <- ifelse(preds > 0.5, "Pred1", "Pred0")

table <- table(wide$connection, preds)
wide$preds <- preds

degrees <- tibble(node = names, degree = 0, beta = test_model$coefficients)


for (i in 1:n) {
  node <- degrees$node[i]
  
  tbl <- sample %>% filter(node_1 == node | node_2 == node)
  
  degrees$degree[i] <- sum(tbl$connection)
}

degrees$logit <- logit(degrees$degree / (n - 1))
plot(data = degrees, beta ~ logit)
