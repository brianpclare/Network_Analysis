f1 <- function(){
  wide1 <<- tibble(connection = 0, .rows = chunk_length)
}

f2 <- function(){
  wide2 <<- tibble(connection = rep(0, chunk_length))
}

f3 <- function(){
  wide3 <<- data.frame(connection = rep(0, chunk_length))
}

f4 <- function(){
  wide4 <<- data.table(connection = rep(0, chunk_length))
}

chunk_length = 500000

library(microbenchmark)
microbenchmark(f1(), f2(), f3(), f4())

index <- 500
j <- 10
microbenchmark(
  .subset2(all_edges, 3)[index + j],
  all_edges$connection[index + j]
)

# wide1 and wide2 are tibbles
# wide3 is a data.frame, wide4 is a data.table
library(nycflights13)


r2 <- function(){
  m2 <- flights %>% as_tibble()
  x2 <- m2[m2$origin == "EWR", ]
}

r3 <- function(){
  m3 <- flights %>% as.data.frame()
  x3 <- m3[m3$origin == "EWR", ]
}

r4 <- function(){
  m4 <- flights %>% as.data.table()
  x4 <- m4[m4$origin == "EWR", ]
}

microbenchmark(r2(), r3(), r4())

a1 <- function(){
  wide1[["newcol"]] <<- 0 
}

a2 <- function(){
  wide2$newcol <<- 0
}

a4 <- function(){
  wide4[, "newcol" := 0]
}

microbenchmark(a1(), a2(), a4())

# what is := even doing, wide4 doesn't change

# example
DT = data.table(a = LETTERS[c(3L,1:3)], b = 4:7)
DT = DT[, c := 8] # add a numeric column, 8 for all rows

# try set?

t2 <- function(){
  wide2 <<- tibble(connection = rep(0, chunk_length))
  
  for (i in 1:n) {
    wide2[[names[i]]] <- 0
  }
  
}

t4 <- function(){
  wide4 <<- data.table(connection = rep(0, chunk_length))
  
  for(j in names){
    set(wide4, j = j,value = 0)
  }
}

microbenchmark(t2(), t4())
# finally, data.table improves speed for something!

# maybe another use - subsetting? example first

library(data.table)
DT <- data.table(a=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                 b=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                 c=sample(20), key=c('a', 'b'))

sub <- subset(DT, a == 'a')
all.equal(key(sub), key(DT))

subset(DT, a == "a" & b == "b")


microbenchmark(all_edges$node_1[8], .subset2(all_edges, 1)[8])

microbenchmark(
  {ab <- fread("1k_test_edges.csv", colClasses = list(character = 1:2))
  colnames(ab) <- c("node_1", "node_2", "connection")},
  bc <- read_rds("rds_all_edges_1ktest")
)
#freading a csv is faster than read_rds!

microbenchmark(
  all_edges %>% filter(node_1 == "10004") %>% select(connection),
  select(filter(all_edges, node_1 == "10004"), connection)
)

# pipes take a tiny bit longer to run. probably not relevant most of the time