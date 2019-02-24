
make.data<-function(urlname, chunksize,...){
  conn<-NULL
  function(reset=FALSE){
    if(reset){
      if(!is.null(conn)) close(conn)
      conn<<-url(urlname,open="r")
    } else{
      rval<-read.table(conn, nrows=chunksize,...)
      if (nrow(rval)==0) {
        close(conn)
        conn<<-NULL
        rval<-NULL
      }
      return(rval)
    }
  }
}

airpoll<-make.data("http://faculty.washington.edu/tlumley/NO2.dat",
                   chunksize=150,
                   col.names=c("logno2","logcars","temp","windsp",
                               "tempgrad","winddir","hour","day"))

b<-bigglm(exp(logno2)~logcars+temp+windsp,
          data=airpoll, family=Gamma(log),
          start=c(2,0,0,0), maxit=30)

summary(b)

library(microbenchmark)
microbenchmark(
  "[32, 11]"      = mtcars[32, 11],
  "$carb[32]"     = mtcars$carb[32],
  "[[c(11, 32)]]" = mtcars[[c(11, 32)]],
  "[[11]][32]"    = mtcars[[11]][32],
  ".subset2"      = .subset2(mtcars, 11)[32]
)
