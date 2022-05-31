#' @title Fuzzy:Fuzzy Comprehension Evaluation Method
#'
#' @description Fuzzy is a analysis method based on fuzzy mathematics for comprehensive evaluation.
#'
#' @param r a numeric matrix
#' @param w a numeric vector
#' @param v a vector
#' @param s a numeric vector
#'
#' @return a list
#' @export
#'
#' @examples
#' r<-matrix(c(0.1029,0.3088,0.4853,0.1029,
#'             0.1764,0.2941,0.4216,0.1078,
#'             0.1471,0.3382,0.4118,0.1029),3,4,
#'             dimnames=list(c("Z1","Z2","Z3"),c("A","B","C","D")))
#' w<-c(0.1,0.3,0.6)
#' v<-c("A","B","C","D")
#' s<-c(100,80,60,40)
#' Fuzzy(r,w,v,s)

Fuzzy<-function(r,w,v,s){
  w1<-t(as.matrix(w))
  b<-w1 %*% r
  s1<-as.matrix(s)
  score<-(b %*% s1)/100
  res<-list("Comment level&score"=paste(v,":",s),"Result matrix"=b,"Index"=score[[1]])
  return(res)
}
