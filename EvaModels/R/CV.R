#' @title CV:Coefficient of variation method
#'
#' @description CV is a method that can be used to determine indicator weights.
#'
#' @param data a numeric data frame or tibble
#'
#' @return a data frame that contains:mean,std,CV,w
#' @export
#'
#' @examples
#' data<-iris[,-5]
#' CV(data)

CV<-function(data){
  mean<-as.matrix(apply(data,2,mean))
  sd<-as.matrix(apply(data,2,sd))
  CV<-abs(sd/mean)
  w<-CV/sum(CV)
  res<-cbind(mean,sd,CV,w)
  res<-data.frame(res)
  names(res)<-c("mean","sd","CV","weight")
  return(res)
}
