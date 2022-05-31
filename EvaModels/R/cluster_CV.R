#' @title cluster_CV:R-type system clustering combined with coefficient of variation method
#'
#' @description cluster_CV is a methods of screening evaluation indicators.
#'
#' @param data a numeric data frame or tibble
#' @param k a number
#'
#' @return a list
#' @export
#'
#' @examples
#' data<-as.data.frame(t(iris[,-5]))
#' k<-2
#' cluster_CV(data,k)
cluster_CV<-function(data,k){
  data1<-scale(data)
  result <- dist(data1, method = "euclidean")
  result_hc <- hclust(d = result, method = "ward.D2")
  plot(result_hc, hang = -1)
  re <- rect.hclust(result_hc, k = k,border = "red")
  cat("Clustering analysis results","\n")
  print(re)

  cat("Indicator screening results","\n")
  for (i in 1:k){
    cdata<-data[re[[i]],]
    t_cdata<-t(cdata)
    mean<-as.matrix(apply(t_cdata,2,mean))
    sd<-as.matrix(apply(t_cdata,2,sd))
    CV<-abs(sd/mean)
    CV<-data.frame("CV"=CV)
    max_CV<-subset(CV,CV==max(CV))
    max_CV<-data.frame(max_CV)
    names(max_CV)="max_CV"
    res<-list("CV_list"=CV,"max_CV"=max_CV)
    cat(paste("category",i,"\n"))
    print(res)
  }
}



