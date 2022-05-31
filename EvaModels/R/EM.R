#' @title EM:Entropy Method
#'
#' @description EM is a method that can be used to determine indicator weights.
#'
#' @param data a numeric data frame or tibble
#' @param type a numeric vector
#'
#' @return a data frame
#' @export
#'
#' @examples
#' data<-iris[,-5]
#' type<-c(1,2,1,1)
#' EM(data,type)

EM<-function(data,type){

  rescale<-function(x,type){
    if (type==1) {
      (x-min(x))/(max(x)-min(x))
    }else if(type==2){
      (max(x)-x)/(max(x)-min(x))
    }
  }

  first1 = function(data){
    x = c(data)
    for(i in 1:length(data))
      x[i] = data[i]/sum(data[])
    return(x)
  }

  first2 = function(data){
    x = c(data)
    for(i in 1:length(data)){
      if(data[i] == 0){
        x[i] = 0
      }else{
        x[i] = data[i] * log(data[i])
      }
    }
    return(x)
  }

  data2<-purrr::map2_dfc(data,type,rescale)
  dataframe1<-apply(data2,2,first1)
  dataframe2<-apply(dataframe1,2,first2)
  k <- 1/log(length(dataframe2[,1]))
  d <- -k * colSums(dataframe2)
  d <- 1-d
  w <- d/sum(d)
  outcome_w<-data.frame("indicator weight"=w)
  return(outcome_w)
}

