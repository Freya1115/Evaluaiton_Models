#' @title norm:Normalization
#'
#' @description Norm is a method for normalizing positive and negative indicators.
#'
#' @param data a numeric data frame or tibble or matrix
#' @param type a numeric vector
#'
#' @return a normalized data frame
#' @export
#'
#' @examples
#' data<-iris[1:10,-5]
#' type<-c(1,1,1,1)
#' norm(data,type)

norm<-function(data,type){
  rescale<-function(x,type){
    if (type==1) {
      (x-min(x))/(max(x)-min(x))
    }else if(type==2){
      (max(x)-x)/(max(x)-min(x))
    }
  }
  norm_data<-purrr::map2_dfc(data,type,rescale)
  norm_data<-as.data.frame(norm_data)
  return(norm_data)
}

