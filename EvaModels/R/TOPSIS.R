#' @title TOPSIS:Technique for Order Preference by Similarity to an Ideal Solution
#'
#' @description TOPSIS is a ranking method approximating to the ideal solution for comprehensive evaluation.
#'
#' @param data a numeric data frame or tibble
#' @param w a numeric vector
#' @param type a numeric vector
#'
#' @return a data frame
#' @export
#'
#' @examples
#' data<-iris[,-5]
#' w<-c(0.2,0.1,0.3,0.4)
#' type<-c(1,1,1,1)
#' TOPSIS(data,w,type)

TOPSIS<-function(data,w,type){

  rescale<-function(x,type){
    if (type==1) {
      (x-min(x))/(max(x)-min(x))
    }else if(type==2){
      (max(x)-x)/(max(x)-min(x))
    }
  }
  N<-purrr::map2_dfc(data,type,rescale)
  N1<-as.matrix(N)

  weight<-w
  weight<-weight/sum(weight)
  W<-diag(weight)
  V<-N1 %*% W


  u = apply(V, 2, max)
  l = apply(V, 2, min)

  distance_u<-function(x){
    sqrt(sum(x-u)^2)
  }

  distance_l<-function(x){
    sqrt(sum(x-l)^2)
  }

  du <- apply(V, 1, distance_u)
  dl <- apply(V, 1, distance_l)

  score <- dl/(dl + du)
  outcome_score<-data.frame("object"=rownames(data),"index"=score,"rank"=rank(-score))
  return(outcome_score)
}

