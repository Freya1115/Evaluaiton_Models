#' @title AHP: Analytic Hierarchy Process
#'
#' @description AHP is a multi-objective decision analysis method that can be used to determine indicator weights.
#'
#' @param data a numeric matrix
#'
#' @return a list
#' @export
#'
#' @examples
#' data<-matrix(c(1,1/2,1/6,2,1,1/4,6,4,1),3,3)
#' AHP(data)

AHP<-function(data){

  n = ncol(data)
  cumProd <- vector(length=n)
  cumProd <- apply(data, 1, prod)
  weight <- cumProd^(1/n)
  weight <- weight/sum(weight)


  RI<-c(0, 0, 0.52, 0.89, 1.12, 1.26, 1.36, 1.41, 1.46, 1.49, 1.52,1.54)
  Wi=weight
  n<-length(Wi)
  W<-matrix(Wi,ncol=1)
  DataIn<-as.matrix(data)
  judgeW<-data %*% W
  judgeW1<-as.vector(judgeW)
  lamda<-sum(judgeW1/Wi)/n
  CI=(lamda-n)/(n-1)
  CR=CI/RI[n]
  if (CR <= 0.1){
    res<-list("Consistency test results" = "pass","CI" = CI,"CR"=CR,"weight"=Wi)
    return(res)
  }
  else{
    res<-list("Consistency test results" = "fail","indicator weight"=Wi)
    return(res)
  }
}
