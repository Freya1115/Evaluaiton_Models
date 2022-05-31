#' @title GRA: Grey Relational Analysis
#'
#' @description GRA is a multivariate statistical analysis method for comprehensive evaluation.
#'
#' @param data a numeric matrix
#' @param r a number
#' @param w a numeric vector
#'
#' @return  a list
#' @export
#'
#' @examples
#' r=0.4
#' w=c(0.2,0.2,0.3,0.1,0.1,0.1)
#' data<-matrix(c(1988,2061,2335,2750,3356,3806,
#'                386,408,422,482,511,561,
#'                839,846,960,1258,1577,1893,
#'                763,808,953,1010,1268,1352),
#'                4,6,byrow = TRUE,
#'                dimnames = list(c("A0","A1","A2","A3"),
#'                                c("Z1","Z2","Z3","Z4","Z5","Z6")))
#'GRA(data,r,w)

GRA<-function(data,r,w){

  mean<-t(matrix(apply(data,2,mean)))
  N<-matrix(nrow=nrow(data),ncol=ncol(data))
  for (i in 1:nrow(data)){
    for (j in 1:ncol(data)){
      N[i,j]<-data[i,j]/mean[,j]
    }
  }
  dimnames(N)<-dimnames(data)

  ref<-t(matrix(N[1,]))
  N1<-N[-1,]

  deta<-matrix(nrow=nrow(N1),ncol=ncol(N1))
  for (i in 1:nrow(N1)){
    for (j in 1:ncol(N1)){
      deta[i,j]<-abs(N1[i,j]-ref[[j]])
    }
  }
  dimnames(deta)<-dimnames(N1)

  min<-min(deta)
  max<-max(deta)
  eta<-(min+r*max)/(abs(deta)+r*max)
  score<-eta %*% w
  dimnames(score)[[1]]<-dimnames(eta)[[1]]
  dimnames(score)[[2]]<-"relational grade"

  outcome_score<-data.frame("object"=dimnames(score)[[1]],"relational grade"=score,"rank"=rank(-score))
  return(outcome_score)
}
