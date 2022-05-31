#' @title PCA:Principal Component Analysis
#'
#' @description PCA is a statistical method of dimension reduction that can be used to determine indicator weights.
#'
#' @param data a numeric data frame or tibble
#'
#' @return a list
#' @export
#'
#' @examples
#' data<-iris[,-5]
#' PCA(data)

PCA<-function(data){
  p<-princomp(data,cor = TRUE)
  m<-matrix(p$loadings,ncol=ncol(data))
  rownames(m)<-names(data)
  comp_name<-rownames(as.data.frame(p$sdev))
  colnames(m)<-comp_name

  m2<-matrix(nrow=nrow(m),ncol=ncol(m))
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      if(m[i,j]<0){
        m2[i,j]<--m[i,j]
      }else{
        m2[i,j]<-m[i,j]
      }
    }
  }

  sd<-p$sdev
  v<-m2/sqrt(sd)

  pv1 <- eigen(cor(data))$values
  pv2 <- pv1/sum(pv1)
  pv2_t<-t(pv2)
  pv2_t2<-pv2_t
  colnames(pv2_t2)<-comp_name
  pv2_t3<-matrix(pv2_t2)

  v2<-(v%*%pv2_t3)/sum(pv2_t3)
  w<-v2/sum(v2)
  outcome_w<-data.frame("indicator weight"=w)
  rownames(outcome_w)<-colnames(data)
  return(outcome_w)
}

