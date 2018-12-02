

#' @title hclustr
#' @param x base
#' @param methdist  methode de distance
#' @param methHclust  methode de hclustering
#'
#'
#' @return classifH dendrogramme
#'
#' @export hclustr

hclustr<-function(x,methdist,methHclust,k2,ploting){


  x1<- scale(x,center=T,scale=T)
  #matrice des distances entre individus
  d.x1 <- dist(x1,methdist)

  classifH <- hclust(d.x1,method=methHclust)


  classes=cutree(classifH,k2)
  X_hc=cbind.data.frame(x,as.factor(classes))
  if(ploting==T){
    #affichage dendrogramme
    plot(classifH)}
  return(classifH)

}

