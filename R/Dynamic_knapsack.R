#' Knapsack using Dynamic Programming
#' @description 
#' implementation of knapsack ysing dynamic programing algorithm
#' 
#' @param x dataframe containing v and w
#' @param W weight threshold  
#' @return value and objects that made up that value
#' @examples
#' n<-10000
#' knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
#' @export
#' 

knapsack_dynamic<-function(x,W){
  
  if(class(x)!="data.frame"){
    stop("x is not a dataframe")
  }
  if(any(x<0,na.rm=TRUE)){
    stop("x must contain positive values")
  }
  if(length(x)!=2){
    stop("x should have two columns")
  }
  if (colnames(x)[1] != "w" || colnames(x)[2] != "v")
  {
    stop("x does not contain w and/or v coloumns")
  }
 
  if(!(W>=0 && length(W)==1 && is.numeric(W))){
    stop("W must be one positive numeric value")
  }
  
  n<-nrow(x)
c<-W+1
r<-n+1
  mat<-matrix(ncol=c,nrow=r)
  mat[1,]<-rep(0,W+1)
  
  value<-x$v
  weight<-x$w
  i<-1
  
  while (i<=n) {
    for(j in 0:W){
      if(weight[i] > j){
        mat[i+1,j+1]<-mat[i,j+1]
      }else{
        mat[i+1,j+1]<-max(mat[i,j+1],mat[i,j+1-weight[i]]+value[i])
      }
     
    }
    i<-i+1
  }
  
   
  j=j+1  
  i<-which.max(mat[,j])
  elements<-length(n)
  k<-1
  elements[k]<-i-1
  
  while(mat[i,j]!=0 && j!=1 && i!=0){
    k<-k+1
    j<-(j-weight[i-1])
    i<-which(mat[,j] == mat[i-1,j])[1]
    elements[k]<-i-1
  }
  
  value<-round(mat[n+1,W+1])
  t<-elements[which(elements>0)]
  elements<-sort(t)
  
  values<-list(value=value,elements=elements)  
  return(values)
}
 #set.seed(42)
 #n <- 500
 #start_time <- Sys.time()
 #knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
 #knapsack_dynamic(x = knapsack_objects[1:8,],W=3500)
 #end_time <- Sys.time()
 #print(end_time - start_time)