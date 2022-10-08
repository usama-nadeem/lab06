#' Knapsack using greedy heuristic to reduce complexity
#' @description 
#' implementation of knapsack greedy algorithm
#' 
#' @param x dataframe containing v and w
#' @param W weight threshold  
#' @return value and objects that made up that value
#' @examples
#' n<-10000
#' knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' @export
#' 

greedy_knapsack<-function(x,W)
{    
  if (W <= 0 || !(is.numeric(W)))
  {
    stop("Weight W must be positive number")
  }
  
  else if (class(x)!="data.frame")
  {
    stop("x is not a dataframe")
  }
  else if (length(x)!=2)
  {
    stop("there must be two columns")
  }
  else if (colnames(x)[1] != "w" || colnames(x)[2] != "v")
  {
    stop("x does not contain w and/or v coloumns")
  }
  
  sorted <- x[order(x$v / x$w, decreasing = TRUE),]
  
  IndexArray <- array(dim=length(sorted$ratio))
  m <- 0
  for (row in 1:nrow(sorted)) 
  {
    if (sorted[row,'w'] <= W) 
    {
      IndexArray[row] <- TRUE
      m <- m + sorted[row,'v']
      W = W - sorted[row,'w']
    }
    else
    {
      IndexArray[row] <- W/sorted[row,'w']
      m <- m + sorted[row,"v"]*W/sorted[row,'w']
      break
    }
  }
  
  ValueInd <- which(IndexArray==TRUE)
  final_value <- round(sum(sorted[ValueInd,"v"]))
  e <- unlist(lapply(rownames(sorted[ValueInd,]), as.integer))
  ans= list(final_value,e)
  names(ans) <- c("value", "elements")
  return(ans)
  
}
#set.seed(42)
#n <- 1000000
#start_time <- Sys.time()
#knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
#greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#end_time <- Sys.time()
#print(end_time - start_time)

