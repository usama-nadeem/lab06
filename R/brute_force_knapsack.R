brute_force_knapsack<-function(x,W)
{    
  #stop if W is not integer or it is not greater than 0
  if (W <= 0 || !(is.numeric(W)))
  {
    stop("Weight W must be positive number")
  }
  
  else if (!(is.data.frame(x)))
  {
    stop("x is not a dataframe")
  }
  else if (colnames(x)[1] != "w" || colnames(x)[2] != "v")
  {
    stop("x does not contain w and/or v coloumns")
  }
  CurrentMax<-0
  value<-0
  NumOfElements<-dim(x)[1]
  elements<-length(n)
  
  lapply(1:NumOfElements, function(i)
  {
    PossibleCombinations <-combn(NumOfElements,i)
    j<-1
    while(j <= ncol(PossibleCombinations))
    {
      #check some of weights of all PossibleCombinationsinations to be less than upper limit
      if(sum(x$w[PossibleCombinations[,j]]) <= W)
      {
        value<-sum(x$v[PossibleCombinations[,j]])
        
        if(value > CurrentMax)
        {
          #if the value of the current PossibleCombinationsination is more than the previous max value, we will store it as new max 
          #since it is the new max value in knapsack, so we will store elements that made this value 
          CurrentMax <<- value
          elements <<- PossibleCombinations[,j]
        }
        
      }
      
      j<-j+1
    }
  })
  
  values<-list(value=round(CurrentMax),elements=elements)
  return(values)
}

#random seed is not consistent on different R versions, so use RNGkind
# RNGkind(sample.kind = "Rounding")
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# 
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
