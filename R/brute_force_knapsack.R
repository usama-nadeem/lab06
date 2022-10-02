brute_force_knapsack<-function(x,W, parallel=FALSE)
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
  
  value<-0
  CurrentMax<-0
  NumOfElements<-dim(x)[1]
  elements<-length(n)
  final_list = list()
  if (!parallel)
  {
    lapply(1:NumOfElements, function(i)
    {
      PossibleCombinations <-combn(NumOfElements,i)
      itr<-1
      while(itr <= ncol(PossibleCombinations))
      {
        #check some of weights of all PossibleCombinations to be less than upper limit
        if(sum(x$w[PossibleCombinations[,itr]]) <= W)
        {
          value<-sum(x$v[PossibleCombinations[,itr]])
          if(value > CurrentMax)
          {
            #if the value of the current PossibleCombinations is more than the previous max value, we will store it as new max 
            #since it is the new max value in knapsack, so we will store elements that made this value 
            CurrentMax <<- value
            elements <<- PossibleCombinations[,itr]
          }
          
        }
        itr<-itr+1
      }
    })
  
    final_list<-list(value=round(CurrentMax),elements=elements)
  }
  
  else if (parallel)
  {
    nodes <- parallel::makeCluster(parallel::detectCores()/2)
    #exporting variables
    parallel::clusterExport(nodes, varlist=c("x","W","n","elements","CurrentMax","value"), envir=environment())
    parallel::clusterEvalQ(nodes, library(utils))
    Value <- parallel::parLapply(nodes, 1:NumOfElements, function(i, x, W) {
      
      PossibleCombinations <- utils::combn(NumOfElements,i)
      
      itr <- 1
      while(itr<=ncol(PossibleCombinations))
      { 
        if(sum(x$w[PossibleCombinations[,itr]]) <= W)
        {
          value<-sum(x$v[PossibleCombinations[,itr]])
          if(CurrentMax<value)
          {
            elements<-PossibleCombinations[,itr]
            CurrentMax<-value
          }
        }
        itr <- itr+1
      }
      
      return(list(value=round(CurrentMax),elements=elements))
      
    }, x, W )
    
    i=1
    while(Value[[i]]["value"]!=0)
    {
      elements<-Value[[i]]["elements"]
      value<-Value[[i]]["value"]
      i<-i+1
    }
    final_list= c(value,elements)
    parallel::stopCluster(nodes) 
  }
  
  return (final_list)
}

# WithoutParallelTime = system.time (brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel=FALSE))
# ParallelTime = system.time (brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel=TRUE))
# print ("Without Parallelisation: ")
# print (unname(WithoutParallelTime[1]))
# print ("ParallelTime: ")
# print (unname((ParallelTime[1])))
# Diff= ((WithoutParallelTime[1])-ParallelTime[1])
# print ("Percentage Improvement due to parallelisation: ")
# print (unname(Diff))
