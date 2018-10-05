diagnose = function(network, cases)
{
  
  samples = matrix(0,1000,9)
  samples[1,]=cases[1,]
  samples[1,1] = sample(0:1,1)
  samples[1,4] = sample(0:1,1)
  samples[1,6] = sample(0:1,1)
  samples[1,7] = sample(0:1,1)
  
  
  for (i in 1:1000) {
    temp = samples[i,]
    
    # we evaluate the probability of the configuration i
    probi = 
    
    
    for (j in c(1,4,6,7)) {
      
      # we assign a new value to the unknown parameter j
      if(samples[i,j] == 0){
        temp[j] = 1
      }else{
        temp[j] = 0
      }
      
      # we evalute the configuration with changed parameter j
      
    }
    
    
    
  }
  
  
  
  
}