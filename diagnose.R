diagnose = function(network, cases)
{
  
  samples = data.frame(Pn = rep(0,1000), Te = rep(0,1000), VTB = rep(0,1000), TB = rep(0,1000), Sm = rep(0,1000),
                       LC = rep(0,1000), Br = rep(0,1000), XR = rep(0,1000), Dy = rep(0,1000))
  
  samples[1,] = cases[1,]
  samples$Pn[1] = sample(0:1,1)
  samples$TB[1] = sample(0:1,1)
  samples$LC[1] = sample(0:1,1)
  samples$Br[1] = sample(0:1,1)
  
  
  for (i in 2:1000) {
    
    temp = samples[i-1,]
    
    for (j in c(Pn,TB,LC,Br)) {
      # we evalute the configuration with changed parameter j
      probTemp_old = network$Pn[temp$Pn+1]*network$Sm[temp$Sm+1]*network$VTB[temp$VTB+1]
      probTemp_old = probTemp_old*dnorm(temp$Te,network$Te[temp$Pn+1,1],network$Te[temp$Pn+1,2])
      probTemp_old = probTemp_old*network$TB[temp$VTB+1,temp$TB+1]
      probTemp_old = probTemp_old*network$LC[temp$Sm+1,temp$LC+1]
      probTemp_old = probTemp_old*network$Br[temp$Sm+1,temp$Br+1]
      probTemp_old = probTemp_old*network$Dy[(temp$LC)*2+temp$Br+1,temp$Dy+1]
      probTemp_old = probTemp_old*network$XR[(temp$pn)*4 + (temp$TB)*2+temp$LC+1,temp$XR+1]
      
      
      # we assign a new value to the unknown parameter j
      if(samples$j[i-1] == 0){
        temp$j = 1
      }else{
        temp$j = 0
      }
      
      # we evalute the configuration with changed parameter j
      probTemp_new = network$Pn[temp$Pn+1]*network$Sm[temp$Sm+1]*network$VTB[temp$VTB+1]
      probTemp_new = probTemp_new*dnorm(temp$Te,network$Te[temp$Pn+1,1],network$Te[temp$Pn+1,2])
      probTemp_new = probTemp_new*network$TB[temp$VTB+1,temp$TB+1]
      probTemp_new = probTemp_new*network$LC[temp$Sm+1,temp$LC+1]
      probTemp_new = probTemp_new*network$Br[temp$Sm+1,temp$Br+1]
      probTemp_new = probTemp_new*network$Dy[(temp$LC)*2+temp$Br+1,temp$Dy+1]
      probTemp_new = probTemp_new*network$XR[(temp$pn)*4 + (temp$TB)*2+temp$LC+1,temp$XR+1]
      
      if(probTemp_new < probTemp_old){
        if(runif(1)> probTemp_new/probTemp_old ){
          if(samples$j[i-1] == 0){
            temp$j = 1
          }else{
            temp$j = 0
          }
          
        }
      }
      samples[i,] = temp
    }
     
  }
  
}