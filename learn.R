learn = function (hist)
  {
  
  #Pneumonia probability matrix
  probPn = matrix(0,1,2)
  probPn[1,1] = 1 - sum(hist$Pn)/length(hist$Pn)
  probPn[1,2] = 1 - probPn[1,1]
  
  #Visited TB spot probability matrix
  probVTB = matrix(0,1,2)
  probVTB[1,1] = 1-sum(hist$VTB)/length(hist$VTB)
  probVTB[1,2] = 1 - probVTB[1,1]
  
  #Smokes probability matrix
  probSm = matrix(0,1,2)
  probSm[1,1] = 1-sum(hist$Sm)/length(hist$Sm)
  probSm[1,2] = 1- probSm[1,1]
  
  #Probability Temperature given Pneumonia
  probTe = matrix(0,2,2)
  probTe[1,1] = mean(hist$Te[hist$Pn==0])
  probTe[1,2] = sd(hist$Te[hist$Pn==0])
  probTe[2,1] = mean(hist$Te[hist$Pn==1])
  probTe[2,2] = sd(hist$Te[hist$Pn==1])
  
  #Probability TB given VTB
  probTB = matrix(0,2,2)
  probTB[1,1] = 1-sum(hist$TB[hist$VTB==0])/length(hist$TB[hist$VTB==0])
  probTB[1,2] = 1-probTB[1,1]
  probTB[2,1] = 1-sum(hist$TB[hist$VTB==1])/length(hist$TB[hist$VTB==1])
  probTB[2,2] = 1 - probTB[2,1]
  
  #Probability lung cancer given smokes
  probLC = matrix(0,2,2)
  probLC[1,1] = 1-sum(hist$LC[hist$Sm==0])/length(hist$LC[hist$Sm==0])
  probLC[1,2] = 1-probLC[1,1]
  probLC[2,1] = 1-sum(hist$LC[hist$Sm==1])/length(hist$LC[hist$Sm==1])
  probLC[2,2] = 1 - probLC[2,1]
  
  #Probability bronchitis given smokes
  probBr = matrix(0,2,2)
  probBr[1,1] = 1-sum(hist$Br[hist$Sm==0])/length(hist$Br[hist$Sm==0])
  probBr[1,2] = 1-probBr[1,1]
  probBr[2,1] = 1-sum(hist$Br[hist$Sm==1])/length(hist$Br[hist$Sm==1])
  probBr[2,2] = 1 - probBr[2,1]
 
  
  
  #Probability dyspnea given lungcancer and bronchitis
  probDy = matrix(0,4,2)
  probDy[1,1] = 1-sum(hist$Dy[hist$Br[hist$LC==0]==0])/length(hist$Dy[hist$Br[hist$LC==0]==0])
  probDy[1,2] = 1-probDy[1,1]
  probDy[2,1] = 1-sum(hist$Dy[hist$Br[hist$LC==0]==1])/length(hist$Dy[hist$Br[hist$LC==0]==1])
  probDy[2,2] = 1-probDy[2,1]
  probDy[3,1] = 1-sum(hist$Dy[hist$Br[hist$LC==1]==0])/length(hist$Dy[hist$Br[hist$LC==1]==0])
  probDy[3,2] = 1-probDy[3,1]
  probDy[4,1] = 1-sum(hist$Dy[hist$Br[hist$LC==1]==1])/length(hist$Dy[hist$Br[hist$LC==1]==1])
  probDy[4,2] = 1-probDy[4,1]
  
  #Probability X-Ray given lungcancer, tuberculosis, pneumonia
  probXR = matrix(0,8,2)
  probXR[1,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==0]==0]==0])/length(hist$XR[hist$LC[hist$TB[hist$Pn==0]==0]==0])
  probXR[1,2] = 1 - probXR[1,1]
  probXR[2,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==0]==0]==1])/length(hist$XR[hist$LC[hist$TB[hist$Pn==0]==0]==1])
  probXR[2,2] = 1 - probXR[2,1]
  
  probXR[3,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==0]==1]==0])/length(hist$XR[hist$LC[hist$TB[hist$Pn==0]==1]==0])
  probXR[3,2] = 1 - probXR[3,1]
  probXR[4,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==0]==1]==1])/length(hist$XR[hist$LC[hist$TB[hist$Pn==0]==1]==1])
  probXR[4,2] = 1 - probXR[4,1]
  
  probXR[5,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==1]==0]==0])/length(hist$XR[hist$LC[hist$TB[hist$Pn==1]==0]==0])
  probXR[5,2] = 1 - probXR[5,1]
  probXR[6,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==1]==0]==1])/length(hist$XR[hist$LC[hist$TB[hist$Pn==1]==0]==1])
  probXR[6,2] = 1 - probXR[6,1]
  
  probXR[7,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==1]==1]==0])/length(hist$XR[hist$LC[hist$TB[hist$Pn==1]==1]==0])
  probXR[7,2] = 1 - probXR[7,1]
  probXR[8,1] = 1 - sum(hist$XR[hist$LC[hist$TB[hist$Pn==1]==1]==1])/length(hist$XR[hist$LC[hist$TB[hist$Pn==1]==1]==1])
  probXR[8,2] = 1 - probXR[8,1]
  
  network = list(probPn = probPn, probBr = probBr, probDy = probDy, probLC = probLC, probSm = probSm,
                 probVTB = probVTB, probTB = probTB, probXR = probXR, probTe = probTe)
  
  return(network)
}


