myDiagnose <- function(myNet,cases){
    myNet$XR_PnTBLC[8,] = c(0.001,0.999)
    for(n in seq(10)){
        obs = cases[n,]
        obs[,c('Pn','TB','LC','Br')]=0
        results <- c(0,0,0,0)
        prob = getprob(obs, myNet)
        randnum = runif(4000, 0, 1)
        for(i in seq(1000)){
            target = c('Pn','TB','LC','Br')
            while(length(target) > 0){
                cur = sample(target, 1, prob = rep(1, length(target)))
                target <- target[target != cur]
                proposal = obs
                proposal[cur] = (proposal[cur] + 1) %% 2
                
                new_prob =  getprob(proposal, myNet)
                if(new_prob >= prob){
                    prob = new_prob
                    obs = proposal
                }else{
                    if(randnum[1] > new_prob/prob){
                        obs = proposal
                        prob = new_prob
                        randnum = randnum[-1]
                    }
                }
            }
            if(i>100){
                results <- results+obs[,c('Pn','TB','LC','Br')]
            }
        }
        results = results/900
        cases[n, c('Pn','TB','LC','Br')] = results
    }
    return(cases)
}

getprob <- function(obs, myNet){
    
    PPn = myNet$Pn[1, obs$Pn+1]
    PTe = dnorm(obs$Te, myNet$Te_Pn[obs$Pn+1, 1], myNet$Te_Pn[obs$Pn+1, 2])
    PTB = myNet$TB_VTB[(obs$VTB+1), obs$TB+1]
    PLC = myNet$LC_Sm[(obs$Sm+1), obs$LC+1]
    PBr = myNet$Br_Sm[(obs$Sm+1), obs$Br+1]
    
    XR_line = (obs$Pn)*4+(obs$TB)*2+(obs$LC)*1
    PXR = myNet$XR_PnTBLC[XR_line+1,obs$XR+1]
    Dy_line = (obs$Br)*2+(obs$LC)*1
    PDy = myNet$Dy_BrLC[Dy_line+1,obs$Dy+1] 
    
    prob = PPn*PTe*PTB*PLC*PBr*PXR*PDy
    
    return(prob)
}





