

myDiagnose <- function(myNet,cases){
    results <- matrix(nrow = 10, ncol = 4)
    rownames(results) <- 1:10
    colnames(results) <- c('Pn','TB','LC','Br')
    
    #init: Pn=0,TB=0,LC=0,Br=0
    obs = cases[1,]
    obs[,c('Pn','TB','LC','Br')]=0
    PPn = myNet$Pn[1,obs$Pn]
    PTe = dnorm(obs$Te,myNet$Te_Pn[obs$Pn,1],myNet$Te_Pn[obs$Pn,2])
    PTB = myNet$TB_VTB[(obs$VTB),obs$TB]
    PLC = myNet$LC_Sm[(obs$Sm),obs$PLC]
    PBr = myNet$Br_Sm[(obs$Sm),obs$Br]
    PXR = myNet$XR_PnTBLC[obs$Pn,obs$TB,LC]
    
    
    return(list('a','b'))
}

