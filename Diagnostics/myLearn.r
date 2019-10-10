library(Diagnostics)

myLearn <- function(data){
    prob_Pn <- matrix(nrow = 1,ncol = 2)
    prob_VTB <- matrix(nrow = 1,ncol = 2)
    prob_Sm <- matrix(nrow = 1,ncol = 2)
    prob_Pn[1,1] = nrow(data[data$Pn==0,])/nrow(data)
    prob_Pn[1,2] = 1- prob_Pn[1,1]
    prob_VTB[1,1] = nrow(data[data$VTB==0,])/nrow(data)
    prob_VTB[1,2] = 1- prob_VTB[1,1]
    prob_Sm[1,1] = nrow(data[data$Sm==0,])/nrow(data)
    prob_Sm[1,2] = 1- prob_Sm[1,1]
    
    prob_Br_Sm <- matrix(nrow = 2, ncol = 2)
    prob_Br_Sm[1,1] <- nrow(data[data$Sm==0&data$Br==0,])/nrow(data[data$Sm==0,])
    prob_Br_Sm[1,2] = 1- prob_Br_Sm[1,1]
    prob_Br_Sm[2,1] <- nrow(data[data$Sm==1&data$Br==0,])/nrow(data[data$Sm==1,])
    prob_Br_Sm[2,2] = 1- prob_Br_Sm[2,1]

    prob_LC_Sm <- matrix(nrow = 2, ncol = 2)
    prob_LC_Sm[1,1] <- nrow(data[data$Sm==0&data$LC==0,])/nrow(data[data$Sm==0,])
    prob_LC_Sm[1,2] = 1- prob_Br_Sm[1,1]
    prob_LC_Sm[2,1] <- nrow(data[data$Sm==1&data$LC==0,])/nrow(data[data$Sm==1,])
    prob_LC_Sm[2,2] = 1- prob_Br_Sm[2,1]
    
    prob_TB_VTB <- matrix(nrow = 2, ncol = 2)
    prob_TB_VTB[1,1] <- nrow(data[data$VTB==0&data$TB==0,])/nrow(data[data$VTB==0,])
    prob_TB_VTB[1,2] = 1- prob_TB_VTB[1,1]
    prob_TB_VTB[2,1] <- nrow(data[data$VTB==1&data$TB==0,])/nrow(data[data$VTB==1,])
    prob_TB_VTB[2,2] = 1- prob_TB_VTB[2,1]
    
    prob_Dy_BrLC <- matrix(nrow = 4, ncol = 2)
    prob_Dy_BrLC[1,1] <- nrow(data[data$LC==0&data$Br==0&data$Dy==0,])/nrow(data[data$LC==0&data$Br==0,])
    prob_Dy_BrLC[1,2] = 1- prob_Dy_BrLC[1,1]
    prob_Dy_BrLC[2,1] <- nrow(data[data$LC==1&data$Br==0&data$Dy==0,])/nrow(data[data$LC==1&data$Br==0,])
    prob_Dy_BrLC[2,2] = 1- prob_Dy_BrLC[2,1]
    prob_Dy_BrLC[3,1] <- nrow(data[data$LC==0&data$Br==1&data$Dy==0,])/nrow(data[data$LC==0&data$Br==1,])
    prob_Dy_BrLC[3,2] = 1- prob_Dy_BrLC[3,1]
    prob_Dy_BrLC[4,1] <- nrow(data[data$LC==1&data$Br==1&data$Dy==0,])/nrow(data[data$LC==1&data$Br==1,])
    prob_Dy_BrLC[4,2] = 1- prob_Dy_BrLC[4,1]
    
    prob_Te_Pn <- matrix(nrow = 2, ncol = 2)
    prob_Te_Pn[1,1] <- mean(data[data$Pn==0,]$Te)
    prob_Te_Pn[1,2] <- sd(data[data$Pn==0,]$Te)
    prob_Te_Pn[2,1] <- mean(data[data$Pn==1,]$Te)
    prob_Te_Pn[2,2] <- sd(data[data$Pn==1,]$Te)
    
    prob_XR_PnTBLC <- matrix(nrow = 8, ncol = 2)
    prob_XR_PnTBLC[1,1] <- nrow(data[data$Pn==0&data$TB==0&data$LC==0&data$XR==0,])/
        nrow(data[data$Pn==0&data$TB==0&data$LC==0,])
    prob_XR_PnTBLC[1,2] = 1- prob_XR_PnTBLC[1,1]
    prob_XR_PnTBLC[2,1] <- nrow(data[data$Pn==1&data$TB==0&data$LC==0&data$XR==0,])/
        nrow(data[data$Pn==1&data$TB==0&data$LC==0,])
    prob_XR_PnTBLC[2,2] = 1- prob_XR_PnTBLC[2,1]
    prob_XR_PnTBLC[3,1] <- nrow(data[data$Pn==0&data$TB==1&data$LC==0&data$XR==0,])/
        nrow(data[data$Pn==0&data$TB==1&data$LC==0,])
    prob_XR_PnTBLC[3,2] = 1- prob_XR_PnTBLC[3,1]
    prob_XR_PnTBLC[4,1] <- nrow(data[data$Pn==0&data$TB==0&data$LC==1&data$XR==0,])/
        nrow(data[data$Pn==0&data$TB==0&data$LC==1,])
    prob_XR_PnTBLC[4,2] = 1- prob_XR_PnTBLC[4,1]
    prob_XR_PnTBLC[5,1] <- nrow(data[data$Pn==1&data$TB==1&data$LC==0&data$XR==0,])/
        nrow(data[data$Pn==1&data$TB==1&data$LC==0,])
    prob_XR_PnTBLC[5,2] = 1- prob_XR_PnTBLC[5,1]
    prob_XR_PnTBLC[6,1] <- nrow(data[data$Pn==1&data$TB==0&data$LC==1&data$XR==0,])/
        nrow(data[data$Pn==1&data$TB==0&data$LC==1,])
    prob_XR_PnTBLC[6,2] = 1- prob_XR_PnTBLC[6,1]
    prob_XR_PnTBLC[7,1] <- nrow(data[data$Pn==0&data$TB==1&data$LC==1&data$XR==0,])/
        nrow(data[data$Pn==0&data$TB==1&data$LC==1,])
    prob_XR_PnTBLC[7,2] = 1- prob_XR_PnTBLC[7,1]
    prob_XR_PnTBLC[8,1] <- nrow(data[data$Pn==1&data$TB==1&data$LC==1&data$XR==0,])/
        nrow(data[data$Pn==1&data$TB==1&data$LC==1,])
    prob_XR_PnTBLC[8,2] = 1- prob_XR_PnTBLC[8,1]
    myNet <- list(prob_Pn,prob_VTB,prob_Sm,prob_Te_Pn,prob_TB_VTB,prob_Br_Sm,
                  prob_LC_Sm,prob_Dy_BrLC,prob_XR_PnTBLC)
    names(myNet) <- c('Pn','VTB','Sm','Te_Pn','TB_VTB','Br_Sm','LC_Sm','Dy_BrLC','XR_PnTBLC')
    
    for(i in seq(length(myNet))){
        rownames(myNet[[i]]) <- 0:(nrow(myNet[[i]]) -1) 
        colnames(myNet[[i]]) <- 0:(ncol(myNet[[i]]) -1)
    }
    
    return(myNet)
}