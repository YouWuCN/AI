library(WheresCroc)

myFunction <- function(moveInfo,readings,positions,edges,probs){
    
    if (length(moveInfo$mem)==1){
        moveInfo$mem$stateprob = initS(positions[1],positions[2])
    }
    
    if(moveInfo$mem$status == 1){
        moveInfo$mem$stateprob = initS(positions[1],positions[2])
        moveInfo$mem$status = 0
    }

    trans = getTransProbs(moveInfo,positions, edges)
    emiss = getEmission(readings, probs)
    
    if(!is.na(positions[1]) & positions[1]<0){
        moveInfo$mem$stateprob = rep(0,40)
        moveInfo$mem$stateprob[-positions[1]] = 1
    }else if(!is.na(positions[2]) & positions[2]<0){
        moveInfo$mem$stateprob = rep(0,40)
        moveInfo$mem$stateprob[-positions[2]] = 1
    }else{
        for(i in seq(40)){
            p = moveInfo$mem$stateprob[i]
            moveInfo$mem$stateprob[i] <- 
                (p * trans[[i]][2,2] +(1-p)* trans[[i]][1,2]) * emiss[i]
        }
        moveInfo$mem$stateprob[c(positions[1],positions[2])] = 0
    }
    sumation = sum(moveInfo$mem$stateprob)
    for (i in seq(40)){
        moveInfo$mem$stateprob[i] = moveInfo$mem$stateprob[i]/sumation
    }
    
    cur <- positions[3]
    if(moveInfo$mem$stateprob[1] < 0){
        print(moveInfo$mem$stateprob[1])
    }
    goal <- which.max(moveInfo$mem$stateprob)
    
    paths = bfs(cur, goal, edges)
    
    if(length(paths) == 1){
        moveInfo$moves = c(0, 0)
    }else if(length(paths) == 2){
        moveInfo$moves = c(paths[2], 0)
    }else{
        moveInfo$moves = c(paths[2], sample(c(paths[3],0), 1, prob = c(1,0)))
    }
        
    return(moveInfo)
}

getOptions <- function(position, edges){
    mv1 = edges[,2][edges[,1] == position]
    mv2 = edges[,1][edges[,2] == position]
    options = c(mv1, mv2)
    return(options)
}

transProbs <- function(moveInfo,position, edges){
    transmatrix <- matrix(nrow= 2,ncol = 2)
    options = getOptions(position,edges)
    transmatrix[2,2] = 1/(length(options)+1)
    transmatrix[2,1] = 1-transmatrix[2,2]
    
    inprob = 0
    for (option in options){
        atprob = moveInfo$mem$stateprob[option]
        inprob = atprob * 1/(length(getOptions(option,edges))+1) + inprob
    }
    transmatrix[1,2] = inprob
    transmatrix[1,1] = 1 - inprob
    return(transmatrix)
}

getTransProbs <- function(moveInfo,positions,edges){
    trans <- list()
    for (i in seq(40)){
        trans <- c(trans,list(transProbs(moveInfo, i, edges)))
    }
    return(trans)
}

getEmission <- function(readings, probs){
    emission <- c()
    for(position in 1:40){
        Psalinity = dnorm(readings[1], 
                          mean = probs$salinity[position,][1], 
                          sd = probs$salinity[position,][2])
        Pphosphate = dnorm(readings[2], 
                           mean = probs$phosphate[position,][1], 
                           sd = probs$phosphate[position,][2])
        Pnitrogen = dnorm(readings[3], 
                          mean = probs$nitrogen[position,][1], 
                          sd = probs$nitrogen[position,][2])
        e = Psalinity*Pphosphate*Pnitrogen
        emission = c(emission, e)
    }
    return(emission)
}

initS <- function(t1, t2){
    init_state = rep(0, 40)
    count = 38
    
    if(t1 < 0 & !is.na(t1)){
        init_state[-t1] = 1
    }
    else if(t2 < 0 & !is.na(t2)){
        init_state[-t1] = 1
    }
    else{init_state = rep(1, 40)
        if(is.na(t1)){
            count = count + 1
        }
        else{
            init_state[t1] = 0
        }
        if(is.na(t2)){
            count = count + 1
        }
        else{
            init_state[t2] = 0
        }
        
        init_state = init_state/(count)
    }
    
    return(init_state)
}

bfs <- function(p, goal, edges){
    frontier <- list(list(pos = p, path = p))
    current_node <- frontier[[1]]
    visited <- current_node
    while(current_node$pos != goal){
        extend <- getOptions(current_node$pos, edges)
        extend <- setdiff(extend, visited)
        for(ext in extend){
            if(ext == goal){
                return(c(current_node$path, ext))
            }
            frontier <- c(frontier, list(list(pos = ext, path = c(current_node$path, ext))))
            visited <- c(visited, ext)
        }
        frontier <- frontier[-1]
        current_node <- frontier[[1]]
    }
    return(current_node$path)
}

