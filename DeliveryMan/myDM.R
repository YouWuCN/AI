library(DeliveryMan)

myFunction <- function(trafficMatrix, carInfo, packageMatrix)
{
  if(carInfo$load == 0){
    carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
  }else{
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  carInfo$nextMove <- nextMove(trafficMatrix, carInfo)
  return(carInfo)
}

nextPickup <- function(trafficMatrix, carInfo, packageMatrix){
  distanceVector <-
    abs(packageMatrix[,1] - carInfo$x) +
    abs(packageMatrix[,2] - carInfo$y)

  distanceVector[packageMatrix[,5] != 0] <- Inf
  
  nearestPackage <- which.min(distanceVector)
  return(packageMatrix[nearestPackage, c(1,2)])
}

nextMove <- function(trafficMatrix, carInfo){
  x = carInfo$x 
  y = carInfo$y
  hscore = heuristic(carInfo, x, y)
  if(hscore == 0){
    return(5)
  }
  frontier <- list(list(x=x, y=y,
                        m=NULL, g=0, h=hscore, f=hscore))
  visited <- list()
  current_element <- frontier[[1]]
  frontier <- frontier[-1]
  visited <- c(visited, list(current_element))
  
  while(current_element$h != 0){
    #extend node
    if (current_element$x < 10){
      x = current_element$x + 1
      y = current_element$y
      gscore = current_element$g + trafficMatrix$hroads[x-1,y] + 1
      hscore = heuristic(carInfo, x, y)
      if(if_extend(x,y,visited)){
      frontier <- c(frontier,list(list( x=x, y=y,
                                        m = c(current_element$m, 6), 
                                        g = gscore, 
                                        h = hscore, f = gscore + hscore)))}
    }
    if (current_element$y < 10){
      x = current_element$x
      y = current_element$y + 1
      gscore = current_element$g + trafficMatrix$vroads[x,y-1] + 1
      hscore = heuristic(carInfo, x, y)
      if(if_extend(x,y,visited)){
      frontier <- c(frontier,list(list( x=x, y=y,
                                        m = c(current_element$m, 8), 
                                        g = gscore, 
                                        h = hscore, f = gscore + hscore)))}
    }
    if (current_element$x > 1){
      x = current_element$x - 1
      y = current_element$y
      gscore = current_element$g + trafficMatrix$hroads[x,y] + 1
      hscore =  heuristic(carInfo, x, y)
      if(if_extend(x,y,visited)){
      frontier <- c(frontier,list(list( x=x, y=y,
                                        m = c(current_element$m, 4), 
                                        g = gscore, 
                                        h = hscore, f = gscore + hscore)))}
    }
    if (current_element$y > 1){
      x = current_element$x
      y = current_element$y - 1
      gscore = current_element$g + trafficMatrix$vroads[x,y] + 1
      hscore = heuristic(carInfo, x, y)
      if(if_extend(x,y,visited)){
      frontier <- c(frontier,list(list( x=x, y=y,
                                        m = c(current_element$m, 2), 
                                        g = gscore, 
                                        h = hscore, f = gscore + hscore)))}
    }
    
    scores <- sapply(frontier, function(node) node$f)
    best <- which.min(scores)
    
    current_element <- frontier[[best]]
    frontier <- frontier[-best]
    visited <- c(visited, list(current_element))
  }
  return(current_element$m[1])
  
}

if_extend <- function(x, y, Visited){
  if(length(Visited) == 1){
    return(TRUE)
  }
  for(node in Visited){
    if(node$x == x & node$y == y){
      return(FALSE)
    }
  }
  return(TRUE)
}

heuristic <- function(carInfo, x, y){
  mDistance <- abs(carInfo$mem$goal[1] - x) + 
    abs(carInfo$mem$goal[2] - y)
  return(3 * mDistance)
}