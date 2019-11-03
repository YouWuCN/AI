library(WobbiesWorld)

myFunction <- function(maze){
  decideAction <- function(cont, maze_){
    
    cont$wobbie_cur = which(maze_$maze$x == maze_$wobbie[1] & 
                            maze_$maze$y == maze_$wobbie[2])
    cont$monster_cur = which(maze_$maze$x == maze_$monster1[1] & 
                             maze_$maze$y == maze_$monster1[2])
    
    Q_table = matrix(0, 729, 4, dimnames = list(c(),c('2','4','6','8')))
    Q_row_index = 27*(cont$wobbie_cur-1) + cont$monster_cur

    if(is.null(cont$Q_table)){
      cont$Q_table = Q_table
    }
    
    explore_rate = 0.05
    if(cont$doRand & runif(1,0,1) < explore_rate){
      list(move = sample(c(2, 4, 6, 8), 1), control = cont)
    }else{
      move = as.numeric(names(which.max(cont$Q_table[Q_row_index,])))
      list(move = move, control = cont)
    }
  }
  
  update <- function(cont, maze_){
    lr = 0.05 #learning rate
    gama = 0.8 #discount factor
    explore_rate = 0.05
    
    wobbie_next = which(maze_$maze$x == maze_$wobbie[1] &
                           maze_$maze$y == maze_$wobbie[2])
    monster_next = which(maze_$maze$x == maze_$monster1[1] & 
                           maze_$maze$y == maze_$monster1[2])
    
    
    old_row_index = 27*(cont$wobbie_cur-1) + cont$monster_cur
    new_row_index = 27*(wobbie_next-1) + monster_next
    
    if(cont$doRand & runif(1,0,1) < explore_rate){
      next_move = sample(c('2','4','6','8'), 1)
    }else{
      next_move = names(which.max(cont$Q_table[new_row_index,]))
    }
    
    last_move = as.character(maze_$lastAction)
    Q = cont$Q_table[old_row_index, last_move]
    Q_prime = cont$Q_table[new_row_index, next_move]
    
    cont$Q_table[old_row_index, last_move] = Q + lr*(maze_$reward + gama*Q_prime - Q)
    
    return(cont)
  }
  return(list(decideAction = decideAction, update = update, doRand = TRUE))
}