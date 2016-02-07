#algorithm to solve the polygon puzzle:

polygon <- function(sides = 4, initial_vals = 1:4, cycles = 1){
  if(sides < 3){
    stop("Error: Number of sides must be greater than or equal to three.\n\n")
  }
  if(length(initial_vals) != sides){
    stop("Error: Number of initial values must match number of sides of the polygon.\n\n")
  }
  else{
    for(i in 1:cycles){
      temp2 <- vector('numeric',sides-1)
      for(j in 1:sides){
        if(j == 1){
          temp <- mean(c(initial_vals[sides], initial_vals[j+1]))
          next
        }
        if(j == sides){
          temp3 <- mean(c(initial_vals[1], initial_vals[j - 1]))
          next
        }
        else{
          temp2[j] <- mean(c(initial_vals[j - 1], initial_vals[j+1]))
        }
      }
      initial_vals <- c(temp,temp2[2:length(temp2)], temp3)
    }
    for(x in 1:sides){
      y <- as.character(x)
      print(paste(paste('Corner ',y,':', sep = ''),initial_vals[x]))
    }
  }
}