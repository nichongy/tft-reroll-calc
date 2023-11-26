library(expm)
library(ggplot2)

reroll_calc <- function(unit_cost,
                        level,
                        desired_gone,
                        same_cost_gone,
                        gold) {
  
  #eorros for impossible scenarios
  if (unit_cost <= 0 || unit_cost >= 6){
    stop("Unit cost is not from 1 to 5")
  }
  
  if (level <= 0 || level >= 11){
    stop("Level is not from 1 to 10")
  }
  
  #number of copies of a unit for each tier
  total_units <- c(29, 22, 18, 12, 10)
  #number of different units for each tier
  distinct_units <- c(13, 13, 13, 12, 8)
  
  #finding the number of remaining units
  desired_left = total_units[unit_cost] - desired_gone
  same_cost_left = total_units[unit_cost]*distinct_units[unit_cost] - same_cost_gone
  
  if (desired_left <= -1 || same_cost_left <= -1){
    stop("There are more units taken from the pool than are available")
  }
  
  #shop odds for each level
  level_df <- matrix(c(
    c(1, 1, 0.75, 0.55, 0.45, 0.25, 0.19, 0.16, 0.09, 0.05),
    c(0, 0, 0.25, 0.30, 0.33, 0.40, 0.30, 0.20, 0.15, 0.10),
    c(0, 0, 0, 0.15, 0.20, 0.30, 0.35, 0.35, 0.30, 0.20),
    c(0, 0, 0, 0, 0.02, 0.05, 0.15, 0.25, 0.30, 0.40),
    c(0, 0, 0, 0, 0, 0, 0.01, 0.04, 0.16, 0.25)),
    nrow = 10, ncol = 5
  )
  
  #initializing for the transition matrix
  transition_mat = matrix(0, nrow = 10, ncol = 10)
  
  prob = c()
  
  #if finding a new unit fails
  for (i in 1:9){
    prob = 1 - level_df[level,unit_cost]*desired_left/same_cost_left
    prob = min(prob, 1)
    transition_mat[i,i] = prob
    desired_left = desired_left - 1
    same_cost_left = same_cost_left - 1
  }
  
  #reinitializing
  desired_left = total_units[unit_cost] - desired_gone
  same_cost_left = total_units[unit_cost]*distinct_units[unit_cost] - same_cost_gone
  
  #if finding a new unit succeeds
  for(i in 1:9){
    prob = level_df[level,unit_cost]*desired_left/same_cost_left
    prob = max(prob, 0)
    transition_mat[i,i+1] = prob
    desired_left = desired_left - 1
    same_cost_left = same_cost_left - 1
    
  }
  #if there are 9 of the same unit, no more of that unit can be found
  transition_mat[10,10] = 1
  
  #finding the number of rolls
  rolls = floor(5*gold/2)
  #bringing the transition matrix to the power of the number of rolls
  res_mat = transition_mat%^%rolls
  
  #creating the bar plot
  ggplot() +
    geom_bar(aes(x = as.character(1:9),
                 y = rev(cumsum(rev(res_mat[1,][-1])))),
             stat = "identity") +
    geom_text(aes(x = as.character(1:9),
                  y = rev(cumsum(rev(res_mat[1,][-1]))),
                  label = round((rev(cumsum(rev(res_mat[1,][-1])))),digits = 3)),
              vjust = -0.5,
              color = "black") +
    labs(x = "Probability of getting at least x unit(s)", y = "") +
    theme_classic()
}
