
#-----------------------------
# Author: Carlos Gonzalez
# Date: 08/09/2022
# Title: Intuitive Random Forest
#-----------------------------

#-----------------------------
# Description: The following piece runs a randomized version of naive_forest()
#-----------------------------

#install.packages("randomForest")
library(dplyr)
library(randomForest)

#-------------------------------
# Section 1: Randomized Forest
#-------------------------------

# Algorithm follows Algorithm XX in Gonzalez (2022)

intuitive_forest = function(df, n, random, n_trees = 200){
  
  # df is a list of cap_t periods with a "lying" variable declared
  # n is a vector 1xcap_t
  
  # Set T
  cap_t = length(df)
  
  # Initialize store vectors for RF training and analysis
  K_store = tibble()
  L_store = c()
  share_success = matrix(NA, nrow = cap_t, ncol = 1)
  output_naive = list( #"models" = list() # No need to store the models
  )
  
  # Also maybe we dont need to build the forest with all the previous observations
  # Maybe we can just use the last three or four periods! (Or even one!)
  
  for (t in 1:cap_t){
    
    # Set variables for period t
    data = df[[t]]
    N_t = nrow(data)
    n_t = n[t] #Number of investigations in period t
    
    print(t)
    
    if (t == 1){
      
      # Randomly select n_t actions
      index_a1 = sample(1:N_t, size = n_t, replace = FALSE)
      investigations = data[index_a1,]
      
      # Store variable values in vectors for RF analysis
      # Note, rows must be stored always in the same order
      x_inv = investigations |> select(-lying)
      y_inv = investigations$lying
      share_success[t] = sum(y_inv)/sum(data$lying)
      L_store = c(L_store, y_inv) |> as.factor() #For randomForest() convenience
      K_store = rbind(K_store, x_inv)
      
    }else{
      
      # Run RF using training data
      forest = randomForest(x = K_store, y = L_store, ntree = n_trees,
                            nodesize = 5)
      
      # Notes on RF:
      # -------------
      # classwt [i.e.: c(0.05, 0.95)] could be added for weighting unequal classification errors      
      # RF selects by default \sqrt(K) variables in each step
      # Cutoff set to 0.5 by default
      # nodesize is set = 5 (not 1 as default in classif trees)
      # Reason being large covariate space (consider alternative vals)      
      
      # output_naive$models[[t-1]] = forest # Store randomForest trained in period t-1
      # Maybe there is no need to store the forest themselves
      # They are too heavy
      
      # Set variables for period t
      x_all = data |> select(-lying)
      
      # Predict x_all using {1,..., t-1} data
      forest_predict = predict(forest, x_all, predict.all = TRUE)
      
      # Notes on predict.randomForest:
      # ------------------------------
      # predict.all returns $individual with data on predict in each tree      
      # nodes returns attr() indicating ending bucket for each obs in each tree      
      
      # Compute relevant stats (update Thesis)
      pk = forest_predict$individual |> as.character() |> as.numeric() |>
        matrix(nrow = N_t, ncol = n_trees, byrow = FALSE) |> rowMeans() |>
        cbind(seq(1:N_t)) |> as_tibble()
      colnames(pk) = c("pk", "index")
      
      # Select top-predictions [80%]
      inv_forest = slice_max(pk, order_by = pk, n = (1-random)*n_t)
      aux_seq = 1:N_t
      investigations = sample(aux_seq[-inv_forest$index], size = random*n_t,
                              replace = FALSE) |> c(inv_forest$index)
      
      # Store variable values in vectors for RF analysis
      y_inv = data[investigations,]$lying
      share_success[t] = sum(y_inv)/sum(data$lying)
      x_inv = data[investigations,] |> select(-lying)
      L_store = L_store |> as.character() |> as.numeric()
      L_store = c(L_store, y_inv) |> as.factor()
      K_store = rbind(K_store, x_inv)  
      
    }
    
  }
  
  # Prepare export output
  output_naive$L_store = L_store
  output_naive$K_store = K_store
  output_naive$share_success = share_success
  
  output_naive
  
}

