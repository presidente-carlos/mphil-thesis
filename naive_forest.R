
#-----------------------------
# Author: Carlos Gonzalez
# Date: 05/09/2022
# Title: Naive Random Forest
#-----------------------------

#-----------------------------
# Description: The following piece runs a myopic pure-exploitation RF using as
# reference the randomForest package
#-----------------------------

#install.packages("randomForest")
library(dplyr)
library(randomForest)

#-------------------------------
# Section 1: Sequentialize function
#-------------------------------

# This function can be used to sequentialize long data with no existing order
# It also returns a vector of suggested investigation rates

clean_forest = function(df, cap_t){
  
  #df is a data frame with a declared "lying" variable
  #cap_T is a positive integer number
  
  N_t = nrow(df) / cap_t
  data_list = list("dataset" = list())
  n_t = c()
  liers = c()
  
  for (t in 1:cap_t){
    
    index = sample(1:nrow(df), size = N_t, replace = TRUE)
    data_list$dataset[[t]] = df[index,]
    
    # Ensure n_t is large enough
    
    liers[t] = sum(data_list$dataset[[t]]$lying)
    n_t[t] = liers[t] + sample(seq(from = 0, to = 0.2*N_t,
                                   length.out = 10), size = 1) |>
             floor()
    
  }
  
  data_list$n_t = n_t
  
  data_list
  
}

#-----------------------------
# Section 2: Generate Toy Data
#-----------------------------

toy_data = function(K = 100, N = 20000, swap_prob = 0.05){

  # K = number of variables
  K = K
  
  # Total number of observations
  N = N
  
  # Determine lying rules
  index_lie = sample(seq(1:K), 14, replace = FALSE)
  index_lie_1 = index_lie[1:10]
  index_lie_2 = index_lie[11:14]
  index_lie_3 = sample(seq(1:K), 5)
  index_lie_4 = sample(seq(1:K), 3, replace = FALSE) 
  
  swap_prob = swap_prob # Introducing some noise in data
  
  # Fill covariate space (binary covariates)
  
  covariates = replicate(N, sample(c(0,1), K, replace = T)) |> t()
  lying =  rep(0, 20000)
  swap_probs = sample(c(0,1), prob = c(swap_prob, 1-swap_prob), 
                      size = N, replace = T)
  
  for (n in 1:N){
    
    lying[n] = ifelse(mean(covariates[n, index_lie_1]) >= 0.7, 1, lying[n])
    lying[n] = ifelse(mean(covariates[n,index_lie_2]) >= 0.4, 1, lying[n])
    lying[n] = ifelse(mean(covariates[n,index_lie_1]) >= 0.5 &
                      mean(covariates[n,index_lie_2]) >= 0.3, 1, lying[n])
    lying[n] = ifelse(any(covariates[n,index_lie_3]) == 1, 1, lying[n])
    lying[n] = ifelse(all(covariates[n,index_lie_4]) == 0, 0, lying[n])
  
    }
  
  # Naming covariates
  covariates = covariates |> as_tibble()
  colnames(covariates) = paste0("cov",seq(1:K))
    
  # Final assemble
  df_toy = tibble(covariates, 
                  "lying" = lying, "swap_prob" = swap_probs) |>
    
        # Introducing randomness in model
        mutate(lying = ifelse(swap_prob == 0 & lying == 1, 0, lying),
               lying = ifelse(swap_prob == 0 & lying == 0, 1, lying)) |>
        select(-swap_prob)

}

#-------------------------------
# Section 3: Naive Forest
#-------------------------------

# Algorithm follows Algorithm 1 in Gonzalez (2022)

naive_forest = function(df, n, n_trees = 200){
  
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
      
      # Select top-predictions
      investigations = slice_max(pk, order_by = pk, n = n_t)
      
      # Store variable values in vectors for RF analysis
      y_inv = data[investigations$index,]$lying
      share_success[t] = sum(y_inv)/sum(data$lying)
      x_inv = data[investigations$index,] |> select(-lying)
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

#-------------------------------
# Section 4: Analysis
#-------------------------------

# Objetivos mañana: 
# Pensar en la teoría. Why is it not working better?
# Mirar lo de proximity que puede ser muy interesante tb
# Ver cómo cambia el success cuando deja de elegir el bottom 10% del top rank
# y elige gente aleatoria (este número podría ser fácilmente tuneado)

