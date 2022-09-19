
#-----------------------------
# Author: Carlos Gonzalez
# Date: 07/09/2022
# Title: Testing file
#-----------------------------

#-----------------------------
# Description: The following piece tests the different algorithms in
# Gonzalez (2022) using different ML datasets from UCI ML Repository
#-----------------------------

library(dplyr)
library(ggplot2)

setwd("C:/Users/carlo/Desktop/Oxford/MPhil/Year 2/Thesis")
source("Code/naive_forest.R")
source("Code/intuitive_forest.R")

#-------------------------------
# Section 1: Import Data
#-------------------------------

# 1.1. Toy data
#-------------------------------
toy_data_raw = toy_data()
toy_data = clean_forest(toy_data_raw, 40)
toy_forest = naive_forest(df = toy_data$dataset, n = toy_data$n_t)
toy_int_forest = intuitive_forest(df = toy_data$dataset, n = toy_data$n_t,
                                  random = 0.2)

share_diff = (toy_forest$share_success - toy_int_forest$share_success)*1000
results_toy = tibble("share_success" = c(toy_forest$share_success,
                                         toy_int_forest$share_success),
                     "period" = c(1:40, 1:40),
                     "model" = c(rep("naive",40), rep("intuitive", 40)))

results_toy_2 = tibble("share_diff" = share_diff,
                       "period" = 1:40)

ggplot(data=results_toy, aes(x=period, y=share_success, color = model, group=1)) +
  geom_line()

ggplot(data=results_toy_2, aes(x=period, y=share_diff, group=1)) +
  geom_line()

mean(share_diff<=0) # My algorithm does weakly better 60% of times!

# 1.2. Forest Cover
#-------------------------------
# Source: https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/
# Dataset info also and citation in the link

fc_raw = read.csv(gzfile("Data/covtype.data.gz"), header = FALSE)  

# Prepare data (binarize continuous variables in equal frequency vars)

binary_vars = matrix(NA, nrow = nrow(fc_raw), ncol = 0)

# Vars 1:10 [Info extracted from dataset.info]
for (vars in 1:10){
  temp_vector = cut(fc_raw[,vars], 5, labels = 1:5)
  temp_matrix = sapply(levels(temp_vector),
                       function(x) as.integer(x == temp_vector))
  colnames(temp_matrix) = paste0("V",vars,"_", 1:5)
  binary_vars = cbind(binary_vars, temp_matrix)
}

# Lying variable = 1(forest_type == 3 | forest_type == 7)
# Aprox 10% of data
fc_bin = fc_raw |> as_tibble() |>
                   cbind(binary_vars) |>
                   mutate(lying = ifelse((V55==3 | V55==7), 1, 0)) |>
                   select(-c(1:10, V55))

fc = clean_forest(fc_bin, cap_t = 30)
fc_forest = naive_forest(df = fc$dataset, n = fc$n_t)




