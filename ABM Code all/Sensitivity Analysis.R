


################# Sensitivity Analysis ########################

install.packages("sensitivity")
install.packages("lhs")
install.packages("fast")
library(sensitivity)
library(lhs)
library(fast)


model_wrapper <- function(params) {
  mu <- params[1]
  total_attributes <- params[2]
  epochs <- as.integer(params[3])
  proximity_threshold <- params[4]
  p <- params[5]
  
  result <- updateVoteProbability_8(agents, proximity_matrix, mu, total_attributes, epochs, proximity_threshold, p)
  
  # Assuming you want to analyze the mean voting probabilities at the last epoch
  final_epoch_results <- result$interactions_epoch %>%
    filter(epoch == max(epoch)) %>%
    summarise(mean_vote_prob = mean(vote_prob))
  
  return(as.numeric(final_epoch_results$mean_vote_prob))
}



# Using Latin Hypercube Sampling to generate the parameter samples

# Define the number of samples
n_samples <- 1000

# Define the parameter ranges
param_ranges <- data.frame(
  mu = c(0.001, 1),
  total_attributes = c(1, 5),
  epochs = c(1, 50),
  proximity_threshold = c(0, 1),
  p = c(0.1, 10)
)

# Generate the samples
param_samples <- lhs::randomLHS(n_samples, ncol(param_ranges))
param_samples <- as.data.frame(t(apply(param_samples, 1, function(x) {
  x * (param_ranges[,2] - param_ranges[,1]) + param_ranges[,1]
})))
names(param_samples) <- names(param_ranges)


sobol_results <- sobol2007(
  model = model_wrapper,
  X1 = param_samples,
  nboot = 100
)

print(sobol_results)

# Plot the Sobol indices
plot(sobol_results, type = "l")

