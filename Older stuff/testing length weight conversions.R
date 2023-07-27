# Testing of length-weight conversion
# Using mean length or individual lengths of a sample

set.seed(123) # for reproducibility
pop_lengths <- rnorm(100, 500, 150) # create population of 100 fish, mean length 500, sd 150
hist(pop_lengths)

# Length weight W = a*x^b ---- W = -4.5918 * x ^ 2.9476
a = -4.5198
b = 2.9476
weights <- 10^(a)*pop_lengths^b

# total weight of population
total_weight <- sum(weights)/1000 # 354.93


### What if we use mean length in a sample of 50
sample_lengths <- sample(pop_lengths, 50) # random sample of 50
mean_length_sample = mean(sample_lengths) # mean length of fish in sample
mean_weight_sample = (10^(a)*mean_length_sample^b)/1000  # weight of fish mean length

mean_weight_sample*100 # 400.299 ### OH NO - out by almost 20%
accuracy1 <- (mean_weight_sample*100)/total_weight
accuracy1 # 75% accurate...


### What if we calculate weights from sample of 50 then get mean weight of sample
sample_weights <- (10^(a)*sample_lengths^b)/1000 
mean_sample_weight <- mean(sample_weights)
est_pop_weight_from_sample_mean_weight <- mean_sample_weight*100
est_pop_weight_from_sample_mean_weight
accuracy2 <- est_pop_weight_from_sample_mean_weight/(sum(weights)/1000)
accuracy2 # 3% overestimate 


### now loop for a larger test

# lists to store results
length_list <- numeric()
weight_list <- numeric()
jerom_list <- numeric()

for(i in (1:1000)){

set.seed(i) # for reproducibility
pop_lengths <- rnorm(100, 500, 150) # create population of 100 fish, mean length 500, sd 150
#hist(sample_lengths)

# Length weight W = a*x^b ---- W = -4.5918 * x ^ 2.9476
a = -4.5198
b = 2.9476
weights <- 10^(a)*pop_lengths^b

# total weight of population
total_weight <- sum(weights)/1000 # 354.93


### What if we use mean length in a sample of 50
sample_lengths <- sample(pop_lengths, 50) # random sample of 50
mean_length_sample = mean(sample_lengths) # mean length of fish in sample
mean_weight_sample = (10^(a)*mean_length_sample^b)/1000  # weight of fish mean length

mean_weight_sample*100 # 400.299 ### OH NO - out by almost 20%
accuracy1 <- (mean_weight_sample*100)/total_weight
accuracy1 # 75% accurate...

length_list <- append(length_list, accuracy1)


### What if we calculate weights from sample of 50 then get mean weight of sample
sample_weights <- (10^(a)*sample_lengths^b)/1000 
mean_sample_weight <- mean(sample_weights)
est_pop_weight_from_sample_mean_weight <- mean_sample_weight*100
est_pop_weight_from_sample_mean_weight
accuracy2 <- est_pop_weight_from_sample_mean_weight/(sum(weights)/1000)
accuracy2 # 3% overestimate 

weight_list <- append(weight_list, accuracy2)

### What if we use Jerom's method - mulitply the biomass of sample by ratio of catch to biomass (2 in this case)
sample_weights <- (10^(a)*sample_lengths^b)/1000 
total_sample_weight <- sum(sample_weights)
est_pop_weight_from_ratio <- total_sample_weight *2
accuracy3 <- est_pop_weight_from_ratio/(sum(weights)/1000)
accuracy3 # 3% overestimate 

jerom_list <- append(jerom_list, accuracy3)

}


hist(length_list) # so using mean length is bad
hist(weight_list) # using mean weight is OK but there is up to 20% error based on how representative the sample of 50 is
hist(jerom_list) 
cor(weight_list, jerom_list)
plot(weight_list, jerom_list)
