
#############################################
#Programming module (West Nile Virus)
#Author: William Norfolk
#Date: 5/18/2020
#############################################

#**Exercise. Write a script to load the West Nile virus data and use ggplot to create a histogram for the total number of cases in each state in each year. Follow the format of the *prototypical script*  advocated in the presentation: Header, Load Packages, Declare Functions, Load Data, Perform Analysis.**
  
#Load packages
library(tidyverse)

#load data
wnv_data <- read.csv("./wnv.csv")

#plot total cases per year for each state
ggplot(wnv_data, aes(Total)) + geom_histogram(binwidth = 50) +
  theme(legend.position = "none", axis.text = element_text(size = 6, family = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank()) + xlab("Total Cases") + ylab("Count") + ggtitle("Case Distribution (regardless of year or state)")

#**Exercise. The state-level and case burden is evidently highly skewed. Plot a histogram for the logarithm of the number of cases. Do this two different ways.**
  
#Option one within the plotting code
ggplot(wnv_data, aes(log10(Total))) + geom_histogram(binwidth = 0.2) 
#Option two using mutate to create a log10 variable
wnv_data <- wnv_data %>% dplyr::mutate(log_Total = log10(Total))
ggplot(wnv_data, aes(log_Total)) + geom_histogram(binwidth = 0.2) 


#**Exercise. Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state in each year. Plot a histogram of the calcated CFRs.**
wnv_data <- wnv_data %>% dplyr::mutate(cfr = Fatal / Total)
ggplot(wnv_data, aes(cfr)) + geom_histogram()

#**Exercise. Use arithmetic operators, logical operators, and the function `sum` to verify that the variable `Total` is simply the sum of the number of febrile cases, neuroinvasive cases, and other cases.**
wnv_data <- wnv_data %>% dplyr::mutate(total_check = EncephMen + Fever + Other)
all(wnv_data$Total == wnv_data$total_check)

#**Exercise. Use modular arithmetic to provide an annual case count for each state rounded (down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated with this calculate, then add the errors to obtain the total error.**
#round the case counts for all entries in the total variable but integer dividing the entry by 12 (to round to dozen) then multiply this by 12 to get the rounded case count.
wnv_data <- wnv_data %>% dplyr::mutate(total_round = (Total %/% 12) * 12)
#Subtract the rounded total from the proper total
wnv_data <- wnv_data %>% dplyr::mutate(error = Total - total_round)
#Sum all error values. Total error = 1241
sum(wnv_data$error)

#**Exercise. Write a function to calculate the mean and standard error (standard deviation divided by the square root of the sample size) of the neuroinvasive disease rate for all the states in a given list and given set of years. Follow the Google R style and remember to place the function near the top of your script. Use your function to calculate the average severe disease rate in California, Colorado, and New York.**

mean_and_stderror <- function(x){
  # Function calculates the mean and standard error
  #
  # Args:
  #   x = a vector of values for which the mean and standard error will be calculated
  #
  # Returns:
  #   the mean and standard error
  #
  # calculates the mean 
  m <- mean(x)
  # calculates the standard deviation
  std_deviation <- sd(x)
  # calculates the square root of the sample size (for determining statndard error)
  sqrt_sample_size <- sqrt(length(x))
  
  # calculates the standard error
  # Computation
  se <- std_deviation/sqrt_sample_size
  return(c(m,se))
}

#We will start the the thre subsetted states
#Subset these locations then sort and summarize using the created function. Specify position [1] and [2] to represent mean and standard error respectively.
three_state_fun <- subset(wnv_data, State == "Colorado" | State == "California" | State == "New York") %>%
  group_by(State) %>%
  summarise(mean = mean_and_stderror(neuro_rate)[1],
            stderr = mean_and_stderror(neuro_rate)[2])
#plot the data with errorbars
ggplot(three_state_fun, aes(State, mean)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(State, ymin = mean - stderr, ymax = mean + stderr))

#Average neuroinvasive rate in California was 0.5123430 with a standard error of 0.1113079 
#Average neuroinvasive rate in Colorado was 0.22358004 with a standard error of 0.04218512 
#Average neuroinvasive rate in New York was 0.81486420  with a standard error of 0.03717168 

#apply the function to all states
#group by and summarize data by the function while creating two new columns
state_rate_dat <- wnv_data %>%
  group_by(State) %>%
  summarise(mean = mean_and_stderror(neuro_rate)[1],
            stderr = mean_and_stderror(neuro_rate)[2])
#plot summary data
ggplot(state_rate_dat, aes(State, mean, fill = State)) + geom_bar(stat = "identity") + 
  geom_errorbar(aes(State, ymin = mean - stderr, ymax = mean + stderr)) + theme(
    axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"
  ) + ylab("Mean Neuroinvasice Disease Rate") + xlab("")

#**Exercise. Use your function and ggplot to show the neurovinvasive disease rate for all states.**
  
#I was not able to determine how to modify a variable with a function that prodces a list of two values as opposed to a single value. However I was successful in making this plot using the aggregate function.
#Aggregate all neuro_rate to the mean by State
neuro_agg <- aggregate(neuro_rate ~ State, wnv_data, FUN = mean)
#plot mean rates for each state
ggplot(neuro_agg, aes(State, neuro_rate, fill = State)) + geom_bar(stat = "identity") + theme(
  axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"
) + ylab("Mean Neuroinvasice Disease Rate") + xlab("")

#**Exercise. Use pipes to produce the same plots without using your function.**
#start with the three states
#use plotrix to simplify standard error calculation
library(plotrix)
ca_co_ny <- subset(wnv_data, State == "Colorado" | State == "California" | State == "New York") %>%
  group_by(State) %>%
  dplyr::summarise(neuro_mean = mean(neuro_rate), standard_error = std.error(neuro_rate))
#then plot as above
ggplot(ca_co_ny, aes(State, neuro_mean)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(State, ymin = neuro_mean - standard_error, ymax = neuro_mean + standard_error))

#next move to the full us plot. Will assign a new object (wnv_data_2) to retain our dataframe for continued analysis
wnv_data_2 <- wnv_data %>%
  group_by(State) %>%
  dplyr::summarise(neuro_mean = mean(neuro_rate), standard_error = std.error(neuro_rate))
#Then plot the summaries
ggplot(wnv_data_2, aes(State, neuro_mean, fill = State)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(State, ymin = neuro_mean - standard_error, ymax = neuro_mean + standard_error))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none"
  ) + ylab("Mean Neuroinvasice Disease Rate") + xlab("")

#**Exercise. Choose a longitude to designate the "center" of the country. Use the function `ifelse` to assign each state to an "Eastern" region or a "Western" region.**
#we will set Kansas as our middle longitude (-98.21)
wnv_data <- wnv_data %>% dplyr::mutate(region = ifelse(Longitude >= -98.21, "Eastern", "Western"))

#**Exercise. Analyse your data to compare case fatality rates in the Eastern vs. Weestern United States.**
  #_It appears that the Eastern states have a higher case fatality rate when compared to western states._
#make total plot
ggplot(wnv_data, aes(x = State, y = cfr, fill = region)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + xlab("") + ylab("Case Fatality Rate")

#summarize data to view averages by region
region_data <- wnv_data %>% 
  group_by(region) %>%
  dplyr::summarise(cfr_mean = mean(cfr), standard_error = std.error(cfr))

ggplot(region_data, aes(region, cfr_mean, fill = region)) + geom_bar(stat = "identity") +
  theme(legend.position = "none") + xlab("") + ylab("Mean Case Faitality Rate")

#**Exercise. Is there evidence for a *latitudinal gradient* in case fatality rate?**
 # _Based on the plots below, there is evidence of a latitudinal gradient where Southern States appear to have a higher case fatality rate. Though it should be noted that there difference between the two regions is minor on average and variable by state._

#Since kansas is fairly mid country we will also use this state for latitude
wnv_data <- wnv_data %>% dplyr::mutate(region_lat = ifelse(Latitude >= 38.56, "Northern", "Southern"))

#will replicate the code as above for latitude
#make total plot
ggplot(wnv_data, aes(x = State, y = cfr, fill = region_lat)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + xlab("") + ylab("Case Fatality Rate")

#summarize data to view averages by region
region_lat_data <- wnv_data %>% 
  group_by(region_lat) %>%
  dplyr::summarise(cfr_mean = mean(cfr), standard_error = std.error(cfr))

ggplot(region_lat_data, aes(region_lat, cfr_mean, fill = region_lat)) + geom_bar(stat = "identity") +
  theme(legend.position = "none") + xlab("") + ylab("Mean Case Faitality Rate")

#**Exercise. Loop over all the years in the WNV data set (1999-2007) and compute the following statistics: Total number of states reporting cases, total number of reported cases, total number of fatalities, and case fatality rate. Produce some plots to explore how these quantities change over and with respect to each other. Explain what you have learned or suspect based on these plots.**
 # _Based on the information visualized from the loop plots, it appears that total cases remain relatively constant with a large spike in 2003, fatalities are consistent with total case counts showing an increase in years with increased total cases, case fatality rate has been declining since the early 2000s, and lastly; more states have reported data in the latter years of the study._
#set years sequence
years <- unique(wnv_data$Year)

#loop over years to find total cases
total_cases <- c()
for(year in years){
  look.at <- wnv_data$Year == year #Look.at is a boolean vector that directs the outcome of each iteration
  total_cases <- append(total_cases, sum(wnv_data$Total[look.at], na.rm = TRUE))
}

#add yearly cases plot
plot(years, total_cases, type='p')

#loop over years to find total fatalities
total_fatal <- c()
for(year in years){
  look.at <- wnv_data$Year == year #Look.at is a boolean vector that directs the outcome of each iteration
  total_fatal <- append(total_fatal, sum(wnv_data$Fatal[look.at], na.rm = TRUE))
}

#add yearly fatalities plot
plot(years, total_fatal, type='p')

#loop over years to find mean fatality rate
mean_fate_rate <- c()
for(year in years){
  look.at <- wnv_data$Year == year #Look.at is a boolean vector that directs the outcome of each iteration
  mean_fate_rate <- append(mean_fate_rate, mean(wnv_data$cfr[look.at], na.rm = TRUE))
}

#add yearly case fatality rate plot
plot(years, mean_fate_rate, type='p')

#loop over years to find total reporting states
total_states <- c()
for(year in years){
  look.at <- wnv_data$Year == year #Look.at is a boolean vector that directs the outcome of each iteration
  total_states <- append(total_states, length(unique(wnv_data$State[look.at], na.rm = TRUE)))
}

#add yearly cases plot
plot(years, total_states, type='p')

#**Exercise. How does your choice of longitudinal breakpoint matter to the evidence for a geographic difference in case fatality rate? Combine conditional execution and looping to study the difference in case fatality rate over a range of breakpoints. What is the "best" longitude for separating case fatality rate in the East vs. West?**
#_The selected breakpoint greatly influences the geographic differences in case fatality rate by including or excluding regions which may have a higher disease incidence and/or mortality rate due to confounding factors (vector presence, environmental health, sanitation, etc.). The inclusion or exclusions of "hot zone" geographic regions based on personal opinion of what is Eastern adn Western United States can skew the case fatality rate based on the individual opinion of the breakpoint setter. Based on the breakpoint figures produced below it appears that a longitude of approximately -85 (aligned near western Alabama) would serve as the best seperator for the east and western United States. Once you pass longitude -85 cases tend to drop more severely likely indicating the end of the ideal range for the Lyme vector and/or spirochete._

#Not the tidy-est code I have ever made

#set breakpints into a vector
breakpoints <- wnv_data$Longitude
#create empty objects for data storage
east <- NULL
west <- NULL
east_break <- NULL
west_break <- NULL

#loop through breakpoints
for(i in breakpoints){
  look.at <- wnv_data$Longitude == i[i] #set boolean
  if(wnv_data$Longitude <= i){
    east <- append(east, mean(wnv_data$cfr[look.at], na.rm = TRUE))
    east_break <- append(east_break, unique(wnv_data$Longitude[look.at])) #create for comparable vector length
  }
  else{
    west <- append(west, mean(wnv_data$cfr[look.at], na.rm = TRUE))
    west_break <- append(west_break, unique(wnv_data$Longitude[look.at]))
  }
}
#plot results
plot(east_break, east, type = 'p')
plot(west_break, west, type = 'p')

#**Exercise. We may interpret raw case fatality rate (i.e. ratio of the number of deaths, $x$, to number of infections, $n$) as a realization from a binomial process with $n$ trials and $x$ "successes" generated by an unknown rate parameter $p$. This $p$ may be the quantity truly of interest (for instance, if we wish to ask if the case fatality rate in California is significantly different from the case fatality rate in Colorado. In R, the *estimated rate* and its *confidence interval* can be obtained using the function `prop.test` for testing equal proportions. Use the help to determine the proper usage of `prop.test` and calculate confidence intervals for the case fatality rates in all states for which there have been reported cases of WNV.**
#_The prop.test function runs a porportion test which tells us how probable it is that two porportions are the same. Lower prop.test values indicate differences where a higher number would indicate similaraity._

#pull help tab for prop.test
?prop.test

#test prop.test for all states with reported values
#aggregate to mean cfr for each state
wnv_prop_test <- wnv_data %>%
  group_by(State) %>%
  dplyr::summarise(cfr_mean = mean(cfr)) %>%
  dplyr::mutate(recover_mean = 1 - cfr_mean) 
state_names <- wnv_prop_test$State
wnv_prop_test <- wnv_prop_test %>% select(-State)
wnv_prop_test_2 <- as.matrix(wnv_prop_test)
rownames(wnv_prop_test_2) <- state_names #retain state names
#run prop.test by states
prop_result <- prop.test(wnv_prop_test_2)
prop_result

#**Exercise. The "See Also" section of the help for `prop.test` states that a different function might be useful for an exact test of our hypotheses. Use the help to identify what this function is, learn how to use it, and compare the differences.**
 # _The function is called a binom.test which compares the number of successess in a given number of trials with a hypothesized probability of success. For this function x denotes the number of successes, n denotes the number of trials and p is the probability of success._

     
#we can recycle or dataframe from above here.
wnv_prop_test_2 <- as.tibble(wnv_prop_test_2)
binom_vec <- round(wnv_prop_test_2$cfr_mean * 100) #Will use 100 trials for this example and assume that fatality is "success" interms of the binom.test to compare with the prop.test
#I was unable to determine how to apply the binom.test function to a full list of successful outcomes due to a persistant vector length error so I selected a few random states below to compare

#Alabama
binom.test(14, 100, p =0.5) #Compares to prop 1 test above
#Connecticut
binom.test(6, 100, p =0.5) #Compares to prop 6 test above
#West Virginia
binom.test(22, 100, p =0.5) #Compares to prop 46 test above

#The results of both tests appear to be relatively similar with the major differences likely due to the rounding of successes in the binom.test data prep.   
                                                                                                                                                                                                                                                                                              


