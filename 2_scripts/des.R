library(readxl)  # read in excel file
library(simmer)  # For Discrete Event Simulation
library(magrittr)  # for pipe %>% function
library(simmer.plot)  # for visualization of simulation results
library(tidyverse)
library(data.table)


set.seed(10212)  # set seed for random number generation

# Cancer parameters ----

# Male Parameters
#p_male = 0.0625 # male CRC prevalence
#death_male_CRC = 0.02941176 # male death from CRC
#age_49_male = 0.041 # proportion of male cases <49 years old
age_50_59_male = 0.11 # proportion of male cases 50-59 years old
age_60_69_male = 0.248 # proportion of male cases 60-69 years old
age_70_79_male = 0.327 # proportion of male cases 70-79 years old
#age_80_male = 0.274 # proportion of male cases >80 years old
#five_yr_surv_male = 0.66

# Female Parameters
# p_female = 0.05263158 # female CRC prevalence
#death_female_CRC = 0.025 # female death from CRC
#age_49_female = 0.048 # proportion of female cases <49 years old
age_50_59_female = 0.108 # proportion of female cases 50-59 years old
age_60_69_female = 0.213 # proportion of female cases 60-69 years old
age_70_79_female = 0.279 # proportion of female cases 70-79 years old
#age_80_female = 0.355 # proportion of female cases >80 years old
#five_yr_surv_male = 0.67


# Get age and sex from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710005701&pickMembers%5B0%5D=1.6&pickMembers%5B1%5D=3.2&pickMembers%5B2%5D=4.63&cubeTimeFrame.startYear=2021&cubeTimeFrame.endYear=2058&referencePeriods=20210101%2C20580101


NUM_PEOPLE <- 8501833	  # number of people in Quebec in 2021
df <- read_excel("./1_data/ST2.xlsx")  # SHEET 2
df$Male <- df$Male * 1000  # get actual number of individuals
df$Female <- df$Female * 1000  # get actual number of individuals

age <- c()
sex <- c()

for (i in 1:length(df$Male)) {
  male_age <- rep.int(df$Age[i], round(df$Male[i]))
  female_age <- rep.int(df$Age[i], round(df$Female[i]))  # GOTCHA: round to make sure get integer
  age <- c(age, male_age, female_age)
  
  male_sex <- rep("M", round(df$Male[i]))
  female_sex <- rep("F",round(df$Female[i]))  # GOTCHA: round to make sure get integer
  sex <- c(sex, male_sex, female_sex)
  
}

num_male_age_50_to_59 <- sum(df$Male[1:5])
num_female_age_50_to_59 <- sum(df$Female[1:5])

num_male_age_60_to_69 <- sum(df$Male[6:10])
num_female_age_60_to_69 <- sum(df$Female[6:10])

num_male_age_70_to_79 <- sum(df$Male[11:15])
num_female_age_70_to_79 <- sum(df$Female[11:15])


# cancer (repeated from above)
age_50_59_male = 0.11 # proportion of male cases 50-59 years old
age_60_69_male = 0.248 # proportion of male cases 60-69 years old
age_70_79_male = 0.327 # proportion of male cases 70-79 years old
age_50_59_female = 0.108 # proportion of female cases 50-59 years old
age_60_69_female = 0.213 # proportion of female cases 60-69 years old
age_70_79_female = 0.279 # proportion of female cases 70-79 years old)

# 0 = No cancer, 1 = Cancer
has_cancer <- c(sample(x=seq(0,1), size=num_male_age_50_to_59, replace=TRUE, prob=c(1-age_50_59_male, age_50_59_male)),
                sample(x=seq(0,1), size=num_male_age_60_to_69, replace=TRUE, prob=c(1-age_60_69_male, age_60_69_male)),
                sample(x=seq(0,1), size=num_male_age_70_to_79, replace=TRUE, prob=c(1-age_70_79_male, age_70_79_male)),
                sample(x=seq(0,1), size=num_female_age_50_to_59, replace=TRUE, prob=c(1-age_50_59_female, age_50_59_female)),
                sample(x=seq(0,1), size=num_female_age_60_to_69, replace=TRUE, prob=c(1-age_60_69_female, age_60_69_female)),
                sample(x=seq(0,1), size=num_female_age_70_to_79, replace=TRUE, prob=c(1-age_70_79_female, age_70_79_female)))



####### Initial parameters
# # FIT compliance of 90% (original condition)
# FIT_COMPLIANCE_RATE <- 0.9  # Scenarios: 100%, 90% (original), 80%, 70%, 60%, 50% ... of population over 50 years old etc
# CAPACITY_RATE <- 1 # Scenario analysis capacity_rate -50%, -40%, -30%, -20%, -10%, original, +10%, +20%, +30%, + 40%, +50%

run_des <- function(FIT_COMPLIANCE_RATE=0.9, CAPACITY_RATE=1) {
  #' Runs a single discrete event simulation 
  #'
  #' Parameters are as follows:
  #' - FIT_COMPLIANCE_RATE: the rate of FIT testing compliance. Modified for scenario analysis. Original rate is 0.9
  #' - CAPACITY_RATE: the capacity rate. Modified for scenario analysis. Original rate is 1.
  #' 
  #' Returns: a list of two variables pertaining to waitlist size and median waittime 
  
  data <- data.frame(age, sex, has_cancer)
  table(data$has_cancer)
  
  
  # Assumed everyone is mailed FIT Test - 100%
  data$mailed_FIT <- ifelse(data$has_cancer==0, 1, 0)
  
  # 90% comply with FIT test
  data$comply_FIT <- data$mailed_FIT 
  
  # Calculate the total number of 1s in the dataframe
  total_ones <- sum(data$comply_FIT == 1)
  
  # Calculate the number of 1s to be set to 0 based on the 90% criteria
  ones_to_zero <- floor( (1-FIT_COMPLIANCE_RATE) * total_ones)
  
  # Randomly select the required number of 1s to set to 0
  ones_index <- which(data$comply_FIT == 1)
  ones_to_zero_index <- sample(ones_index, ones_to_zero, replace = FALSE)
  
  # Set the selected 1s to 0 in the dataframe
  data$comply_FIT[ones_to_zero_index] <- 0
  
  table(data$comply_FIT)
  
  
  
  
  data$positive_FIT <- data$comply_FIT 
  
  # Calculate the total number of 1s in the dataframe
  total_ones <- sum(data$positive_FIT == 1)
  
  # Calculate the number of 1s to be set to 0 based on the 4.9% criteria
  ones_to_zero <- floor(0.961 * total_ones)
  
  # Randomly select the required number of 1s to set to 0
  ones_index <- which(data$positive_FIT == 1)
  ones_to_zero_index <- sample(ones_index, ones_to_zero, replace = FALSE)
  
  # Set the selected 1s to 0 in the dataframe
  data$positive_FIT[ones_to_zero_index] <- 0
  
  table(data$positive_FIT)  # number of individuals that complied and tested positive for FIT
  
  
  
  
  
  
  # view possible methods
  # methods(class="trajectory")
  
  # Parameters
  NUM_DAYS_IN_YEAR <- 365  # 1 year
  SCREENING_RATE = 0.45  # screening = 0.45, surveillance = 0.25, symptoms = 0.3
  
  NUM_COLONOSCOPIES_PERFORMED_2021 <- 258178  # Power BI dashboard
  THEORETICAL_ELECTIVE_ENDOSCOPIC_CAPACITY <- round(NUM_COLONOSCOPIES_PERFORMED_2021 * SCREENING_RATE)  # Number of colonoscopies delegated for screening
  CAPACITY_PER_DAY <- THEORETICAL_ELECTIVE_ENDOSCOPIC_CAPACITY / NUM_DAYS_IN_YEAR  # Number of colonoscopies that can be performed in a single day
  
  # Scenario analysis ----
  CAPACITY_PER_DAY <- round(CAPACITY_PER_DAY * CAPACITY_RATE)  # Scenario analysis capacity_rate -50%, -40%, -30%, -20%, -10%, original, +10%, +20%, +30%, + 40%, +50%
  
  # Queue size in Quebec 2021-2022 for all colonoscopies (screening, surveillance, symptoms)
  QUEUE_SIZE = round(116127 * 0.6)  # queue size of screening colonoscopies
  
  NUM_POSITIVE_FIT <- sum(data$positive_FIT == 1)  # 5% of FIT test are Positive
  
  ARRIVING_TIME <- sort(runif(NUM_POSITIVE_FIT, 0, NUM_DAYS_IN_YEAR))  # uniform distribution arrival time
  
  
  
  
  
  patient <-
    trajectory("Patient's path") %>%
    log_("Request a colonoscopy") %>%
    set_attribute("start_time", function() {now(colonoscopy)}) %>%
    seize("doctor") %>%
    log_(function() {paste("Waited: ", now(colonoscopy) - get_attribute(colonoscopy, "start_time"))}) %>%
    timeout(1) %>%  # assume colonoscopy takes 1 time step to complete
    release("doctor") %>%
    log_(function() {paste("Finished: ", now(colonoscopy))})
  
  # create simulation environment
  colonoscopy <- 
    simmer("colonoscopy") %>% 
    add_resource("doctor", capacity=CAPACITY_PER_DAY) %>%   # Number of doctors
    
    # set initial queue size: people enter at time 0
    add_generator(name_prefix = "Initial Queued",
                  trajectory = patient,
                  distribution = at(rep(0, QUEUE_SIZE)),
                  mon=2) %>%
    
    add_generator(name_prefix = "Patient", 
                  trajectory = patient,  
                  distribution = at(ARRIVING_TIME),
                  mon=2)
  
  
  
  colonoscopy %>% run(until = NUM_DAYS_IN_YEAR) %>% get_mon_attributes()  # get resource queue related data
  
  colonoscopy %>% get_mon_arrivals() %>% transform(waiting_time = end_time - start_time - activity_time)  # get stats
  
  result <- colonoscopy %>% get_mon_arrivals() %>% transform(waiting_time = end_time - start_time - activity_time)  # get stats
  
  # paste("Average wait for ", sum(result$finished), " completions was ",
  #       mean(result$waiting_time), "days")
  
  
  median_waittime <- median(result$waiting_time)
  print(paste("Median wait for ", sum(result$finished), " completions was ",
              median_waittime, "days"))
  waitlist_size <- get_queue_count(colonoscopy, "doctor")
  print(paste("Final waitlist size was ", waitlist_size))

  output<-list(median_waittime, waitlist_size)
  return(output)
}



# Scenario Analysis ----
scenario_fit_compliance <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5)
scenario_capacity_rate <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)

# initialize dataframe for storing results of simulation
results_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(results_df) <- c("fit_compliance","capacity","median_wait_time","final_waitlist_size")

#' - results_df: the dataframe storing the results (wait time, waitlist size for each scenario analyzed)
for (FIT_COMPLIANCE_RATE in scenario_fit_compliance) {
  for (CAPACITY_RATE in scenario_capacity_rate) {
    
    output <- run_des(FIT_COMPLIANCE_RATE=FIT_COMPLIANCE_RATE, 
                      CAPACITY_RATE=CAPACITY_RATE)
    
    results_df <- results_df %>% add_row(fit_compliance = FIT_COMPLIANCE_RATE, 
                                         capacity= CAPACITY_RATE, 
                                         median_wait_time = output[[1]], 
                                         final_waitlist_size = output[[2]])
  }
}

print(results_df)
# save results
fwrite(results_df, "./3_intermediate/results.csv")
