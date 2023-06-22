#reading data file.
dat <- read.table(file ="C:/Users/SURAJ SHINDE/OneDrive/Desktop/TAMU Courses/probablity/graph1.txt", header = TRUE)
#important values from data
set.seed(123)

Value<-max(dat)
RowS <-nrow(dat)

#creating the current configuration

current_conf<-list()

#creating random numbers, which will become the configuration (spin glass model)

current_conf<- runif(RowS,min =-1,max=1)

#rounding the configuration to 0 and 1

for (i in 1:RowS){
  if (current_conf[i]<0){
    current_conf[i]=-1
    
  }
  else{
    current_conf[i]=1
  }
}

#Write a function that has a graph and a configuration (a vector of size n) as inputs 
#and returns the corresponding weight of the cut in the given graph.

#storing all the max value in a list
All_Max_value <-vector()
#updating the best cut
Best_cut <- list()
#k = 10e5 iterations, Beta (-1,0,1)

count = 0 
k <- 100000
B<- 0                                            #value can be change to -1,0,1
Best_value <- -1000                              #base value for the first iteration (reference in for loop)

for (i in 1:k){
  weight_proposed<-0                                        #INITIALISING weight_proposed
  weight_current<-0                                         #INITIALISING weight_current
  accept_value<-0                                           #INITIALISING accept_value
  
  select <- round(runif(1, min = 1, max = RowS))            #selecting 1 row in random 
  
  flip <- current_conf[select] * (-1)                        #flipping the bit i.e. changing the -1 to 1 or 1 to -1, which creates a new configuration 
  
  Proposed_conf = replace (current_conf,select,flip)         #creating a list for proposed configuration using the flipped bit, which will help us to get the new weighted cut
  
  for (l in 1:RowS){
    
    weight_current = 0.5*(dat$Weight[l])*(1-(current_conf[dat$i[l]]*current_conf[dat$j[l]]))+weight_current # weight of current cut
    
    weight_proposed =0.5*(dat$Weight[l])*(1-(Proposed_conf[dat$i[l]]*Proposed_conf[dat$j[l]]))+weight_proposed # weight of proposed cut

  }

  #acceptance Function, using gibbs distribution.
  accept_value <- (exp(-1*B*(weight_proposed - weight_current)))
  #if the value is greater than 1 we accept the proposed value and note all the max values
  if (accept_value >= 1){
    # we will accept the max values with  in weight_proposed weight_current in a list.
    All_Max_value = append (max(weight_current,weight_proposed),All_Max_value)
    # making the proposed configuration as current configuration, which will be used in further iterations.
    current_conf <- Proposed_conf
    # finding the maximum value, and updating the best configuration.
    if (All_Max_value[i] >= Best_value){
      Best_value = All_Max_value[i]
      Best_conf <- current_conf
    }
    
  }
  if (is.na(accept_value)){
    next
  }
  # we accept the current values is the accepted value is less than 1
  if(accept_value < 1){
    All_Max_value = append (weight_current,All_Max_value)
    current_conf <- current_conf
  }  
}

plot(All_Max_value) # plotting all values for k = 10e5 iterations. 
Best_value # displaying the max/highest value within all the iterations.
Best_conf # displaying the configuration of the max/highest value within all iterations. 

