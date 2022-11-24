
#reading data file" name to be changed"
dat <- read.table(file ="C:/Users/SURAJ SHINDE/OneDrive/Desktop/probablity/graph1.txt", header = TRUE)

#important values from data

Value<-max(dat)
RowS <-nrow(dat)

#creating the current configuration

current_conf<-list()

#creating random numbers

current_conf<- runif(Value,min =-1,max=1)

#rounding the configuration

for (i in 1:Value){
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

#k = 10e6repetition, Beta (0,-1,-10)


k <- 100
B<- 0                                              #value can be change to 0,-1,-10
Best_value <- -100 #base value for the first iteration (reference in fro loop)
for (i in 1:k){
  weight_proposed<-0                                        #INITIALISING weight_proposed
  weight_current<-0                                        #INITIALISING weight_current
  accept_value<-0                                        #INITIALISING accept_value
  weight_diff<-0                                        #INITIALISING weight_diff
  select <- round(runif(1, min = 1, max = Value))            #selecting 1 row in random 
  
  flip <- current_conf[select] * (-1)                        #flipping the bit
  
  Proposed_conf = replace (current_conf,select,flip)         #creating a list for proposed configuration using the flipped bit
  
  for (l in 1:RowS){
    
    weight_current = 0.5*(dat$Weight[l])*(1-(current_conf[dat$i[l]]*current_conf[dat$j[l]]))+weight_current #weight of current cut
    
    weight_proposed =0.5*(dat$Weight[l])*(1-(Proposed_conf[dat$i[l]]*Proposed_conf[dat$j[l]]))+weight_proposed #weight of proposed cut

  }

  weight_diff <- weight_proposed - weight_current     #comparing the weight
  #acceptance Function
  accept_value <- (exp(-1*B*weight_diff))
  #if the value is greater than 1 we accept the value and note all the max values
 
  if (accept_value >= 1){
    
    All_Max_value = append (max(weight_current,weight_proposed),All_Max_value)
    # making the proposed configuration as current configuration.
    current_conf <- Proposed_conf
    # finding the maximum value
    for (z in All_Max_value){
      if (z>Best_value){
        Best_value = z
        Best_conf <- current_conf
      }
    }
    
  }
  if (is.na(accept_value)){
    next
  }
  else{
    next
  }  
    #verify this
    # the only thing reaming is to store the configuration of maximum value
}

#plotting max cut values of for k iterations 
plot(All_Max_value)
Best_value
Best_conf

