
# Solution script
#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm (list=ls ())
ages <- c ()
offsprings <- c ()
recruits <- c ()
#----------------------------------------------------------
# Main Code part
#----------------------------------------------------------
# Creating the model for 1000 female whales
for (female in 1:1000)
{
#declaring the variable according to the provided values
co <- rnorm (1,100,10)
age <- 15
alive <- 1
sr <- 0.8
enMu <- 20
enSD <- 1
s0 <- -2
s1 <- 0.05
Page 21 of 26
calf <- 0
calfage <- 0
offspring <- 0
b0 <- -10
b1 <- 0.1
b <- 3
inv <- 10
probCalf_5 <- 0.8
probCalf_6to15 <- 0.98
#----------------------------------------------------------
# Implementing conditions
#----------------------------------------------------------
# Creating a loop that runs on condition, female should be alive and of reproductive age.
while (alive == 1 && age >= 15 && age <= 39)
{
alive <- rbinom (1,1, sr)
if (alive == 1)
{
age <- age+1
}
co <- co + rnorm (1, enMu, enSD)
sr <- exp(s0+s1*co)/ (1+ exp(s0+s1*co))
if (calf == 0)
{
b <- exp(b0+b1*co)/(1+exp(b0+b1*co))
# Using this probability (b) as part of a Bernoulli trial, to simulate the event of birth
# Change value of calf to 1 if a calf is born, otherwise leave it at zero
calf <- rbinom (1,1, b)
}
# If the female has a calf,
Page 22 of 26
else
{
co <- co-inv;
calf <- rbinom (1,1, probCalf_5);
# If the calf passes the period of first 5 years,
if (calf == 1)
{
calfage <- calfage+1;
if (calfage > 5)
{
offspring <- offspring+1;
calf <- 0;
calfage <- 0;
}
}
# If the calf does not pass period,
else
{
calfage <- 0;
calf <- 0;
}
}
}
# Add values to the list of ages and offsprings after the first period checking
ages<- c (ages, age)
offsprings<- c (offsprings, offspring)
# Declaring a temporary variable to run the while loop for the number offspring that become
temp <- 0
irecruits_15 <- 0
years <- 1
Page 23 of 26
c_alive <- 0
while (temp < offspring) {
for (years in 1:10) {
c_alive <- rbinom (1,1, probCalf_6to15);
# if the calf dies during any of these 10 years, exit the loop
if (c_alive == 0) {
years <- 11;
}
}
# if the calf dies during any of these 10 years, exit the loop
if (c_alive == 1) {
irecruits_15 <- irecruits_15+1;
}
temp <- temp+1
}
recruits <- c (recruits, irecruits_15)
}
#----------------------------------------------------------
# Print Section
#----------------------------------------------------------
print(ages)
print(offsprings)
print(recruits)
avg <- sum(recruits)/1000;
print(sum(offsprings))
print(sum(recruits))
print(avg)
Page 24 of 26
#----------------------------------------------------------
# Plotting Histogram
#----------------------------------------------------------
# Histogram for age of death of Female Killer Whales
hist(ages,axes = TRUE,breaks = 50,main ="Ages at end of reproduction",xlab ="Age",ylab
="Frequency",col =" cyan",border =" brown4")
# Histogram for inclusive fitness of Female Killer Whales
hist(recruits,axes = TRUE,breaks = 50,main ="Inclusive fitness",xlab ="Recruits produced by each
female",ylab ="Frequency",col =" cyan",border =" brown4")
#----------------------------------------------------------
# End of Code
#----------------------------------------------------------
