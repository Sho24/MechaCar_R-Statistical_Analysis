install.packages("tidyverse")
library(tidyverse) #import tidyverse package

#import MechaCar data
mechaCarData <- read.csv(file='Resources/MechaCar_mpg.csv', check.names=F, stringsAsFactors = F) 
#display first rows of imported data
head(mechaCarData) 

# Rename columns of the data set
MechaCar_mpg <- mechaCarData %>% 
  rename(
    vlength = "vehicle_length", 
    vweight = "vehicle_weight", 
    spangle = "spoiler_angle", 
    grclearance = "ground_clearance")
head(MechaCar_mpg)

# Generate multiple linear regression model
lm(formula = mpg ~ vlength + vweight + spangle + grclearance + AWD, data = MechaCar_mpg)

# Generate summary statistics
summary(lm(mpg ~ vlength + vweight + spangle + grclearance + AWD,MechaCar_mpg))

#import suspension coil data
suspension_coil_data <- read.csv(file='Resources/Suspension_Coil.csv', check.names=F, stringsAsFactors = F) 
head(suspension_coil_data)

#create total summary table
total_summary <- suspension_coil_data %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
total_summary

#create lot summary table
lot_summary <- suspension_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups = 'keep') 
lot_summary

#randomly sample 50 data points
global_sample_table <- suspension_coil_data %>% sample_n(50) 

#import all coil data into ggplot2
plt <- ggplot(suspension_coil_data,aes(x=PSI)) 
#visualize distribution with density plot
plt + geom_density() 


#import sample coil data into ggplot2
plt <- ggplot(global_sample_table,aes(x=PSI)) 
#visualize distribution with density plot
plt + geom_density() 


#import sample coil data into ggplot2
plt <- ggplot(global_sample_table,aes(x=log10(PSI))) 
#visualize distribution with density plot
plt + geom_density() 


#compare sample versus population mean
t.test(global_sample_table$PSI,mu=mean(suspension_coil_data$PSI)) 


#create a sample table of 25 data points with Lot 1
psi_lot1_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot1') %>% sample_n(25) 
#compare Lot1 sample versus population mean
t.test(psi_lot1_sample$PSI,mu=mean(suspension_coil_data$PSI)) 

#create a sample table of 25 data points with Lot 2
psi_lot2_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot2') %>% sample_n(25) 
#compare Lot2 sample versus population mean
t.test(psi_lot2_sample$PSI,mu=mean(suspension_coil_data$PSI)) 

#create a sample table of 25 data points with Lot 3
psi_lot3_sample <- suspension_coil_data %>% subset(Manufacturing_Lot=='Lot3') %>% sample_n(25) 
#compare Lot3 sample versus population mean
t.test(psi_lot3_sample$PSI,mu=mean(suspension_coil_data$PSI)) 


