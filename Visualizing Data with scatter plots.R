#import data
library(readxl)
Decomp2023data <- read_excel("Decomp2023data.xlsx")
View(Decomp2023data)

# Pull Red Maple Data as an Index
RedMaple <- Decomp2023data[1:64, 1:6]
RedSpruce <- Decomp2023data[65:155, 1:6]

rmheat <- Decomp2023data[1:32, 1:6]
rmcon <- Decomp2023data[33:64, 1:6]
rsheat <- RedSpruce[41:80, 1:6]
rescon <- RedSpruce[1:40, 1:6]

#create dataframes

rmheatdf <- data.frame(x = rmheat$TIME, y = rmheat$MASS, group = "heat")
rmcondf <- data.frame(x=rmheat$TIME, y=rmheat$MASS, group= "control")

#combine into a single data frame

rmdataframe <- rbind(rmheatdf, rmcondf)

#Make a ScatterPlot of the combined dataset
library(ggplot2)

#Treatment effects on Red Spruce
ggplot(Decomp2023data, aes(Decomp2023data$TIME, Decomp2023data$MASS, color = TRT, shape = SP)) +
  geom_point() +
  scale_color_manual(values = c("green", "red")) +
  scale_shape_manual(values = c(30,17)) +
  xlab("Time")+
  ylab("Percent Mass Remaining") +
  ggtitle( "The effect of heated soil on red spruce leaf decomposition")

#Treatment effect on Red Maple
ggplot(Decomp2023data, aes(Decomp2023data$TIME, Decomp2023data$MASS, color = TRT, shape = SP)) +
  geom_point() +
  scale_color_manual(values = c("green", "red")) +
  scale_shape_manual(values = c(16,30)) +
  xlab("Time")+
  ylab("Percent Mass Remaining") +
  ggtitle( "The effect of heated soil on red maple leaf decomposition")  


#Species effect on control plots
ggplot(Decomp2023data, aes(Decomp2023data$TIME, Decomp2023data$MASS, color = SP, shape = TRT)) +
  geom_point() +
  scale_color_manual(values = c("blue", "gold")) +
  scale_shape_manual(values = c(16,30)) +
  xlab("Time")+
  ylab("Percent Mass Remaining") +
  ggtitle( "How decomposition rates differ between red spruce and red maple leaves in control plots") 


#Species effect on heated plots
ggplot(Decomp2023data, aes(Decomp2023data$TIME, Decomp2023data$MASS, color = SP, shape = TRT)) +
  geom_point() +
  scale_color_manual(values = c("blue", "gold")) +
  scale_shape_manual(values = c(30,17)) +
  xlab("Time")+
  ylab("Percent Mass Remaining") +
  ggtitle( "How decomposition rates differ between red spruce and red maple leaves in heated plots") 


#BOX PLOTS

boxplot(RedMaple$MASS)

#Two Way ANOVA- tests the effect of 2 independent variables on a dependent variable

#step 1 create a dataframe
anovadf <- data.frame(Decomp2023data)

aov_result <- aov( Decomp2023data$MASS~ Decomp2023data$SP + Decomp2023data$TRT, data= anovadf)
summary(aov_result) #reveals no significant difference, I'm not sure if this analysis is correct

#splitting data into time specific data frames
time5 <- Decomp2023data[37:72, 1:6] #not selecting the correct values
Time15 <- Decomp2023data[73:108, 1:6] #are not selecting the correct values
Time25 <- Decomp2023data[109:144, 1:6]

time5aov <- aov(time5$MASS ~ time5$SP + time5$TRT, data= time5)
summary(time5aov) #not significant

time15aov <- aov(Time15$MASS ~ Time15$SP + Time15$TRT, data= Time15)
