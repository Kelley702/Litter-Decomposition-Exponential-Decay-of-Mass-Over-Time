library(ggplot2)

# Import and Setup Original Data__________________________________________________________
Decomp_Exam_Data <- read.csv("C:\\Users\\kelle\\OneDrive\\Spring 2023\\Advanced Forest Soils\\Soils_TakeHomeTest\\Decomp2023data_Sorted.csv")
colnames(Decomp_Exam_Data) <- c("Species", "Treatment", "Plot", "Subplot", "MassLoss", "Time" )

# Box Plot
ggplot(Decomp_Exam_Data, aes(x=as.factor(Time), y=MassLoss, fill=Treatment)) + 
  geom_boxplot() + 
  facet_wrap(~Species)

#what I really used for Boxplots

#Boxplot Red Maple

boxplot(MASS ~ TRT, data = RedMaple, main = " Impact of Temperature Increase on Red Maple Decomposition",
        xlab = "Treatment", ylab = "Percent Mass Remaining")

#Boxplot Red Spruce

boxplot(MASS ~ TRT, data = RedSpruce, main = " Impact of Temperature Increase on Red Spruce Decomposition",
        xlab = "Treatment", ylab = "Percent Mass Remaining")



# Import and Setup SORTED Data__________________________________________________________
Decomp_Exam_Data_Sorted <- read.csv("C:\\Users\\kelle\\OneDrive\\Spring 2023\\Advanced Forest Soils\\Soils_TakeHomeTest\\Decomp2023data_Sorted.csv")
colnames(Decomp_Exam_Data_Sorted) <- c("Plot", "Subplot", "Time", "RM_Control", "RM_Heat", "RS_Control", "RS_Heat")

# T-Tests - Comparing treatments for each species at 3 time points
## RM~0.5
RM_CONT_0.5 <- c(rnorm(8, mean = 0.612625, sd = 0.125422643))
RM_HEAT_0.5 <- c(rnorm(8, mean = 0.583125, sd = 0.05810689))
t.test(RM_CONT_0.5, RM_HEAT_0.5, paired = FALSE)
## RM~1.5
RM_CONT_1.5 <- c(rnorm(8, mean = 0.534375, sd = 0.071229884))
RM_HEAT_1.5 <- c(rnorm(8, mean = 0.531708333, sd = 0.036292654))
t.test(RM_CONT_1.5, RM_HEAT_1.5, paired = FALSE)
## RM~2.5
RM_CONT_2.5 <- c(rnorm(8, mean = 0.494857143, sd = 0.034909796))
RM_HEAT_2.5 <- c(rnorm(8, mean = 0.474625, sd = 0.062646257))
t.test(RM_CONT_2.5, RM_HEAT_2.5, paired = FALSE)
## RS~0.5
RS_CONT_0.5 <- c(rnorm(10, mean = 0.687, sd = 0.07469196))
RS_HEAT_0.5 <- c(rnorm(10, mean = 0.693, sd = 0.06074537))
t.test(RS_CONT_0.5, RS_HEAT_0.5, paired = FALSE)
## RS~1.5
RS_CONT_1.5 <- c(rnorm(10, mean = 0.579, sd = 0.04040077))
RS_HEAT_1.5 <- c(rnorm(10, mean = 0.553, sd = 0.054170513))
t.test(RS_CONT_1.5, RS_HEAT_1.5, paired = FALSE)
## RS~2.5
RS_CONT_2.5 <- c(rnorm(10, mean = 0.467333, sd = 0.048196465))
RS_HEAT_2.5 <- c(rnorm(10, mean = 0.381666, sd = 0.055382486))
t.test(RS_CONT_2.5, RS_HEAT_2.5, paired = FALSE)

# fit data to get K values
## RM - Control
RM_CONT.model <- nls(RM_Control ~ exp(K * Time), data = Decomp_Exam_Data_Sorted, start = list(K = 0.1))
summary(RM_CONT.model)
## RM - Heat
RM_HEAT.model <- nls(RM_Heat ~ exp(K * Time), data = Decomp_Exam_Data_Sorted, start = list(K = 0.1))
summary(RM_HEAT.model)
## RS - Control
RS_CONT.model <- nls(RS_Control ~ exp(K * Time), data = Decomp_Exam_Data_Sorted, start = list(K = 0.1))
summary(RS_CONT.model)
## RS - Heat
RS_HEAT.model <- nls(RS_Heat ~ exp(K * Time), data = Decomp_Exam_Data_Sorted, start = list(K = 0.1))
summary(RS_HEAT.model)

