###############################  Problem 20  ################################

yield = c(85.5, 72.8, 76.7, 89.9, 
          75.3, 79.8, 60.9, 65.4, 72.8, 72.2, 
          78.1, 83.5, 72.6, 9.5, 96.8, 84.7, 
          93.6, 72.8, 75.9)
variety = c(rep("A", 4), 
            rep("B", 6), 
            rep("C", 6), 
            rep("D", 3))

df = data.frame(yield, variety)


#............... (i)
# Perform ANOVA and find summary...
anova_model = aov(yield ~ variety, data = df)
summary(anova_model)

#Comment: 
#Since the p-value 0.751 > 0.05, we may fail to reject the null hypothesis.


#............... (ii)
# Subset data for varieties B and D
variety_B = subset(df, variety == "B")
variety_D = subset(df, variety == "D")

# Perform two-sample t-test
t_test_BD = t.test(variety_B$yield, variety_D$yield, var.equal = TRUE)
t_test_BD

#Comment: 
#The p-value 0.1436 > 0.05, we may fail to reject the null hypothesis.


#.......................... (iii)
# Subset data for varieties A and C...
variety_A = subset(df, variety == "A")
variety_C = subset(df, variety == "C")

# Perform t-test to get the CI...
t_test_AC = t.test(variety_A$yield, variety_C$yield, var.equal = TRUE, conf.level = 0.95)
t_test_AC$conf.int


