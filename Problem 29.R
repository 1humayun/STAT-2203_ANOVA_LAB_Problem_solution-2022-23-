###########################  Problem 29  #################################
yield= c(
   449,444,401,299,292,
   463,375,323,264,415,
   393,353,278,404,425,
   371,241,441,410,392,
   258,430,450,385,347
)

treat= c(
   'A','B','C','D','E',
   'B','C','D','E','A',
   'C','D','E','A','B',
   'D','E','A','B','C',
   'E','A','B','C','D'
)

row= factor(rep(1:5, each=5))
col= factor(rep(1:5, times=5))
df = data.frame(yield, treat, row, col)

#.................... (i)
# Latin square ANOVA
anova_model= aov(yield ~ row + col + treat, data = df)
summary(anova_model)

#................... (ii)
#Duncan's new multiple range test for comparing all treatment means

#calculate mean for each treatment
means= tapply(yield, treat, mean)
means
means_sorted = sort(means, decreasing = TRUE)            #Sort means descending
means_sorted

k= length(means_sorted)
r= 5      #each treatment appears 5 times

#extract Mean sq error and Error df
MSE= summary(model)[[1]][3,3]
MSE
dfE= summary(model)[[1]][3,1]
dfE

# Critical q values for each step (r = 2, 3, 4, 5)
q_values= qtukey(0.95, nmeans = 2:5, df = dfE)
q_values

#Calculate the critical value: Least Significant Ranges (LSR) 
LSR= q_values * sqrt(MSE / r)
LSR

#Mean differences (B-A, B-C, B-D, B-E)
diff_mat= outer(means_sorted, means_sorted, "-")
diff_mat

#Interpretation: [if mean diff > LSE, Significant]
#Pairwise comparison shows that fertilizers B and A do not differ significantly, 
#while all other differences (B–C, B–D, B–E, etc.) exceed their respective LSRs and are therefore significant.
#Hence, Fertilizer B gives the highest yield and E the lowest.

#................. (iii)
mean_diff= means["C"] - means["E"]
SE_diff= sqrt(2 * MSE / 5)
t_val= qt(0.975, dfE)

CI= mean_diff + c(-1,1) * t_val * SE_diff
CI

