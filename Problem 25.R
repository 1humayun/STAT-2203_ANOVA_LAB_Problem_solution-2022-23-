###########################  Problem 25  #################################
deff_pieces = c(25, 26, 25, 19, 24, 21, 23, 26, 24, 20, 23, 22,
                26, 28, 31, 21, 27, 23, 34, 27, 29, 26, 28, 24,
                28, 25, 33, 27, 25, 24, 26, 28, 24, 32, 28, 29,
                30, 29, 28, 15, 13, 21, 25, 29, 29, 18, 20, 24)
machine = factor(rep(rep(c("A1", "A2", "A3", "A4"), each = 3), 4))
workman = factor(rep(c("B1", "B2", "B3", "B4"), each = 12))

# Create the data frame...
df = data.frame(deff_pieces, machine, workman)

# Perform the two-way ANOVA...
anova_model = aov(def_pieces ~ machine * workman, data = df)
summary(anova_model)
