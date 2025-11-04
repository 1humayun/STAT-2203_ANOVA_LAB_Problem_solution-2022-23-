#############################  Example-12 (Page-252) ################################
Block = factor(rep(1:4, each = 8))

A = factor(c(0, 1, 1, 0, 0, 1, 1, 0,         # 0 represents absence of factor effect
            1, 0, 0, 1, 1, 0, 1, 0,         # 1 represents presence of factor effect
            0, 0, 0, 1, 1, 0, 1, 1,
            1, 1, 0, 0, 1, 0, 0, 1))

B = factor(c(0, 0, 1, 1, 0, 0, 1, 1, 
             0, 0, 1, 1, 0, 0, 1, 1,
            1, 1, 0, 0, 1, 0, 1, 0,
            1, 0, 0, 1, 0, 1, 0, 1))

C = factor(c(0, 0, 0, 0, 1, 1, 1, 1, 
            1, 0, 0, 1, 0, 1, 0, 1,
            0, 1, 1, 0, 0, 0, 1, 1,
            0, 1, 0, 1, 0, 0, 1, 1))

Yield = c(257, 232, 230, 211, 210, 176, 186, 175,
         267, 276, 262, 220, 256, 269, 285, 272,
         188, 186, 160, 188, 164, 214, 182, 166,
         204, 206, 239, 224, 254, 269, 252, 301)

df = data.frame(Block, A, B, C, Yield)
df

#.............................. (ii) ...
anova_model = aov(Yield~Block + A*B*C, data = df)
summary(anova_model)

#.............................. (i) ...
# To estimate the main effects and interactions:
model_coefficients <- coef(anova_model)
model_coefficients

#.............................. (iii) ...
# Fit the ANOVA model (confounding ABC with blocks)
model <- aov(Yield ~ Block + A * B * C - A:B:C, data = df)
#Anova table (confunded ABC)
summary(model)

#the solution completes here.


#.........................................................................................
#Another way to define the data frame (use any one of them, which is better for you)
Block = factor(rep(1:4, each = 8))
Treatment = factor(c("(1)", "a", "ab", "b", "c", "ac", "abc", "bc",
                     "ac", "(1)", "b", "abc", "a", "c", "ab", "bc",
                     "b", "bc", "c", "a", "ab", "(1)", "abc", "ac",
                     "ab", "ac", "(1)", "bc", "a", "b", "c", "abc"))
Yield = c(257, 232, 230, 211, 210, 176, 186, 175,
         267, 276, 262, 220, 256, 269, 285, 272,
         188, 186, 160, 188, 164, 214, 182, 166,
         204, 206, 239, 224, 254, 269, 252, 301)
A = ifelse(grepl("a", Treatment), 1, 0)
B = ifelse(grepl("b", Treatment), 1, 0)
C = ifelse(grepl("c", Treatment), 1, 0)

df = data.frame(Block, A, B, C, Yield)
df
#.........................................................................................
