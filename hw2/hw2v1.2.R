# author: Yu Tianxiong
# based on R files prepared by Yuri Trifonov, NRU HSE

# import related libraries
library('readxl')     # for reading excel files
library('rcompanion') # for plotting normal curve over histograms easily
library('psych')
library('ggplot2')    # for plotting beautiful graphs
library('readxl')     # for reading excel files
library('car')
library('lmtest')


# dataloader
df <- read.csv("D:/Master1grade/Econometrics/module2/hw2/Team4 HW1/TV_dataset.csv", encoding="UTF-8")
View(df)


# Part 1a (a) construct a basic model
model_baseline <- lm(rate ~ size + sellp + origp + brand + reso + os , data = df)
summary(model_baseline)


vif(model_baseline)  # calculate VIFs, sellp = 7.235743, origp = 8.891786, suspected multicollinearity




# b) Propose a linear hypothesis and test it.
# F-test for testing brand=size=0
linearHypothesis(model_baseline, c("brand=0", "size=0"))


# one more example, F-test for testing sellp and origp contribute same to rank
linearHypothesis(model_baseline, "sellp - origp = 0")




# c) Propose an alternative model which can be considered as an extended version 
# of the baseline model. Conduct F-tests to choose between the models.


# create the log variable for both prices, but not now
# df$ln_sellp <- log(df$sellp)
# df$ln_origp <- log(df$origp)


# define discount rate
df$disr <- (df$origp - df$sellp) / df$origp


# transfer brand to dummy variables, makes more sense
library(dplyr)
df$brand_ <- as.factor(df$brand)
brand_dummy <- model.matrix(~brand_-1,data=df)  # transfer brand to 5 dummy variables

df <- cbind(df, brand_dummy) # add only 4 into model
df <- select(df,-brand_0) # add brand_1 to brand_3 and drop brand_0, brand_4 will be tested later
df <- select(df,-brand_)


# add all of these above into the baseline model
model_1a_c <- lm(rate ~ size + sellp + origp + brand + reso + os + 
                  + disr + brand_1 + brand_2 + brand_3, data = df)
summary(model_1a_c)


# test which model to choose using F-test
linearHypothesis(model_1a_c, c("disr=0", "brand_1=0", "brand_2=0", "brand_3=0"))


# i. various sets of explanatory variables;
model_1b_e_i1 <- lm(rate ~ size + sellp + origp + brand + reso + os , data = df) # model_baseline
model_1b_e_i2 <- lm(rate ~ size + sellp + reso + os 
                      + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df)
model_1b_e_i3 <- lm(rate ~ size + sellp + reso 
                      + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # a good model we find
model_1b_e_i4 <- lm(rate ~ size + sellp 
                    + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # drop too much


summary(model_1b_e_i1)
summary(model_1b_e_i2)
summary(model_1b_e_i3)

# J-test
jtest(model_1b_e_i1, model_1b_e_i2) # i1 in i2
jtest(model_1b_e_i1, model_1b_e_i3) # i1 in 13
jtest(model_1b_e_i1, model_1b_e_i4) # inconclusive


# non nested F test cannot be conducted here


# ii. linear vs. non-linear form of the model;

model_1b_e_ii1 <- lm(rate ~ size + sellp + reso 
                     + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # linear model
summary(model_1b_e_ii1)

# create the log variable for both prices
df$ln_size <- log(df$size)
df$ln_rate <- log(df$rate)
df$ln_sellp <- log(df$sellp)
df$ln_origp <- log(df$origp)


# not for disr because TV can have 0 discount
model_1b_e_ii2 <- lm(ln_rate ~ ln_size + ln_sellp + reso 
                     + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # log model
summary(model_1b_e_ii2)


# PE test
petest(model_1b_e_ii1, model_1b_e_ii2)      # conduct a PE test


# iii. Ramsey test;
model_1b_e_ii3 <- lm(rate ~ size + sellp + reso 
                     + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # linear rate
resettest(model_1b_e_i3, power=2:2) # conduct a RESET test



# iv. Chow test.

model_pooled <- lm(rate ~ size + sellp + reso 
                   + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df) # estimate the pooled model

model_android <- lm(df$rate[df$os == 1] ~ 
                    df$size[df$os == 1] + 
                    df$sellp[df$os == 1] +
                    df$reso[df$os == 1] +
                    df$disr[df$os == 1] +
                    df$brand_1[df$os == 1] +
                    df$brand_2[df$os == 1] +
                    df$brand_3[df$os == 1] +
                    df$brand_4[df$os == 1] , data = df)

model_no_android <- lm(df$rate[df$os == 0] ~ 
                         df$size[df$os == 0] + 
                         df$sellp[df$os == 0] +
                         df$reso[df$os == 0] +
                         df$disr[df$os == 0] +
                         df$brand_1[df$os == 0] +
                         df$brand_2[df$os == 0] +
                         df$brand_3[df$os == 0] +
                       df$brand_4[df$os == 0] , data = df)


# calculate rss values from each regression 
rss_pooled <- sum(model_pooled$residuals^2)     # calculate RSS value for the pooled model
rss_1 <- sum(model_android$residuals^2)           # calculate RSS value for the model with oss
rss_2 <- sum(model_no_android$residuals^2)        # calculate RSS value for the model without oss

# Compute a test statistic
K <- 9                                        # define the number of parameters
n1 <- length(model_android$residuals)             # observations in the first subsample
n2 <- length(model_no_android$residuals)          # observations in the second subsample

f_obs <- ((rss_pooled - (rss_1 + rss_2)) / K) / ((rss_1 + rss_2)/(n1 + n2 - 2 * K))   # calculate F statistic

# and we can calculate a p-value
p_value <- 1 - pf(f_obs, df1 = K, df2 = n1 + n2 - 2 * K)  # p_value = 0.7308

# Inference: We cannot reject the null hypothesis ->> There is not a structural break


#f) Conduct tests or procedures not listed above if you find them relevant;


# g) Using steps of Part 1b choose the final model. Write down an equation 
# with estimated parameters that respects to the chosen model. Interpret the coefficients.

model_final <- lm(rate ~ size + sellp + reso 
                  + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df)


summary(model_final)




# Part 2. Violations of other assumptions (30 points)


# a) multicollinearity, nope~
vif(model_final) 


model_2_a <- lm(rate ~ size + sellp + reso 
                + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df)
summary(model_2_a)

# b) heteroscedasticity
resid <- model_2_a$residuals      # let's extract residuals

#create residual vs. fitted plot
plot(fitted(model_2_a), resid, xlab='fitted values', ylab='residuals')
abline(0,0)


# Goldfeld Quandt Test 
gqtest(model_2_a,                 # p-value = 0.0482
       order.by = df$sellp,  # for which variable do we sort our observations
       fraction = 0.25)       # which fraction from the center we ELIMINATE


# Breusch Pagan Test (include only linear dependence by default)
bptest(model_2_a) # p-value = 0.0000


# White Test
bptest(model_2_a, # p-value = 0.0032
       varformula = ~ I(size^2) + I(sellp^2) + reso + disr + brand_1 + brand_2 + brand_3 + brand_4,
       data = df)


# all of these tests indicate the possibility of heteroscedasticity

# WLS approach
# define weights to use
wt <- 1 / lm(abs(model_2_a$residuals) ~ model_2_a$fitted.values)$fitted.values^2

# perform weighted least squares regression
wls_model <- lm(rate ~ size + sellp + reso 
                + disr + brand_1 + brand_2 + brand_3 + brand_4, data = df, weights = wt)

# summary of WLS
summary(wls_model)


## Now, let's estimate the model with White standard errors
library(sandwich)


vcovHC(model_2_a)                  # robust estimate of the covariance matrix
coeftest(model_2_a, vcovHC(model_2_a)) # regression with the new cov matrix
summary(model_2_a)                 # compare with the original



