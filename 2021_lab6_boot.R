## -------------------------------------------------------------
## Stat/Q Sci 403 Lab 6 | Spring 2021 | University of Washington
##     Based on labs of Yen-Chi Chen and Vladimir Minin
## -------------------------------------------------------------

###                       ###
### Part 1: Getting Data  ###
###                       ###

### Example: 
### Abalone Data Set in UCI repository
### http://archive.ics.uci.edu/ml/datasets/Abalone
###  You can also find this data file on the class Canvas

getwd()
# check the current location of the console

## Or you can
# set the location of the console to a specific place
# you can also use "Session" > "Set Working Directory" > "Choose Directory"

setwd("/Users/thompson/2017_files/STAT403_2020/My_stuff_2021/Labs_2021/")

# sep: how variables are separated
# header: T or F, controlling if the first line contains the name of 
#         the variable

# Or, an easier way is to use your computer's file search:

data0 = read.table(file.choose(), sep=",")

head(data0)
# check the first few lines

names(data0)

names(data0) = c("Sex","Length","Diameter","Height","Whole weight",
                 "Shucked weight", "Viscera weight","Shell weight","Rings")
# set the name of the variables
head(data0)

summary(data0)
# a summary describing contents in this dataset

## basic exploratory data analysis
table(data0$Sex)

hist(data0$Length, col="palegreen")

hist(data0$`Whole weight`, col="palegreen")

hist(data0$Rings, col="palegreen")
# 'Rings' is a proxy of the age of the abalone

###                         ###
### Part 2: Fitting a Model ###
###                         ###
### Whole weight -- 
hist(data0$`Whole weight`, col="palegreen")
## for the variable "Whole weight", it seems to be a decaying pattern
## let's try to fit a Exponential distribution for it

lambda_est = 1/mean(data0$`Whole weight`)

lambda_est


## plot the fitted density

# Remember to set "probability = T"

hist(data0$`Whole weight`, col="palegreen", probability = T)
l_seq = seq(from=0, to=5, by=0.05)
lines(x=l_seq, y=dexp(l_seq,rate = lambda_est), lwd=2, col="purple")
# kind of fitting something but not that well

###                             ###
### Part 2: Empirical Bootstrap ###
###                             ###
### Getting the abalone dataset if we did't already have it

## setwd("/Users/thompson/2017_files/STAT403_2020/My_stuff_2021/Labs_2021")
## data0 = read.table("./Data_sets/Abalone/abalone_data.csv", sep=",")

data0 = read.table(file.choose(), sep=",")
names(data0) = c("Sex","Length","Diameter","Height","Whole weight",
                 "Shucked weight", "Viscera weight","Shell weight","Rings")

head(data0)

X = data0$`Whole weight`
hist(X, col="palegreen")

### Median: 
X_med = median(X)
X_med

abline(v=X_med, lwd=3, col='red')

n = length(X)
  # sample size

sample(10, 10, replace=T)
  # the sample() function allows us to sample with replacement
  # the key it to set 'replace = T'


## one run of the bootstrap
w = sample(n,n,replace=T)
X_BT = X[w]
median(X_BT)


B = 5000
X_med_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  X_BT = X[w]
  X_med_BT[i_BT] = median(X_BT)
    # save each bootstrap median
}

X_med_BT
  # value of the bootstrap sample

hist(X_med_BT, col="skyblue")
abline(v=X_med, lwd=3, col="red")

## Bootstrap estimate
var(X_med_BT)   # this is the bootstrap variance (VAR)

mean((X_med_BT-X_med)^2)   # this is the bootstrap MSE

mean(X_med_BT)- X_med      # bootstrap estimate of bias

c(X_med-qnorm(0.95)*sd(X_med_BT),X_med+qnorm(0.95)*sd(X_med_BT))
  # the bootstrap 90% CI using normality

quantile(X_med_BT, c(0.05,0.95))
  # the bootstrap 90% CI using the quantile


### Bootstrap IQR (Q3-Q1):
summary(X)
X_IQR = IQR(X)

B = 5000
X_IQR_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  X_BT = X[w]
  X_IQR_BT[i_BT] = IQR(X_BT)
  # save each bootstrap IQR
}

X_IQR_BT
# value of the bootstrap sample

hist(X_IQR_BT, col="tan")
abline(v=X_IQR, lwd=3, col="purple")


## Bootstrap estimate
var(X_IQR_BT)
  # this is the bootstrap variance

mean((X_IQR_BT-X_IQR)^2)
  # this is the bootstrap MSE

c(X_IQR-qnorm(0.95)*sd(X_IQR_BT), X_IQR+qnorm(0.95)*sd(X_IQR_BT))
  # the bootstrap 90% CI using normality

quantile(X_IQR_BT, c(0.05,0.95))
  # the bootstrap 90% CI using the quantile

### maximum
X_max = max(X)

X_max         #  the sample value

B = 5000
X_max_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  X_BT = X[w]
  X_max_BT[i_BT] = max(X_BT)
  # save each bootstrap median
}

X_max_BT
# value of the bootstrap sample

hist(X_max_BT, col="tan")
abline(v=X_max, lwd=3, col="purple")

###                              ###
### Part 3: Parametric Bootstrap ###
###                              ###
X = data0$`Whole weight`
hist(X, col="palegreen")

n = length(X)

## Although this may sound like a bad idea, we try to fit a Normal 
## distribution to this dataset.

X_med = median(X)

X_mean = mean(X)
X_sd = sd(X)
  # these are the estimate of the mean and SD of the Normal distribution

rnorm(10, mean=X_mean, sd=X_sd)
  # this generates random points from the fitted Normal distribution

median(rnorm(n, mean=X_mean, sd=X_sd))
  # the sample 'median' of a (parametric) bootstrap sample

B = 10000
X_med_BT = rep(NA, B)
for(i_BT in 1:B){
  X_BT = rnorm(n, mean=X_mean, sd=X_sd)
    # We generate new bootstrap sample from the fitted model!
  X_med_BT[i_BT] = median(X_BT)
  #X_med_BT[i_BT] = mean(X_BT)
}

hist(X_med_BT, col="violet")
abline(v= X_med, col="blue", lwd=6)
  # here you see that if we fit a wrong model to the data and use the 
  # parametric bootstrap, we are off a lot!

var(X_med_BT)
mean((X_med_BT-X_med)^2)
  # bootstrap VAR and MSE differs a lot!

c(X_med-qnorm(0.95)*sd(X_med_BT), X_med+qnorm(0.95)*sd(X_med_BT))

quantile(X_med_BT, c(0.05,0.95))
  # the two CI's does not agree each other!


### Using 'iris' dataset, 'Sepal.Width'

?iris     # a famous data set: built in in R

head(iris)
hist(iris$Sepal.Width, col="orange")
  # seems to be normal  (my comment:  ?? really)

X = iris$Sepal.Width

n = length(X)

X_med = median(X)
X_mean = mean(X)
X_sd = sd(X)

X_med
X_mean
X_sd

B = 10000
X_med_BT = rep(NA, B)
for(i_BT in 1:B){
  X_BT = rnorm(n, mean=X_mean, sd=X_sd)
  X_med_BT[i_BT] = median(X_BT)
}

hist(X_med_BT, col="violet", xlim=c(2.8,3.2))
abline(v= X_med, col="blue", lwd=6)
  ## still does not work well!

var(X_med_BT)
mean((X_med_BT-X_med)^2)
  # VAR underestimates the error (MSE) by too much

quantile(X_med_BT, c(0.05,0.95))



###                          ###
### Part 4: Permutation Test ###
###                          ###
data0$Sex
dataM = data0$`Whole weight`[data0$Sex=="M"]
dataF = data0$`Whole weight`[data0$Sex=="F"]
t.test(dataM, dataF)
ks.test(dataM,dataF)

hstM = hist(dataM, plot=F)
hstF = hist(dataF, plot=F)

plot(hstM, col='red', freq=FALSE, xlim=c(0,3))
plot(hstF, col='green', freq=FALSE, xlim=c(0,3))

## permutation test + median
dataM_med = median(dataM)

dataF_med = median(dataF)
diff_med = abs(dataM_med-dataF_med)

n_M = length(dataM)
n_F = length(dataF)
n = n_M+n_F

data_pull = c(dataM, dataF)

N_per = 10000
diff_med_per = rep(NA, N_per)
for(i_per in 1:N_per){
  w_per = sample(n, n, replace=F)
  data_per = data_pull[w_per]
  # data after permutation
  dataM_new = data_per[1:n_M]
  dataF_new = data_per[(n_M+1):n]
  # first n_M are new group M; the others are new group F
  diff_new = abs(median(dataM_new)-median(dataF_new))
  # compute the difference
  diff_med_per[i_per] = diff_new
}

which(diff_med_per>diff_med)

# this is the p-value of permutation test;  >=, and N_per+1
(length(which(diff_med_per >= diff_med))+1)/(N_per+1)

par(mfrow=c(1,1))
hist(diff_med_per, col="cyan")
abline(v = diff_med, col="red", lwd=6)


## permutation test + 10% quantile
dataM_q1 = quantile(dataM,0.1)
dataF_q1 = quantile(dataF,0.1)
diff_q1 = abs(dataM_q1-dataF_q1)

n_M = length(dataM)
n_F = length(dataF)
n = n_M+n_F

data_pull = c(dataM, dataF)

N_per = 10000
diff_q1_per = rep(NA, N_per)
for(i_per in 1:N_per){
  w_per = sample(n, n, replace=F)
  data_per = data_pull[w_per]
  dataM_new = data_per[1:n_M]
  dataF_new = data_per[(n_M+1):n]
  diff_new = abs(quantile(dataM_new,0.1)-quantile(dataF_new,0.1))
  # now we use 10% quantile difference
  diff_q1_per[i_per] = diff_new
}

hist(diff_q1_per, col="orchid")
abline(v = diff_q1, col="limegreen", lwd=6)

which(diff_q1_per>diff_q1)

(length(which(diff_q1_per >= diff_q1))+1)/(N_per+1)

# very low p-value!


#### Example: 
#### Again we will focus on the two sample problem of Male versus Female
#### Abalone. This time we use the variable "Shucked weight". 
#### (1) First we try to use t.test and ks.test to check if they are 
####     different. What are the corresponding p-values?
{dataM = data0$`Shucked weight`[data0$Sex=="M"]
dataF = data0$`Shucked weight`[data0$Sex=="F"]
}
t.test(dataM, dataF)
ks.test(dataM,dataF)

#### (2) Now do a permutation test using the diffrence of 10% quantile.
####     What are the p-value? Are they significantly different under
####     significance level alpha = 0.05?
{dataM_q1 = quantile(dataM,0.1)
dataF_q1 = quantile(dataF,0.1)
diff_q1 = abs(dataM_q1-dataF_q1)

n_M = length(dataM)
n_F = length(dataF)
n = n_M+n_F

data_pull = c(dataM, dataF)

N_per = 10000
diff_q1_per = rep(NA, N_per)
for(i_per in 1:N_per){
  w_per = sample(n, n, replace=F)
  data_per = data_pull[w_per]
  dataM_new = data_per[1:n_M]
  dataF_new = data_per[(n_M+1):n]
  diff_new = abs(quantile(dataM_new,0.1)-quantile(dataF_new,0.1))
  diff_q1_per[i_per] = diff_new
}

which(diff_q1_per>diff_q1)
length(which(diff_q1_per>diff_q1)+1)/(N_per +1)
}

###                               ###
### Part 5: Bootstrap Convergence ###
###   Leave as Exercise?          ###
###                               ###
## We now will compare the quality of bootstrap estimate.
## We first compute the error of sample median using Monte Carlo Simulation.
n = 1000
X = rnorm(n)
med0 = 0
  

X_med = median(X)
X_med

N = 10000
  # number of Monte Carlo
X_med_MC = rep(NA, N)
for(i_MC in 1:N){
  X = rnorm(n)
  X_med_MC[i_MC] = median(X)
}
X_med_VAR = var(X_med_MC)
X_med_VAR

X_med_MSE = mean((X_med_MC-med0)^2)
X_med_MSE


## bootstrap case
X = rnorm(n)
  # just one sample
X_med = median(X)
X_med

B = 10000
X_med_BT = rep(NA, B)
for(i_BT in 1:B){
  w = sample(n,n,replace=T)
  X_BT = X[w]
  X_med_BT[i_BT] = median(X_BT)
}
X_med_BT_VAR = var(X_med_BT)
X_med_BT_VAR

X_med_BT_MSE = mean((X_med_BT-X_med)^2)
X_med_BT_MSE

## comparison
X_med_BT_VAR
X_med_VAR

X_med_BT_MSE
X_med_MSE


### different sample size
N = 10000
B = 10000

n_seq = c(50, 200, 500, 1000, 2000, 5000)

info_matrix = matrix(NA, nrow=length(n_seq), ncol=4)
colnames(info_matrix) = c("VAR","MSE","VAR_BT","MSE_BT")
rownames(info_matrix) = n_seq
info_matrix


for(i_n in 1:length(n_seq)){
  n = n_seq[i_n]
  X_med_MC = rep(NA, N)
  for(i_MC in 1:N){
    X = rnorm(n)
    X_med_MC[i_MC] = median(X)
  }
  info_matrix[i_n,1] = var(X_med_MC)
  info_matrix[i_n,2] = mean((X_med_MC-med0)^2)

  X = rnorm(n)
  X_med = median(X)
  X_med_BT = rep(NA, B)
  for(i_BT in 1:B){
    w = sample(n,n,replace=T)
    X_BT = X[w]
    X_med_BT[i_BT] = median(X_BT)
  }
  info_matrix[i_n,3] = var(X_med_BT)
  info_matrix[i_n,4] = mean((X_med_BT-X_med)^2)
}

info_matrix


plot(NULL, xlim=c(0,5000),ylim=c(0.5, 2.5),
     ylab="VAR_BT / VAR", xlab="Sample size", main="Bootstrap VAR")
abline(h=1, lwd=3, col="red")
points(x=n_seq, y=info_matrix[,3]/info_matrix[,1], pch=20,
       col="slateblue", cex=2)
lines(x=n_seq, y=info_matrix[,3]/info_matrix[,1], col="slateblue", lwd=3)


plot(NULL, xlim=c(0,5000),ylim=c(0.5, 2.5),
     ylab="MSE_BT / MSE", xlab="Sample size", main="Bootstrap MSE")
abline(h=1, lwd=3, col="blue")
points(x=n_seq, y=info_matrix[,4]/info_matrix[,2], pch=20,
       col="palevioletred", cex=2)
lines(x=n_seq, y=info_matrix[,3]/info_matrix[,1], col="palevioletred", lwd=3)

