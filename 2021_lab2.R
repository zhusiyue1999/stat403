## -------------------------------------------------------------
## Stat/Q Sci 403 Lab 2 | Spring 2021 | University of Washington
##    Modified from 2020 and based on earlier labs of 
##           Vladimir Minin and Yen-Chi Chen
##
## The goal of this lab is to give examples about CLT, about the CDF, to
##  introduce the ecdf function, and its convergens to the tru CDF,
##   and to give more examples of making plots.
## -------------------------------------------------------------

###                                      ###
### Part 1: Random variables and CLT     ###
###                                      ###

## Exanoke 1: distribution of sample  medians
n_rep = 10000
n = 100
df0 = 4

sample_median = rep(NA,n_rep)
for(i in 1:n_rep){
  data = rchisq(n, df = df0)
  # generate n data points from Chi-sq(df0)
  sample_median[i] = median(data)
}

hist(sample_median, probability = T, col="skyblue", breaks=50)
# The median of n=100 Chisq has an approximately normal distribution! 


## Example: central limit theorem for the saample mean
n_rep = 10000
n = 200
rate0 = 3

sample_mean = rep(NA, n_rep)

for(i in 1:n_rep){
  data = rexp(n,rate = rate0)
  # generate n data points from Exp(rate0)
  sample_mean[i] = mean(data)
  # i-th element of object "sample_mean" is a sample average
}

hist(sample_mean, probability = T, col="orchid", breaks=100)
x_base = seq(from=0, to=1, by=0.001)
lines(x=x_base, y=dnorm(x_base, mean=1/rate0, sd=1/(sqrt(n)*rate0)),
      lwd=3, col="dodgerblue")

## Example: failure of central limit theorem for the Cauchy distribution
n_rep = 100
n = 100000

sample_mean = rep(NA, n_rep)

for(i in 1:n_rep){
  data = rcauchy(n)
  # generate n data points from Cauchy(0,1) distirbution 
  # the "mean" of Cauchy distribution does not exist!
  sample_mean[i] = mean(data)
}

hist(sample_mean, probability = T, col="pink", breaks=100)
x_base = seq(from=0, to=1, by=0.001)
# with some probability, the sample mean could be very large/small


#####
##### Exercise 1.1: the distribution of sample standard deviation.
#####    Generate n=100 data points from Exp(5), compute the sample SD.
#####    Repeat the procedure n_rep=10000 times and plot the histogram.
#####    You can use the function sd() to compute sample SD.

n_rep = 10000
n = 100

sample_sd = rep(NA, n_rep)

for(i in 1:n_rep){
  data = rexp(n,rate = 5)
  sample_sd[i] = sd(data)
}

hist(sample_sd, probability = T, col="rosybrown", breaks=30)

### Part 2: CDF  ###
###              ###

### Example 1: normal distribution 

# pnorm gives the CDF values, so then we just plot the CDF curve

c(pnorm(-2),pnorm(-1),pnorm(0),pnorm(1),pnorm(2))

x_base = seq(from=-4, to=4, by=0.1)
plot(x_base, pnorm(x_base), type="l", 
     lwd=5, col="red", lty=4)

plot(x=x_base, y=pnorm(x_base), type="l", lwd=3, col="chocolate1")
abline(h=0.5, lwd=5, col="red")
  # abline(h=0): add the horizontal line at y=0

### Example 2: t-distribution

help(pt)       # check out the functions

x_base = seq(from=-4, to=4, by=0.01)

plot(x=x_base, y=pt(x_base, df=10), type="l", ylim=c(0,1))
  # ylim=c(0,1): set the range of Y-axis, df=degrees of freedom of t =10
abline(h=0, col="gray")
abline(h=1, col="gray")

## overlay multiple curves: norml and t
x_base = seq(from=-4, to=4, by=0.01)
plot(x_base, pnorm(x_base), type="l", lwd=3, col="blue")
lines(x_base, pt(x_base, df=10), lwd=3, col="red")
abline(h=0)

legend("bottomright", c("Normal","T (df=10)"), 
       lwd=3, col=c("blue","red"), cex=0.7)
  # add a legend; play around with each argument to
  # see its functionality.

####
#### Exercise 2.1: Compare t with different degrees of Freedom
####

plot(x=x_base, y=pnorm(x_base), type="l", lwd=3, 
     col="blue")
lines(x=x_base, y=pt(x_base, df=1), lwd=3, 
      col="red")
lines(x=x_base, y=pt(x_base, df=2), lwd=3, 
      col="orchid")
lines(x=x_base, y=pt(x_base, df=5), lwd=3, 
      col="purple")
abline(h=0)
abline(h=0.5)
abline(h=1)

legend("topleft", cex=0.7,
       c("Normal","T (df=1)","T (df=2)", "T (df=5)"), 
       lwd=3, col=c("blue","red","orchid","purple"))
  # as df increases, t-distribution gets closer to the Normal distribution

#### Exercise 2.2: 
#### In one plot, show the CDF of uniform distribution over [-4,4]
####  and Exp(2) and N(0,1) for x within [-5, 5].
{
x_base = seq(from=-5, to=5, by=0.01)
plot(x=x_base, y=punif(x_base, min=-4, max=4), type="l", 
     lwd=3, col="orchid")
lines(x=x_base, y=pexp(x_base, rate=2), lwd=3, col="limegreen")
lines(x=x_base, y=pnorm(x_base), lwd=3, col="dodgerblue")
abline(h=0)
legend("topleft", legend=c("Uni[-4,4]","Exp(2)","N(0,1)"), 
       col=c("orchid","limegreen","dodgerblue"), lwd=5)
}

###                                         ###
### Part 3: Intro to the EDF and ecdf()     ###
###                                         ###

# The function ecdf() gives the empirical CDF of a sample
#   A more basic way of computing it constructing explicit line segments
#            is given in the homework Rmd template
#   If samples are large, one may as well use a continuous curve as the jumps
#     small,  but remember it is really always a discrete random variable

### Example 3.1
### Given 6 data points 1,3,3,4,7,11 show the EDF curve using these points.

x = c(1,3,3,4,7,11)
x_edf = ecdf(x)   # ecdf() is R's function for the EDF
plot(x_edf)       # It is an object we can plot!


###  What is this x_edf object??  
###

x_edf

str(x_edf)       # 'x_edf' is a function along the real line

knots(x_edf)     # points where the EDF jumps

x_edf(0)         #  Note x_edf is a function -- these are function values
x_edf(2)
x_edf(4)
x_edf(6)
x_edf(10)

####
####  Exercise 3.1 Experiment with alternative plots of the EDF
####    (Remember to recompute it above if you have not already set it)
####
plot(x_edf, xlim=c(-5, 20), col="firebrick", pch=2,
     lwd=3, verticals=T)

plot(x_edf, xlim=c(-5, 20), col="royalblue", lwd=5, cex=2,
     pch=20,verticals=FALSE)
  # you can adjust many aspects of the plot

## Example 3.2: EDF of a randomly generated sample
data = rnorm(100)
data_edf = ecdf(data)

plot(data_edf, xlim=c(-4,4), do.points=F, col="blue")

plot(data_edf, xlim=c(-4,4), do.points=F, col="red", lwd=4)
  # do.points=F: no the big bullet for data points

plot(data_edf, xlim=c(-4,4), do.points=F, col="red",
     verticals = T)
# compare to the N(0,1) CDF
x_base = seq(from=-4, to=4, by=0.01)
lines(x=x_base, y=pnorm(x_base), col="blue")

## Example 3.3: compare 10 curvesa: a small sample size
plot(x=x_base, y=pnorm(x_base), col="red", type="l",
     lwd=5)
for(i in 1:10){                   #  10 samples
  data = rnorm(10)                #  each of size 10
  data_edf = ecdf(data)
  lines(data_edf, do.points=F, lwd=1, verticals=T)
}

## compare 10 curves using a large sample size sample size
plot(x=x_base, y=pnorm(x_base), col="royalblue", type="l",
     lwd=5)
for(i in 1:10){
  data = rnorm(10000)
  data_edf = ecdf(data)
  lines(data_edf, do.points=F, lwd=2, col='red', verticals=T)
}
  # this demonstrates the convergence of EDF to CDF

### Exercise 3.2
### Generate 50 points from N(0,1), call it data1.
### Generate another new 50 points from Exp(1), call it data2.
### In the same plot, show two EDF curves, one is from data1 and
### the other is from data2.
### You can then use The Kolmogorov Smirnov test uses the ECDF to
###  test whether two samples come from the same distribution (CDF)
{
  data1 = rnorm(50)
  data2 = rexp(50)
  data1_edf = ecdf(data1)
  data2_edf = ecdf(data2)
  
  plot(data1_edf, xlim=c(-4,4), do.points=F, col="dodgerblue",
       verticals = T, lwd=2)
  lines(data2_edf,  do.points=F, col="orchid",
        verticals = T, lwd=2)
  legend("topleft", legend=c("N(0,1)","Exp(1)"), 
         col=c("dodgerblue","orchid"), lwd=5)
}

# The Kolmogorov Smirnov test uses the ECDF to test whether two samples
#    come from the same distribution (CDF)

ks.test(data1,data2)

#################  

###
###  Part 4:  EDF-based confidence bouands for the CDF  ###
###


### The EDF provides Pointwise confidence interval for the CDF? ###
n=100
data = rnorm(n)
data_edf = ecdf(data)
x_base = seq(from=-4, to=4, by=0.01)

# In real life, we do not know true F, so use ecdf in variance estimate
var_edf = data_edf(x_base)*(1-data_edf(x_base))/n   
se_edf = sqrt(var_edf)

# Now construct the 90% CI -- note we use qnorm(0.95)=1.645 -- 5% each side
plot(data_edf, do.points=F, lwd=3, verticals=T,
     col="blue")
lines(x=x_base, y=data_edf(x_base)-qnorm(0.95)*se_edf,
      col="dodgerblue", lwd=2, lty=2)
lines(x=x_base, y=data_edf(x_base)+qnorm(0.95)*se_edf,
      col="dodgerblue", lwd=2, lty=2)
# lty=2: dashed curves

####
####  Exercise 4.1:   An alternative plot of these confidence bounds.
####      Again remember to regenerate your data kines 272-279 above if
####        they are not already saved for you

plot(data_edf, do.points=F, lwd=3, verticals=T,  col="blue")
polygon(c(x_base,rev(x_base)), 
        y=c(data_edf(x_base)-qnorm(0.95)*se_edf,
            rev(data_edf(x_base)+qnorm(0.95)*se_edf)), 
        col ="cyan", border = "cyan")          # a 90% CI,  qnorm(0.95)=1.645
# polygon: a function to generate a polygon from points
lines(data_edf, do.points=F, lwd=3, verticals=T,
      col="blue")
#  Again superimose the true curve (in red)
lines(x=x_base, y=pnorm(x_base), col="red", lwd=2)

####
####

### Example 4.2: For a given x,  
###    the EDF value should converge to a Normal distribution
## Example using the exponential distribution at the value x0=2

x0 = 2

# First generate a single value to test

data1 = rexp(100, rate = 1)
data1_edf = ecdf(data1)
data1_edf(x0)

#  Now repeat this 10,000 times and store the values
cdf_x0 = rep(NA, 10000)
for(i in 1:10000){
  x0 = 2
  data1 = rexp(100, rate = 1)
  data1_edf = ecdf(data1)
  
  cdf_x0[i] = data1_edf(x0)
}
# and plot the histogram
hist(cdf_x0, probability = T, col="lightgreen")

# Now to compare this with the true limiting Normal
#   Here we know the mean and SD, since our sample comples from an 
#     exponential distribution  F(x0)=pexp(2)

x_base = seq(from=0.70, to=1, by=0.001)
mu0 = pexp(2)
sd0 = sqrt(pexp(2)*(1-pexp(2))/100)  

lines(x=x_base, y=dnorm(x_base, mean=mu0,
                        sd= sd0), lwd=3, col="purple")
# purple: central limit theorem
# green:  the histogram of 10,000 samples 

####
#### Exercise 4.2
####

# Repeat the above example for an exponential distribution with rate 0.5. 
#   at the same value x0 = 2:  note in this case x0 is the mean but of course
#   F(x0)  is NOT 0.5.  
#   This time I am not providing solution code, as you can just very slightly modify
#    the code above.




