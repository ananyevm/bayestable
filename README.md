# Bayes Table: Summarizing and Presenting Results of MCMC Inference in Plain Text/LaTex/HTML
It is customary for empirical researchers to estimate many models and then show their results side-by-side in a table. This package provides this capability for the models, where inference is done through MCMC. Built on top of [Philip Leifield's](https://www.philipleifeld.com/) [texreg](https://cran.r-project.org/web/packages/texreg/index.html) package, `bayestable` provides a way to present summaries of several models in one LaTex/HTML/Word table.

## Installation
You can install the package from GitHub using `devtools`:
```{r}
devtools::install_github("ananyevm/bayestable")
```
This is still an early development version, so expect some adventures.

## Example
### Context
Here, we consider a simple use case. Let's begin with loading packages and generating some fake data:
```{r}
## Loading packages
library(rjags)
library(bayestable)

## Generating fake data
N <- 100
x1 <- rnorm(N)
x2 <- rnorm(N)
beta <- c(0.3,-0.3)
y <- 0.1+beta[1]*x1+beta[2]*x2+rnorm(N)

```
Here we have a simple linear data generating process. Our goal is to estimate three models: an intercept-only model, a model with predictor `x1`, and a model with both `x1` and `x2`.
Let's define these models with JAGS:

```{r}
## Intercept-only model 
code1<-"
  model{
    for (i in 1:N){
      y[i] ~ dnorm(mu[i], sigma)
      mu[i] <- a
    }
   a ~ dnorm(0, 0.001)
   sigma <- pow(tau, -2)
   tau ~ dunif(0,100)
}"

## Bivariate linear model
code2<-"
 model{
   for (i in 1:N){
     y[i] ~ dnorm(mu[i], sigma)
     mu[i] <- a+beta1*x1[i]
   }
   a ~ dnorm(0, 0.001)
   beta1 ~ dnorm(0,0.001)
   sigma <- pow(tau, -2)
   tau ~ dunif(0,100)
}"

## Linear model with two predictors
code3<-"
  model{
   for (i in 1:N){
    y[i] ~ dnorm(mu[i], sigma)
    mu[i]<-a+beta1*x1[i]+beta2*x2[i]
  }
  a ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0,0.001)
  beta2 ~ dnorm(0,0.001)
  sigma <- pow(tau, -2)
  tau ~ dunif(0,100)
}"
```

Now, let's connect and compile the models

```{r}
## Connecting and compiling

model1.spec <- textConnection(code1)
model2.spec <- textConnection(code2)
model3.spec <- textConnection(code3)

jags1 <- jags.model(model1.spec, data = list('N' = N, 'y' = y))
jags2 <- jags.model(model2.spec, data = list('x1' = x1,'N' = N, 'y' = y))
jags3 <- jags.model(model3.spec, data = list('x1' = x1,'x2' = x2,'N' = N, 'y' = y))
```

After the models are compiled, we can sample the coefficients (`a`, `beta1`, and `beta2` -- depending on the model), and fitted values `mu`:

```{r}
## Sampling coefficients
samples1 <- coda.samples(jags1, variable.names = c("a"), n.iter=1000,nchain=4)
samples2 <- coda.samples(jags2, variable.names = c("a", "beta1"), n.iter=1000,nchain=4)
samples3 <- coda.samples(jags3, variable.names = c("a", "beta1", "beta2"),n.iter=1000,nchain=4)

## sampling fitted values
samples1.rep <- coda.samples(jags1, variable.names = c("mu"), n.iter=1000,nchain=4)
samples2.rep <- coda.samples(jags2, variable.names = c("mu"), n.iter=1000,nchain=4)
samples3.rep <- coda.samples(jags3, variable.names = c("mu"), n.iter=1000,nchain=4)
```

### Generating Default Table
Now, let's show the results of three models in one table using function `bayes.table` 

First, we need to prepare lists to pass to the function
```{r}
### List of the parameter samples
datalist<-list(samples1, samples2, samples3)

## List of outcomes for different models (here we used the same set of outcomes)
ylist<-list(y,y,y)
``` 

Let's generate a table with default settings (only generating plain text table instead of LaTex table):
```{r}
bayes.table(datalist, ylist, output="word")
```

As a result, we get a table that looks like this:
```
==========================================================
              Model 1        Model 2        Model 3       
----------------------------------------------------------
a                0.18           0.16          0.11        
              [-0.04; 0.39]  [-0.05; 0.35]  [-0.09;  0.31]
beta1                           0.43         0.43       
                             [ 0.23; 0.63]  [ 0.23;  0.61]
beta2                                        -0.30       
                                            [-0.50; -0.11]
----------------------------------------------------------
Num. obs.      100            100           100           
Eff. Size     1000.00        1000.00        814.71        
Geweke Diag.     1.04           2.02          1.01        
==========================================================
```

Means of posterior distributions are shown as point estimates, and 95-percent Highest Probability Density intervals are shown below the point estimates. Also, for every model, the table shows number of observations, [effective sample size of MCMC draws](https://www.johndcook.com/blog/2017/06/27/effective-sample-size-for-mcmc/), and maximum absolute value of [Geweke diagnostic](http://ugrad.stat.ubc.ca/R/library/coda/html/geweke.diag.html).

### Customizing the Table

The package provides several options to customize the table. First, if we include the list of the draws of fitted values, we can calculate R-squared for each of the models. Secondly, we can give our variables meaningful names. Thirdly, we can show standard errors of posterior distributions of the coefficients instead of HPD intervals.

The code would like like this:
```

bayes.table(datalist, ylist, yreplist, HPDI=F,
            custom.coef.map = list("a"="Intercept",
                                   "beta1"="Ruggedness",
                                   "beta2"="GDP"), 
			include.rsquared = T, 
			output="word")
```

The output would look like:

```
=========================================
              Model 1   Model 2   Model 3
-----------------------------------------
Intercept        0.18      0.16     0.11 
                (0.11)    (0.11)   (0.10)
Ruggedness                 0.43     0.43 
                          (0.11)   (0.10)
GDP                                -0.30 
                                   (0.10)
-----------------------------------------
Num. obs.      100       100      100    
R^2              0.00      0.16     0.23 
Eff. Size     1000.00   1000.00   814.71 
Geweke Diag.     1.04      2.02     1.01 
=========================================
```

Other things that can be customized: inclusion of any of the goodness-of-fit and convergence metric, names of the models, caption, probability for the HPD intervals. Please consult the manual pages for options.

Please feel free to report any issues or request features.