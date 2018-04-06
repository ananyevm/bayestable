# Bayes Table: Summarizing and Presenting Results of MCMC Inference in LaTex/HTML
It is customary for empirical researchers to estimate many models and then show their results side-by-side in a table. This package provides this capability for the models, where inference is done through MCMC. Buily on the top of [Philip Leifield's](https://www.philipleifeld.com/) [texreg](https://cran.r-project.org/web/packages/texreg/index.html) package, `bayestable` provides a way to presents summaries of several models in one LaTex/HTML/Word table.

## Example
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

model1.spec<-textConnection(code1)
model2.spec<-textConnection(code2)
model3.spec<-textConnection(code3)

jags1<-jags.model(model1.spec, data = list('N' = N, 'y' = y))
jags2<-jags.model(model2.spec, data = list('x1' = x1,'N'=N, 'y'=y))
jags3<-jags.model(model3.spec, data = list('x1' = x1,'x2' = x2,'N' = N, 'y' = y))
```


