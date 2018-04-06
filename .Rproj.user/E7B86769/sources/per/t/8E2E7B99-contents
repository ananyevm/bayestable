#' Put results of MCMC sampling into a nicely formatted table for Latex, HTML, or plaint text
#' @description Put results of MCMC sampling into a nicely formatted table for Latex, HTML, or plaint text. It processes MCMC output and the outcome data. This function is built on top of Philip Leifeld's \emph{texreg} package.
#' @param datalist a list of coda::mcmc.list objects from different models
#' @param ylist a list of vectors of the instances of a dependent variable
#' @param yreplist (optional) a list coda::mcmc.list object with the samples of predicted values
#' @param include.nobs TRUE if number of observations should be included, FALSE otherwisr
#' @param include.rquared TRUE if quasi-bayesian r-squared should be included, FALSE otherwise
#' @param include.eff.size TRUE if effective size of MCMC chain should be included, FALSE otherwise
#' @param include.geweke if TRUE, Geweke diagnostic is calculated for all the chains, and the largest value is included
#' @param custom.coef.map list of new names for the variables, should be in the form \code{list("oldname1"="newname1", "oldname2"="newname2")}. Variables that are not in this mapping are not included. This is passed directly to \code{\link[texreg]{texreg}} function
#' @param custom.model.names a vector of model labels. This is passed directly to \code{\link[texreg]{texreg}} function
#' @param HPDI if TRUE, Highest Probability Density Intervals are shown instead of standard errors. Default is TRUE
#' @param HPDI.prob probability for HPD intervals. Defauld is 0.95
#' @param caption a caption for the table. This is passed directly to \code{\link[texreg]{texreg}} function
#' @param output output format: can be \emph{latex}, \emph{html}, or \emph{word}. Default is \emph{latex}
#' @return a character string that contains Late, HTML, or plain text for the rable
#' @examples
#' library(rjags)
#' library(coda)
#' library(texreg)
#' library(bayestable)
#'
#' ## Generating some fake data
#' N<-100
#' x1<-rnorm(N)
#' x2<-rnorm(N)
#' beta<-c(0.3,-0.3)
#' y<- 0.1 + beta[1]*x1 + beta[2]*x2+rnorm(N)
#'
#' ## Define three JAGS models
#' code1<-"
#'   model{
#'     for (i in 1:N){
#'       y[i] ~ dnorm(mu[i], sigma)
#'       mu[i]<-a
#'     }
#'    a ~ dnorm(0, 0.001)
#'    sigma <- pow(tau, -2)
#'    tau ~ dunif(0,100)
#' }"
#'
#' code2<-"
#'  model{
#'    for (i in 1:N){
#'      y[i] ~ dnorm(mu[i], sigma)
#'      mu[i]<-a+beta1*x1[i]
#'    }
#'    a ~ dnorm(0, 0.001)
#'    beta1 ~ dnorm(0,0.001)
#'    sigma <- pow(tau, -2)
#'    tau ~ dunif(0,100)
#' }"
#'
#' code3<-"
#'   model{
#'    for (i in 1:N){
#'     y[i] ~ dnorm(mu[i], sigma)
#'     mu[i]<-a+beta1*x1[i]+beta2*x2[i]
#'   }
#'   a ~ dnorm(0, 0.001)
#'   beta1 ~ dnorm(0,0.001)
#'   beta2 ~ dnorm(0,0.001)
#'   sigma <- pow(tau, -2)
#'   tau ~ dunif(0,100)
#' }"
#' ## connecting to the models
#' model1.spec<-textConnection(code1)
#' model2.spec<-textConnection(code2)
#' model3.spec<-textConnection(code3)
#'
#' ## compiling the code into JAGS models
#' jags1<-jags.model(model1.spec, data = list('N'=N, 'y'=y), quiet = T)
#' jags2<-jags.model(model2.spec, data = list('x1'=x1,'N'=N, 'y'=y), quiet = T)
#' jags3<-jags.model(model3.spec, data = list('x1'=x1,'x2'=x2,'N'=N, 'y'=y), quiet = T)
#'
#' ## Sampling regression parameters
#' samples1<-coda.samples(jags1, variable.names = c("a"), n.iter=1000,nchain=4)
#' samples2<-coda.samples(jags2, variable.names = c("a", "beta1"), n.iter=1000,nchain=4)
#' samples3<-coda.samples(jags3, variable.names = c("a", "beta1", "beta2"),n.iter=1000,nchain=4)
#'
#' ## sampling fitted values
#' samples1.rep<-coda.samples(jags1, variable.names = c("mu"), n.iter=1000,nchain=4)
#' samples2.rep<-coda.samples(jags2, variable.names = c("mu"), n.iter=1000,nchain=4)
#' samples3.rep<-coda.samples(jags3, variable.names = c("mu"), n.iter=1000,nchain=4)
#'
#'#' ## creating a list of samples from three different models
#' datalist<-list(samples1, samples2, samples3)
#'
#' ## creating a list of outcomes for different models (here we used the same set of outcomes)
#' ylist<-list(y,y,y)
#'
#' ## creating a list of samples of fitted values from different models
#' yreplist<-list(samples1.rep, samples2.rep, samples3.rep)
#'
#' ## generating table
#' bayes.table(datalist, ylist, yreplist,
#'            custom.coef.map = list("a"="Intercept",
#'                                    "beta1"="GDP",
#'                                    "beta2"="Polity"),
#'            include.rsquared = T, HPDI.prob = 0.97)
#'
#'
bayes.table<-function(datalist, ylist, yreplist=NULL,
                      include.nobs=TRUE,
                      include.rsquared = FALSE,
                      include.eff.size = TRUE,
                      include.geweke=TRUE,
                      custom.coef.map=NULL,
                      custom.model.names=NULL,
                      HPDI=TRUE,
                      HPDI.prob=0.95,
                      output="latex",
                      caption=""){

  ## Making sure that valid inputs are supplied

  assertthat::assert_that(!is.null(datalist))
  assertthat::assert_that(assertthat::not_empty(datalist))
  assertthat::assert_that(assertthat::not_empty(ylist))
  assertthat::assert_that(assertthat::are_equal(length(datalist), length(ylist)))
  if (!is.null(yreplist)){
    assertthat::assert_that(assertthat::not_empty(yreplist))
  }
  assertthat::assert_that(HPDI.prob>0 & HPDI.prob<1)
  for (i in 1:length(ylist)){
    assertthat::assert_that(assertthat::noNA(ylist[[i]]))
  }

  ## creating mcmcfit objects out of supplied data
  mcmc.datalist<-list()

  for (i in 1:length(datalist)){
    ppredict<-NULL
    if(!is.null(yreplist)){
      ppredict<-yreplist[[i]]
    }
    mcmc.datalist[[i]]<-.create.mcmcfit(datalist[[i]],
                                       ylist[[i]], ppredict)
  }

  ## creating a list of custom HPDI intervals
  low.hpdi.list<-0
  high.hpdi.list<-0
  if (HPDI==TRUE){
    low.hpdi.list<-list()
    high.hpdi.list<-list()
    for(i in 1:length(datalist)){
      low.hpdi.list[[i]]<-coda::HPDinterval(mcmc.datalist[[i]]$samples, prob = HPDI.prob)[[1]][,1]
      high.hpdi.list[[i]]<-coda::HPDinterval(mcmc.datalist[[i]]$samples, prob = HPDI.prob)[[1]][,2]
    }
  }
  if(output=="latex"){
    p1<-texreg::texreg(mcmc.datalist, include.geweke=include.geweke,
           include.rsquared=include.rsquared,
           include.nobs=include.nobs, custom.note = "",
           custom.coef.map = custom.coef.map,
           include.eff.size = include.eff.size,
           custom.model.names = custom.model.names,
           ci.force = HPDI, override.ci.low = low.hpdi.list,
           override.ci.up = high.hpdi.list, caption = caption)
    ## for some reason, texreg can sometimes put significance stars
    ## even if it is asked not to do it, so we need to remove
    ## the stars after the table has been created
    gsub("^{*}","", p1, fixed=T)
  }else if (output=="html"){
    p1<-texreg::htmlreg(mcmc.datalist, include.geweke=include.geweke,
            include.rsquared=include.rsquared,
            include.nobs=include.nobs, custom.note = "",
            custom.coef.map = custom.coef.map,
            include.eff.size = include.eff.size,
            custom.model.names = custom.model.names,
            ci.force = HPDI, override.ci.low = low.hpdi.list,
            override.ci.up = high.hpdi.list)
    p1<-gsub("^{*}","", p1, fixed=T)
    gsub("*","", p1, fixed=T)
  }else if (output=="word"){
    p1<-texreg::screenreg(mcmc.datalist, include.geweke=include.geweke,
              include.rsquared=include.rsquared,
              include.nobs=include.nobs, custom.note = "",
              custom.coef.map = custom.coef.map,
              include.eff.size = include.eff.size,
              custom.model.names = custom.model.names,
              ci.force = HPDI, override.ci.low = low.hpdi.list,
              override.ci.up = high.hpdi.list)
    p1<-gsub("^{*}","", p1, fixed=T)
    gsub("*","", p1, fixed=T)

  }else{
    print("Error: invalid output format supplied.")
  }
}
