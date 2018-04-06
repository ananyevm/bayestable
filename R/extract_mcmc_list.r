## While this is in general not a good practice to put "library" in a package,
## we load texreg package because the generic function "extract" is defined
## there

library(texreg)


# Defining a class that would contain MCMC samples or parameters,
# outcomes, and (optionally) MCMC samples of predicted values.
mcmcfit<-setClass("mcmcfit",package = "bayestable")


# function that creats mcmcfit object out of coda mcmc.list object of
# MCMC samples of parameters, a vector of outcomes, and mcmc.list objet
# that contains an MCMC sample of predicted values
.create.mcmcfit<-function (mcmc, y, ppredict=NULL){
  mcmcfit<-list("samples"=mcmc,
                "outcomes"=y,
                "ppredict"=ppredict)
  class(mcmcfit)<-"mcmcfit"
  return(mcmcfit)
}


# Method "extract" for mcmcfit object. This is the main method that
# calculates all the quantities to be displayed in the table

setMethod("extract", "mcmcfit",
          function(model, include.nobs = TRUE,
                   include.rsquared = TRUE,
                   include.eff.size = TRUE,
                   include.geweke = FALSE,...){
            samples<-model$samples
            s<-summary(samples)
            names<-colnames(samples[[1]])
            if(class(s$statistics)=="matrix"){
              co<-s$statistics[,1]
              se<-s$statistics[,2]
            }else if (class(s$statistics)=="numeric"){
              co<-s$statistics[1]
              se<-s$statistics[2]
            }
            pval<-numeric(0)

            gof<-numeric()
            gof.names<-character()
            gof.decimal<-logical()

            mu.rep<-numeric(0)

            if(!is.null(model$ppredict)){
              mu.rep<-calc.predict.medians(model$ppredict)
            }


            if(include.nobs==TRUE){
              n<-length(model$outcomes)
              gof<-c(gof, n)
              gof.names<-c(gof.names, "Num. obs.")
              gof.decimal<-c(gof.decimal, FALSE)
            }
            if(include.rsquared==TRUE){
              if(is.null(model$ppredict)){
                print ("For calculating R-squared your should supply
                       a matrix of draws of fitted values")
              }else{
                rsq<-calc.r.sq(y, mu.rep)
                gof<-c(gof, rsq)
                gof.names<-c(gof.names, "R$^2$")
                gof.decimal<-c(gof.decimal, TRUE)
              }
            }
            if(include.eff.size==TRUE){
              eff.size = min(coda::effectiveSize(model$samples))
              gof<-c(gof, eff.size)
              gof.names<-c(gof.names, "Eff. Size")
              gof.decimal<-c(gof.decimal, TRUE)
            }

            if(include.geweke==TRUE){
              geweke = calc.geweke(model$samples)
              gof<-c(gof, geweke)
              gof.names<-c(gof.names, "Geweke Diag.")
              gof.decimal<-c(gof.decimal, TRUE)
            }

            tr<-createTexreg(
              coef.names = names,
              coef = co,
              se = se,
              pvalues = pval,
              gof.names = gof.names,
              gof = gof,
              gof.decimal = gof.decimal)

            return(tr)
          })
