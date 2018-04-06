# Bayes Table: Summarizing and Presenting Results of MCMC Inference in LaTex/HTML
It is customary for empirical researchers to estimate many models and then show their results side-by-side in a table. This package provides this capability for the models, where inference is done through MCMC. Buily on the top of [Philip Leifield's](https://www.philipleifeld.com/) [texreg](https://cran.r-project.org/web/packages/texreg/index.html) package, `bayestable` provides a way to presents summaries of several models in one LaTex/HTML/Word table.

## Example
Let's begin with loading packages and generating some fake data:
```{r}
library(rjags)
library(bayestable)
```

