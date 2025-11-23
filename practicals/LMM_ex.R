## -----------------------------------------------------------------------------
#| warning: false
#| message: false
#| code-summary: "Load libraries"

library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)
library(inlabru)     
# load some libraries to generate nice plots
library(scico)


## -----------------------------------------------------------------------------
#| code-fold: show
#| code-summary: "Simulate Data from a LM"

# set seed for reproducibility
set.seed(1234) 

# Fix the model parameters
beta = c(2,0.5)
sd_error = 0.1

# simulate the data
n = 100
x = rnorm(n)
y = beta[1] + beta[2] * x + rnorm(n, sd = sd_error)

# create the data frame object
df = data.frame(y = y, x = x)  











## -----------------------------------------------------------------------------
#| code-summary: "Fit LM in `inlabru`"
fit.lm = bru(cmp, lik)


## -----------------------------------------------------------------------------
#| code-summary: "Model summaries"
#| collapse: true
summary(fit.lm)


## -----------------------------------------------------------------------------
new_data = data.frame(x = c(df$x, runif(10)),
                      y = c(df$y, rep(NA,10)))
pred = predict(fit.lm, new_data, ~ beta_0 + beta_1,
               n.samples = 1000)


## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data and 95% credible intervals
#| echo: false
#| message: false
#| warning: false
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

pred %>% ggplot() + 
  geom_point(aes(x,y), alpha = 0.3) +
  geom_line(aes(x,mean)) +
  geom_line(aes(x, q0.025), linetype = "dashed")+
  geom_line(aes(x, q0.975), linetype = "dashed")+
  xlab("Covariate") + ylab("Observations")






## -----------------------------------------------------------------------------

pred2 = predict(fit.lm, new_data, 
               formula = ~ {
                 mu = beta_0 + beta_1
                 sigma = sqrt(1/Precision_for_the_Gaussian_observations)
                 list(q1 = qnorm(0.025, mean = mu, sd = sigma),
                      q2 =  qnorm(0.975, mean = mu, sd = sigma))},
               n.samples = 1000)
round(c(pred2$q1$mean, pred2$q2$mean),2)

