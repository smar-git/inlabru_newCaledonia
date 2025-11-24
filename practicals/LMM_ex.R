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


## -----------------------------------------------------------------------------
#| eval: false
# ?iris


## -----------------------------------------------------------------------------
data("iris")

iris %>% ggplot() + geom_point(aes(Sepal.Length, Petal.Length, color= Species)) +
  facet_wrap(.~Species)


## -----------------------------------------------------------------------------
mod1 = lm(Petal.Length ~ Species, data  = iris)
summary(mod1)




## -----------------------------------------------------------------------------

cmp = ~ -1 +  cov(Species, model = "iid", fixed = T, initial = log(0.001))
lik = bru_obs(formula =Petal.Length ~ .,
              data = iris)
fit1b = bru(cmp, lik)


## -----------------------------------------------------------------------------
fit1b$summary.random$cov




## -----------------------------------------------------------------------------
mod2 = lm(Petal.Length ~ Species:Sepal.Length, data = iris)
mod2




## -----------------------------------------------------------------------------

cmp = ~Intercept(1)  + 
  slope(Species, Sepal.Length, model = "iid", fixed = T, initial = log(0.001))

lik = bru_obs(formula =Petal.Length ~ .,
              data = iris)
fit2b = bru(cmp, lik)






## -----------------------------------------------------------------------------
#| code-summary: "Simulate data from a LMM"
#|
set.seed(12)
beta = c(1.5,1)
sd_error = 1
tau_group = 1

n = 100
n.groups = 5
x = rnorm(n)
v = rnorm(n.groups, sd = tau_group^{-1/2})
y = beta[1] + beta[2] * x + rnorm(n, sd = sd_error) +
  rep(v, each = 20)

df = data.frame(y = y, x = x, j = rep(1:5, each = 20))


## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-cap: Data for the linear mixed model example with 5 groups
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

ggplot(df) +
  geom_point(aes(x = x, colour = factor(j), y = y)) +
  theme_classic() +
  scale_colour_discrete("Group")



## -----------------------------------------------------------------------------
# Define model components
cmp =  ~ -1 + beta_0(1) + beta_1(x, model = "linear") +
  u(j, model = "iid")


## -----------------------------------------------------------------------------
# Construct likelihood
lik =  bru_obs(formula = y ~.,
            family = "gaussian",
            data = df)


## -----------------------------------------------------------------------------
#| collapse: true
#| code-summary: "Fit a LMM in inlabru"
fit = bru(cmp, lik)
summary(fit)


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "LMM fitted values"
#| fig-align: center
#| fig-width: 4
#| fig-height: 4

# New data
xpred = seq(range(x)[1], range(x)[2], length.out = 100)
j = 1:n.groups
pred_data = expand.grid(x = xpred, j = j)
pred = predict(fit, pred_data, formula = ~ beta_0 + beta_1 + u)


pred %>%
  ggplot(aes(x=x,y=mean,color=factor(j)))+
  geom_line()+
  geom_ribbon(aes(x,ymin = q0.025, ymax= q0.975,fill=factor(j)), alpha = 0.5) +
  geom_point(data=df,aes(x=x,y=y,colour=factor(j)))+
  facet_wrap(~j)


