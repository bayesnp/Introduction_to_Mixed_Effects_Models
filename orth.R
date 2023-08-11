getwd()

data(Orthodont, package = "nlme")   # include Orthodont data

library('ggplot2')
# take a look at the growth curves per child, each child
# has a specific slope and intercept that may be 
# different from the overall slope and intercept.
g_orth <- ggplot(data = Orthodont, aes(x = age, y = distance)) +
  # geom_line(aes(y = distance, x = age), col = "blue") +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  geom_point(size = 1.0, shape = 21, fill = 'white') +
  facet_wrap(Subject ~ ., ncol = 3) +
  ggtitle('Orthodont Data')
g_orth

# simple regression, no hierarchical structure
orth_lm <- lm(distance ~ age, data = Orthodont)
summary(orth_lm)

# make a local copy of Orthodont, easier to make changes to it.
orth <- Orthodont
summary( lm(distance ~ I(age - 11), data = orth) )

# centering by average age to make the (Intercept) more interpretable
orth$y_lm <- predict(lm(distance ~ I(age - 11), data = orth))

# lme4, not Bayesian, but it gives you some intuition about models
library(lme4)
fitted_lms <- lmList( distance ~ I(age - 11) | Subject, data = orth)
coefs <- coef(fitted_lms)
grp <- substring(row.names(coef(fitted_lms)), first = 1, last = 1)
par.old <- par(mfcol = c(2, 1)) # save a copy of old graphics params
boxplot(list(intM=coefs[grp=="M", "(Intercept)"], 
             intF=coefs[grp=="F", "(Intercept)"]) )
boxplot(list(slpM=coefs[grp=="M", "I(age - 11)"], 
             slpF=coefs[grp=="F","I(age - 11)"]) )
par(par.old)          # restore original old par()
# 'varying intercepts' model: each Subject has a unique intercept
orth_varyint <- lmer(distance ~ I(age - 11) + (1 | Subject), data = orth)

# 'varying intercepts, varying slopes' model
orth_vintslope <- lmer(distance ~ I(age - 11) + (1 + I(age - 11) | Subject), data = orth)

# Next, different models make different predictions.
# Later we plot these predictions against the observed data to 
# visually inspect which model fits the data best.
orth$y_vint <- predict(orth_varyint)
orth$y_vintslope <- predict(orth_vintslope)
anova(orth_varyint, orth_vintslope)   # AIC shows no big difference

# What if we add differences between boys and girls, does it fit better?
orth_agesex <- lmer(distance ~ I(age - 11)*Sex + (1 + I(age - 11) | Subject), data = orth)
anova(orth_varyint, orth_vintslope, orth_agesex)  # it does

orth$y_agesex <- predict(orth_agesex)

library('reshape')

orth_long.df <- reshape(data = orth, varying = list(c("distance", 
  "y_lm", "y_vint", "y_vintslope", "y_agesex")), 
  v.names="y", direction = "long")

orth_long.df$time <- factor(orth_long.df$time, levels = 1:5, 
    labels = c("obs", "lm", "vint","vintslope", "agesex"))

g_orth <- ggplot(data = orth_long.df, 
  aes(x =age, y=y, group = time, color = time)) + 
  geom_line() + facet_wrap(Subject ~ ., ncol = 5) + 
  scale_color_manual(values=c("black", "red","blue","green","pink"))
g_orth

svg('g_orth.svg', height = 8, width = 10)
g_orth
graphics.off()

# Bayesian HLM using library(rstanarm)
library("rstanarm")
options(mc.cores = parallel::detectCores())

m_orth <- stan_lmer(distance ~ I(age - 11)*Sex + (1 | Subject ),  
   data = orth,
   adapt_delta = 0.995, refresh = 0, 
   iter = 5000, chains = 4,
   prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
   prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
   prior_covariance = decov( regularization = 1), # jointly uniform
   # See https://mc-stan.org/rstanarm/articles/glmer.html
   open_progress = FALSE)

summary(m_orth, prob = c(0.025, 0.50, 0.975), digits = 3)
prior_summary(m_orth)
m_orth_mat <- as.matrix(m_orth)             # into MCMC matrix
t.names <- dimnames(m_orth_mat)$parameters  # colnames
####
