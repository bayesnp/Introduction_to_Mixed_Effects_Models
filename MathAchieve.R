#
# Varying-intercepts, varying-slopes model using the MathAchieve dataset
# As explained in the chapter by Fox and Weisberg (2014), 
# https://socialsciences.mcmaster.ca/jfox/Courses/soc761/Appendix-Mixed-Models.pdf
# last accessed March 13, 2023
#

# First, make the MathAchieve and MathAchSchool datasets available
data(MathAchieve, package = "nlme")    # individual students
data(MathAchSchool, package = "nlme")  # school-level data
head(MathAchieve, 10)
dim(MathAchieve)
head(MathAchSchool, 10)
dim(MathAchSchool)

# Calculate the average SES scores for each school
mses <- with(MathAchieve, tapply(SES, School, mean))
mses[as.character(MathAchSchool$School[1:10])] # for first 10 schools

# Plot a few schools to get the first impressions
Bryk <- as.data.frame(MathAchieve[, c("School", "SES", "MathAch")])
names(Bryk) <- tolower(names(Bryk))
set.seed(12345) # for reproducibility
(sample20 <- sort(sample(nrow(Bryk), 20))) # 20 randomly sampled students

Bryk[sample20, ]

sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool)
Bryk <- within(Bryk,{ 
	       meanses <- as.vector(mses[as.character(school)]) 
	       cses <- ses - meanses
	       sector <- sector[as.character(school)] 
               })
Bryk[sample20, ]

# extract a random sample of 20 Catholic schools and Public schools
cat <- with(Bryk, sample(unique(school[sector == "Catholic"]), 20))
Cat.20 <- Bryk[is.element(Bryk$school, cat), ]
dim(Cat.20)
pub <- with(Bryk, sample(unique(school[sector == "Public"]), 20))
Pub.20 <- Bryk[is.element(Bryk$school, pub), ]
dim(Pub.20)


library(lattice) # for Trellis graphics
trellis.device(color=FALSE)
xyplot(mathach ~ cses | school, data=Cat.20, main="Catholic", 
       panel=function(x, y){ 
	       panel.xyplot(x, y)
	       panel.loess(x, y, span=1)
	       panel.lmline(x, y, lty=2)
       } ) 
xyplot(mathach ~ cses | school, data=Pub.20, main="Public", 
       panel=function(x, y){ 
	      panel.xyplot(x, y)
       	      panel.loess(x, y, span=1)
	      panel.lmline(x, y, lty=2) 
       } )


library("nlme")
cat.list <- lmList(mathach ~ cses | school, subset = sector=="Catholic",
		   data = Bryk)
pub.list <- lmList(mathach ~ cses | school, subset = sector=="Public",
		   data = Bryk)
plot(intervals(cat.list), main="Catholic")
plot(intervals(pub.list), main="Public")

cat.coef <- coef(cat.list)
head(cat.coef, 10)          # schools have different int & slopes
pub.coef <- coef(pub.list)
head(pub.coef, 10)

old <- par(mfrow=c(1, 2))
boxplot(cat.coef[, 1], pub.coef[, 1], main="Intercepts",
	names=c("Catholic", "Public"))
boxplot(cat.coef[, 2], pub.coef[, 2], main="Slopes", 
	names=c("Catholic", "Public"))
par(old) # restore

# Try preliminary models
Bryk$sector <- factor(Bryk$sector, levels=c("Public", "Catholic"))
contrasts(Bryk$sector)
library(lme4)
bryk.lmer.1 <- lmer(mathach ~ meanses*cses + sector*cses +
	  ( 1 + cses | school), data=Bryk)
summary(bryk.lmer.1)

bryk.lmer.2 <- lmer(mathach ~ meanses*cses + sector*cses +
	  ( 1 | school), data=Bryk) # omitting random slopes of cses 
anova(bryk.lmer.1, bryk.lmer.2)

bryk.lmer.3 <- lmer(mathach ~ meanses*cses + sector*cses +
	  ( -1 + cses | school), data=Bryk)# omitting random intercept
anova(bryk.lmer.1, bryk.lmer.3)

library("rstanarm")
options(mc.cores = parallel::detectCores())

m_math <- stan_lmer(mathach ~ meanses*cses + sector*cses + 
	(1 + cses | school ),  
   data = Bryk,
   adapt_delta = 0.995, refresh = 0, 
   iter = 5000, chains = 4,
   prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
   prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
   prior_covariance = decov( regularization = 1), # jointly uniform
   # See https://mc-stan.org/rstanarm/articles/glmer.html
   open_progress = FALSE)

summary(m_math)
