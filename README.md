# Introduction_to_Mixed_Effects_Models

Mixed-effects models (MIXED) are different from methods like Generalized Linear Models (GLM) and repeated-measures ANOVA. The main differences are that MIXED handles correlated data and unequal variances.

Correlated data are very common in behavioral research
  - Repeated measurements of survey respondents or experimental subjects (e.g., https://stats.oarc.ucla.edu/sas/library/sas-libraryrepeated-measures-anova-using-sas-proc-glm/)
  - MIXED extends repeated measures ANOVA in GLM 
	1. Allow an unequal number of repetitions
	2. Complex nesting hierarchy, e.g, students nested in schools and then nested in a school district

Today, we learn by going over examples:
  - Orthodontic growth curves in children
  - Bryk & Raudenbush (2002) Math Achievement (this is a classic example)
  - Ahles et al. (2014): changes in neurocognitive performance after cancer

Goals: 
  - Not enough time to cover everything about mixed-effects models
  - Just a couple of examples
  - Give you the right kind of intuition about MIXED
  - So that you can learn more on your own
  - Numerous other applications 
  - Warning: don't just blindly apply syntax recipes, don't use point and click in SPSS, always write your own syntax

Notes:
  - The same methods are called different names in different fields
	1. Mixed-effects models in Statistics (Laird & Ware, 1982)
  	2. Hierarchical Linear Models (HLM) in Psychology, popularized by Bryk & Raudenbush
   	3. Multilevel/hierarchical models (Gelman & Hill, 2007)
