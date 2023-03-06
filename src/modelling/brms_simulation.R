library(brms)
library(tidyr)
library(ggplot2)
library(tidybayes)
library(ggdist)

set.seed(1234) #for reproducability

nG <- 20 #number of groups 
nJ <- 10 #cluster size (number of individuals within each group)
# Coefficient for level 2 explanatory variable 

W1 <- 2 #level 2 coeff
# Coefficient for level 1 explanatory variable
X1 <- 3 #level 1 coeff

# Generate 20 random numbers, 
# these, will serve as explanatory variables for each of the levels
tmp2 <- rnorm(nG)
l2 <- rep(tmp2, each = nJ) #all units in l2 have the same value
group <- gl(nG, k = nJ) #creating cluster variable


tmp2 <- rnorm(nG) #error term for level 2
err2 <- rep(tmp2, each = nJ) #all units in l2 have the same value

l1 <- rnorm(nG * nJ) #total sample size is nG * nJ
err1 <- rnorm(nG * nJ) #level 1 

#putting it all together
y <- W1 * l2 + X1 * l1 + err2 + err1
dat <- data.frame(y, group, l2, err2,l1, err1)

ggplot(dat, aes(y=y, x=l2))+
  geom_point()+
  geom_vline(aes(xintercept=l2))

ggplot(dat, aes(y=y, x=l1))+
  geom_point()+
  geom_vline(aes(xintercept=l1))

synth_data <- brms::rskew_normal(n = 1000,mu = 5,sigma = 1,alpha = 5)
plot(synth_data)





mlm0 <- brm(y ~ l2 + l1+(1|group), data = dat) #unconditional

summary(mlm0)
plot(mlm0)

brms::conditional_effects(mlm0)


res <- residuals(mlm0,summary = T)
head(res)
# vpc <- function(model, params){
#   
#   draws_df <- results <- as_draws_df(model)[params]
#   
#   vpcs <- list()
#   for (param in params){
#     other_params <- params[params!=param]
#     
#     vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
#   }
#   vpcs <- vpcs %>% as_tibble()
#   
#   return(vpcs)
# }
# 
# get_variables(mlm0)
# vpc(mlm0, params=c("sd_group__Intercept","sigma"))

