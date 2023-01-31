library(gamlss)


N <- 10000

set.seed(404)
X <- cbind(1,runif(N, 0, 100)) # Generate N uniform random numbers between 0-100
# 
# # B_mu <- c(0.1, 0.5) # Intercept and coefficient
# # B_sigma <- c(-2, 0.01) # Intercept and coefficient
# # B_nu <- c(-1,-0.01) # Intercept and coefficient
# # B_tau <- c(0.01,0.01) # Intercept and coefficient
# # 
# # mu <- c(X %*% B_mu)
# # sigma <- c(exp(X %*% B_sigma))
# # nu <- c(X %*% B_nu)
# # tau <- c(exp(X %*% B_tau))
# 
# # Y <- rBCT(N, mu=mu, sigma=sigma, nu=nu, tau=tau) %>% tibble::as_tibble()
# 
# 
# B_mu <- c(0.1, 0.5) # Intercept and coefficient
# B_sigma <- c(-1, 0.05) # Intercept and coefficient
# B_nu <- c(1,0.01) # Intercept and coefficient
# B_tau <- c(0,0.01) # Intercept and coefficient
# 
# mu <- c(X %*% B_mu)
# sigma <- c(exp(X %*% B_sigma))
# nu <- c(X %*% B_nu)
# tau <- c(exp(X %*% B_tau))

# Y_sn <- rSN1(N, mu=mu, sigma=sigma, nu=nu) %>% tibble::as_tibble()
Y_sn <- rSN1(N, mu = 5, sigma = 2, nu = 3) %>% tibble::as_tibble()
hist(Y_sn$value)
Y_sn <- tibble::as_tibble(
  list(
    dist_type="Skew Normal",
    # mu=mu,
    # sigma=sigma,
    # nu=nu,
    # tau=NA,
    
    # X=X[,2],
    value=Y_sn$value
  )
)

Y_bct <- rBCTo(N, mu = 10, sigma = 1, nu = 2, tau = 3)  %>% tibble::as_tibble()
# hist(Y_bct$value)
# Y_bct <- rBCTo(N, mu=mu, sigma=sigma, nu=nu, tau=tau)  %>% tibble::as_tibble()
Y_bct <- tibble::as_tibble(
  list(
    dist_type="Box-Cox-t",
    # mu=mu,
    # sigma=sigma,
    # nu=nu,
    # tau=tau,
    # X=X[,2],
    value=Y_bct$value
  )
)
# Y_exp <- rEXP(N, mu=mu)
Y_exp <- rEXP(N)
 # hist(Y_exp)

Y_exp <- tibble::as_tibble(
  list(
    dist_type="Exponential",
    # mu=mu,
    # sigma=NA,
    # nu=NA,
    # tau=NA,
    # X=X[,2],
    value=Y_exp
  )
)
# Y_ga <- rGA(N, mu=mu,sigma = sigma)

Y_ga <- rGA(N)
# hist(Y_ga)

Y_ga <- tibble::as_tibble(
  list(
    dist_type="Gamma",
    # mu=mu,
    # sigma=sigma,
    # nu=NA,
    # tau=NA,
    # X=X[,2],
    value=Y_ga
  )
)
Y_lgno <- rLNO(N,mu = 10, sigma = 5, nu = 0.75)
 # hist(Y_lgno)

# Y_lgno <- rLOGNO(N, mu=mu,sigma = sigma)
Y_lgno <- tibble::as_tibble(
  list(
    dist_type="Lognormal",
    # mu=mu,
    # sigma=sigma,
    # nu=NA,
    # tau=NA,
    # X=X[,2],
    value=Y_lgno
  )
)
Y_pareto <- rPARETO(N,mu = 200)

# hist(Y_pareto)
# Y_pareto <- rPARETO(N, mu=mu)
Y_pareto <- tibble::as_tibble(
  list(
    dist_type="Pareto",
    
    # mu=mu,
    # sigma=NA,
    # nu=NA,
    # tau=NA,
    # X=X[,2],
    value=Y_pareto
  )
)

Y_wei <- rWEI(N)
hist(Y_wei)
# Y_wei <- rWEI(N, mu=mu,sigma = sigma)
Y_wei <- tibble::as_tibble(
  list(
    dist_type="Weibull",
    
    # mu=mu,
    # sigma=sigma,
    # nu=NA,
    # tau=NA,
    # X=X[,2],
    value=Y_wei
  )
)

Y_gb2 <- rGB2(N, mu = 5, sigma = 2, nu = 1, tau = 5)
hist(Y_gb2)



# Setting Distributions Params for Coefficients
all_data <- rbind(
  Y_sn,
  Y_bct,
  Y_exp,
  Y_ga,
  Y_lgno,
  Y_pareto,
  Y_wei
)
sample <- sample(c(TRUE, FALSE), nrow(all_data), replace=TRUE, prob=c(0.8,0.2))

train_df <- all_data[sample,]
test_df <- all_data[!sample,]


# Plotting Data -----------------------------------------------------------
unique(train_df$dist_type)
distribution_plot <- ggplot(train_df, aes(x= value))+
  geom_histogram(aes(y=..density..),
                
                 color="black", 
                 fill="dodgerblue4")+
 
  labs(title="Distribution of Simulated Responses from Continous Positive Distributions", 
       x=""
  )+ facet_wrap(~dist_type,scales = "free")


ggplot(train_df, aes(x=X,y=y))+
  geom_point() +
  geom_line(aes(y=mu))+
  
  labs(title="Scattered Simulated Responses from Skew Normal Distribution", 
       x="X",
       y="Variable of Interest"
  )

