
library("gamlss")
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)
library(quantreg)


# Creating Sample Data ----------------------------------------------------
N <- 1000

set.seed(404)
X <- cbind(1,runif(N, 0, 100)) # Generate N uniform random numbers between 0-100

# B_mu <- c(0.1, 0.5) # Intercept and coefficient
# B_sigma <- c(-2, 0.01) # Intercept and coefficient
# B_nu <- c(-1,-0.01) # Intercept and coefficient
# B_tau <- c(0.01,0.01) # Intercept and coefficient
# 
# mu <- c(X %*% B_mu)
# sigma <- c(exp(X %*% B_sigma))
# nu <- c(X %*% B_nu)
# tau <- c(exp(X %*% B_tau))

# Y <- rBCT(N, mu=mu, sigma=sigma, nu=nu, tau=tau) %>% tibble::as_tibble()


B_mu <- c(0.1, 0.5) # Intercept and coefficient
B_sigma <- c(-1, 0.05) # Intercept and coefficient
B_nu <- c(1,0.01) # Intercept and coefficient

mu <- c(X %*% B_mu)
sigma <- c(exp(X %*% B_sigma))
nu <- c(X %*% B_nu)

Y <- rSN1(N, mu=mu, sigma=sigma, nu=nu) %>% tibble::as_tibble()

# Setting Distributions Params for Coefficients
all_data <- tibble::as_tibble(
  list(
    mu=mu,
    sigma=sigma,
    nu=nu,
    tau=tau,
    X=X[,2],
    y=Y$value
  )
)

sample <- sample(c(TRUE, FALSE), nrow(all_data), replace=TRUE, prob=c(0.8,0.2))

train_df <- all_data[sample,]
test_df <- all_data[!sample,]


# Plotting Data -----------------------------------------------------------

distribution_plot <- ggplot(train_df, aes(x= y))+
  geom_histogram(aes(y=..density..),
                 binwidth = 10, 
                 center=5,
                 color="black", 
                 fill="dodgerblue4")+
  geom_density(alpha=0.3, 
               color="black", 
               fill="grey",
               bw=10)+
  
  labs(title="Distribution of Simulated Responses from Skew Normal Distribution", 
       x="Variable of Interest"
  )


ggplot(train_df, aes(x=X,y=y))+
  geom_point() +
  geom_line(aes(y=mu))+
  
  labs(title="Scattered Simulated Responses from Skew Normal Distribution", 
       x="X",
       y="Variable of Interest"
  )



ggplot(Y, aes(x=value))+
  geom_histogram()


# flat_model <- gamlss(y~1, 
#                      sigma.formula = ~1, 
#                      nu.formula = ~1, 
#                      tau.formula = ~1 ,
#                      data =train_df,  
#                      family=BCT(),
#                      control = gamlss.control(n.cyc = 200)
# )
# 
# location <- gamlss(y~X, 
#                    sigma.formula = ~1, 
#                    nu.formula = ~1, 
#                    tau.formula = ~1 ,
#                    data =train_df,  
#                    family=BCT(),
#                    control = gamlss.control(n.cyc = 200)
# )
# 
# location_scale <- gamlss(y~X, 
#                          sigma.formula =~X, 
#                          nu.formula = ~1, 
#                          tau.formula = ~1 ,
#                          data =train_df,  
#                          family=BCT(),
#                          control = gamlss.control(n.cyc = 200)
# )
# 
# location_scale_skew <- gamlss(y~X, 
#                               sigma.formula =~X, 
#                               nu.formula = ~X, 
#                               tau.formula = ~1 ,
#                               data =train_df,  
#                               family=BCT(),
#                               control = gamlss.control(n.cyc = 200)
# )
# 
# location_scale_skew_kurtosis <- gamlss(y~X, 
#                                        sigma.formula =~X, 
#                                        nu.formula = ~X, 
#                                        tau.formula = ~X ,
#                                        data =train_df,  
#                                        family=BCT(),
#                                        control = gamlss.control(n.cyc = 200)
# )
flat_model <- gamlss(y~1, 
                     sigma.formula = ~1, 
                     nu.formula = ~1, 
                     data =train_df,  
                     family=SN1(),
                     control = gamlss.control(n.cyc = 200)
)

location <- gamlss(y~X, 
                   sigma.formula = ~1, 
                   nu.formula = ~1, 
                   data =train_df,  
                   family=SN1(),
                   control = gamlss.control(n.cyc = 200)
)

location_scale <- gamlss(y~X, 
                         sigma.formula =~X, 
                         nu.formula = ~1, 
                         data =train_df,  
                         family=SN1(),
                         control = gamlss.control(n.cyc = 200)
)

location_scale_skew <- gamlss(y~X, 
                              sigma.formula =~X, 
                              nu.formula = ~X, 
                              data =train_df,  
                              family=SN1(),
                              control = gamlss.control(n.cyc = 200)
)



quantreg_results <- quantreg::rq(y~X,tau = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6)/100,data = train_df)


results <- as.data.frame(quantreg_results$fitted.values)
colnames(results) <- paste0("Q",c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6))
results$X <- train_df$X

tibble::as_tibble(quantreg_results$coefficients)

# Model Summaries



summary(flat_model)
summary(location)
summary(location_scale)
summary(location_scale_skew)
# summary(location_scale_skew_kurtosis)

plot(flat_model)
plot(location)
plot(location_scale)
plot(location_scale_skew)
# plot(location_scale_skew_kurtosis)




quantile_reg_centile_plot <- function(quantile_results,
                                      probs=c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                                      test_data=test_df,
                                      ylim,
                                      title,
                                      xlabel,
                                      ylabel){
  
  colours <- RColorBrewer::brewer.pal(n=length(probs)-1,name = "Paired")
  
  quantiles <- paste0("Q", probs)
  centile_conversions <- tibble(
    centile_min = quantiles[-length(quantiles)],
    centile_max = quantiles[-1]
    
    
  )
  
  centiles <- tidyr::pivot_longer(quantile_results,cols = colnames(quantile_results)[colnames(quantile_results)!="X"])
  centiles <- centiles %>%
    rename(centile_name=name)
  
  centiles_min <- centiles %>% merge(centile_conversions, by.x="centile_name", by.y="centile_min")
  centiles_max <- centiles %>% merge(centile_conversions, by.x="centile_name", by.y="centile_max")
  
  
  centiles_min <- centiles_min %>% 
    rename(min_value=value) %>% 
    rename(centile_min=centile_name)
  
  centiles_max <- centiles_max %>% 
    rename(max_value=value)%>% 
    rename(centile_max=centile_name)
  
  
  centiles_min_max <- centiles_min %>% merge(centiles_max, by = c("centile_min"="centile_min", "centile_max"="centile_max","X"="X"))
  
  
  
  prediction_plot <- ggplot()
  prediction_plot <- prediction_plot+
    geom_point(data=test_data, aes(x=X,y=y, shape="Data Point"), color="dodgerblue4") +
    scale_shape_manual("",values=c(16))
  
  prediction_plot <- prediction_plot + 
    geom_line(data=quantile_results,aes(x=X, y=Q50, color="Estimate"))+
    scale_colour_manual(c("",""),values=c("black","black"))
  
  
  
  prediction_plot <- prediction_plot +
    geom_ribbon(data=centiles_min_max,aes(x=X, ymax=max_value, ymin=min_value, fill=centile_min), alpha=0.5)
  
  
  
  values <- colours 
  names(values) <-centile_conversions$centile_min
  prediction_plot <- prediction_plot +
    scale_fill_manual(name="Quantiles",values=values, labels=paste0(centile_conversions$centile_min, " - ",centile_conversions$centile_max)) 
  ylim(ylim)
  
  
  prediction_plot
  return(prediction_plot)
  
}

centile_plot <- function(fit,
                         test_data,
                         probs=c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                         ylim,
                         title,
                         xlabel,
                         ylabel){
  # Plot Centiles
  centiles <- gamlss::centiles.pred(fit,
                                    xname = "X",
                                    cent = probs,
                                    xvalues = seq(1,100)) %>% as_tibble()
  
  centiles <- centiles %>%
    tidyr::pivot_longer(cols = colnames(centiles)[colnames(centiles)!="x"])
  centiles <- centiles %>%
    rename(centile=name)
  
  quantiles <- paste0("Q",probs)
  
  centiles$centile_name <- paste0("Q",centiles$centile)
  
  colours <- RColorBrewer::brewer.pal(n=length(quantiles)-1,name = "Paired")
  
  
  centile_conversions <- tibble(
    centile_min = quantiles[-length(quantiles)],
    centile_max = quantiles[-1]
    
    
  )
  
  centiles_min <- centiles %>% merge(centile_conversions, by.x="centile_name", by.y="centile_min")
  centiles_max <- centiles %>% merge(centile_conversions, by.x="centile_name", by.y="centile_max")
  
  centiles_min <- centiles_min %>% 
    rename(min_value=value) %>% 
    rename(centile_min=centile_name)
  
  centiles_max <- centiles_max %>% 
    rename(max_value=value)%>% 
    rename(centile_max=centile_name)
  
  centiles_min_max <- centiles_min %>% merge(centiles_max, by = c("centile_min"="centile_min", "centile_max"="centile_max","x"="x"))
  
  
  
  prediction_plot <- ggplot()
  prediction_plot <- prediction_plot+
    geom_point(data=test_data, aes(x=X,y=y, shape="Data Point"), color="dodgerblue4") +
    scale_shape_manual("",values=c(16))+
    labs(title=title,
         x=xlabel,
         y=ylabel)
  
  prediction_plot <- prediction_plot + 
    geom_line(data=centiles[centiles$centile==50,],aes(x=x, y=value, color="Estimate"))+
    scale_colour_manual(c("",""),values=c("black","black"))
  
  
  
  prediction_plot <- prediction_plot +
    geom_ribbon(data=centiles_min_max,aes(x=x, ymax=max_value, ymin=min_value, fill=centile_min), alpha=0.5)
  
  
  
  values <- colours 
  names(values) <-centile_conversions$centile_min
  prediction_plot <- prediction_plot +
    scale_fill_manual(name="Quantiles",values=values, labels=paste0(centile_conversions$centile_min, " - ",centile_conversions$centile_max)) 
  ylim(ylim)
  
  
  prediction_plot
  return(prediction_plot)
  
  # centiles$centile <- as.numeric(centiles$centile)
  # prediction_plot +
  #   ggridges::geom_vridgeline(stat="ydensity",
  #                           mapping=aes(x=50,
  #                                       y=value,width=..density..),
  #                           data=centiles[centiles$centile<75 & centiles$centile>25,],inherit.aes = F,
  #                           scale=-200,
  #                           alpha=0.9,
  #                           fill="black",
  #                           trim=F
  #                           )+
  # ggridges::geom_vridgeline(stat="ydensity",
  #                           mapping=aes(x=15,
  #                                       y=value,width=..density..),
  #                           data=centiles[centiles$centile<25 & centiles$centile>0,],inherit.aes = F,
  #                           scale=-200,
  #                           alpha=0.9,
  #                           fill="black",
  #                           trim=F)+
  #   ggridges::geom_vridgeline(stat="ydensity",
  #                             mapping=aes(x=85,
  #                                         y=value,width=..density..),
  #                             data=centiles[centiles$centile<100 & centiles$centile>74,],inherit.aes = F,
  #                             scale=-200,
  #                             alpha=0.9,
  #                             fill="black",
  #                             trim=F)
  # 
  # 
  
}

centile_plot(fit = flat_model,
             test_data = train_df,
             ylim=c(-100,300),
             title="Flat Model",
             xlabel = "X",
             ylabel="Y")

centile_plot(fit = location,
             test_data = train_df,
             ylim=c(-100,300),
             title="Location Only Model",
             xlabel = "X",
             ylabel="Y")

centile_plot(fit = location_scale,
             test_data = train_df,
             ylim=c(-100,300),
             title="Location Scale Model",
             xlabel = "X",
             ylabel="Y")

centile_plot(fit = location_scale_skew,
             test_data = train_df,
             ylim=c(-100,300),
             title="Location Scale Shape Model",
             xlabel = "X",
             ylabel="Y")
# centile_plot(fit = location_scale_skew_kurtosis,test_data = train_df,ylim=600)





prediction_flat <- gamlss::predictAll(flat_model,newdata = test_df)
prediction_location <- gamlss::predictAll(location,newdata = test_df)
prediction_location_scale <- gamlss::predictAll(location_scale,newdata = test_df)
prediction_location_scale_skew <- gamlss::predictAll(location_scale_skew,newdata = test_df)

results_flat <- test_df
results_flat$mu_pred <- prediction_flat$mu
results_flat$sigma_pred <- prediction_flat$sigma
results_flat$nu_pred <- prediction_flat$nu

ggplot(results_flat, aes(y=mu_pred, x=mu))+
  geom_point()

ggplot(results_flat, aes(y=sigma_pred, x=sigma))+
  geom_point()

ggplot(results_flat, aes(y=nu_pred, x=nu))+
  geom_point()


set.seed(500)

prediction_distribution <- function(
    row_number,
    prediction_flat,
    prediction_location,
    prediction_location_scale,
    prediction_location_scale_skew
    
    
){
  
  pred_quantiles <- seq(1,100,0.01)
  # row_number <- sample(c(1:nrow(test_df)), 1)
  sample_result <- test_df[row_number,]
  X <- test_df[row_number,"X"]
  
  mu_pred_flat <- prediction_flat$mu[row_number]
  sigma_pred_flat <- prediction_flat$sigma[row_number]
  nu_pred_flat <- prediction_flat$nu[row_number]
  density_flat <- dSN1(x=pred_quantiles,
                       mu=mu_pred_flat,
                       sigma = sigma_pred_flat,
                       nu = nu_pred_flat)
  density_flat <- tibble::as_tibble(list(
    model_type=rep("flat", length(pred_quantiles)),
    x=pred_quantiles,
    y=density_flat
  ))
  
  
  mu_pred_location <- prediction_location$mu[row_number]
  sigma_pred_location <- prediction_location$sigma[row_number]
  nu_pred_location <- prediction_location$nu[row_number]
  density_location <- dSN1(x=pred_quantiles,
                           mu=mu_pred_location,
                           sigma = sigma_pred_location,
                           nu = nu_pred_location)
  density_location <- tibble::as_tibble(list(
    model_type=rep("location", length(pred_quantiles)),
    
    x=pred_quantiles,
    y=density_location
  ))
  
  mu_pred_location_scale <- prediction_location_scale$mu[row_number]
  sigma_pred_location_scale <- prediction_location_scale$sigma[row_number]
  nu_pred_location_scale <- prediction_location_scale$nu[row_number]
  density_location_scale <- dSN1(x=pred_quantiles,
                                 mu=mu_pred_location_scale,
                                 sigma = sigma_pred_location_scale,
                                 nu = nu_pred_location_scale)
  density_location_scale <- tibble::as_tibble(list(
    model_type=rep("location_scale", length(pred_quantiles)),
    
    x=pred_quantiles,
    y=density_location_scale
  ))
  
  mu_pred_location_scale_skew <- prediction_location_scale_skew$mu[row_number]
  sigma_pred_location_scale_skew <- prediction_location_scale_skew$sigma[row_number]
  nu_pred_location_scale_skew <- prediction_location_scale_skew$nu[row_number]
  density_location_scale_skew <- dSN1(x=pred_quantiles,
                                      mu=mu_pred_location_scale_skew,
                                      sigma = sigma_pred_location_scale_skew,
                                      nu = nu_pred_location_scale_skew)
  density_location_scale_skew <- tibble::as_tibble(list(
    model_type=rep("location_scale_skew", length(pred_quantiles)),
    
    x=pred_quantiles,
    y=density_location_scale_skew
  ))
  
  
  mu_pred_true <- test_df$mu[row_number]
  sigma_pred_true <- test_df$sigma[row_number]
  nu_pred_true <- test_df$nu[row_number]
  density_true <- dSN1(x=pred_quantiles,
                       mu=mu_pred_true,
                       sigma = sigma_pred_true,
                       nu = nu_pred_true)
  density_true <- tibble::as_tibble(list(
    model_type=rep("true", length(pred_quantiles)),
    
    x=pred_quantiles,
    y=density_true
  ))
  
  
  
  summary_densities <- rbind(density_flat,
                             density_location,
                             density_location_scale,
                             density_location_scale_skew,
                             density_true
  )
  
  plot_distribution_comparison <- ggplot(summary_densities)+
    geom_line(aes(color=model_type, x=x, y=y))+
    geom_area(aes(fill=model_type, x=x, y=y), position = "identity", alpha=0.3)+
    labs(title=paste0("Predicted Distributions: X = ",round(X,2)))

  plot_distribution_comparison
  return(plot_distribution_comparison)
}


quantiles <- quantile(test_df$X, probs = c( 0.2,0.4, 0.6, 0.8))

x20 <- which(test_df$X==quantiles[1])
x40 <- which(test_df$X==quantiles[2])
x60 <- which(test_df$X==quantiles[3])
x80 <- which(test_df$X==quantiles[4])


 prediction_distribution(
    x20,
    prediction_flat,
    prediction_location,
    prediction_location_scale,
    prediction_location_scale_skew)
 
 
 prediction_distribution(
   x40,
   prediction_flat,
   prediction_location,
   prediction_location_scale,
   prediction_location_scale_skew)
 
 prediction_distribution(
   x60,
   prediction_flat,
   prediction_location,
   prediction_location_scale,
   prediction_location_scale_skew)
 
 prediction_distribution(
   x80,
   prediction_flat,
   prediction_location,
   prediction_location_scale,
   prediction_location_scale_skew)
 
 
 











