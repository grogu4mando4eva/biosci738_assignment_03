library(tidyverse)
library(ggplot2)
library(dplyr)

clicks <- readr::read_csv("whale_clicks.csv")
clicks


#playing around with the data 
meanbydepth <- clicks %>% 
  group_by(mean_depth)%>% 
  summarise(mean(n_click))

##histogram
mybreaks <- c(0,25,50,75,100,125)
ggplot(clicks, aes(x=n_click)) +
  geom_histogram(binwidth = 1) +
  labs(x="number of clicks per minute", y="frequency", title = "Distribution of Number of Clicks per Minute")+
  scale_x_continuous(breaks=mybreaks)

##calculating mean clicks/min and mean clicks/sec
mean_min <- mean(clicks$n_click)
mean_min
mean_sec <- mean_min/60
mean_sec

#bootstrapping
nreps <- 1000
## initialize empty array to hold results
bootstrap_means <- numeric(nreps)

for (i in 1:nreps) {
  bootstrap_sample <- sample(clicks$n_click, replace = TRUE)
  ##  bootstrapped mean resample
  bootstrap_means[i] <- mean(bootstrap_sample)
}
## results
results <- data.frame(bootstrap_means = bootstrap_means)
ggplot(data = results, aes(x = bootstrap_means)) +
  geom_histogram() +
  geom_vline(xintercept = as.numeric(mean)) +
  ggtitle("Sampling distribution of the mean") +
  xlab("Bootstrap means")  + ylab("") + theme_classic()

#bootstrap bias
bias <- as.numeric(mean_min) - mean(results$bootstrap_means)
bias

#bootstrap se
sd(results$bootstrap_means)

#CIs
as.numeric(mean) + c(-1,1) * qt(0.95,843)*sd(results$bootstrap_means)

#############Question 2###########################################2

#use dpois
lambdaseq <- seq(10, 60, by = 0.1)
likelihoodvals <- dpois(x=30, lambdaseq, log=TRUE)

plot(lambda, likelihoodvals,
     pch=20)
(abline(v=30, col="red"))







#ceating my function
lambda <- 52.79
obvs <- 843
llh <- function(lambda, obvs){
  loglikevals <- sum(dpois(lambda, obvs, log = TRUE))#creates and sums log likelihood values
  results <- data.frame(SumlogL = loglikevals, #create data frame to show sum value and parameters
                        Lambda = lambda, 
                        Obesrvations = obvs)
  return(results)
}
llh(lambda, obvs)


optimise(llh, c(0, 125), maximum = TRUE)


#############Question 3###########################################3
# datawrangle to negative
clicks$neg_depths = clicks$mean_depth*(-1)
  
#data wrangle >0 to 1
#mutated <- clicks %>% clicks$n_click[clicks$n_click>0] <- 1 ###this worked but I dont think I actually need it 


#Plot recreation
ggplot(clicks,aes(x=minute, y=neg_depths))+
  geom_line(size=2)+
  geom_point(data = subset(clicks, n_click != 0), aes(color=n_click), size =1)+
  scale_colour_gradient(low = "yellow", high = "red")+
  labs(x="Minutes since tagging", y="Mean depth from surface", title="Number of whale clicks by depth")
  

data <- clicks[1:50,]

#fit linear model
slm <- lm(n_click ~ mean_depth, data = data)
summary(slm)$coef
   
#plot the fitted model
gglm::gglm(slm)

##fit a poisson model 
glm_whales <- glm(n_click ~ mean_depth, data = data, family = "poisson")
summary(glm_whales)
ggplot(ndata, aes(x = leafHeight, y = fit)) +
  geom_line() +
  geom_rug(aes(y = visited, colour = lvisited), data = wasp) +
  scale_colour_discrete(name = 'Visited') +
  labs(x = 'Leaf height (cm.)', y = 'Probability of visitation')

#account for log 
exp(coef(glm_whales))

# #plotting the lm
# plot(residuals(glm_whales) ~ 
#        predict(glm_whales,type="response"),xlab=expression(hat(mu)),
#      ylab="Deviance residuals",pch=20,col="red")

# #testing the model
# ## extract the residual deviance
# dev <- as.numeric(glm_whales$deviance)
# dev
# df <- glm_whales$df.residual
# df
# 1 - pchisq(dev, df) #test the null hypoth that the model is correct






#clicks at the surface
clicks_0m <- 2.307 + 0.00275*(0)
exp(clicks_0m)

#calculate the expected clicks on the surface at an average depth of 100m
clicks_100m <- 2.307 + 0.00275*(100)
exp(clicks_100m)






##calculate 95CIs for clicks at 0m
ci <- exp(confint(glm_whales)[1,])
ci

10.04425-8.533747
10.04425-11.767065

upper <- exp(log(10.04425)+sqrt(1/(843*10.04425)))
upper
lower <- exp(log(10.04425)-sqrt(1/(843*10.04425)))
lower

##calculate 95CIs for clicks at 100m
#method 1 - uses rpois to generate data and then estimate a CI
x <- rpois(843, 13.22356)
exp(confint(glm(x ~ 1, family=poisson)))

#method 2 - uses the CIs from above to estimate the confidence
13.22356-1.510503
13.22356+1.722815

#method 3 - uses the formula to calculate the CIs 
upper <- exp(log(13.22356)+sqrt(1/(843*13.22356)))
upper
lower <- exp(log(13.22356)-sqrt(1/(843*13.22356)))
lower




## response residuals
par(mfrow=c(1,2))#makes them into one graph
resids_response <- residuals(glm_whales, type = "response")
plot_resp <- plot(resids_response)
## deviance residuals
resids_deviance <- residuals(glm_whales, type = "deviance")
plot_resid <- plot(resids_deviance)


#zeroinflated models
library("pscl")
zipm50 <- zeroinfl(n_click ~ mean_depth, data = data)
zipm50

zipm843 <- zeroinfl(n_click ~ mean_depth, data = clicks)#sample sizes are different as we did the glm with 50

#hurdle model
hurd50 <- hurdle(n_click ~ mean_depth, data = data)
hurd50

#vuong test

install.packages("nonnest2")
library("nonnest2")
vuongtest(hurd50, zipm50)
