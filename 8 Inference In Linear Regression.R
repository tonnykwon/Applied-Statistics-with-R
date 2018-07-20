set.seed(42)
sample_size = 100
x = seq(-1, 1, length =sample_size)
Sxx = sum((x-mean(x))^2)

beta_0 = 3
beta_1 = 6
sigma = 2

(var_beta_1_hat = sigma^2/Sxx)
(var_beta_0_hat = sigma^2 *(1/sample_size+mean(x)^2/Sxx))


num_samples = 10000
beta_0_hats = rep(0, num_samples)
beta_1_hats = rep(0, num_samples)

for(i in 1:num_samples){
  eps = rnorm(sample_size, mean=0, sd=sigma)
  y = beta_0 + beta_1 *x + eps
  
  sim_model = lm(y~x)
  
  beta_0_hats[i] = coef(sim_model)[1]
  beta_1_hats[i] = coef(sim_model)[2]
}

hist(beta_1_hats, prob= TRUE, breaks=20, xlab= expression(hat(beta)[1]), main="", border="dodgerblue")
curve(dnorm(x, mean=beta_1, sd= sqrt(var_beta_1_hat)), col="darkorange", add=TRUE, lwd=3)

par(mar=c(5,5,1,1))
plot(cumsum(beta_1_hats)/ (1:length(beta_1_hats)), type = "l", ylim = c(5.95, 6.05),
     xlab = "Number of Simulations", col="dodgerblue")
abline(h=6, col="darkorange", lwd=2)


# t distribution
x = seq(-4, 4, length=100)
plot(x, dnorm(x), type= "l", lty = 1, lwd= 2)
lines(x, dt(x, df=1), lty=3, lwd=2, col="darkorange")
lines(x, dt(x, df=10), lty=2, lwd=2, col="darkblue")


############### car example
stop_dist_model = lm(dist~speed, data=cars)
summary(stop_dist_model)$coefficients

x = cars$speed
y = cars$dist
y_hat = stop_dist_model$fitted.values
e = y-y_hat
n = length(e)
s2_e = sum(e^2)/(n-2)

Sxx = sum((x-mean(x))^2)
Sxy = sum((x-mean(x))*(y-mean(y)))
beta_1_hat = Sxy/Sxx
(beta_1_hat)/(sqrt(s2_e)/sqrt(Sxx))


# store model data
stop_dist_model_test_info = summary(stop_dist_model)$coefficients

beta_0_hat      = stop_dist_model_test_info[1, 1] # Estimate
beta_0_hat_se   = stop_dist_model_test_info[1, 2] # Std. Error
beta_0_hat_t    = stop_dist_model_test_info[1, 3] # t value
beta_0_hat_pval = stop_dist_model_test_info[1, 4] # Pr(>|t|)

beta_1_hat      = stop_dist_model_test_info[2, 1] # Estimate
beta_1_hat_se   = stop_dist_model_test_info[2, 2] # Std. Error
beta_1_hat_t    = stop_dist_model_test_info[2, 3] # t value
beta_1_hat_pval = stop_dist_model_test_info[2, 4] # Pr(>|t|)

2*pt(abs(beta_1_hat_t), df=length(resid(stop_dist_model))-2, lower.tail=FALSE)


############ confidence and prediciton bands
speed_grid = seq(min(cars$speed), max(cars$speed))
dist_ci_band = predict(stop_dist_model,newdata= data.frame(speed=speed_grid), interval = "confidence", level=0.99)
dist_pi_band = predict(stop_dist_model, newdata=data.frame(speed=speed_grid), interval="prediction", level=0.99)

plot(dist~speed, data=cars,
     xlab= "Speed (in Miles per hour)",
     ylab = "Stoppping distance (in Feet)",
     main = "Stopping distance vs speed",
     pch =20,
     cex=2,
     col="grey",
     ylim= c(min(dist_pi_band), max(dist_pi_band)))
abline(stop_dist_model, lwd=5, col="darkorange")
lines(speed_grid, dist_ci_band[,"lwr"], col="dodgerblue", lwd=3, lty=2)
abline(stop_dist_model, lwd=5, col="darkorange")
lines(speed_grid, dist_ci_band[,"upr"], col="dodgerblue", lwd=3, lty=2)
abline(stop_dist_model, lwd=5, col="darkorange")
lines(speed_grid, dist_pi_band[,"lwr"], col="dodgerblue", lwd=3, lty=3)
abline(stop_dist_model, lwd=5, col="darkorange")
lines(speed_grid, dist_pi_band[,"upr"], col="dodgerblue", lwd=3, lty=3)
points(mean(cars$speed), mean(cars$dist), pch = "+", cex = 3)


anova(stop_dist_model)
anova(lm(dist ~ 1, data = cars), lm(dist ~ speed, data = cars))