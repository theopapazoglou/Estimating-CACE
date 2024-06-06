library(dplyr)
library(descr)
library(estimatr)
library(Metrics)


#All or None Compliance
#80% of Compliers, 20% of Non-Compliers (All whom are Never-Takers, i.e. D(1)=D(0)=0)
simfun <- function(i, n = 2000, prob = 0.5, p_noncompliers = 0.2, p_compliers = 0.8,
                   mean0 = 30, mean1 = 45, sd = 1){
  Category <- c(rep(1, round(p_noncompliers * n)), rep(2, n - round(p_noncompliers * n)))
  Category <- sample(Category, n)
  dt <- data.frame(ID = 1:n, Category = Category)
  dt$Z <- rbinom(n = n, size = 1, prob = prob)
  dt$D0 <- 0
  dt$D1 <- ifelse(dt$Category == 2, 1, 0)
  dt$D <- ifelse(dt$Z == 1, dt$D1, dt$D0)
  dt$Y0 <- rnorm(n, mean = mean0, sd = sd)
  dt$Y1 <- rnorm(n, mean = mean1, sd = sd)
  dt$Y <- ifelse(dt$D == 1, dt$Y1, dt$Y0)
  dt$Compliance <- factor(dt$Category, labels = c("NT", "CO"))
  compliers <- dt %>% filter(Compliance == "CO")
  truth <- mean(compliers$Y1) - mean(compliers$Y0)
  fit_PPE <- lm(Y ~ Z, data = compliers)
  PPE <- coef(fit_PPE)[2]
  PPE_bias <- bias(truth,PPE)
  fit_CACE <- iv_robust(Y ~ D | Z, data = dt)
  CACE <- coef(fit_CACE)[2]
  CACE_bias <- bias(truth,CACE)
  out <- data.frame(
    i = i,
    truth <- truth,
    PPE <- PPE,
    PPE_bias <- PPE_bias,
    CACE <- CACE,
    CACE_bias <- CACE_bias
  )
  return(out)
}
#Run 1000 simulations, use random seed 24
set.seed(24)
nsim <- 1000
estimates <- data.frame(matrix(ncol = 6, nrow = nsim))
x <- c("simulation","true_effect","PPE","PPE_bias","CACE","CACE_bias")
colnames(estimates) <- x
for (r in 1:nsim) {
  estimates[r,] <- simfun(i = r)
  
}
truth <- mean(estimates$true_effect)  
PPE <- mean(estimates$PPE)  
CACE <- mean(estimates$CACE)  
cat("True effect:", round(truth, 2), "\n")
cat("PP effect:", round(PPE, 2), "\n")
cat("CACE effect:", round(CACE, 2), "\n")
  
  






