# Using Bayesian updating ------------------------

library(bain) 

prepbain <- function(students) {
  
  # calculate mean
  m.age <- lm(age~1, data = students)
  
  meanage <- coef(m.age)[[1]]
  names(meanage) <- "mage"
  
  # save residual variance
  var <- (summary(m.age)$sigma)**2
  
  # save sample size
  ssize <- nrow(m.age$model)
  
  # compute variance
  cov <- matrix(var/ssize,1,1)
  
  # run bain
  set.seed(1337)
  ### alternative hyp: mage = 264; mage < 264; mage > 264
  res <- bain(meanage, "mage = 264", n=ssize, Sigma=cov)
  
  # return objects needed for bain
  return(res)
}

# store age data
students <- data.frame(student = 1:12, 
                       age = rep(NA, 12))

students$age[1] <- 20*12 + 7
students$age[2] <- 22*12 + 10
students$age[3] <- 27*12 + 9

# run bain
result1 <- prepbain(students)

print(result1)

# collect more samples
students$age[4] <- 18*12+3
students$age[5] <- 27*12+0
students$age[6] <- 19*12

result2 <- prepbain(students)
print(result2)

# collect more samples
students$age[7] <- 24*12+6
students$age[8] <- 29*12+1
students$age[9] <- 23*12+8

result3 <- prepbain(students)
print(result3)

# collect final samples
students$age[10] <- 21*12+3
students$age[11] <- 19*12+5
students$age[12] <- 25*12+4

result4 <- prepbain(students)
print(result4)

# plot results
N <- c(3,6,9,12)
H1 <- c(result1$fit$PMPb[1],
        result2$fit$PMPb[1],
        result3$fit$PMPb[1],
        result4$fit$PMPb[1]
)

Hc <- c(result1$fit$PMPb[2],
        result2$fit$PMPb[2],
        result3$fit$PMPb[2],
        result4$fit$PMPb[2]
)

# create a legenda for the plot
leg <- c("H1","Hc")

# give each line a different appearance
lty <- c(1,2)

# plot NperGRoup versus p1 using a line plot (type = "l") and a y-axis bounded between 0 and 1 (ylim=(0:1.0))
plot(N, H1, type = "l",ylab = "posterior probabilities",xlab="N",ylim=(0:1.0))
# add a line for p2, p3, p4, and p5 to the plot
lines(N, Hc,lty=2)
# insert the legend in the plot at x=17 and y=.95 
legend(x = 12.5, y = 0.95,legend = leg, lty =lty)

# bayes factors
BFs <- c(result1$fit$BF.u[1],
        result2$fit$BF.u[1],
        result3$fit$BF.u[1],
        result4$fit$BF.u[1]
)

plot(N, BFs, type = "l",ylab = "BF",xlab="N")

# Using fixed alpha ------------------------------

# Create the function
hypothesis.test.mean <- function(data, mean, sig = 0.05){
  # Conduct the t-test and retrieve p value and 
  p <- t.test(x = data, mu = mean)$p.value
  n <- length(na.omit(data))
  
  # Decide to stop or continue trial
  if (p > sig){
    Decision <- 'Continue'
  } else {
    Decision <- 'Stop'
  }
  
  # Format output
  output <- list(p, n, Decision)
  names(output) <- c('P-value', 'Sample Size', 'Decision')
  
  return(output)
}

# store age data again
students <- data.frame(student = 1:15, 
                       age = rep(NA, 15))

students$age[1] <- 252
students$age[2] <- 216
students$age[3] <- 276

# run analysis
decision1 <- hypothesis.test.mean(students$age, 264)
print(decision1)

# collect more samples
students$age[4] <- 240
students$age[5] <- 264
students$age[6] <- 228

decision2 <- hypothesis.test.mean(students$age, 264)
print(decision2)

# collect more samples
students$age[7] <- 264
students$age[8] <- 252
students$age[9] <- 228

decision3 <- hypothesis.test.mean(students$age, 264)
print(decision3)

# collect final samples
students$age[10] <- NA
students$age[11] <- NA
students$age[12] <- NA

decision4 <- hypothesis.test.mean(students$age, 264)
print(decision4)

# collect final samples
students$age[13] <- NA
students$age[14] <- NA
students$age[15] <- NA

decision5 <- hypothesis.test.mean(students$age, 264)
print(decision5)

### plot results
N <- c(3,6,9,12,15)
timepoints <- c(decision1[[1]],
                decision2[[1]],
                decision3[[1]],
                decision4[[1]],
                decision5[[1]]
)

# Add a stopping sample size if there is one
find_stop_index <- function(decision_list) {
  stopsize <- NA
  for (i in 1:length(decision_list)){
    if (length(grep('Stop', decision_list[[i]])) > 0){
      if (is.na(stopsize)){ # Find only the first stop
        stopsize <- decision_list[[i]]$`Sample Size` # Return the sample size at that stop
      }
    }
  }
  if (length(stopsize) > 0) {
    return(stopsize)
  } else {
    stopsize <- decision_list[[length(decision_list)]]$`Sample Size`
    return(stopsize)
  }
}

# Group decision variables and find stop
decision_vars <- list(decision1, decision2, decision3, decision4)
stopsize <- find_stop_index(decision_vars)

# plot NperGRoup versus p value
plot(N, timepoints, type = "l", ylab = "P value", xlab="N", ylim=(0:1.0), xlim=(3:stopsize))

