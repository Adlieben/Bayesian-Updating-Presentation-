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
students <- data.frame(student = 1:14, 
                       age = rep(NA, 14))

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

# compare to alpha spending function
