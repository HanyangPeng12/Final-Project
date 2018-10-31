mh.beta <- function(iterations, start,shape1, shape2,c) {
  theta.old <- start
  draws <- c()
  theta.update <- function(theta.old, shape1, shape2) {
    theta.new <- rbeta(1, c*theta.old, c*(1-theta.old))
    accept.prob <- dbeta(theta.new, shape1 = shape1, shape2=shape2)/dbeta(theta.old,shape1 = shape1, shape2 = shape2)*
      (dbeta(theta.old,c*theta.new,c*(1-theta.new))/dbeta(theta.new,c*theta.old,c*(1-theta.old)))
    #Because the Beta distribution is not symmetric, so the accept.prob
    #should have another part, which is neutralized in symmetric situation
    #So we should add that part again.
    if (runif(1) <= accept.prob) 
    {theta.update=theta.new} 
    else 
        {theta.update=theta.old}
  }
  for (i in 1:iterations) {
    draws[i] <- theta.old <- theta.update(theta.old, shape1 = shape1,
                                          shape2 = shape2)
  }
  return(draws[1:iterations])
}


draws <- mh.beta(10000, start = runif(1),shape1 = 6, shape2 = 4,c=1)

par(mfrow=c(1,3))  #1 row, 3 columns
plot(draws); 
acf(draws);
hist(draws)  #plot commands







