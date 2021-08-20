## TESTING GOODNESS OF FIT
# Normality
set.seed(51680)
x1 <- rnorm(1000) # mean = 5, sd = 2 
x2 <- rgamma(1000, shape = 12.5, scale = 0.4) # mean = shape x scale, variance = shape x scale^2
plot(density(x1))
plot(density(x2))

# shapiro test
shapiro.test(x1) # p-value = 0.44 --> cant reject H0
shapiro.test(x2) # p-value  --> p-value 1.2e-10, so reject H0

# anderson-darling test
# Notice that by definition this test places more importance to the tails 
# as per Klugman et al (Loss Models)
library(nortest)

ad.test(x1)
ad.test(x2)

# kolmogorov smirnov allows for other distributions
# First we'll need to do some point estimation
# for example, for x2 here we'll need to guess shape and scale:

mu <- mean(x2); sigmasq <- var(x2) # we know var/mean = scale, and mean/scale = shape
(scale <- sigmasq/mu);  (shape <- mu/scale)

ks.test(x2, function(...) pgamma(...,shape = shape, scale = scale))

# fitting a distribution to your data requires much thought about 
# where the data comes from, so be sure to understand what is generating your data
# if there's doubt among several possible distributions, you can run the test
# testing several options:

my_dist <- c(
  function(...) pgamma(...,shape = shape, scale = scale),
  function(...) pnorm(..., mean = mu, sd = sqrt(sigmasq))
)

sapply(my_dist, function(dist) ks.test(x2, dist))