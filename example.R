library(piecewiseSEM)
library(tidySEM)

set.seed(1)
data <- data.frame(
  x = runif(100),
  y1 = runif(100),
  y2 = rpois(100, 1),
  y3 = runif(100)
)

modelList <- psem(
  lm(y1 ~ x, data),
  glm(y2 ~ x, "poisson", data),
  lm(y3 ~ y1 + y2, data),
  data
)

summary(modelList)

# Get path coefficients
paths <- coefs(modelList)

# Build a tidy SEM model
sem_model_tidy <- build_sem(paths)

# Now plot
plot(sem_model_tidy)

