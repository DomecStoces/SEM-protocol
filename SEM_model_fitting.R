#Works only in newer version of R 4.5.0
library(piecewiseSEM)
library(lme4)
library(MASS)
library(dplyr)
library(DHARMa) 
library(semPlot)
library(lavaan)
library(multcompView)
library(DiagrammeR)
library(tidySEM)

# 1. Simulate example datasheet
set.seed(123)
n_samples <- 30

# Randomly assign management types and plot IDs
management <- rep(c("Extensive", "Intensive"), each = 15)
PlotID <- factor(rep(1:6, each = 5))

# Simulate environmental variables affected by management
CWD <- ifelse(management == "Extensive", rnorm(n_samples, 5, 1), rnorm(n_samples, 2, 1))
FWD <- ifelse(management == "Extensive", rnorm(n_samples, 2, 0.5), rnorm(n_samples, 4, 0.5))
HerbCover <- ifelse(management == "Extensive", rnorm(n_samples, 50, 10), rnorm(n_samples, 20, 10))
LitterDepth <- ifelse(management == "Extensive", rnorm(n_samples, 6, 1), rnorm(n_samples, 2, 1))

# Simulate carabid response variables (Poisson distributed)
SpeciesDensity <- rpois(n_samples, lambda = exp(1 + 0.05 * CWD - 0.03 * FWD + 0.02 * HerbCover))
ActivityDensity <- rpois(n_samples, lambda = exp(1 + 0.03 * CWD + 0.01 * LitterDepth))
ConservationValue <- rpois(n_samples, lambda = exp(0.5 + 0.04 * HerbCover))

# Assemble into one dataframe
data <- data.frame(management, PlotID, CWD, FWD, HerbCover, LitterDepth, 
                   SpeciesDensity, ActivityDensity, ConservationValue)

# 2. Create the list of models
sem_model <- psem(
  
  # Environmental variables explained by management
  lm(CWD ~ management, data = data),
  lm(FWD ~ management, data = data),
  lm(HerbCover ~ management, data = data),
  lm(LitterDepth ~ management, data = data),
  
  # Carabid community responses explained by habitat structure
  glm(SpeciesDensity ~ CWD + FWD + HerbCover + LitterDepth, family = poisson, data = data),
  glm(ActivityDensity ~ CWD + FWD + HerbCover + LitterDepth, family = poisson, data = data),
  glm(ConservationValue ~ CWD + FWD + HerbCover + LitterDepth, family = poisson, data = data),
  
  # Dataset used
  data = data
)

# 3. Summarize the SEM
summary(sem_model)

# 4. Optional: Address conflicts if needed (missing paths)
# Example using conservative assumption
# summary(sem_model, conserve = TRUE)
# OR specifying direction if needed
# summary(sem_model, direction = c("ActivityDensity <- HerbCover"))

# 5. Basic SEM plot
plot(sem_model)

# 6. Notes
# - Fisher's C tests whether the model as a whole fits well (high P-value > 0.05 = good fit)
# - You can inspect the summary to see direct and indirect effects.

grViz("
digraph sem {
  management -> CWD
  management -> FWD
  management -> HerbCover
  management -> LitterDepth
  CWD -> SpeciesDensity
  FWD -> SpeciesDensity
  HerbCover -> SpeciesDensity
  LitterDepth -> SpeciesDensity
  CWD -> ActivityDensity
  FWD -> ActivityDensity
  HerbCover -> ActivityDensity
  LitterDepth -> ActivityDensity
  CWD -> ConservationValue
  FWD -> ConservationValue
  HerbCover -> ConservationValue
  LitterDepth -> ConservationValue
}
")
