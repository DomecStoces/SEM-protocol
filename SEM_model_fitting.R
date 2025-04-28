#Works only in newer version of R 4.5.0
library(piecewiseSEM)
library(lme4)
library(dplyr)
library(DHARMa) 
library(multcompView)
library(DiagrammeR)

# 1. Simulate example datasheet (Gaussian data)
set.seed(123)
n_samples <- 30

management <- rep(c("Extensive", "Intensive"), each = 15)
PlotID <- factor(rep(1:6, each = 5))

# Simulate environmental variables
CWD <- ifelse(management == "Extensive", rnorm(n_samples, 5, 1), rnorm(n_samples, 2, 1))
FWD <- ifelse(management == "Extensive", rnorm(n_samples, 2, 0.5), rnorm(n_samples, 4, 0.5))
HerbCover <- ifelse(management == "Extensive", rnorm(n_samples, 50, 10), rnorm(n_samples, 20, 10))
LitterDepth <- ifelse(management == "Extensive", rnorm(n_samples, 6, 1), rnorm(n_samples, 2, 1))

# RESPONSE variables now NORMAL, not Poisson
SpeciesDensity <- 1 + 0.05 * CWD - 0.03 * FWD + 0.02 * HerbCover + rnorm(n_samples, 0, 2)
ActivityDensity <- 1 + 0.03 * CWD + 0.01 * LitterDepth + rnorm(n_samples, 0, 2)
ConservationValue <- 0.5 + 0.04 * HerbCover + rnorm(n_samples, 0, 2)

# Assemble
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
  glm(SpeciesDensity ~ CWD + FWD + HerbCover + LitterDepth, family = gaussian, data = data),
  glm(ActivityDensity ~ CWD + FWD + HerbCover + LitterDepth, family = gaussian, data = data),
  glm(ConservationValue ~ CWD + FWD + HerbCover + LitterDepth, family = gaussian, data = data),
  
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

# 7. Apply DHARMa residual checks for each model
models <- sem_model$models

par(mfrow = c(3, 3)) # Arrange plots in a 3x3 grid (adjust if needed)

for (i in seq_along(models)) {
  cat("\nModel", i, "\n")
  res <- simulateResiduals(models[[i]])
  plot(res, main = paste("Model", i))
}
par(mfrow = c(1, 1)) # Reset layout

# 8. Visualization of psem model. You need to manually set grid from summary(sem_model)
grViz("
digraph SEM {
  
  rankdir=LR;
  
  node [shape=box, style=filled, color=lightblue, fontname=Helvetica];
  
  management [label='Management']
  CWD [label='Coarse Woody Debris\\nR²=0.79']
  FWD [label='Fine Woody Debris\\nR²=0.82']
  HerbCover [label='Herb Cover\\nR²=0.74']
  LitterDepth [label='Litter Depth\\nR²=0.87']
  SpeciesDensity [label='Species Density\\nR²=0.32']
  ActivityDensity [label='Activity Density\\nR²=0.27']
  ConservationValue [label='Conservation Value\\nR²=0.21']
  
  # Management paths
  management -> CWD [color=red, penwidth=2]
  management -> FWD [color=red, penwidth=2]
  management -> HerbCover [color=red, penwidth=2]
  management -> LitterDepth [color=red, penwidth=2]
  
  # Paths to SpeciesDensity
  CWD -> SpeciesDensity [color=red, penwidth=2]
  FWD -> SpeciesDensity [color=grey, penwidth=1]
  HerbCover -> SpeciesDensity [color=green, penwidth=2]
  LitterDepth -> SpeciesDensity [color=green, penwidth=2]
  
  # Paths to ActivityDensity
  CWD -> ActivityDensity [color=green, penwidth=2]
  FWD -> ActivityDensity [color=green, penwidth=2]
  HerbCover -> ActivityDensity [color=green, penwidth=2]
  LitterDepth -> ActivityDensity [color=red, penwidth=2]
  
  # Paths to ConservationValue
  CWD -> ConservationValue [color=grey, penwidth=1]
  FWD -> ConservationValue [color=red, penwidth=2]
  HerbCover -> ConservationValue [color=red, penwidth=2]
  LitterDepth -> ConservationValue [color=grey, penwidth=1]
}
")
