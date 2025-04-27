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

# Simulate carabid response variables (neg binomial distributed)
SpeciesDensity <- rnbinom(n_samples, size = 1, mu = 5 + 0.5 * CWD - 0.3 * FWD + 0.2 * HerbCover)
ActivityDensity <- rnbinom(n_samples, size = 1, mu = 10 + 0.3 * CWD + 0.1 * LitterDepth)
ConservationValue <- rnbinom(n_samples, size = 1, mu = 2 + 0.4 * HerbCover)

# Assemble dataset
data <- data.frame(management, PlotID, CWD, FWD, HerbCover, LitterDepth, 
                   SpeciesDensity, ActivityDensity, ConservationValue)

# 2. Fit GLMMs (component models)
# Environmental variables explained by management
glmm_CWD <- glmer.nb(CWD ~ management + (1|PlotID), data = data)
glmm_FWD <- glmer.nb(FWD ~ management + (1|PlotID), data = data)
glmm_HerbCover <- glmer.nb(HerbCover ~ management + (1|PlotID), data = data)
glmm_LitterDepth <- glmer.nb(LitterDepth ~ management + (1|PlotID), data = data)

# Carabid community responses explained by environmental variables
# Species density
model_species_density <- glmer.nb(SpeciesDensity ~ CWD + FWD + HerbCover + LitterDepth + (1|PlotID), data = data)

# Activity density
model_activity_density <- glmer.nb(ActivityDensity ~ CWD + FWD + HerbCover + LitterDepth + (1|PlotID), data = data)

# Conservation value
model_conservation_value <- glmer.nb(ConservationValue ~ CWD + FWD + HerbCover + LitterDepth + (1|PlotID), data = data)

# 3. Combine models into a piecewise SEM
sem_model <- psem(
  glmm_CWD,
  glmm_FWD,
  glmm_HerbCover,
  glmm_LitterDepth,
  model_species_density,
  model_activity_density,
  model_conservation_value
)

# 4. Summarize the SEM model
summary(sem_model)

# 5. SEM diagram using semPlot
# Convert piecewiseSEM to lavaan object for plotting
sem_lavaan <- as_lavaan(sem_model)
semPlot::semPaths(sem_lavaan, "std", edge.label.cex=1.2, sizeMan=7, fade=FALSE)

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
