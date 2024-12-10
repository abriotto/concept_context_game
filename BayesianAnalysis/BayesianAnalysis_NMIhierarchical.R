setwd(normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path)))
df <- read.csv('data_for_R_NMI_hierarchical.csv')

# BEST package is no longer active, but can still be downloaded from CRAN archive
# Download package tarball from CRAN archive
#url <- "https://cran.r-project.org/src/contrib/Archive/BEST/BEST_0.5.4.tar.gz"
#pkgFile <- "BEST_0.5.4.tar.gz"
#download.file(url = url, destfile = pkgFile)
# make sure the dependencies coda and rjags are installed
# Install package from downloaded file
#install.packages(pkgs=pkgFile, type="source", repos=NULL)

library(BEST)
library(tidyverse)

# NMI----------------------------

# df_NMI_fine <- df %>% 
#   filter(condition == 'fine')

# df_NMI_mixed <- df %>% 
#   filter(condition == 'mixed')

# df_NMI_coarse <- df %>% 
#   filter(condition == 'coarse')

agg_NMI <- df %>%
  summarize(mean = mean(value),
            sd = sd(value))

grand_mean <- agg_NMI$mean
grand_sd <- agg_NMI$sd

# calculate a region of practical equivalence with zero according to recommendation by Kruschke (2018)
rope <- c(-0.1*grand_sd, 0.1*grand_sd)

priors <- list(muM = grand_mean, muSD = grand_sd)

fine_generic_concepts <- df %>% 
  filter(hierarchy_level == 0, condition == 'fine') %>% 
  select(value, condition)

fine_specific_concepts <- df %>% 
  filter(hierarchy_level == 4, condition == 'fine') %>% 
  select(value, condition)

mixed_generic_concepts <- df %>% 
  filter(hierarchy_level == 0, condition == 'mixed') %>% 
  select(value, condition)

mixed_specific_concepts <- df %>% 
  filter(hierarchy_level == 4, condition == 'mixed') %>% 
  select(value, condition)

coarse_generic_concepts <- df %>% 
  filter(hierarchy_level == 0, condition == 'coarse') %>% 
  select(value, condition)

coarse_specific_concepts <- df %>% 
  filter(hierarchy_level == 4, condition == 'coarse') %>% 
  select(value, condition)

# either load or generate models
#load("BEST_fine_hierarchical.Rda")
#load("BEST_mixed_hierarchical.Rda")
#load("BEST_coarse_hierarchical.Rda")
BEST_fine_hierarchical <- BESTmcmc(fine_specific_concepts$value, fine_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_mixed_hierarchical <- BESTmcmc(mixed_specific_concepts$value, mixed_generic_concepts$value, priors=priors, parallel=TRUE)
BEST_coarse_hierarchical <- BESTmcmc(coarse_specific_concepts$value, coarse_generic_concepts$value, priors=priors, parallel=TRUE)

# check for convergence
print(BEST_fine_hierarchical)
print(BEST_mixed_hierarchical)
print(BEST_coarse_hierarchical)
# -> all models converged

Diff_fine <- (BEST_fine_hierarchical$mu1 - BEST_fine_hierarchical$mu2)
meanDiff_fine <- round(mean(Diff_fine), 2)
hdiDiff_fine <- round(hdi(BEST_fine_hierarchical$mu1 - BEST_fine_hierarchical$mu2),2)
plotAll(BEST_fine_hierarchical)
plot(BEST_fine_hierarchical, ROPE=rope)
summary(BEST_fine_hierarchical)
# CrI does not include 0
# 97.6% probability that the difference in means is larger than 0 (pd)
# 3% in ROPE

Diff_mixed <- (BEST_mixed_hierarchical$mu1 - BEST_mixed_hierarchical$mu2)
meanDiff_mixed <- round(mean(Diff_mixed), 2)
hdiDiff_mixed <- round(hdi(BEST_mixed_hierarchical$mu1 - BEST_mixed_hierarchical$mu2),2)
plotAll(BEST_mixed_hierarchical)
plot(BEST_mixed_hierarchical, ROPE=rope)
summary(BEST_mixed_hierarchical)
# CrI includes 0
# 91.5% probability that the difference in means is smaller than 0 (pd), 
# i.e. negative and large enough
# 19% in ROPE

Diff_coarse <- (BEST_coarse_hierarchical$mu1 - BEST_coarse_hierarchical$mu2)
meanDiff_coarse <- round(mean(Diff_coarse), 2)
hdiDiff_coarse <- round(hdi(BEST_coarse_hierarchical$mu1 - BEST_coarse_hierarchical$mu2), 2)
plotAll(BEST_coarse_hierarchical)
plot(BEST_coarse_hierarchical, ROPE=rope)
summary(BEST_coarse_hierarchical)
# CrI does not include 0
# 100% probability that the difference in means is larger than 0 (pd)
# 0% in ROPE

# save all models for reproducibility
write.csv(BEST_fine_hierarchical, "BEST_fine_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_hierarchical,file="BEST_fine_hierarchical.Rda")
write.csv(BEST_fine_hierarchical, "BEST_mixed_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_fine_hierarchical,file="BEST_mixed_hierarchical.Rda")
write.csv(BEST_coarse_hierarchical, "BEST_coarse_hierarchical.csv", row.names=FALSE, quote=FALSE) 
save(BEST_coarse_hierarchical,file="BEST_coarse_hierarchical.Rda")
