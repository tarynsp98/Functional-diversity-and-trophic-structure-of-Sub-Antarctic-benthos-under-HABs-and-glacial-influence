#Trophic structure and basal source contributions to benthic communities in Sub-Antarctic fjords

install.packages("ggplot2")
install.packages("reshape2")
install.packages("plyr")
install.packages("ggridges")
install.packages("simmr")
install.packages("R2jags")
install.packages("coda")

library(ggplot2)
library(reshape2)
library(plyr)
library(ggridges)
library(simmr) 


# Load each of the .csv files required for the SIMMR model separately

# Consumer isotope data
mix.data = read.csv("simmrcons.csv", sep = ";")
head(mix.data)

# Basal source data
sources.data <- read.csv("simmrsource.csv", sep = ";")
head(sources.data)

# Trophic Enrichment Factors (TEFs) file (e.g., 3.4‰ by Post)
corrections.data <- read.csv("simmrtef.csv", sep = ";")
head(corrections.data)

# C and N concentration data of sources 
# Exclude this file from the model if concentration data are not available for all sources. In this opportunity, I won't be using it for my thesis.
concs <- read.csv("simmrconc.csv")
head(concs)

# Specify the isotope columns (δ13C and δ15N) for the consumers
mix <- cbind(mix.data$d13C, mix.data$d15N)

# Define consumer groups using the numeric code column
grp <- mix.data$Code

# Enter mean and standard deviation values for the sources
s_names <- as.vector(sources.data$Means)
s_means <- cbind(sources.data$meand13C, sources.data$meand15N)
s_sds   <- cbind(sources.data$sdd13C,   sources.data$sdd15N)

# Enter correction factors (TEFs)
c_means <- cbind(corrections.data$meand13C, corrections.data$meand15N)
c_sds   <- cbind(corrections.data$sdd13C, corrections.data$sdd15N)

# Create the SIMMR input object with all the information (with TEF correction)
simmr_in = simmr_load(mixtures = mix,
                      source_names = s_names,
                      source_means = s_means,
                      source_sds = s_sds,
                      correction_means = c_means,
                      correction_sds = c_sds,
                      group = grp)

# Alternatively, create the SIMMR object without TEF correction
simmr_in = simmr_load(mixtures = mix,
                      source_names = s_names,
                      source_means = s_means,
                      source_sds = s_sds,
                      group = grp)

# Plot the mixing model as a biplot of δ13C vs δ15N
plot(simmr_in, 
     xlab = expression(paste(delta^13, "C (‰)", sep = "")), 
     ylab = expression(paste(delta^15, "N (‰)", sep = "")), 
     title = 'xxxx', 
     group = 1:4)

# Run the Bayesian mixing model (MCMC)
simmr_out <- simmr_mcmc(simmr_in)

# Diagnostics for each group – values close to 1 indicate a good model fit
summary(simmr_out, type = 'diagnostics', group = 1:4)

# Posterior predictive check
post_pred = posterior_predictive(simmr_out)

# Plot of posterior predictive fit – if most points fall within the blue intervals,
# the model fit is considered good
print(post_pred)

# Summary of proportional contributions
summary(simmr_out, type = 'statistics', group = 1:4)

# Quantiles of proportional contributions
summary(simmr_out, type = 'quantiles', group = 1:4)

# Boxplot of source contributions for each group
plot(simmr_out, type = "boxplot", group = 1, title = "xxxxx")
plot(simmr_out, type = "boxplot", group = 2, title = "xxxxx")
plot(simmr_out, type = "boxplot", group = 3, title = "xxxxx")
plot(simmr_out, type = "boxplot", group = 4, title = "xxxxx")