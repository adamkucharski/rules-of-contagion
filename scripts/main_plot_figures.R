# Plotting code to accompany "Rules of Contagion"
# Author: AJ Kucharski (2020)

library(lubridate)
library(deSolve)
library(pracma)
library(extrafont)
library(igraph)
library(tidyverse)
library(anytime)
library(cholera)

rm(list=ls()) # Clear workspace

setwd("~/Documents/GitHub/rules-of-contagion/") # Set local directory

source("R/plotting_functions.R") # Load plotting functions

# Set colours
col.list = list(rgb(0,0,0),rgb(0.35,0.35,0.35),rgb(0.7,0.7,0.7),rgb(0.5,0.5,0.5))
network.color= rgb(0.2,0.2,0.2)

# Set output parameters
width.main=6
height.main=3
height.wide=2
height.square=5

# Chapter 1 ---------------------------------------------------------------

# Plot independent happening curve and housing data
C1_independent()

# Plot Ross model S-curve and VCR data
C1_s_curve()

# Plot curve with exponential growth throughout
C1_s_curve_exponential_limit()

# Plot basic SIR model with influenza parameters
C1_SIR_model()

# Plot Zika in French Polynesia
C1_zika_data()

# Plot SIR model with plague data
C1_plague_data()

# Chapter 2 ---------------------------------------------------------------

# Plot South Sea Bubble data
C2_south_sea()

# Plot R0 with and without vaccination
C2_generations()

# Plot common source Typhoid outbreak
C2_common_source()

# Plot Erdos-Renyi networks
C2_erdos_renyi()

# Chapter 3 ---------------------------------------------------------------

# Plot 2009 influenza pandemic in the UK (also output Introduction plot)
C3_pandemic_2009()

# Chapter 4 ---------------------------------------------------------------

# Calculate ratio of deaths in the Crimea
C4_crimea()

# Estimate shooting superspreading in Chicago
C4_gun_chain_distribution()

# Simulate and plot 50 shootings with Chicago parameters
C4_gun_network_simulated()

# Plot 1854 cholera outbreak
C4_cholera_timeseries()

# Plot diphtheria outbreak delays
C4_diphtheria()

# Plot 1976 Ebola outbreak
C4_ebola_1976()

# Chapter 5 ---------------------------------------------------------------

# Plot Higgs retweet network
C5_higgs_network()

# Plot Royal Institution YouTube views for 'The Perfect Bet'
C5_youtube()


