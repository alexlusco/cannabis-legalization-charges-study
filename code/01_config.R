###########################
# Author:
# Date: February 17, 2026
# Description: R script to load and install necessary libraries
# Notes: 
#   - pacman needs to be installed before script can run
###########################

# script starts here

# install.packages("pacman")
pacman::p_load(readr, readxl, dplyr, tibble, tidyr, stringr, ggplot2, gt, webshot2, effsize, MASS, clipr, ggthemes)
