###### Homework 3.1 Analysis

# Loading libraries and data

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

read.table('C:/Users/mirac/Documents/GitHub/econ470_ma/hw5/data/output/acs_medicaid.txt', header=TRUE, sep="\t") -> df


# Question 1 
