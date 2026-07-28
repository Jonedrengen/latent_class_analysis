#load libs
library(R2jags)
library(rjags)
load.module("glm")
library(label.switching)
library(ggplot2)
library(optparse)
library(grDevices)
library(testthat)

source()

#parse args
opt_lst <- list(
    make_option(c("-l","--input"),
              help = "please provide input"),
    make_option(c("-o","--output"),
              help = "please provide output location") 
)
