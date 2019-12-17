# rm(list = ls())
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, testthat)
# testthat::test_file("s610f19_final_test.R")

library(testthat)
context("Test edge list partition function")
source("s610f19_final_function.R")
load("judicial.RData")

allcites2 = allcites %>% 
  mutate(citing_year = judicial$year[match(allcites$V1, judicial$caseid)])

edgelist = sample_n(allcites2, 1000)
step = 20

test_that("partition has the correct output as sparse matrix", {
  expect_is(partition_el(edgelist, step), "ngCMatrix")
})

