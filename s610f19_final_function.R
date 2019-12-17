rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Matrix, tictoc, doMC)

partition_el = function(edgelist = allcites2, step) {
  el = edgelist %>% 
    filter(citing_year < (step + 1800)) %>% 
    select(-citing_year) %>% 
    data.matrix()
  sparse_mat = sparseMatrix(i = el[, 1], j = el[, 2],
                            dims = c(30288, 30288))
  return(sparse_mat)
}

HITS = function(edgelist = allcites2, step) { 
  adj = partition_el(edgelist, step)
  nodes = dim(adj)[1]
  auth = c(rep(1, nodes))
  hub = c(rep(1, nodes))
  result = data.frame(caseid = integer(0),
                      step = integer(0),
                      auth = numeric(0),
                      hub = numeric(0)
                      )
  
  for (i in 1:step) {
    t_adj = t(adj) 
    auth = t_adj %*% hub 
    hub = adj %*% auth 
    sum_sq_auth = sum(auth * auth) 
    sum_sq_hub = sum(hub * hub) 
    auth = auth / sqrt(sum_sq_auth) 
    hub = hub/sqrt(sum_sq_hub)
  }
  
  temp = data.frame(
      caseid = seq(1, nodes), 
      step = rep(step, nodes), 
      auth = as.matrix(auth), 
      hub = as.matrix(hub)
    )
  result = rbind(result, temp)
  return(result) 
}

hits_parallel = function(n_vec, edgelist) {
  plyr::ldply(n_vec, function(n) {
    plyr::ldply(n, function(x) HITS(edgelist, x))
  }, .parallel = TRUE)
}
