





poissonAverage = function(input, lambda){
  dims = dim(input)
  d = length(dims)
  ndepths = dims[d]
  p.depth = dpois(0:(ndepths-1), lambda) / ppois(ndepths-1, lambda)
  apply(input*array(rep(p.depth, each = prod(dims[-d])), dim = dims),
        MARGIN = (1:(d-1)),
        FUN = sum)
}

recurseToM.matrix <- function(alph, eta.S, eta.R, util, p, lambda){
  n.depths = qpois(0.95, lambda) # 25
  prior = rep(0.5,11)
  store.ksay.k = array(NA, dim = c(11, 11, n.depths))
  store.ksay.k.simple = array(NA, dim = c(11, 11, n.depths))
  store.bs.ksay = array(NA, dim = c(11, n.depths))
  store.bs.ksay.simple = array(NA, dim = c(11, n.depths))
  for(depth in 1:n.depths){
    if(depth == 1){
      store.bs.ksay[,depth] = prior
      store.bs.ksay.simple[,depth] = prior
    } else {
      marginalized.caller = poissonAverage(store.ksay.k.simple[,,1:(depth-1)], lambda)
      
      store.bs.ksay.simple[,depth] = mapply(p.D_bs.ksay.r,
                                            0:10,
                                            p,
                                            util,
                                            alph,
                                            eta.R,
                                            lastlvl=FALSE,
                                            p_true.ksay(p.k(0:numMarbles, p),
                                                        marginalized.caller)) 
      store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
                                     0:10,
                                     p,
                                     util,
                                     alph,
                                     eta.R,
                                     lastlvl=TRUE,
                                     p_true.ksay(p.k(0:numMarbles, p),
                                                 marginalized.caller)) #mean of previous levels; weigh?
      
    }
    marginalized.liar = poissonAverage(matrix(store.bs.ksay.simple[,1:depth], nrow=numMarbles+1), lambda)
    
    store.ksay.k.simple[,,depth] = p.L_ksay.k.r(util,
                                                alph,
                                                eta.S,
                                                p,
                                                lastlvl=FALSE,
                                                marginalized.liar)
    store.ksay.k[,,depth] = p.L_ksay.k.r(util,
                                         alph,
                                         eta.S,
                                         p,
                                         lastlvl=TRUE,
                                         marginalized.liar)
  }
  return(list(store.bs.ksay, store.ksay.k))
  # [[1]] receiver P(BS | k*)
  # [[2]] sender P(k* | k)
}


lapseRate <- function(matr, weight){
  dims = dim(matr)[1]
  offDiag <- (1-diag(dims))*matr
  regOffDiag <- sweep(offDiag,2, colSums(offDiag),`/`)
  weightedOffDiag = weight*regOffDiag + (1-weight)*1/11
  onDiag <- diag(dims)*matr
  weightedS <- sweep(weightedOffDiag,2, (1-diag(onDiag)),`*`) + onDiag # lapse rate on lies only
  return(weightedS)
}

# k = 11
# M = matrix(runif(k^2), ncol=k)
# M = t(t(M)/colSums(M))


recurseToM.weighted <- function(alph, eta.S, eta.R, util, p, lambda, weight){
  matrices <- recurseToM.matrix(alph, eta.S, eta.R, util, p, lambda)
  n.depths = dim(matrices[[1]])[2] # should be equal to dim(matrices[[2]])[3]
  weightedR <- poissonAverage(matrices[[1]], lambda)
  predS <- poissonAverage(matrices[[2]], lambda)
  weightedS <- lapseRate(predS, weight)
  return(list(weightedR, weightedS))
}


recurseToM.pred <- function(alph, eta.S, eta.R, lambda, weight){
  weight = logitToProb(pmin(8, pmax(-8, weight)))
  lambda = pmax(pmin(exp(lambda), 12),0.1)
  store.ksay.k.full = array(NA, dim = c(11, 11, 6))
  store.bs.ksay.full = array(NA, dim = c(11, 6))
  utils = c(1,-1)
  ps = c(0.2, 0.5, 0.8)
  for(u in 1:length(utils)){
    for(p in 1:length(ps)){
      matr <- recurseToM.weighted(alph, eta.S, eta.R, utils[u], ps[p], lambda, weight)
      store.bs.ksay.full[,(u-1)*length(ps)+p] <- matr[[1]]
      store.ksay.k.full[,,(u-1)*length(ps)+p] <- matr[[2]]
    }
  }
  return(list(store.bs.ksay.full, store.ksay.k.full))
}

# recurseToM.pred(
#   0.2, # recurseToMeval@coef['alph','Estimate'],
#   recurseToMeval@coef['eta.S','Estimate'],
#   0, #recurseToMeval@coef['eta.R','Estimate'],
#   5, #recurseToMeval@coef['lambda','Estimate'],
#   recurseToMeval@coef['weight','Estimate'])[[1]] %>%
#   as_tibble() %>% 
#   mutate(ksay = 0:10) %>% 
#   pivot_longer(-ksay, names_to = 'condition', values_to='probability') %>% 
#   mutate(condition = as.numeric(substr(condition, 2, 10))-1,
#          expt = ifelse(condition < ceiling(max(condition)/2), "red", "blue"),
#          expt = factor(expt, levels=c("red","blue"))) %>%
#   mutate(p = rep(c(0.2, 0.5, 0.8), 22),
#          p = as.factor(p)) %>%
#   select(-condition) %>%
#   relocate(c(expt,p), .before = ksay) %>%
#   arrange(expt, p, ksay) %>%
#   ggplot(aes(x=ksay, y=probability, colour=p)) +
#   geom_point() +
#   geom_line() +
#   scale_colour_manual(values=my_red) +
#   facet_wrap(~expt) +
#   theme_minimal()
