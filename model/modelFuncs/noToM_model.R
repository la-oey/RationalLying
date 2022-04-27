#source("ToMModelFunctions.R") # load sender and receiver rational actions


noToM.lapseRate <- function(matr, weight){
  predS = array(matr, dim=c(11,11,6))
  offDiag <- array(1-diag(11), dim=c(11,11,6))*predS
  offDiagSums <- colSums(offDiag)
  regOffDiag <- sweep(offDiag, MARGIN=c(2,3), offDiagSums,`/`)
  weightedOffDiag <- weight*regOffDiag + (1-weight)*1/11
  onDiag <- array(diag(11), dim=c(11,11,6))*predS
  weightedS <- sweep(weightedOffDiag, MARGIN=c(2,3), offDiagSums,`*`) + onDiag # lapse rate when lying only
  return(weightedS)
}

noToM.s.pred <- function(alph, eta.S, weight){
  weight = logitToProb(pmin(10, pmax(-10, weight)))
  predS = mapply(function(i, j){
    p.L_ksay.k.r(j, alph, eta.S, i, lastlvl=TRUE, rep(0.5,11))
    }, 
    rep(c(0.2,0.5,0.8), 2), rep(c(1,-1), each=3))
  noToM.lapseRate(predS, weight)
}


p_t.ksay.r_p.L <- function(p, p.L) { #probability of not telling the truth
  P.K <- matrix(rep(p.k(0:numMarbles, p), each=numMarbles+1), nrow=numMarbles+1)
  P.L_KSAY.K <- matrix(p.L, nrow=numMarbles+1, ncol=numMarbles+1)
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}

null0.2 <- p_true.ksay(p.k(0:numMarbles, 0.2),
                       matrix(rep(1/11,121), nrow=numMarbles+1, ncol=numMarbles+1))
null0.5 <- p_true.ksay(p.k(0:numMarbles, 0.5),
                       matrix(rep(1/11,121), nrow=numMarbles+1, ncol=numMarbles+1))
null0.8 <- p_true.ksay(p.k(0:numMarbles, 0.8), 
                       matrix(rep(1/11,121), nrow=numMarbles+1, ncol=numMarbles+1))

nullVec <- function(br, kstar){
  case_when(
    br == 0.2 ~ null0.2[kstar+1],
    br == 0.5 ~ null0.5[kstar+1],
    br == 0.8 ~ null0.8[kstar+1]
  )
}


noToM.r.pred <- function(alph, eta.R){
  matrix(
    mapply(
      function(i,j,k) p.D_bs.ksay.r(i, j, k, alph, eta.R, lastlvl=TRUE, nullVec(j, i)), 
      rep(0:10,6), 
      rep(rep(c(0.2,0.5,0.8),each=11),2), 
      rep(c(1,-1), each=33)
    ),
    nrow=11
  )
}

# noToM.r.pred(0.2, 0) %>%
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

