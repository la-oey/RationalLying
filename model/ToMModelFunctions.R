

numMarbles = 10 # total # of marbles sampled each trial
KSAY = matrix(rep(0:numMarbles,numMarbles+1),nrow=numMarbles+1) # matrix: columns 0-10
K = matrix(rep(0:numMarbles, each=numMarbles+1), nrow=numMarbles+1) # matrix: rows 0-10
BET = 0.8 # in prior, BET = weighting of binomial sampling distribution; 1-BET = uniform sampling
liePenalty = 10 # points sender loses for getting caught in a lie
faPenalty = 5 # penalty on top of receiver's points for false accusations


softmax <- function(alph, allEV) { ## allEV = vector of numerics
  aev = exp(alph * allEV)
  return(aev/sum(aev))
}

p.k <- function(k, p) { # prior probability of k (given p)
  BET*dbinom(k, numMarbles, p) + (1-BET)*1/(numMarbles+1) # mixed distribution - mostly binomial but some uniform
}




## lie = T/F sender reported a lie
## BS = T/F receiver called BS
## utilStr(ucture) = [-1, +1] "pts for red" = +1, "pts for blue" = -1
## eta.L = "aversion penalty" when sender *lies* applied after player evaluates opponent's behavior (last level)
## eta.D = "aversion penalty" when receiver *calls BS* applied after player evaluates opponent's behavior (last level)
## lastLvl = player chooses action based on opponent's marginalized likely actions

# sender's (liar's) utility
u.L <- function(ksay, lie, BS, utilStr, eta.L, lastLvl=FALSE) {
  utility = case_when(
    !BS ~ utilStr*2*ksay - utilStr*10, # if R doesn't call BS, S gets points for reported red/blue
    !lie ~ utilStr*2*ksay - utilStr*10 + faPenalty, # if R calls BS BUT S tells the truth, S gets points for reported + false alarm
    lie ~ -liePenalty # if R calls BS AND S lies, S loses points for getting caught
  )
  return(ifelse(lie & lastLvl, utility - eta.L, utility)) # if last lvl AND S lied, applied S aversion penalty
}

# receiver's (detector's) utility
u.D <- function(ksay, lie, BS, utilStr, eta.D, lastLvl=FALSE) {
  utility = case_when(
    !BS ~ utilStr*10 - utilStr*2*ksay,
    !lie ~ utilStr*10 - utilStr*2*ksay - faPenalty ,
    lie ~ liePenalty
  )
  return(ifelse(BS & lastLvl, utility - eta.D, utility))
}


EV.D_bs.ksay.r <- function(ksay, p, bs, util, eta.D, lastlvl=FALSE, p.L) { ## both are vectors
  u.D(ksay, lie=TRUE, BS=bs, util, eta.D, lastLvl=lastlvl) * p.L + 
    u.D(ksay, lie=FALSE, BS=bs, util, eta.D, lastLvl=lastlvl) * (1-p.L)
}
### test
### EV.D_bs.ksay.r(0:10, 0.5, TRUE, 1, 7, TRUE, rep(0.5,11))
### EV.D_bs.ksay.r(0:10, 0.5, TRUE, 1, 7, TRUE, mapply(p_t.ksay.r, 0:10, 0.5, rep(0.5,11)))

p.D_bs.ksay.r <- function(ksay, p, util, alph, eta.D, lastlvl=FALSE, p.L) {
  EV.BS <- EV.D_bs.ksay.r(ksay, p, TRUE, util, eta.D, lastlvl=lastlvl, p.L)
  EV.noBS <- EV.D_bs.ksay.r(ksay, p, FALSE, util, eta.D, lastlvl=lastlvl, p.L)
  softmax(alph, c(EV.BS, EV.noBS))[1]
}
### test
### mapply(p.D_bs.ksay.r, 0:10, 0.5, 1, 0.25, 7, FALSE, 0.4)
### mapply(p.D_bs.ksay.r, 0:10, 0.5, 1, 0.25, 7, FALSE, p_t.ksay.r(1, 0.25, 2, 0.5, rep(0.1,11)))

EV.L_ksay.k.r <- function(k, ksay, util, eta.L, p, lastlvl=FALSE, p.D) {
  mapply(u.L, ksay, lie=ksay!=k, BS=TRUE, util, eta.L, lastLvl=lastlvl) * p.D + 
    mapply(u.L, ksay, lie=ksay!=k, BS=FALSE, util, eta.L, lastLvl=lastlvl) * (1-p.D)
}
EV.L_ksay.k.r(5, 0:10, 1, 7, 0.5, lastlvl=F, 0.5)
### test
### mapply(function(i) EV.L_ksay.k.r(i, 0:10, 1, 7, 0.5, lastlvl=F, rep(0.5,11)), 0:10)


p.L_ksay.k.r <- function(util, alph, eta.L, p, lastlvl=FALSE, p.D) { #look into this
  EV.all <- mapply(
    function(i) EV.L_ksay.k.r(i, 0:numMarbles, util, eta.L, p, lastlvl=lastlvl, p.D), 
    0:numMarbles)
  apply(EV.all,2,softmax, alph)
}
### test
### round(p.L_ksay.k.r(1, 1, 7, 0.5, TRUE, rep(0.5,11)),5)
### round(p.L_ksay.k.r(0.5, rep(0.5,11)) * .8 + 1 / length(0:numMarbles) *.2,4)

p_true.ksay <- function(prob.k, prob.ksay.k) {
  P.K <- matrix(rep(prob.k, each=numMarbles+1), nrow=numMarbles+1)
  LIE = 1-diag(numMarbles+1)
  rowSums(P.K*prob.ksay.k*LIE)/rowSums(P.K*prob.ksay.k)
}





  ################
 ##### test #####
################

n.depths = 40
store.ksay.k = array(NA, dim = c(11, 11, n.depths))
store.bs.ksay = array(NA, dim = c(11, n.depths))

prior = rep(0.1,11)
util = 1
p = 0.5
eta.L = 1
eta.D = 1
alph = 0.5

for(depth in 1:n.depths){
  if(depth == 1){
    store.bs.ksay[,depth] = prior
  } else if(depth == 2) {
    
    store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
           0:10, 
           p, 
           util, 
           alph, 
           eta.D, 
           lastlvl=depth==n.depths,
           p_true.ksay(p.k(0:numMarbles, p), 
                       store.ksay.k[,,depth-1]))
  } else {
    store.bs.ksay[,depth] = mapply(p.D_bs.ksay.r,
                                   0:10, 
                                   p, 
                                   util, 
                                   alph, 
                                   eta.D, 
                                   lastlvl=depth==n.depths,
                                   p_true.ksay(p.k(0:numMarbles, p), 
                                               apply(store.ksay.k[,,1:(depth-1)], MARGIN = c(1, 2), FUN = mean)))
    
  }
   
  store.ksay.k[,,depth] = p.L_ksay.k.r(util, 
                                       alph, 
                                       eta.L, 
                                       p, 
                                       lastlvl=depth==n.depths, 
                                       apply(matrix(store.bs.ksay[,1:depth], nrow=numMarbles+1), MARGIN = 1, FUN = mean))
                                       #store.bs.ksay[,depth])
}

# store.ksay.k[,,18] %>% as_tibble() %>% 
#   mutate(idx = 0:10) %>% 
#   pivot_longer(-idx, names_to = 'column', values_to='probability') %>% 
#   mutate(column = as.numeric(substr(column, 2, 10))-1) %>% 
#   ggplot(aes(x=column, y=idx, fill=probability))+
#   geom_tile()+
#   xlab('k')+ylab('ksay')
# 
# plot(store.bs.ksay[,35])

store.ksay.k[,,1:10] %>%
  as_tibble() %>%
  mutate(idx = 0:10) %>%
  pivot_longer(-idx, names_to='column', values_to='probability') %>%
  mutate(column = as.numeric(substr(column, 2, 10)),
         depth = ceiling(column / 11),
         column = (column - 1) %% 11) %>%
  rename(k = column,
         ksay = idx) %>%
  ggplot(aes(x=k, y=ksay, fill=probability))+
  geom_tile()+
  scale_x_continuous("true roll", breaks=seq(0,10,2), expand=c(0,0)) +
  scale_y_continuous("reported roll", breaks=seq(0,10,2), expand=c(0,0)) +
  scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
  facet_wrap(~depth, nrow=1) +
  theme_bw()
