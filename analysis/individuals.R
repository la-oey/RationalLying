library(ggExtra)

## Individual-level analysis of senders and receivers in expt 1

indiv.rates <- data.frame()
for(s in unique(expt.S.full$subjID)){
  temp.rate = filter(expt.S.full, subjID == s)
  m.rate <- summary(glm(lie ~ k, family=binomial(), data=temp.rate))
  
  temp.lie = expt.S.full %>%
    filter(subjID == s, k != ksay) %>%
    mutate(k = k - 5)
  b0.est = NA
  b0.se = NA
  b1.est = NA
  b1.p = NA
  if(nrow(temp.lie) > 0){
    tryCatch({
      m.lie <- summary(lm(ksay ~ k, data=temp.lie))
      b0.est = coef(m.lie)['(Intercept)','Estimate']
      b0.se = coef(m.lie)['(Intercept)','Std. Error']
      b1.est = coef(m.lie)['k','Estimate']
      b1.p = coef(m.lie)['k','Pr(>|t|)']
    }, error = function(e){
      message(e)
    })
  }
  
  indiv.rates = indiv.rates %>%
    bind_rows(data.frame(
      subjID = s,
      util = unique(temp.rate$util),
      p = unique(temp.rate$p),
      rate.k.est = coef(m.rate)['k','Estimate'],
      rate.k.z = coef(m.rate)['k','z value'],
      rate.k.p = coef(m.rate)['k','Pr(>|z|)'],
      lie.b0.est = b0.est,
      lie.b0.se = b0.se,
      lie.b1.est = b1.est,
      lie.b1.p = b1.p
    ))
}
indiv.rates %>%
  mutate(rate.k.est = round(rate.k.est, 2),
         rate.k.z = round(rate.k.z, 2),
         rate.k.p = round(rate.k.p, 4))
indiv.rates %>%
  filter(rate.k.est >= -4.2 & rate.k.est <= 4.2) %>%
  filter(lie.b0.est >= 0 & lie.b0.est <= 10) %>%
  nrow()

indiv.rates %>%
  mutate(p = paste("p =", p)) %>%
  ggplot(aes(x=rate.k.est, y=lie.b0.est, colour=p)) +
  geom_point(size=0.5) +
  stat_ellipse(size=0.6) +
  scale_x_continuous("Slope of lying rate", limits=c(-4.2, 4.2)) +
  scale_y_continuous("Lie Size", limits=c(0,10)) +
  scale_colour_manual("",values=my_red) +
  facet_wrap(~util) +
  theme_bw()
ggsave("img/individuals/expt1params.pdf")





indivs <- expt.S.full %>%
  distinct(subjID, util, p)

indiv.lieprop <- expt.S.full %>%
  count(subjID, lie) %>%
  complete(subjID, lie, fill=list(n=0)) %>%
  group_by(subjID) %>%
  mutate(lieprop = n/50) %>%
  filter(lie == 1) %>%
  select(-c(lie,n))

indiv.bsprop <- expt.R.full %>%
  count(subjID, callBS) %>%
  complete(subjID, callBS, fill=list(n=0)) %>%
  group_by(subjID) %>%
  mutate(bsprop = n/50) %>%
  filter(callBS) %>%
  select(-c(callBS,n))

indiv.comb <- indivs %>%
  left_join(indiv.lieprop, by=c("subjID")) %>%
  left_join(indiv.bsprop, by=c("subjID")) %>%
  mutate(p = factor(paste("p =", p)),
         util = paste("sender gets points for", util),
         util = factor(util, levels=c("sender gets points for red",
                                      "sender gets points for blue")))
plot.indiv.lie.bs <- ggplot(indiv.comb, aes(x=lieprop, y=bsprop, shape=util, colour=p)) +
  geom_jitter() +
  geom_smooth(aes(shape=NULL), colour="black", method="lm", se=F) +
  scale_x_continuous("Individual Subject's Proportion of Lying") +
  scale_y_continuous("Individual Subject's Proportion of Calling BS", limits=c(0,1)) +
  scale_shape_manual("", values=c(16,21)) +
  scale_colour_manual("", values=my_red) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin=margin())
plot.margins <- ggMarginal(plot.indiv.lie.bs, type="histogram")
ggsave("img/individuals/lying_vs_bs_rate.pdf", plot.margins)






# Within subject analysis E2
df.e2.indiv <- df.e2 %>%
  mutate(p.s = (p.s - 0.5)/0.3, # (-1, 0, +1)
         p.r = (p.r - 0.5)/0.3)

df.e2.indiv.s <- filter(df.e2.indiv, role=="Sender")
df.e2.indiv.r <- filter(df.e2.indiv, role=="Receiver")

e2.indiv.s <- data.frame()
for(s in unique(df.e2$subjID)){
  temp.s = filter(df.e2.indiv.s, subjID==s, k != ksay)
  p.interc.est = NA
  p.s.est = NA
  p.r.est = NA
  p.r.t = NA
  p.r.p = NA
  if(nrow(temp.s) > 0){
    tryCatch({
      m <- summary(lm(ksay ~ p.r + p.s, data=temp.s))
      p.interc.est = coef(m)['(Intercept)','Estimate']
      p.s.est = coef(m)['p.s','Estimate']
      p.r.est = coef(m)['p.r','Estimate']
      p.r.t = coef(m)['p.r','t value']
      p.r.p = coef(m)['p.r','Pr(>|t|)']
      
    }, error = function(e){
      message(e)
    })
  }
  
  e2.indiv.s <- e2.indiv.s %>%
    bind_rows(
      data.frame(
        subjID = s,
        n.lies = nrow(temp.s),
        p.interc.est = p.interc.est,
        p.s.est = p.s.est,
        p.r.est = p.r.est,
        p.r.t = p.r.t,
        p.r.p = p.r.p
      )
    )
}
nrow(e2.indiv.s)
e2.indiv.s %>%
  filter(n.lies < 4) %>%
  nrow()
e2.indiv.s %>%
  filter(n.lies >= 4) %>%
  mutate(r.positive = p.r.est > 0) %>%
  summarise(sum(r.positive) / n())

e2.indiv.s %>%
  filter(n.lies >= 4) %>%
  mutate(r.signif.positive = p.r.est > 0 & p.r.p < 0.05) %>%
  summarise(sum(r.signif.positive) / n())

e2.indiv.s %>%
  filter(n.lies >= 4) %>%
  nrow()
  mutate(r.signif.positive = p.r.est < 0 & p.r.p < 0.05) %>%
  summarise(sum(r.signif.positive) / n())
