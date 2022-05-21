## Do the results qualitatively change if the exclusion criteria of 75% changes?

# Analysis with no exclusions from attention.

df.0 <- raw %>%
  filter(exptPart == "trial") %>%
  filter(!subjID %in% exclude.sampling) %>%
  rename(role = roleCurrent,
         p = probabilityRed,
         k = drawnRed,
         ksay = reportedDrawn) %>%
  mutate(role = ifelse(role == "bullshitter", "sender", "receiver"),
         util = factor(util, levels=c("red", "blue"))) %>%
  select(c(subjID, trialNumber, role, util, p, k, ksay, callBS)) %>%
  mutate(lie = as.numeric(k != ksay))

df.0 %>%
  filter(role == "sender") %>%
  mutate(lie = as.numeric(k != ksay),
         util = paste("player gets points for", util),
         util = factor(util, levels=c("player gets points for red", "player gets points for blue")),
         p = paste("p =", p)) %>%
  ggplot(aes(x=k, y=lie, colour=p, fill=p)) +
  stat_summary(size=0.5, colour="black", geom="linerange", position=position_dodge(width=.3), fun.data="mean_se") + 
  stat_summary(shape=21, size=2, colour="black", geom="point", position=position_dodge(width=.3)) + 
  geom_smooth(method="glm", method.args=list(family="binomial"), se=F, show.legend = FALSE) +
  ggtitle("Experiment 1 Sender's Rate of Lying (0% Criterion)") +
  scale_x_continuous("Truth", breaks=seq(0,10,2)) +
  scale_y_continuous("Rate of Lying", limits=c(-0.05,1), breaks=seq(0,1,0.25)) +
  scale_colour_manual("", values=my_red) +
  scale_fill_manual("", values=my_red) +
  facet_grid(~util) +
  theme_bw() +
  theme(strip.background = element_rect(fill="snow2"),
        strip.text = element_text(size=12, vjust=0),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.position = "bottom")
ggsave("img/0criteria/df0ratelies.pdf", width=8, height=4.5)






expt.S.lies.0 <- df.0 %>%
  filter(role == "sender") %>%
  filter(k != ksay)

length(unique(expt.S.lies.0$subjID))

lies.lm.df.0 <- data.frame()
for(pi in unique(expt.S.lies.0$p)){
  for(u in unique(expt.S.lies.0$util)){
    temp = filter(expt.S.lies.0, p==pi & util==u) %>%
      mutate(k = k-5)
    summ = summary(lmer(ksay ~ k + (1|subjID), data=temp))
    lies.lm.df.0 <- bind_rows(lies.lm.df.0,
                              data.frame(p=paste("p =", pi),
                                         util=paste("player gets points for", u),
                                         beta_0=coef(summ)['(Intercept)','Estimate'],
                                         beta_0_SE=coef(summ)['(Intercept)','Std. Error'],
                                         beta_1=coef(summ)['k','Estimate'],
                                         beta_1_SE=coef(summ)['k','Std. Error'],
                                         beta_1_p=coef(summ)['k','Pr(>|t|)']))
  }
}


lies.lm.df.0 <- lies.lm.df.0 %>%
  mutate(beta_0_txt = sprintf("%04.2f", round(beta_0,2)),
         beta_1_txt = str_pad(round(beta_1,2), 4, side="right", pad="0"),
         lm = paste0("y = ", beta_0_txt, " + ", beta_1_txt, "x"),
         beta_1_p = ifelse(beta_1_p < .0001, "p < 0.0001", paste("p =", round(beta_1_p, 4))),
         util=factor(util, levels=c("player gets points for red", "player gets points for blue")))
lies.lm.df.0

expt.S.lies.0 %>%
  mutate(p=paste("p =", p),
         util=paste("player gets points for", util),
         util=factor(util, levels=c("player gets points for red", "player gets points for blue"))) %>%
  ggplot() +
  geom_jitter(aes(x=k, y=ksay), size=0.5, colour="gray70") +
  stat_summary(aes(x=k, y=ksay), size=0.3) +
  geom_smooth(aes(x=k, y=ksay), method="lm", se=F, colour="darkviolet") +
  geom_text(data=lies.lm.df.0, aes(x=0, y=9.5, label=lm), hjust=0, size=3.5, fontface="bold") +
  geom_vline(xintercept=5, linetype=2, size=0.3, colour="gray40") +
  ggtitle("Experiment 1 Sender's Reported Lie vs Truth (0% Criterion)") +
  scale_x_continuous("Truth", limits=c(-.5, 10.5), breaks=seq(0,10,2)) +
  scale_y_continuous("Reported Lie", breaks=seq(0,10,2)) +
  #scale_colour_manual(values=my_red) +
  facet_grid(util ~ p) +
  theme_bw() +
  theme(strip.background = element_rect(fill="snow2"),
        strip.text=element_text(size=10),
        #panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12))
ggsave("img/0criteria/df0lmlies.pdf", width=8.2, height=5)








humanDetect <- df.0 %>%
  filter(role == "receiver") %>%
  mutate(avgRed = p * 10,
         p.f = relevel(as.factor(p), ref="0.5"))

humanDetect.r <- filter(humanDetect, util == "red")
quadr.fits.r<- data.frame(do.call(rbind, by(humanDetect.r, humanDetect.r$p.f, brutefit))) %>%
  add_rownames(var = "prob") %>%
  mutate(util = "red")

humanDetect.b <- filter(humanDetect, util == "blue")
quadr.fits.b <- data.frame(do.call(rbind, by(humanDetect.b, humanDetect.b$p.f, brutefit))) %>%
  add_rownames(var = "prob") %>%
  mutate(util = "blue")

quadr.fits <- bind_rows(quadr.fits.r, quadr.fits.b)

quadr.est.r <- quadr.fits.r %>%
  select(1:8) %>%
  gather("variable", "estimate", 5:8) %>%
  mutate(variable = gsub("est.", "", variable))
quadr.se.r <- quadr.fits.r %>%
  select(-c(5:8)) %>%
  gather("variable", "std.err", 5:8) %>%
  mutate(variable = gsub("se.", "", variable))
quadr.summ.r <- left_join(quadr.est.r, quadr.se.r, by=c("prob","logL","variable"))

# Plot to double check this worked
quadr.long <- quadr.fits %>%
  pivot_longer(cols = starts_with(c("est", "se"))) %>%
  rename("var" = "name")
quadr.est <- quadr.long %>%
  filter(str_detect(var, "est")) %>%
  mutate(var = str_replace(var, "est.", "")) %>%
  rename("est" = "value")
quadr.se <- quadr.long %>%
  filter(str_detect(var, "se")) %>%
  mutate(var = str_replace(var, "se.", "")) %>%
  rename("se" = "value")
quadr.fits <- left_join(quadr.est, quadr.se)


quadr.fits.c <- quadr.fits %>%
  filter(var == "b") %>%
  mutate(util = ifelse(util == 'red', 'blue', 'red'),
         payoff = paste("Receiver gets\npoints for", util)) %>%
  select(p, payoff, est, se) %>%
  bind_rows(data.frame(p=c(0.2,0.5,0.8), 
                       payoff="null", 
                       est=c(2, 5, 8))) %>%
  mutate(p.fact=paste("p =",p),
         payoff=fct_relevel(payoff, c("Receiver gets\npoints for blue", "null", "Receiver gets\npoints for red")),
         compare=ifelse(payoff=="null", TRUE, FALSE)) %>%
  ggplot(aes(x=est, y=p, pch=payoff)) +
  geom_line(aes(lty=compare)) +
  geom_errorbarh(aes(xmin=est-2*se, xmax=est+2*se), height=0.06) +
  geom_point(aes(fill=p.fact, size=payoff)) +
  ggtitle("c) Receiver's believed mode of true reports") +
  scale_y_continuous("Base Rate Probability", limits=c(0,1), breaks=c(0.2,0.5,0.8), expand=c(0,0)) +
  scale_x_continuous("Receiver's Most Accepted Reports", limits=c(0,10), breaks=seq(0,10,1), expand=c(0,0)) +
  scale_shape_manual(values=c(22,21,24)) +
  scale_size_manual(values=c(2,2.25,2)) +
  scale_linetype_discrete(guide="none") +
  scale_fill_manual(values = my_red) +
  guides(shape=guide_legend(title=""), size=guide_legend(title=""), 
         fill=guide_legend(title="", override.aes = list(shape=21, size=2))) +
  theme_minimal() +
  theme(legend.text = element_text(size=9, hjust=0, margin = margin(r = 0.6, unit = "cm")),
        #legend.position = c(0.562,0.9),
        #legend.direction = "horizontal",
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin=margin(),
        legend.key.size = unit(0.36, 'cm'),
        plot.margin = margin(2,5,2,5),
        plot.title = element_text(size=11.5, face="bold"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=axisTitleSize),
        axis.text = element_text(size=axisTextSize),
        axis.text.x = element_text(margin=marginAxisX),
        axis.text.y = element_text(margin=margin(0,4.5,0,2.5)),
        axis.line = element_line(size=axisLineSize, colour=axisLineColour))


quadr.fits.ab <- df.0 %>%
  filter(role == "receiver") %>%
  mutate(probabilityRed.txt = paste("p =", p),
         utilA = ifelse(util=="red", "a", "b"),
         util = ifelse(util=="red", "blue", "red"),
         util = paste0(utilA, ") Receiver gets points for ", util)) %>%
  group_by(probabilityRed.txt, util, ksay) %>%
  summarise(prop = (sum(callBS)+1)/(n()+2),
            se = sqrt((prop*(1-prop)/n())),
            n = n()) %>%
  filter(n > 3) %>%
  ggplot(aes(x=ksay, y=prop, colour=probabilityRed.txt, fill=probabilityRed.txt, shape=util)) +
  geom_line(size=0.8, alpha=alphaLine) +
  geom_ribbon(aes(ymin=prop-se, ymax=prop+se), alpha=0.3, colour="gray32", size=0.25) +
  geom_point(stroke=0.5, size=2, colour="black", alpha=alphaLine) +
  geom_hline(yintercept=0, size=axisLineSize+0.5, colour=axisLineColour) +
  ggtitle("Experiment 1 Receiver's BS Rate (0% Crit)") +
  scale_x_continuous("Reported Red Marbles", limits=c(0,10), breaks=c(0,2,4,6,8,10), expand=c(0,0)) +
  scale_y_continuous("Receiver Prop. BS Called", limits=c(0,1), expand=c(0,0)) +
  scale_colour_manual(values = my_red, guide="none") +
  scale_fill_manual(values = my_red) +
  scale_shape_manual(values = c(22,24)) +
  facet_wrap(~util, scales='free', nrow=2) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text.x = element_text(hjust = -0.05, size=11.5, face="bold"),
        axis.title = element_text(size=axisTitleSize),
        axis.text = element_text(size=axisTextSize),
        axis.text.x = element_text(margin=marginAxisX),
        axis.line.y = element_line(size=axisLineSize, colour=axisLineColour))

quadr.fits.plot <- plot_grid(quadr.fits.ab, 
                             quadr.fits.c, 
                             nrow=2, rel_heights=c(5.5,4.9))
quadr.fits.plot

ggsave(quadr.fits.plot, filename="img/0criteria/df0detectResults.pdf", height=8, width=4)
