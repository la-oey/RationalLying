# examine lie detecting learning

expt.scores <- raw %>%
  filter(exptPart == "trial") %>%
  filter(!subjID %in% exclude.sampling) %>%
  filter(!subjID %in% exclude.catch) %>%
  rename(role = roleCurrent,
         p = probabilityRed,
         k = drawnRed,
         ksay = reportedDrawn) %>%
  mutate(role = ifelse(role == "bullshitter", "sender", "receiver"),
         util = factor(util, levels=c("red", "blue")))

expt.receiver.bin <- expt.scores %>%
  filter(role == "receiver") %>%
  mutate(util = ifelse(util == "red", "blue", "red"),
         util = paste("Receiver gets points for", util),
         pointAdvantage = playerTrialScore - oppTrialScore,
         p = as.factor(paste0(as.numeric(as.character(p))*100, "% red")),
         trialBin = ceiling(trialNumber/10))
meanReceiverPointsTen <- expt.receiver.bin %>%
  group_by(p, util, subjID, trialBin) %>%
  summarise(meanPts = mean(pointAdvantage))
expt.receiver.bin %>%
  ggplot(aes(x=trialNumber, y=pointAdvantage)) +
  geom_jitter(data=meanReceiverPointsTen, 
              aes(x=10*trialBin-5, y=meanPts, colour=p, fill=p, shape=util),
              size=1.5, width=2, height=0, alpha=0.75) +
  geom_smooth(method="lm", colour="black") +
  scale_x_continuous("Trial Number", breaks=seq(0,100,10)) +
  scale_y_continuous("Receiver's Mean Score Advantage", limits=c(-15, 10)) +
  scale_colour_manual("", values=my_red) +
  scale_fill_manual("", values=my_red) +
  scale_shape_manual(values=c(21,1)) +
  guides(color=F, fill=F, shape=F) +
  theme_bw() +
  theme(strip.text = element_text(size=20),
        strip.background = element_rect(fill="snow2"),
        axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=15, margin = margin(r = 0.8, unit = "cm")),
        legend.key.size = unit(1.5, 'lines'),
        legend.position = 'bottom')
ggsave("img/learning/learnScoreReceiverAggr.pdf", width=8, height=6.5)

# 10-bin
expt.sender.bin <- expt.scores %>%
  filter(role == "sender") %>%
  mutate(trialBin = ceiling(trialNumber/10),
         pointAdvantage = playerTrialScore - oppTrialScore,
         util = paste("Sender gets points for", util),
         util = fct_relevel(util, c("Sender gets points for red", "Sender gets points for blue")),
         p = as.factor(paste0(as.numeric(as.character(p))*100, "% red")))
meanSenderPointsTen <- expt.sender.bin %>%
  group_by(p, util, subjID, trialBin) %>%
  summarise(mean=mean(pointAdvantage))
ann_text <- data.frame(trialNumber=0, pointAdvantage=8.3,
                       lab = "max advantage for ToM",
                       p = as.factor("80% red"),
                       util = factor("Sender gets points for red", 
                                     c("Sender gets points for red", "Sender gets points for blue")))
expt.sender.bin %>%
  ggplot(aes(x=trialNumber, y=pointAdvantage)) +
  geom_jitter(data=meanSenderPointsTen,
              aes(x=10*trialBin-5, y=mean, fill=p, colour=p, shape=util),
              size=1.2, width=2, height=0, alpha=0.6) +
  geom_smooth(method="lm", size=1.2, colour="black") +
  scale_x_continuous("Trial Number", breaks=seq(0,100,10)) +
  scale_y_continuous("Sender's Mean Score Advantage", limits=c(-10, 15)) +
  scale_colour_manual("", values=my_red) +
  scale_fill_manual("", values=my_red) +
  scale_shape_manual(values=c(21,1)) +
  guides(color=F, fill=F, shape=F) +
  theme_bw() +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=15),
        legend.text = element_text(size=15, margin = margin(r = 0.8, unit = "cm")),
        legend.key.size = unit(1.5, 'lines'),
        legend.position = 'bottom')
ggsave("img/learning/learnScoreSenderAggr.pdf", width=8, height=6.5)


contrasts(expt.sender.bin$p) <- contr.treatment(3, base=2)
contrasts(expt.receiver.bin$p) <- contr.treatment(3, base=2)

sender.lmer.full <- lmer(pointAdvantage ~ trialNumber + p + p:util + (1  | subjID), data=expt.sender.bin)
sender.lm.full <- lm(pointAdvantage ~ trialNumber + p + p:util, data=expt.sender.bin)
sender.lmer.notrial <- lmer(pointAdvantage ~ p + p:util + (1 | subjID), data=expt.sender.bin)
anova(sender.lmer.notrial, sender.lmer.full, method="chisq")
round(summary(sender.lmer.full)$coefficients, 4)
round(summary(sender.lm.full)$coefficients, 4)
predict.lm(sender.lm.full, 
           newdata = data.frame(
             trialNumber=rep(100,3),
             p=paste0(c(0.2,0.5,0.8)*100, "% red"),
             expt=rep("Sender gets points for red",3)
           )
)



sender.lmer.aggr <- lmer(pointAdvantage ~ trialNumber + (1 | subjID), data=expt.sender.bin)
summary(sender.lmer.aggr)

receiver.lmer.aggr <- lmer(pointAdvantage ~ trialNumber + (1 | subjID), data=expt.receiver.bin)
summary(receiver.lmer.aggr)





