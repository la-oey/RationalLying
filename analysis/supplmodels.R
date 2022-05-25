# setwd("/Users/loey/Desktop/Research/RationalLying/analysis")

expt.S <- expt.S.full %>%
  group_by(util, p, k, ksay) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(util, p, k, ksay, fill=list(n=0)) %>%
  group_by(util, p, k) %>%
  mutate(probability = n / sum(n),
         probTxt = paste0(round(probability*100), "%"))

# functions

ToMToTibble <- function(df){
  df %>%
    as_tibble() %>% 
    mutate(ksay = 0:10) %>% 
    pivot_longer(-ksay, names_to = 'k', values_to='probability') %>% 
    mutate(k = as.numeric(substr(k, 2, 10))-1,
           util = ifelse(k < ceiling(max(k)/2), "red", "blue"),
           util = factor(util, levels=c("red","blue"))) %>%
    relocate(k, .before = ksay) %>%
    arrange(k, ksay) %>%
    mutate(p = rep(rep(c(0.2, 0.5, 0.8), each=121),2),
           p = as.factor(p),
           k = k %% 11) %>%
    relocate(c(util,p), .before = k) %>%
    arrange(util, p, k, ksay) %>%
    mutate(probTxt = paste0(round(probability*100),"%"))
}

heurToTibble <- function(df){
  df %>%
    as_tibble() %>% 
    mutate(ksay = rep(0:10, 11)) %>%
    pivot_longer(-ksay, names_to = 'condition', values_to='probability') %>%
    mutate(condition = as.numeric(substr(condition, 2, 10))-1,
           util = ifelse(condition < ceiling(max(condition)/2), "red", "blue"),
           util = factor(util, levels=c("red","blue")),
           p = condition %% 3,
           p = as.factor(0.2 + 0.3*p),
           k = rep(0:10, each=66),
           probTxt = paste0(round(probability*100), "%")) %>%
    select(-condition) %>%
    relocate(c(util, p, k), .before = ksay) %>%
    arrange(util, p, k, ksay) %>%
    mutate(probTxt = paste0(round(probability*100),"%"))
}

# models

recurse.S.pred.df <- recurseToM.pred(
  0.5, # recurseToMeval@coef['alph','Estimate'], # 
  0, # recurseToMeval@coef['eta.S','Estimate'], # 
  recurseToMeval@coef['eta.R','Estimate'],
  recurseToMeval@coef['lambda','Estimate'],
  recurseToMeval@coef['weight','Estimate'])[[2]] %>%
  ToMToTibble()

noToM.S.pred.df <- noToM.s.pred(
  0.5, # noToMeval@coef['alph','Estimate'], # 
  0, # noToMeval@coef['eta.S','Estimate'], # 
  probToLogit(0.99)) %>% # noToMeval@coef['weight','Estimate']) %>%
  ToMToTibble()

everybodyLies.S.pred.df <- everybodyLies.pred(
  everybodyLiesEval@coef['lambda','Estimate'],
  everybodyLiesEval@coef['weight','Estimate']
) %>% 
  heurToTibble()

someLies.S.pred.df <- someLies.pred(
  somePeopleLieEval@coef['pTrue','Estimate'], 
  somePeopleLieEval@coef['lambda','Estimate'],
  somePeopleLieEval@coef['weight','Estimate']
) %>%
  heurToTibble()

# Combine model predictions + expt results

all.sender <- expt.S %>%
  select(util, p, k, ksay, probability, probTxt) %>%
  mutate(type="human results",
         p = as.factor(p)) %>%
  bind_rows(mutate(recurse.S.pred.df, type="recursive ToM"),
            mutate(noToM.S.pred.df, type="0th order ToM"),
            mutate(everybodyLies.S.pred.df, type="everybody lies"),
            mutate(someLies.S.pred.df, type="some people lie")) %>%
  mutate(type = factor(type, 
                       levels=c("everybody lies","some people lie","0th order ToM","recursive ToM","human results")))

model_labels <- setNames(c("'everybody lies'","'some people lie'","0^th*' order ToM'","'recursive ToM'","'human results'"),
                         levels(all.sender$type))

row1 <- all.sender %>%
  filter(type != "human results") %>%
  filter(util=="red" & p=="0.5") %>%
  mutate(type = as.character(type),
         type = case_when(
           type == "everybody lies" ~ "eq. intrin. avers.",
           type == "some people lie" ~ "uneq. intrin. avers.",
           TRUE ~ type),
         type = factor(type,
                       c("eq. intrin. avers.",
                         "uneq. intrin. avers.",
                         "0th order ToM",
                         "recursive ToM"))
  ) %>%
  ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
  geom_tile() +
  #geom_text(size=2) +
  scale_x_continuous("", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_y_continuous("Reported", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_fill_gradient2("Prob. Report\nGiven Truth", low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1), labels=c("0%","25%","50%","75%","100%")) +
  facet_grid(.~type) +
  guides(fill = FALSE) +
  theme_bw() +
  theme(strip.background = element_rect(fill="snow2"),
        strip.text = element_text(size=11, vjust=0),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))


row2 <- all.sender %>%
  filter(type == "human results") %>%
  filter(util=="red" & p=="0.5") %>%
  ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
  geom_tile() +
  geom_text(size=4) +
  scale_x_continuous("Truth", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_y_continuous("Reported", expand=c(0,0), breaks=seq(0,10,2)) +
  scale_fill_gradient2("Prob. Report\nGiven Truth", low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1), labels=c("0%","25%","50%","75%","100%")) +
  facet_grid(.~type) +
  theme_bw() +
  theme(strip.background = element_rect(fill="snow2"),
        strip.text = element_text(size=11, vjust=0),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8))

tileLegend <- get_legend(row2)


modelLabel1 <- ggdraw() +
  draw_label("Models", size=12, x=0.2, y=0.54, hjust=0) +
  draw_line(x=c(0.1,0.1), y=c(0.25,0.79), size=1)


full_tile <- plot_grid(row1, modelLabel1,
                       row2 + guides(fill=FALSE), tileLegend,
                       nrow=2, ncol=2, 
                       rel_heights=c(30, 70), rel_widths=c(85, 15))

ggsave("img/allpredictions.pdf", full_tile, width=7, height=7)
