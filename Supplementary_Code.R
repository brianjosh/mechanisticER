#Code supporting article: "A mechanistic framework of enemy release", Ecology Letters
#Joshua I. Brian, Jane A. Catford

library(tidyverse)
library(car)
library(ggpubr)

--------------------------------------------------------------------------------
  
#Re-analysis of Xu et al. 2021

#Read in data and logit-transform
alldata <- read.csv("Supplementary_Data_1.csv", header=T, stringsAsFactors = T)
alldata <- alldata %>%
  mutate(logitherbivory = log((herbivory/100)/(1-(herbivory/100)))) %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
str(alldata)

#Overall average for native vs. exotic
aggregate(alldata$logitherbivory, list(alldata$origin), mean)
#Averages by enemy type
aggregate(alldata$logitherbivory, list(alldata$origin, alldata$enemy), mean)

#Test these differences statistically

#Overall test for exotic vs. native damage
mod1 <- lm(logitherbivory ~ origin, data=alldata)
Anova(mod1, type="III")
summary(mod1)

#Exotic vs. native damage accounting for type of enemy
mod2 <- lmer(logitherbivory ~ origin + (1|enemy),
             data=alldata)
Anova(mod2, type="III")
summary(mod2)

#Plot results (Fig. 6)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#Across all enemies
overall <- ggplot(alldata, aes(x=origin, y=logitherbivory, color=origin)) +
  geom_jitter(position=position_jitter(0.25), alpha=0.2) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), color="red", size=0.8, shape=17) +
  scale_color_manual(values=c("#999999", "#E69F00")) + 
  xlab("Origin") + ylab("Annual herbivory rate (logit-transformed)") +
  theme_bw() + 
  theme(axis.title.x=element_blank()) 

#Split by enemy type

enemytype <- ggplot(alldata, aes(x=enemy, y=logitherbivory, color=origin, group=origin)) +
  geom_jitter(position=position_jitterdodge(jitter.width=0.4, dodge.width=0.8), alpha=0.2) +
  stat_summary(fun.data="mean_sdl",  fun.args = list(mult=1), 
               color="red", size=0.5, shape=17, position=position_dodge(0.8)) +
  scale_color_manual(values=c("#999999", "#E69F00")) + 
  xlab("Enemy type") + ylab("Annual herbivory rate (logit-transformed)") +
  theme_bw() +
  theme(legend.position = 'none')

overallfig <- ggarrange(overall, enemytype, nrow=2, labels=c("A", "B"))
ggsave("Xu reanalysis.svg", width=120, height=180, units="mm")

--------------------------------------------------------------------------------
  
#Make base plot for Fig. 2
#Deeply inelegant method but gets the clear red gradient for Fig. 2
  
#Important to follow this order
  
a <- seq(from=0.1, to=1, by=0.01)
b <- seq(from=0.1, to=1, by=0.01)
acolumn <- NULL 
bcolumn <- NULL
abproduct <- NULL
for(i in b){
  acolumn <- c(acolumn, a)
  bcolumn <- c(bcolumn, rep(i, 91))
  abproduct <- c(abproduct, a*i)
}
abheatdata <- data.frame(acolumn, bcolumn, abproduct)

#now re-assign...
a <- seq(from=1, to=10, by=0.1)
b <- seq(from=1, to=10, by=0.1)
acolumn <- NULL 
bcolumn <- NULL
abproduct <- NULL
for(i in b){
    acolumn <- c(acolumn, a)
  bcolumn <- c(bcolumn, rep(i, 91))
  abproduct <- c(abproduct, a*i)
}
cdheatdata <- data.frame(acolumn, bcolumn, abproduct)

#now re-assign for a third time..

a <- seq(from=1, to=10, by=0.1)
b <- seq(from=0.1, to=1, by=0.01)
acolumn <- NULL 
bcolumn <- NULL
abproduct <- NULL
for(i in b){
  acolumn <- c(acolumn, a)
  bcolumn <- c(bcolumn, rep(i, 91))
  abproduct <- c(abproduct, a*i)
}
efheatdata <- data.frame(acolumn, bcolumn, abproduct)

#now re-assign for a fourth time..

a <- seq(from=0.1, to=1, by=0.01)
b <- seq(from=1, to=10, by=0.1)
acolumn <- NULL 
bcolumn <- NULL
abproduct <- NULL
for(i in b){
  acolumn <- c(acolumn, a)
  bcolumn <- c(bcolumn, rep(i, 91))
  abproduct <- c(abproduct, a*i)
}
ghheatdata <- data.frame(acolumn, bcolumn, abproduct)

revisedheatdata <- bind_rows(abheatdata, cdheatdata, efheatdata, ghheatdata)
revisedheatdata$acolumn <- as.factor(revisedheatdata$acolumn)
revisedheatdata$bcolumn <- as.factor(revisedheatdata$bcolumn)
revisedheatdata$logabproduct <- log(revisedheatdata$abproduct)

heatmap <- ggplot(data = revisedheatdata, mapping = aes(x = acolumn, y = bcolumn, fill = logabproduct)) +
  geom_raster() + theme_classic() + scale_fill_gradient(low="white", high="red") +
  geom_abline(intercept = 182, slope=-1, colour="yellow", alpha=0.4, size=1.2) + 
  geom_hline(yintercept=91, colour="yellow", linetype="dashed", alpha=0.4, size=1.2) + 
  geom_vline(xintercept=91, colour="yellow", linetype="dashed", alpha=0.4, size=1.2) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) + theme(legend.title = element_blank())
ggsave("heatmap.svg", width=200, height=180, units="mm")
  
  