rm(list = ls())
library(dplyr)
library(ggplot2)
cv <- read.csv("data/main_followup.csv", sep = ",", stringsAsFactors = FALSE)
cv$Strat = factor(cv$Strat, c("CPV","VLC","SVLC","DC"))
str(cv)
head(cv)

#ggplot
p <- ggplot(cv, aes(x = Strat, y = dogs_aver,label = dogs_aver))+
  geom_col(aes(fill = Campaign), width = 0.7, color="white")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_bw()+ 
  labs(x="Strategy",y="Dogs vaccinated")
p + theme(text = element_text(size=16))
ggsave("Followup.pdf", width = 10, height = 10, units ="cm")



# coverage estimates TP1 & TP2
MERGE <- read.csv("data/strategycoverage.csv", sep = ",")
MERGE$Strategy = factor(MERGE$Strategy)
str(MERGE)
MERGE
library(Hmisc)
CI <- binconf(x=MERGE$microchip, n=MERGE$N, method=c("wilson"))
CI <- data.frame(CI)
MERGE2 <- cbind(MERGE, CI)
MERGE2

# convert to % from proportion
MERGE2$Lower <- MERGE2$Lower*100
MERGE2$Upper <- MERGE2$Upper *100
MERGE2$PointEst <- MERGE2$PointEst *100
MERGE2
write.csv(MERGE, "TP1_2_Summary.csv")

MERGE2$Strategy = factor(MERGE2$Strategy, c("CPV","VLC","SVLC","DC"))

# dot plot
p <- ggplot(MERGE2, aes(TIME, MERGE2$PointEst, colour = Strategy))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin=MERGE2$Lower, ymax=MERGE2$Upper), width=.1)+
  ylab("Dogs vaccinated (%)")+
  scale_y_continuous(limits=c(25, 100), 
  breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  theme_bw() +
  scale_x_discrete(name = "Timepoint", 
  breaks=c("TP1", "TP2"), labels=c("1", "2"))+
  theme(axis.text.x  = element_text(size= 12)) +
  theme(legend.key.size = unit(0.5, 'cm'))

p <- p + #theme(legend.position = c(0.865,0.4)) + 
  theme(legend.text = element_text(colour="black", size=16)) + # legend labels size
  theme(legend.title = element_text(colour="black", size=16)) + # legend title size
  guides(shape = guide_legend(override.aes = list(size = 9)))

cols <- c("CPV" = "#BC3C29FF", "VLC" = "#0072B5FF", "SVLC" = "#20854EFF", "DC" = "#7876B1FF")
p + scale_colour_manual(values = cols)+ theme(text = element_text(size=16))

ggsave("data/TP1_TP2_Vacinated_pointplot.pdf", width = 10, height = 10, units ="cm")



#easiness
easiness <- read.csv("data/easiness.csv", sep = ",")

library(Hmisc)
CI <- binconf(x=easiness$response, n=easiness$N, method=c("wilson"))
CI <- data.frame(CI)
easiness1 <- cbind(easiness, CI)
easiness1

# convert to % from proportion
easiness1$Lower <- easiness1$Lower*100
easiness1$Upper <- easiness1$Upper *100
easiness1$PointEst <- easiness1$PointEst *100
easiness1
write.csv(easiness1, "data/easy.csv")

MERGE4 <- read.csv("data/easy.csv", sep = ",")
MERGE4

# barplot
MERGE4$Location <- factor(MERGE4$Location, levels = c("Very easy", "Easy", "Difficult", "Very difficult"))
m <- ggplot(MERGE4, aes(Strategy, PointEst, fill = Location))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values=c("gray25", "violetred4","navyblue", "grey80"))+
  geom_errorbar(aes(Strategy, PointEst, ymin=Lower, ymax=Upper), width=.2, position=position_dodge(.9))+
  ylab("Responded (%)")+
  scale_y_continuous(limits=c(0, 100), breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  theme_bw() +
  theme(axis.text.x  = element_text(size= 9))
m + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())


ggsave("Easiness.pdf", width = 17, height = 14, units ="cm")

#e + theme(legend.key.height = unit(0.1, "cm"))


ggsave("Easiness.pdf1", width = 17, height = 14, units ="cm")

