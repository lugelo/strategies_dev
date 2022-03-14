rm(list = ls())
library(dplyr)
library(ggplot2)
cv <- read.csv("main_followup.csv", sep = ",", stringsAsFactors = FALSE)
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
p

p + theme(legend.position = c(0.91,0.91))



BS <- read.csv("bystrategy.csv", sep = ",")
BS
str(BS)
library(ggplot2)
pdf(p.pdf)
p <- ggplot(data =BS, aes(x = method, y = proportion,
                         fill = factor(method))) + 
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c("gray25", "violetred4", "grey80"))+ 
  theme_bw() + 
  theme(legend.position = "none")+
  theme(axis.line = element_line(size=0.5, colour = "black"),
  panel.grid.major = element_line(colour = "#d3d3d3"),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(), 
  panel.background = element_blank(),
  plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
  text=element_text(family="Tahoma"),
  axis.text.x=element_text(colour="black", size = 9),
  axis.text.y=element_text(colour="black", size = 9))
p

p + labs(x="Vaccination methods", y="Percentage of dogs vaccinated")
print(p)
dev.off()

#TP2 2020 household data
tp2 <- read.csv("household_data2020C_AB.csv", sep = ",")
tp2[!is.na(tp2$number_dogs_seen), ]
st0 <- subset(tp2, Strategy=="0")
st1 <- subset(tp2, Strategy=="1")
st2 <- subset(tp2, Strategy=="2")
st3 <- subset(tp2, Strategy=="3")

# sum specific strategies denominator, change the number of st
sum(st3$number_dogs_seen, na.rm = T) # na.rm = removes rows with NA

# sum specific strategies numerators
sum(st3$microchip_scanned_dogs_yes, na.rm = T)

# coverage estimates TP1 & TP2
MERGE <- read.csv("strategycoverage.csv", sep = ",")
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

# try dot plot
p <- ggplot(MERGE2, aes(TIME, MERGE2$PointEst, colour = Strategy))+
  geom_point(size = 3)+
  #geom_errorbar(aes(ymin=MERGE2$Lower, ymax=MERGE2$Upper), width=.1)+
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

ggsave("TP1_TP2_Vacinated_pointplot.pdf", width = 10, height = 10, units ="cm")

# vaccination covarege TP1 and TP 2 continous pulled

# PILOT: create summary plot from TP1 and 2
rm(list=ls())
library(dplyr)
library(ggplot2)
TP1 <- read.csv("TP1_Summary.csv") # taken from 'summary RHS.r' script
TP2 <- read.csv("TP2_Summary.csv")

names(TP1)[names(TP1)=='sum.Total.'] <- "N"
names(TP1)[names(TP1)=='sum.TEST.'] <- "microchip"
names(TP1)[names(TP1)=='GROUP'] <- "Strategy"

TP1$Strategy <- as.character(TP1$Strategy)
TP1$Strategy[TP1$Strategy=="CONT"] <- "Community"
TP1$Strategy[TP1$Strategy=="PULSE"] <- "Team"

TP1$TIME <- "TP1"
TP2$TIME <- "TP2"

TP1_red <- TP1 %>% 
  select(c(TIME, Strategy, N, microchip))
TP2_red <- TP2 %>% 
  select(c(TIME, Strategy, N, microchip))

MERGEP <- rbind(TP1_red, TP2_red)
MERGEP

library(Hmisc)
CI <- binconf(x=MERGEP$microchip, n=MERGEP$N, method=c("wilson"))
CI <- data.frame(CI)
MERGE3 <- cbind(MERGEP, CI)
MERGE3

# convert to % from proportion
MERGE3$Lower <- MERGE3$Lower*100
MERGE3$Upper <- MERGE3$Upper *100
MERGE3$PointEst <- MERGE3$PointEst *100
write.csv(MERGEP, "TP1_TP2_PSummary.csv")
MERGE3
MERGE4 <- read.csv("TP1_TP2_PSummary.csv", sep = ",")
MERGE4

# try dot plot
m <- ggplot(MERGE4, aes(TIME, MERGE3$PointEst, colour = Strategy))+
  geom_point(size = 3)+
  #geom_errorbar(aes(ymin=MERGE2$Lower, ymax=MERGE2$Upper), width=.1)+
  ylab("Dogs vaccinated (%)")+
  scale_y_continuous(limits=c(25, 75), breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  #scale_colour_discrete(guide = F)+ 
  theme_bw() +
  scale_x_discrete(name = "Timepoint", breaks=c("TP1", "TP2"), labels=c("1", "2"))+
  theme(axis.text.x  = element_text(size= 9))

m <- m + #theme(legend.position = c(0.865,0.4)) + 
  #scale_colour_discrete(name = "Strategy", labels = c("Continuous", "Pulse"))+
  theme(legend.text = element_text(colour="black", size=6)) + # legend labels size
  theme(legend.title = element_text(colour="black", size=6)) + # legend title size
  guides(shape = guide_legend(override.aes = list(size = 3)))
m

library(ggpubr)
combined <- ggarrange(p, m,
                      labels = c("A", "B"),
                      common.legend = TRUE,
                      legend = "right",
                      ncol = 2, nrow = 1)
combined


ggsave("TP1_TP2_Vacinated_pointplot_pulled.pdf", width = 10, height = 10, units ="cm")



# analysis

TP1_red2 <- MERGE2 %>% 
  filter(TIME=="TP1")

prop.test(TP1_red2$microchip, TP1_red2$N)
# X-squared = 495.82, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   0.2511723 0.2993940
# sample estimates:
#   prop 1    prop 2 
# 0.6557496 0.3804665 

TP2_red2 <- MERGE2 %>% 
  filter(TIME=="TP2")

prop.test(TP2_red2$microchip, TP2_red2$N)
# X-squared = 164.7, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.3207409 -0.2382270
# sample estimates:
#   prop 1    prop 2 
# 0.3210383 0.6005222 

#easiness
easiness <- read.csv("easy.csv", sep = ",")
e <- ggplot(easiness, aes(x=Strategy, y=proportion, fill = Location))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("gray25", "violetred4","navyblue", "grey80"))+
  ylab("Responded (%)")+
  scale_y_continuous(limits=c(0, 100), 
  breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
  theme_bw() +
  theme(legend.position = c(0.1,0.86))+
  theme(axis.text.x  = element_text(size= 9))
j <- e + theme(panel.border = element_blank())
j

e + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())

ggsave("Easiness.pdf", width = 17, height = 14, units ="cm")

#e + theme(legend.key.height = unit(0.1, "cm"))


ggsave("Easiness.pdf1", width = 17, height = 14, units ="cm")
