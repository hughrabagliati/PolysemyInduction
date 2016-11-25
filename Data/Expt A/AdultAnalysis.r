library(Hmisc)
library(ggplot2)
library(doBy)
library(lme4)
library(plyr)

Adult = read.csv("Gelman_Expt2_Data_Ber_Adult.csv", header = T)
Adult$Loc <- "Ber"
Adult <- subset(Adult, trial.type == "critical" &  !exclude %in% c("yes"))
Adult$Choice <- ifelse(Adult$superordinate.match ==1,0, ifelse(Adult$superordinate.match == 0,1,NA))

Adult$Meaning <- ifelse(Adult$concept.type == "Non-Polysemous","Unambiguous (Same Kind)","Polysemous (Distinct Kinds)")
Adult$Meaning <- ordered(Adult$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Adult$Label <- ifelse(Adult$label == "no","No Label", "Shared Label")
Adult$Label <- ordered(Adult$Label, levels = c("Shared Label","No Label") )
summaryBy(Choice ~ age + Meaning + Label, data = subset(Adult, trial.type == "critical"),na.rm=T)

Adult.means <- summaryBy(Choice~ Meaning + Label, data=Adult, na.rm = T, FUN = c(mean,sd))
Adult.means$SE <- Adult.means$Choice.sd/sqrt(length(unique(Adult$ID))/2)

summary(glmer(Choice~Label*Meaning + (1+Label|ID) + (1|triad.type), data = Adult, family = "binomial"))
# Tests against Chance
summaryBy(Choice ~ ID + Meaning + Label, data = Adult, keep.names = T) -> Adult.Sum
t.test(subset(Adult.Sum, Meaning == "Unambiguous (Same Kind)" & Label == "Shared Label")$Choice, mu = 0.5)
t.test(subset(Adult.Sum, Meaning == "Unambiguous (Same Kind)" & Label == "No Label")$Choice, mu = 0.5)
t.test(subset(Adult.Sum, Meaning == "Polysemous (Distinct Kinds)" & Label == "Shared Label")$Choice, mu = 0.5)
t.test(subset(Adult.Sum, Meaning == "Polysemous (Distinct Kinds)" & Label == "No Label")$Choice, mu = 0.5)

avg.plot<- ggplot(Adult.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Adult.means$Choice.mean+Adult.means$SE, ymin=Adult.means$Choice.mean-Adult.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,1))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")


Adult.means <- summaryBy(Choice~ triad.type+Meaning + Label , data=Adult, na.rm = T, FUN = c(mean,sd))
Adult.means$SE <- Adult.means$Choice.sd/sqrt(length(unique(Adult$ID))/2)

avg.plot<- ggplot(Adult.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Adult.means$Choice.mean+Adult.means$SE, ymin=Adult.means$Choice.mean-Adult.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(-0.05,1.05))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")+facet_grid(.~triad.type)+ scale_x_discrete(breaks=levels(Adult.means$Meaning),
                      labels=c("Unambiguous", "Polysemous"))