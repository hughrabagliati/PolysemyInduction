library(Hmisc)
library(ggplot2)
library(doBy)
library(lme4)
library(plyr)

Child.e = read.csv("Gelman_Expt2_Data_Edinburgh.csv", header = T)
Child.e$Loc <- "Edin"
Child.b = read.csv("Gelman_Expt2_Data_Ber.csv", header = T)
Child.b$Loc <- "Ber"
Child.e4b = read.csv("Gelman_Expt2_Data_Ed_For_Ber.csv",header = T)
Child.e4b$Loc <- "EdForBer"
Child <- rbind(Child.e,Child.b, Child.e4b)
Child <- subset(Child, trial.type == "critical" & age %in% c(3,4) & !exclude %in% c("yes"))
Child$Choice <- ifelse(Child$superordinate.match ==1,0, ifelse(Child$superordinate.match == 0,1,NA))

Child$Meaning <- ifelse(Child$concept.type == "Non-Polysemous","Unambiguous (Same Kind)","Polysemous (Distinct Kinds)")
Child$Meaning <- ordered(Child$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Child$Label <- ifelse(Child$label == "no","No Label", "Shared Label")
Child$Label <- ordered(Child$Label, levels = c("Shared Label","No Label") )
summaryBy(Choice ~ age + Meaning + Label, data = subset(Child, trial.type == "critical"),na.rm=T)

Child.means <- summaryBy(Choice~ Meaning + Label, data=Child, na.rm = T, FUN = c(mean,sd))
Child.means$SE <- Child.means$Choice.sd/sqrt(length(unique(Child$ID))/2)

summary(glmer(Choice~Label*Meaning + (1+Label|ID) + (1|triad.type), data = Child, family = "binomial"))
# Tests against Chance
summaryBy(Choice ~ ID + Meaning + Label, data = Child, keep.names = T) -> Child.Sum
t.test(subset(Child.Sum, Meaning == "Unambiguous (Same Kind)" & Label == "Shared Label")$Choice, mu = 0.5)
t.test(subset(Child.Sum, Meaning == "Unambiguous (Same Kind)" & Label == "No Label")$Choice, mu = 0.5)
t.test(subset(Child.Sum, Meaning == "Polysemous (Distinct Kinds)" & Label == "Shared Label")$Choice, mu = 0.5)
t.test(subset(Child.Sum, Meaning == "Polysemous (Distinct Kinds)" & Label == "No Label")$Choice, mu = 0.5)

avg.plot<- ggplot(Child.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Child.means$Choice.mean+Child.means$SE, ymin=Child.means$Choice.mean-Child.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,1))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")


Child.means <- summaryBy(Choice~ triad.type+Meaning + Label , data=Child, na.rm = T, FUN = c(mean,sd))
Child.means$SE <- Child.means$Choice.sd/sqrt(length(unique(Child$ID))/2)

avg.plot<- ggplot(Child.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Child.means$Choice.mean+Child.means$SE, ymin=Child.means$Choice.mean-Child.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,1))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")+facet_grid(.~triad.type)+ scale_x_discrete(breaks=levels(Child.means$Meaning),
                      labels=c("Unambiguous", "Polysemous"))