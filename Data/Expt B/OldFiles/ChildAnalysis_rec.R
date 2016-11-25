library(Hmisc)
library(ggplot2)
library(doBy)
library(lme4)
library(plyr)
Child = read.csv("ChildDataBer_Oct26.csv", header = T)
contrasts(Child$LabelType)[1] <- -1
contrasts(Child$WordType)[1] <- -1
#Child$WordType <- revalue(Child$WordType, c("Unambiguous" = "Same Kind", "Polysemous" = "Polysemous"))
Child$LabelType <- factor(Child$LabelType, levels(Child$LabelType)[c(2,1)])
Child$WordType <- factor(Child$WordType, levels(Child$WordType)[c(2,1)])

Child <- subset(Child, trial.type == "Critical")
Child <- subset(Child, Exclude == "No")

Child$AgeGroup <- "Three"
Child[Child$Age == 4,]$AgeGroup <- "Four"
Child <- Child[!Child$Age > 4,]
Child$AgeGroup <- ordered(Child$AgeGroup, levels = c("Three","Four"))
Child$AgeMoSt <- (Child$Age.Months - mean(Child$Age.Months))/sd(Child$Age.Months)
Child$Age <- as.factor(Child$Age)
Child$Loc <- "Ber"
Child[Child$Location == "Edinburgh ",]$Loc <- "Edin"
Child$Loc <- as.factor(Child$Loc)
contrasts(Child$Expln)[1] <- -1
contrasts(Child$Loc)[1] <- -1
contrasts(Child$Age)[1] <- -1
contrasts(Child$LabelType)[1] <- -1
contrasts(Child$WordType)[1] <- -1
Child$Choice <- ifelse(Child$UnrelatedChoice == 1, 0, ifelse(Child$UnrelatedChoice == 0,1,NA))

Child$Meaning <- ifelse(Child$WordType == "Polysemous","Polysemous (Distinct Kinds)", "Unambiguous (Same Kind)")
Child$Meaning <- ordered(Child$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Child$Label <- ifelse(Child$LabelType == "Same","Shared", "Different")
Child$Label <- ordered(Child$Label, levels = c("Shared","Different") )


#Child$Age <- as.numeric(as.character(Child$Age))
summaryBy(Choice~Age + Meaning + LabelType, data=Child, na.rm = T)
summary(glmer(Choice~Meaning*LabelType + (1+LabelType|SubjNo) + (1+LabelType*Meaning|TriadType), data = Child, family = "binomial"))

# Make sure it isn't due to location or explanations
summary(glmer(Choice~Loc*Meaning*LabelType + (1+LabelType|SubjNo) + (1+LabelType*Meaning|TriadType), data = Child, family = "binomial"))
summary(glmer(Choice~Expln*Meaning*LabelType + (1+LabelType|SubjNo) + (1+LabelType*Meaning|TriadType), data = Child, family = "binomial"))


# Tests against Chance
summaryBy(Choice ~ SubjNo + WordType + LabelType, data = Child, keep.names = T) -> Child.Sum
t.test(subset(Child.Sum, WordType == "Non-Polysemous" & LabelType == "Same")$Choice, mu = 0.5)
t.test(subset(Child.Sum, WordType == "Non-Polysemous" & LabelType == "Different")$Choice, mu = 0.5)
t.test(subset(Child.Sum, WordType == "Polysemous" & LabelType == "Different")$Choice, mu = 0.5)
t.test(subset(Child.Sum, WordType == "Polysemous" & LabelType == "Same")$Choice, mu = 0.5)

summaryBy(Choice~TriadType+Age + WordType + LabelType, data=Child, na.rm = T)


Child.means <- summaryBy(Choice~ Meaning + Label, data=Child, na.rm = T, FUN = c(mean,sd))
Child.means$SE <- Child.means$Choice.sd/sqrt(length(unique(Child$SubjNo))/2)

avg.plot<- ggplot(Child.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Child.means$Choice.mean+Child.means$SE, ymin=Child.means$Choice.mean-Child.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,0.75))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")


Child.means <- summaryBy(Choice~ TriadType+Meaning + Label , data=Child, na.rm = T, FUN = c(mean,sd))
Child.means$SE <- Child.means$Choice.sd/sqrt(length(unique(Child$SubjNo))/2)

avg.plot<- ggplot(Child.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Child.means$Choice.mean+Child.means$SE, ymin=Child.means$Choice.mean-Child.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,0.75))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")+facet_grid(.~TriadType)+ scale_x_discrete(breaks=levels(Child.means$Meaning),
                      labels=c("Unambiguous", "Polysemous"))