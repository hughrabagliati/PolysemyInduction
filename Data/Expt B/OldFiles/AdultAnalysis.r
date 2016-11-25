library(ggplot2)
library(doBy)
library(lme4)
library(plyr)
Adult = read.csv("AdultData.csv", header = T)
contrasts(Adult$LabelType)[1] <- -1
contrasts(Adult$WordType)[1] <- -1
Adult$WordType <- revalue(Adult$WordType, c("Unambiguous" = "Non-Polysemous", "Polysemous" = "Polysemous"))
Adult$LabelType <- factor(Adult$LabelType, levels(Adult$LabelType)[c(2,1)])
Adult$WordType <- factor(Adult$WordType, levels(Adult$WordType)[c(2,1)])


Adult$Choice <- ifelse(Adult$UnrelatedChoice == 1, 0, ifelse(Adult$UnrelatedChoice == 0,1,NA))
Adult$Meaning <- ifelse(Adult$WordType == "Polysemous","Polysemous (Distinct Kinds)", "Unambiguous (Same Kind)")
Adult$Meaning <- ordered(Adult$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Adult$Label <- ifelse(Adult$LabelType == "Same","Shared", "Different")
Adult$Label <- ordered(Adult$Label, levels = c("Shared","Different") )


Adult.means <- summaryBy(Choice~ Meaning + Label, data=Adult, na.rm = T, FUN = c(mean,sd))
Adult.means$SE <- Adult.means$Choice.sd/sqrt(length(unique(Adult$SubjNo))/2)

avg.plot<- ggplot(Adult.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Adult.means$Choice.mean+Adult.means$SE, ymin=Adult.means$Choice.mean-Adult.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(0,1))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")


Adult.means <- summaryBy(Choice~ TriadType+Meaning + Label , data=Adult, na.rm = T, FUN = c(mean,sd))
Adult.means$SE <- Adult.means$Choice.sd/sqrt(length(unique(Adult$SubjNo))/2)

avg.plot<- ggplot(Adult.means, aes(Meaning, Choice.mean, fill = Label)) + 
 stat_summary(fun.y="mean", geom="bar", position = "dodge")
#add error bars
avg.plot+geom_errorbar(aes(ymax=Adult.means$Choice.mean+Adult.means$SE, ymin=Adult.means$Choice.mean-Adult.means$SE), position=position_dodge(.9), width = 0)+theme_bw()+ylab("Proportion choosing related item")+ylim(c(-0.05,1.05))+ geom_hline(yintercept = 0.5, colour = "black",lty = 3)+xlab("")+facet_grid(.~TriadType)+ scale_x_discrete(breaks=levels(Adult.means$Meaning),
                      labels=c("Unambiguous", "Polysemous"))

summary(glmer(UnrelatedChoice~WordType*LabelType + (1+LabelType|SubjNo) + (1+WordType*LabelType|TriadType), data = Adult, family = "binomial"))
# Note that this was the max RE that allowed the model to converge

Adult <- Adult[,!(names(Adult) %in% c("Lang","Comments"))]
Adult$AgeGroup = "Adult"

Adult$AgeGroup <- "Adult"

Comb <- data.frame(AgeGroup = as.factor(c(Adult$AgeGroup, Adult$AgeGroup)), SubjNo = as.factor(c(as.factor(Adult$SubjNo), Adult$SubjNo)), UnrelatedChoice = c(Adult$UnrelatedChoice, Adult$UnrelatedChoice),
	TriadType = as.factor(c(as.character(Adult$TriadType), as.character(Adult$TriadType))), WordType = as.factor(c(as.character(Adult$WordType), as.character(Adult$WordType))), LabelType = as.factor(c(as.character(Adult$LabelType), as.character(Adult$LabelType))))

contrasts(Comb$AgeGroup)[1] <- -0.5
contrasts(Comb$LabelType)[1] <- -0.5
contrasts(Comb$WordType)[1] <- -0.5

contrasts(Comb$AgeGroup)[2] <- 0.5
contrasts(Comb$LabelType)[2] <- 0.5
contrasts(Comb$WordType)[2] <- 0.5


summary(glmer(UnrelatedChoice~AgeGroup*WordType*LabelType + (1+LabelType|SubjNo) + (1+AgeGroup*WordType*LabelType|TriadType), data = Comb, family = "binomial"))
summary(glmer(UnrelatedChoice~WordType*LabelType + (1+LabelType|SubjNo) + (1+WordType*LabelType|TriadType), data = subset(Comb, AgeGroup == "Adult"), family = "binomial"))
summary(glmer(UnrelatedChoice~WordType*LabelType + (1+LabelType|SubjNo) + (1+WordType*LabelType|TriadType), data = subset(Comb, AgeGroup == "Adult"), family = "binomial"))



