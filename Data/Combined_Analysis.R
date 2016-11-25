### For kids
library(Hmisc)
library(ggplot2)
library(doBy)
library(lme4)
library(plyr)

Child.e = read.csv("./Expt 2/Gelman_Expt2_Data_Edinburgh.csv", header = T)
Child.e$Loc <- "Edin"
Child.b = read.csv("./Expt 2/Gelman_Expt2_Data_Ber.csv", header = T)
Child.b$Loc <- "Ber"
Child.e4b = read.csv("./Expt 2/Gelman_Expt2_Data_Ed_For_Ber.csv",header = T)
Child.e4b$Loc <- "EdForBer"
Child <- rbind(Child.e,Child.b, Child.e4b)
Child <- subset(Child, trial.type == "critical" & age %in% c(3,4) & !exclude %in% c("yes"))
Child$Choice <- ifelse(Child$superordinate.match ==1,0, ifelse(Child$superordinate.match == 0,1,NA))

Child$Meaning <- ifelse(Child$concept.type == "Non-Polysemous","Unambiguous (Same Kind)","Polysemous (Distinct Kinds)")
Child$Meaning <- factor(Child$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Child$Label <- ifelse(Child$label == "no","No Label", "Shared Label")
Child$Label <- factor(Child$Label, levels = c("Shared Label","No Label") )
Child$Age <- as.factor(ifelse(Child$age == 3,3,4))
Child$TriadType <- "Chicken"
Child[Child$triad.type == "glass",]$TriadType <- "Glass"
Child[Child$triad.type == "horse",]$TriadType <- "Horse"
summaryBy(Choice ~ age + Meaning + Label, data = subset(Child, trial.type == "critical"),na.rm=T)
Child$Expt <- "Expt2"
Child.Expt2 <- data.frame(ID = Child$ID,Label = Child$Label,Meaning = Child$Meaning,Choice = Child$Choice,Expt = Child$Expt,Age = Child$Age,Triad.Type = Child$TriadType,QuNum = Child$question.number)

Child = read.csv("./Expt 1/ChildDataBer_Oct26.csv", header = T)
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
Child$Meaning <- factor(Child$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Child$Label <- ifelse(Child$LabelType == "Same","Shared Label", "Different Label")
Child$Label <- factor(Child$Label, levels = c("Shared Label","Different Label") )
Child$ID <- Child$SubjNo
Child$Expt <- "Expt1"
Child.Expt1 <- data.frame(ID = Child$ID,Label = Child$Label,Meaning = Child$Meaning,Choice = Child$Choice,Expt = Child$Expt,Age = Child$Age,Triad.Type = Child$TriadType,QuNum = Child$question.number)

Child <- rbind(Child.Expt1,Child.Expt2)
Child$Label_c <- as.factor(ifelse(Child$Label == "Shared Label", "Shared", "Other"))
contrasts(Child$Label_c)[1] <- -1
contrasts(Child$Meaning)[1] <- -1
contrasts(Child$Age)[1] <- -1
contrasts(Child$Expt)[1] <- -1

summary(glmer(Choice ~ Meaning*Label_c*Expt + (1+Label_c|ID) + (1+Meaning|QuNum), data = Child, family = "binomial")) # maximal structure that allows convergence



### And for adults


Adult = read.csv("./Expt 2/Gelman_Expt2_Data_Ber_Adult.csv", header = T)
Adult$Loc <- "Ber"
Adult <- subset(Adult, trial.type == "critical" & !exclude %in% c("yes"))
Adult$Choice <- ifelse(Adult$superordinate.match ==1,0, ifelse(Adult$superordinate.match == 0,1,NA))

Adult$Meaning <- ifelse(Adult$concept.type == "Non-Polysemous","Unambiguous (Same Kind)","Polysemous (Distinct Kinds)")
Adult$Meaning <- factor(Adult$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Adult$Label <- ifelse(Adult$label == "no","No Label", "Shared Label")
Adult$Label <- factor(Adult$Label, levels = c("Shared Label","No Label") )
Adult$TriadType <- "Chicken"
Adult[Adult$triad.type == "glass",]$TriadType <- "Glass"
Adult[Adult$triad.type == "horse",]$TriadType <- "Horse"
Adult$Expt <- "Expt2"
Adult.Expt2 <- data.frame(ID = Adult$ID,Label = Adult$Label,Meaning = Adult$Meaning,Choice = Adult$Choice,Expt = Adult$Expt,Triad.Type = Adult$TriadType,QuNum = Adult$question.number)

Adult = read.csv("./Expt 1/AdultData.csv", header = T)
contrasts(Adult$LabelType)[1] <- -1
contrasts(Adult$WordType)[1] <- -1
#Adult$WordType <- revalue(Adult$WordType, c("Unambiguous" = "Same Kind", "Polysemous" = "Polysemous"))
Adult$LabelType <- factor(Adult$LabelType, levels(Adult$LabelType)[c(2,1)])
Adult$WordType <- factor(Adult$WordType, levels(Adult$WordType)[c(2,1)])
contrasts(Adult$LabelType)[1] <- -1
contrasts(Adult$WordType)[1] <- -1
Adult$Choice <- ifelse(Adult$UnrelatedChoice == 1, 0, ifelse(Adult$UnrelatedChoice == 0,1,NA))

Adult$Meaning <- ifelse(Adult$WordType == "Polysemous","Polysemous (Distinct Kinds)", "Unambiguous (Same Kind)")
Adult$Meaning <- factor(Adult$Meaning, levels = c("Unambiguous (Same Kind)","Polysemous (Distinct Kinds)") )
Adult$Label <- ifelse(Adult$LabelType == "Same","Shared Label", "Different Label")
Adult$Label <- factor(Adult$Label, levels = c("Shared Label","Different Label") )
Adult$ID <- Adult$SubjNo
Adult$Expt <- "Expt1"
Adult.Expt1 <- data.frame(ID = Adult$ID,Label = Adult$Label,Meaning = Adult$Meaning,Choice = Adult$Choice,Expt = Adult$Expt,Triad.Type = Adult$TriadType,QuNum = Adult$ItemNo)

Adult <- rbind(Adult.Expt1,Adult.Expt2)
Adult$Label_c <- as.factor(ifelse(Adult$Label == "Shared Label", "Shared", "Other"))
contrasts(Adult$Label_c)[1] <- -1
contrasts(Adult$Meaning)[1] <- -1
contrasts(Adult$Expt)[1] <- -1

summary(glmer(Choice ~ Meaning*Label_c*Expt + (1|ID) , data = Adult, family = "binomial")) # maximal structure that allows convergence