hen <- read.delim("./Processed/hen", header = F, sep = " ")
chicken <- read.delim("./Processed/chicken", header = F, sep = " ")
drumstick <- read.delim("./Processed/drumstick", header = F, sep = " ")

horse  <- read.delim("./Processed/horse", header = F, sep = " ")
foal  <- read.delim("./Processed/foal", header = F, sep = " ")
rocker  <- read.delim("./Processed/rocker", header = F, sep = " ")

glass  <- read.delim("./Processed/glass", header = F, sep = " ")
cup  <- read.delim("./Processed/cup", header = F, sep = " ")
windowpane  <- read.delim("./Processed/windowpane", header = F, sep = " ")
window  <- read.delim("./Processed/window", header = F, sep = " ")

hen_chicken <- read.delim("./Processed/hen_chicken", header = F, sep = " ")
chicken_hen <- read.delim("./Processed/chicken_hen", header = F, sep = " ")
drumstick_chicken <- read.delim("./Processed/drumstick_chicken", header = F, sep = " ")
chicken_drumstick <- read.delim("./Processed/chicken_drumstick", header = F, sep = " ")


foal_horse <- read.delim("./Processed/foal_horse", header = F, sep = " ")
horse_foal <- read.delim("./Processed/horse_foal", header = F, sep = " ")
rocker_horse <- read.delim("./Processed/rocker_horse", header = F, sep = " ")
horse_rocker <- read.delim("./Processed/horse_rocker", header = F, sep = " ")


windowpane_glass <- read.delim("./Processed/windowpane_glass", header = F, sep = " ")
glass_windowpane <- read.delim("./Processed/glass_windowpane", header = F, sep = " ")
cup_glass <- read.delim("./Processed/cup_glass", header = F, sep = " ")
glass_cup <- read.delim("./Processed/glass_cup", header = F, sep = " ")

window_glass <- read.delim("./Processed/window_glass", header = F, sep = " ")
glass_window <- read.delim("./Processed/glass_window", header = F, sep = " ")

summary(hen)
summary(chicken)
summary(drumstick)
summary(horse)
summary(foal)
summary(rocker)
summary(glass)
summary(cup)
summary(windowpane)
summary(window)
summary(hen_chicken)
summary(chicken_hen)
summary(drumstick_chicken)
summary(chicken_drumstick)
summary(foal_horse)
summary(horse_foal)
summary(rocker_horse)
summary(horse_rocker)
summary(windowpane_glass)
summary(glass_windowpane)
summary(cup_glass)
summary(glass_cup)
summary(window_glass)
summary(glass_window)

# chicken - hen/drumstick associations
chicken_assoc_hen <- sum(c(chicken_hen$V3, hen_chicken$V3)) / 
                                (sum(c(chicken$V1, hen$V1)) - sum(c(chicken_hen$V3, hen_chicken$V3)))
print(paste("chicken_assoc_hen = ",chicken_assoc_hen))

chicken_assoc_drumstick <- sum(c(chicken_drumstick$V3, drumstick_chicken$V3)) / 
  (sum(c(chicken$V1, drumstick$V1)) - sum(c(chicken_drumstick$V3, drumstick_chicken$V3)))
print(paste("chicken_assoc_drumstick = ",chicken_assoc_drumstick))


# horse - foal/rocker associations
horse_assoc_foal <- sum(c(horse_foal$V3, foal_horse$V3)) / 
  (sum(c(horse$V1, foal$V1)) - sum(c(horse_foal$V3, foal_horse$V3)))
print(paste("horse_assoc_foal = ",horse_assoc_foal))


horse_assoc_rocker <- sum(c(horse_rocker$V3, rocker_horse$V3)) / 
  (sum(c(horse$V1, rocker$V1)) - sum(c(horse_rocker$V3, rocker_horse$V3)))
print(paste("horse_assoc_rocker = ",horse_assoc_rocker))

# glass - windowpane/window/cup associations 
glass_assoc_windowpane <- sum(c(glass_windowpane$V3, windowpane_glass$V3)) / 
  (sum(c(glass$V1, windowpane$V1)) - sum(c(glass_windowpane$V3, windowpane_glass$V3)))
print(paste("glass_assoc_windowpane = ",glass_assoc_windowpane))

glass_assoc_window <- sum(c(glass_window$V3, window_glass$V3)) / 
  (sum(c(glass$V1, window$V1)) - sum(c(glass_window$V3, window_glass$V3)))
print(paste("glass_assoc_window = ",glass_assoc_window))

glass_assoc_cup <- sum(c(glass_cup$V3, cup_glass$V3)) / 
  (sum(c(glass$V1, cup$V1)) - sum(c(glass_cup$V3, cup_glass$V3)))
print(paste("glass_assoc_cup = ",glass_assoc_cup))


print("mean Different Sense")
mean(c(glass_assoc_cup, horse_assoc_rocker,chicken_assoc_drumstick))

print("mean Same Sense")
mean(c(glass_assoc_windowpane,chicken_assoc_hen,horse_assoc_foal))
