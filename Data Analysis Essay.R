#21/03/2019

#DATA ANALYSIS ESSAY


gtd <- read.csv("gtd.csv")

gtd

summary(gtd)

#2. DATA 

#2.1

gtd$bombing <- ifelse(gtd$attacktype1_txt == "Bombing/Explosion",1,0)

#

#2.2

gtd$attack.target <- ifelse(gtd$targtype1_txt == "Business", "1",
                               ifelse(gtd$targtype1_txt == "Government (General)", "2", 
                               ifelse(gtd$targtype1_txt == "Military", "3", 
                               ifelse(gtd$targtype1_txt == "Police", "4", 
                               ifelse(gtd$targtype1_txt == "Private Citizens & Property","5", 
                               ifelse(gtd$targtype1_txt == is.na(gtd$targtype1_txt), "0","0"))))))

#

#2.3

gtd$gdp <- as.numeric(gtd$gdp)

q.25 <- quantile(gtd$gdp, probs= c(0.25), na.rm = TRUE)
q.50 <- quantile(gtd$gdp, probs= c(0.50), na.rm = TRUE)
q.75 <- quantile(gtd$gdp, probs= c(0.75), na.rm = TRUE)

gtd$cat.gdp <- ifelse(gtd$gdp < q.25, "1",
                                        ifelse (gtd$gdp>= q.25 & gtd$gdp < q.50, "2",
                                        ifelse(gtd$gdp >= q.50 & gtd$gdp < q.75, "3",
                                        ifelse(gtd$gdp >= q.75, "4", "0"))))
#

#2.4

gtd$low.capacity <- ifelse(gtd$cat.gdp == "1", "1", "0")

##

#3. DATA

#a. On average, are bombings more or less lethal than other types of attack?


bombings.moreorless <- tapply(gtd$nkill[gtd$nkill != "-999"], gtd$bombing[gtd$nkill != "-999"], mean)
bombings.moreorless
rownames(bombings.moreorless) = c("Other types of attack", "Bombings")

#Bombings in 2017 were less lethal than other types of attack.
#Non-bombing terrorist attacks constituted for 1199 fatalities which translates to 54.7% of casualties in 2017.

#b. On average, are suicide bombings more lethal than non-suicide bombings?

#Null hypothesis - On average, suicide bombings are more lethal than non-suicide bombings.

#Alternative hypothesis - On average, suicide bombings are not more lethal than suicide bombings.

suicide.moreorless <- tapply(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt =="Bombing/Explosion"], gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], mean)
rownames(suicide.moreorless) = c("Other types of bombings", "Suicide Bombings")

#On average in 2017, suicide bombings were not more lethal than non-suicide bombings.
#This is substantiated by the fact that the number of deaths suffered in the latter were higher than in the former.

#Relationship between target and severity of attack

#Null hypothesis - There will be no difference in lethality across different target types.

#Alternative hypothesis - There will be a difference in lethality across different target types.

leth.targ <- tapply(gtd$nkill[gtd$nkill != "-999"], gtd$attack.target[gtd$nkill != "-999"], mean)

rownames(leth.targ) = c("Others", "Business", "Government(General)", "Military", "Police", "Private Citizens & Property")
leth.targ

#When comparing the proportion of casualties suffered, a notable contrast emerges between military targets, constituting 34.61% of deaths with those suffered by business targets, representing only 1.32%, 
#Thus, the null hypothesis is refuted as there are significant levels of difference between the lethality incurred across different target types.

#

#Is success more likely against some targets than others? 

#Null hypothesis - The likelihood of success does not vary across the type of target

#Alternative hypothesis - The likelihood of success varies across the type of target

succ.targ <- tapply(gtd$success, gtd$attack.target, sum)
rownames(succ.targ) = c("Others", "Business", "Government(General)", "Military", "Police", "Private Citizens & Property")
succ.targ
prop.table(succ.targ)

##Thus, the null hypothesis is refuted as the succeses of terrorist attacks in 2017 varied according to the target of attack.

#

#Effect of economic capacity on terrorism

#Null hypothesis - Economic capacity is a key variable in deterring terrorism, as governments with low levels of economic capacity have
                   #insufficient funds to combat terrorist activities.

#Alternative hypothesis - Economic capacity is not a key variable in deterring terrorism and low economic capacity does not suggest
                          #the inability to combat terrorist activities.


lowcapsuc <- sum(gtd$success[gtd$low.capacity == "1" & gtd$success == "1"], na.rm = TRUE) #83

highcapsuc <- sum(gtd$success[gtd$low.capacity == "0" & gtd$success == "1"], na.rm = TRUE) #618

lowcapsuc / (lowcapsuc + highcapsuc) #0.1184023
highcapsuc / (lowcapsuc + highcapsuc) #0.8815977

#Thus, the null hypothesis is refuted, as attacks in countries with low gdp capacity only constituted for 11.84% of successful terrorist attacks.

##

#4. DESCRIPTIVE ANALYSIS

#Table of casualties by attack target

cas.table <- round(table(gtd$attack.target[gtd$nkill != "-999"]),2)
addmargins(cas.table)

#Barplot of average casualties for bombings vs. other types of attack

bomb.vs.oth <- tapply(gtd$nkill[gtd$nkill != "-999"], gtd$bombing[gtd$nkill != "-999"], mean)

bomb.vs.oth
barplot(bomb.vs.oth,
        main = "Mean casualties between bombings and other types of attack",
        ylim = c(0.0, 5),
        ylab = "Mean casulaties",
        xlab = "Type of attack",
        col = "cyan",
        names = c("Other types of attack", "Bombings"))

#Barplot of casualties by attack target

mean.of.casualties
mean.of.casualties <- tapply(gtd$nkill[gtd$nkill != "-999"], gtd$attack.target[gtd$nkill != "-999"], mean)
rownames(mean.of.casualties) = c("Others", "Business", "Government(General)", "Military", "Police", "Private Citizens & Property")


barplot(no.of.casualties, 
        main = "Barplot of average casualties by target type",
        xlab = "Target of Attack", 
        ylab = "Average Casualties",
        ylim = c(0.0, 5.0),
        col = "red",
        names.arg = c("Others", "Business", "Government (General)", "Military", "Police", "Private Citizens & Property"))

#Lethality of bombings vs. suicide bombings

mean.of.bombings 
mean.of.bombings <- tapply(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], mean)

barplot(mean.of.bombings,
        main = "Mean casualties of bombings vs. suicide bombings",
        xlab = "Type of bombing",
        ylab = "Casualties",
        ylim = c(0,10),
        col = "green",
        names.arg = c("Bombing", "Suicide Bombing"))

#Barplot of Success and failure

range(prop.cas)
prop.cas <- prop.table(table(gtd$attack.target[gtd$success == "1"]))

barplot(prop.cas, 
        main = "Bar plot to show success of attack by target type",
        xlab = "Target of Attack", 
        ylab = "Proportion of successful terrorist attacks", 
        ylim = c(0.0, 0.5),
        col = "blue",
        names.arg = c("Others", "Business", "Government (General)", "Military", "Police", "Private Citizens & Property"))

#Barplot showing impact of low economic capacity in the success of terrorist attacks

prop.lowcap

prop.lowcap <- prop.table(table(gtd$low.capacity[gtd$success == "1"]))

barplot(prop.lowcap, 
        main = "Impact of low economic capacity on success of terrorist attacks",
        names.arg = c("High Economic Capacity Countries" , "Low Economic Capacity Countries"),
        xlab = "Level of Economic Capacity", 
        ylab = "Proportion of successful terrorist attacks", 
        col = "blue",
        ylim = c(0.0, 1.0))

##

#5. Hypothesis testing

#5.1

#Test if bombings are more or less lethal than other types of attack.

#Null hypothesis - Bombings do not differ in lethality compared to other types of attack.
#Alternative hypothesis - Bombings differ in lethality compared to other types of attack.

tapply(gtd$nkill[gtd$nkill != "-999"], gtd$bombing[gtd$nkill != "-999"], mean)
bombingtbl <- tapply(gtd$nkill[gtd$nkill != "-999"], gtd$bombing[gtd$nkill != "-999"], mean)
rownames(bombingtbl) = c("Other type of attack", "Bombing")  

bombingtbl

t.test(gtd$nkill[gtd$nkill != "-999"] ~ gtd$bombing[gtd$nkill != "-999"], alt = "two.sided")
t.test(gtd$nkill[gtd$nkill != "-999"] ~ gtd$bombing[gtd$nkill != "-999"], alt ="two.sided", conf = 0.90, var.eq = FALSE)
t.test(gtd$nkill[gtd$nkill != "-999"] ~ gtd$bombing[gtd$nkill != "-999"], alt ="two.sided", conf = 0.99, var.eq = FALSE)

t.test(bombingtbl, conf.level = 0.95, alt = "two.sided")

#

#5.2

#Test if suicide bombings are more lethal than non-suicide bombings.

#Null hypothesis -  On average, suicide bombings are less lethal than non-suicide bombings.
#Alternative hypothesis - On average, suicide bombings are more lethal than non-suicide bombings.
?t.test

#Only more - ONE-TAILED TEST

t.test(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"] ~ gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], conf = 0.95, var.eq = FALSE)
t.test(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"] ~ gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], alt = "greater", conf = 0.90, var.eq = FALSE)
t.test(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"] ~ gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], alt = "greater", conf = 0.99, var.eq = FALSE)

suicidetbl <- tapply(gtd$nkill[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], gtd$suicide[gtd$nkill != "-999" & gtd$attacktype1_txt == "Bombing/Explosion"], sum)


0.002698/2


8.277778 - 1.061333 

#5.3

#Does the lethality of an attack differ based on the intended target? 

boxplot(gtd$nkill[gtd$nkill != "-999"] ~ gtd$attack.target[gtd$nkill != "-999"])

?boxplot

#Null hypothesis - The lethality of an attack does not differ based on the intended target. 
#Alternative hypothesis - The lethality of an attack differs based on the intended target.

#Differ - TWO-TAILED TEST

#Variables tested - gtd$nkill != "-999" + gtd$attack.target

anova.test <- aov(formula = gtd$nkill[gtd$nkill != "-999"] ~ gtd$attack.target[gtd$nkill != "-999"])
anova.test

summary(anova.test)

#As the p-value of 0.00269 is smaller than the significance level of 0.05 we can reject the null hypothesis at the 5% error level.
#As the p-value of 0.00269 is smaller than the significance level of 0.10 we can reject the null hypothesis at the 10% error level.
#As the p-value of 0.00269 is smaller than the significance level of 0.10 we can reject the null hypothesis at the 1% error level.

#5.4 

#Does the success of an attack differ based on the intended target?

#Null hypothesis - Likelihood of success does not vary across the type of target.

#Alternative hypothesis - Likelihood of success varies across the type of target.


targsuc <- table(gtd$attack.target, gtd$success)
colnames(targsuc) = c("No Success", "Success")
rownames(targsuc) = c("Others", "Business", "Government(General)", "Military", "Police", "Private Citizens & Property")


chisq.test(targsuc, correct = T)

#5.5 

#Does low economic capacity affect the success of terrorist attacks?

#Null hypothesis - Governments with low levels of capacity have insufficient resources to effectively combat terrorist activities.
#Alternative hypothesis - Governments with low levels of capacity have sufficient resources to effectively combat terrorist activities.

lowcapsuc <- table(gtd$success[gtd$success == "1"], gtd$low.capacity[gtd$success == "1"])
colnames(lowcapsuc) = c("High Capacity", "Low Capacity")
rownames(lowcapsuc) = c("Successes")
lowcapsuc

chisq.test(lowcapsuc, correct = F)

0.00000000000000022/2


lowecontbl <- table(gtd$success, gtd$low.capacity)
colnames(lowecontbl) = c("High Economic Capacity", "Low Economic Capacity")
rownames(lowecontbl) = c("No Success", "Success")

lowecontbl

barplot(lowecontbl, 
        main = "Effect of economic capacity on success of terrorist attacks",
        legend = T, 
        ylim = c(0.0, 1500),
        col = c("black", "pink"),
        xlab = "Level of Economic Capacity",
        ylab = "Number of successful terrorist attacks")

chisq.test(lowecontbl, correct = T)


lowcaptest <- subset(gtd, subset = gtd$low.capacity == "1")   

#5.1 retry

errorlevel05 <- 0.05
errorlevel10 <- 0.10
errorlevel01 <- 0.01

n1 <- nrow(gtd$nkill[gtd$bombing == "1"])
n2 <- nrow(gtd$nkill[gtd$bombing == "0"])

bombing.mean <- mean(gtd$nkill[gtd$bombing == "1" & gtd$nkill != "-999"])
non.bombingmean <- mean(gtd$nkill[gtd$bombing == "0" & gtd$nkill != "-999"])


v1 <- var(gtd$nkill[gtd$bombing == "1" & gtd$nkill != "-999"])
v2 <- var(gtd$nkill[gtd$bombing == "0" & gtd$nkill != "-999"])

bomb.error <- (v1^2/n1) + (v2^2/n2)

t <- ((bombing.mean-non.bombingmean) - 0) / sqrt(bomb.error)

a <- ((1/(n1-1))*(v1^2/n1))^2
b <- ((1/(n2-1))*(v2^2/n2))^2

DF <- (bomb.error^2)/(a + b)

pvalue <- 1- pt(t, df = DF)

pvalue <- pvalue * 2

pvalue

















