reg <- read.csv("/Users/anujthakkar/Documents/DS Project/NBA Regular Season.csv")
reg <- reg[1:22,]
restart <- read.csv("/Users/anujthakkar/Documents/DS Project/NBA Restart.csv")
teams <- c("BOS", "BKN", "DAL", "DEN", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "NOP", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
colors <- c("dark green", "black", "blue", "orange", "red3", "yellow", "red3", "purple", "navy", "red", "dark green", "red3", "blue", "blue", "red3", "orange", "red3", "purple4", "grey", "black", "purple4", "grey")

head(reg)
head(restart)

barplot(reg[,c("W")], names.arg = teams, col = colors, main = "Number of Wins during the Regular Season", ylab = "Games Won")
barplot(restart[,c("W")], names.arg = teams, col = colors, main = "Number of Wins during the Restart Season",  ylab = "Games Won")

mean(reg[,c("W")])
mean(restart[,c("W")])

var(reg[,c("W")])
var(restart[,c("W")])

library(pastecs)
stat.desc(reg[,c("W")])
stat.desc(restart[,c("W")])

###

# assumptions for anova and t.test
sd(reg$EDIFF[reg$CONF == "East"])
sd(reg$EDIFF[reg$CONF == "West"])

# checking for significant difference in means
summary(aov(reg$EDIFF~reg$CONF))
t.test(reg$EDIFF~reg$CONF, alternative = "two.sided")

# test for homogenity in variances
bartlett.test(reg$EDIFF~reg$CONF)

summary(reg$EDIFF[reg$CONF == "East"])
summary(reg$EDIFF[reg$CONF == "West"])
boxplot(reg$EDIFF~reg$CONF, col=c("blue", "red"), xlab='Conference', ylab = "Efficiency Differential")

###

mod1 <- lm(reg$EDIFF~reg$CONS, data = reg) # model 1
plot(reg$EDIFF~reg$CONS, xlab = "Consistency Ratings", ylab = "Efficiency Differentials")
abline(mod1, col="red", reg)
cor1 <- cor.test(reg$EDIFF, reg$CONS)$estimate # correlation test 1
cor1
varianceReg <- cor1^2 # R-squared
varianceReg
plot(mod1) # residual plots of model 1

mod2 <- lm(restart$EDIFF~restart$CONS, data = restart) # model 2
plot(restart$EDIFF~restart$CONS, xlab = "Consistency Ratings", ylab = "Efficiency Differentials")
abline(mod2, col="red", restart)
cor2 <- cor.test(restart$EDIFF, restart$CONS)$estimate # correlation test 2
cor2
varianceRes <- cor2^2 # R-squared
varianceRes
plot(mod2) # residual plots of model 2

###

west_regular <- reg[reg$CONF == "West",]
west_restart <- restart[restart$CONF == "West",]
percentagesRegular <- west_regular$pWIN.*100
percentagesRestart <- west_restart$pWIN.*100

mod3 <- lm(percentagesRegular~west_regular$W) # model 3
mod3
summary(mod3) # anova of linear model 3
plot(percentagesRegular ~ west_regular$W, main = "Regular Season", xlab = "Wins", ylab = "Projected Winning Percentage")
abline(mod3, col = "red", west_regular)
cor3 <- cor.test(percentagesRegular, west_regular$W) # correlation test 3
cor3

plot(mod3)

mod4 <- lm(percentagesRestart~west_restart$W) # model 4
mod4
summary(mod4) # anova of linear model 4
plot(percentagesRestart~west_restart$W, main = "NBA Restart", xlab = "Wins", ylab = "Projected Winning Percentage")
abline(mod4, col = "red", west_restart)
cor4 <- cor.test(percentagesRestart,west_restart$W) # correlation test 4
cor4

plot(mod4)


library(dplyr)

data.frame(west_regular[,c("TEAM","DIVISION", "W", "ACH")] %>%
  arrange(desc(west_regular$ACH)))

west_restart[,c("TEAM","DIVISION", "W", "ACH")] %>%
  arrange(desc(west_restart$ACH))

# comparing variances for Wilcoxon test
var(west_regular$ACH)
var(west_restart$ACH)

barplot(west_regular$ACH~west_regular$TEAM, main = "Regular Season", ylab = "Achievement Level", xlab = "Team")
barplot(west_restart$ACH~west_restart$TEAM, main = "Restart", ylab = "Achievement Level", xlab = "Team")

# looking for shift in the distribution from the median
wilcox.test(west_restart$ACH, west_regular$ACH)

# looking for significant difference in means
t.test(west_regular$ACH, west_restart$ACH, conf.level = .95)


