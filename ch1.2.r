# Chapter 1.2
require(LearnBayes)

defaultChartDim <- 500 # Default size of graphics
dir.create("ch1.2_img")

studentdata[1,]

attach(studentdata)

table(Drink)

png("ch1.2_img/drink_table.png", width=defaultChartDim, height=defaultChartDim)
barplot(table(Drink), xlab="Drink",ylab="Count")
dev.off()

hours.of.sleep = WakeUp - ToSleep

summary(hours.of.sleep)

png("ch1.2_img/hours_of_sleep_by_gender.png", width=defaultChartDim, height=defaultChartDim)
boxplot(hours.of.sleep~Gender,ylab="Hours of Sleep")
dev.off()

female.Haircut=Haircut[Gender=="female"]
male.Haircut=Haircut[Gender=="male"]

summary(female.Haircut)
summary(male.Haircut)

png("ch1.2_img/wakeup_time_vs_hours_of_sleep.png", width=defaultChartDim, height=defaultChartDim)
plot(jitter(ToSleep),jitter(hours.of.sleep))
dev.off()

fit=lm(hours.of.sleep~ToSleep)

fit

png("ch1.2_img/fit_wakeup_time_vs_hours_of_sleep.png", width=defaultChartDim, height=defaultChartDim)
plot(jitter(ToSleep),jitter(hours.of.sleep))
abline(fit)
dev.off()

