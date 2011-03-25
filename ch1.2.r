# Chapter 1
require(LearnBayes)

defaultChartDim <- 500 # Default size of graphics
dir.create("ch1_img")

studentdata[1,]

attach(studentdata)

table(Drink)

png("ch1_img/drink_table.png", width=defaultChartDim, height=defaultChartDim)
barplot(table(Drink), xlab="Drink",ylab="Count")
dev.off()
