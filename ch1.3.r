# Chapter 1.3
require(LearnBayes)

defaultChartDim <- 500 # Default size of graphics
dir.create("ch1.3_img")

x=rnorm(10,mean=50,sd=10)
y=rnorm(10,mean=50,sd=10)

tstatistic=function(x,y)
{
  m=length(x)
  n=length(y)
  sp = sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
  t.stat=(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t.stat)
}

data.x=c(1,4,3,6,5)
data.y=c(5,4,7,6,10)
print(tstatistic(data.x,data.y))

# More in this section that has been omitted.