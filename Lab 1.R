EPI_data <- read.csv("C:/Users/hicha/Downloads/epi2024results06022024.csv")

View(EPI_data)

attach(EPI_data)

EPI.new

NAs <- is.na(EPI.new)

EPI.new.noNAs <- EPI.new[!NAs]
------------------------------------------
"Exercise 1"
summary(EPI.new) 
fivenum(EPI.new,na.rm=TRUE) 
stem(EPI.new) 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)
-------------------------------------------
boxplot(EPI.new, APO.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new, na.rm = TRUE, bw = 1))


lines(density(EPI.new, na.rm = TRUE, bw = "SJ"))


boxplot(EPI.new, APO.new)
rug(EPI.new) 

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)
lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 

"------------------------------------------"
"Exercise 2"

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)


------------------------------------
"Exercise 2a"


plot(ecdf(APO.new), do.points = FALSE, verticals = TRUE, main = "CDF for APO")

plot(ecdf(WRS.new), do.points = FALSE, verticals = TRUE, main = "CDF for WRS")


qqnorm(APO.new, main = "Q-Q Plot for APO")
qqline(APO)

qqnorm(WRS.new, main = "Q-Q Plot for WRS") 
qqline(WRS)

qqplot(rnorm(250), APO.new, xlab = "Q-Q Plot for Normal Distribution (APO)", main = "Q-Q Plot: APO vs Normal")
qqline(APO)

qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q Plot for t-distribution (APO)", main = "Q-Q Plot: APO vs t-distribution")
qqline(APO)

qqplot(rnorm(250), WRS.new, xlab = "Q-Q Plot for Normal Distribution (WRS)", main = "Q-Q Plot: WRS vs Normal")
qqline(WRS)

qqplot(rt(250, df = 5), WRS, xlab = "Q-Q Plot for t-distribution (WRS)", main = "Q-Q Plot: WRS vs t-distribution")
qqline(WRS)

