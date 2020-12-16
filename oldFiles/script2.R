x1=c(0,0.25,0.5,0.75,0.95,1)
y1=c(140,xx[17:20]+5,200)

x2=c(0,20/64,44/64,1)
y2=c(90,120,130,140)

u1=runif(250000)
u2=runif(750000)
yu1=approx(x1,y1,xout=u1)
yu2=approx(x2,y2,xout=u2)

sum(yu2$y<115)/1000000
sum(yu2$y<130)/1000000
sum(yu2$y<140)/1000000

yu=c(yu1$y,yu2$y)
mean(yu<115)
mean(yu<130)
mean(yu<140)

mean(c(yu1$y,yu2$y))
hist(c(yu1$y,yu2$y))


x=c(0,0.05,0.25,0.5,0.75,0.95,1)
y=c(90,xx[1:5]+10,200)

u=runif(100000)
yu=approx(x,y,xout=u)

mean(c(yu$y))
hist(c(yu$y))

mean(yu$y<120)
mean(yu$y<130)
mean(yu$y<140)
