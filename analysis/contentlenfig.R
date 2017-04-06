# note: this script requires .df.anl (data frame for analysis)
# generated in the analysis.R, after cleaning, etc.

# http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

filterDowners = function(.df.anl,thisL) { # to avoid using these lines twice... filters down data
  .d = .df.anl[.df.anl$wlen==thisL&.df.anl$k<=thisL & .df.anl$category=="content",]
  .d = .d[.d$uninfo<median(.d$uninfo),]
  dat = aggregate(.d$RT,by=list(.d$k),FUN = function(x) c(mean = mean(x), sd = sd(x)))
  dat$x[,2] = dat$x[,2]/sqrt(length(unique(.d$s)))
  return(dat)
}

.df.anl$residRT = resid(lm(RT~digraphBL,data=.df.anl))

filterUppers = function(.df.anl,thisL) {
  .d = .df.anl[.df.anl$wlen==thisL&.df.anl$k<=thisL & .df.anl$category=="content",]
  .d = .d[.d$uninfo>=median(.d$uninfo),]
  dat = aggregate(.d$RT,by=list(.d$k),FUN = function(x) c(mean = mean(x), sd = sd(x)))
  dat$x[,2] = dat$x[,2]/sqrt(length(unique(.d$s)))
  return(dat)
}

maxy = 255
miny = 140
lrange = 3:9
ltyp = 'o'
pchHigh = 16
pchLow = 15
lwd = 2

pdf(file='fig3-RT_content_by_k.pdf',width=8,height=5.5)

for (i in 1:length(lrange)) {
  
  thisL = lrange[i] # 
  dat = filterDowners(.df.anl,thisL) # low information
  if (i == 1) {
    plot(dat$Group.1,dat$x[,1],type=ltyp,lwd=lwd, # plot low-information items
         ylim=c(miny,maxy),xlim=c(1.5,54),xaxt='none',
         pch=pchLow,col='darkgreen',
         xlab='',ylab='IKI (ms)',
         main='IKI for Content words by Length and Word Frequency')
  } else {
    points(dat$Group.1+sum(lrange[1:(i-1)])+2*(i-1),dat$x[,1], # superimpose
           type=ltyp,lwd=lwd,ylim=c(miny,maxy),
           pch=pchLow,col='darkgreen')
  }
  #error.bar(1:i,dat$x[,1],2*dat$x[,2])
  
  dat = filterUppers(.df.anl,thisL) # high information
  if (i == 1) {
    points(dat$Group.1,dat$x[,1],type=ltyp,
           lwd=lwd,col='red',pch=pchHigh,lty='solid')
    textx = -.5
    points(c(-.5,-.5),c(0,400),type='l',col='gray')  
  } else {
    points(dat$Group.1+sum(lrange[1:(i-1)])+2*(i-1), # plot high information items
           dat$x[,1],type=ltyp,lwd=lwd,
           col='red',pch=pchHigh,lty='solid')
    thisx = sum(lrange[1:(i-1)])+2*(i-1)-.5
    points(c(thisx,thisx),c(0,400),type='l',col='gray') # aesthetics: dividers
    textx = sum(lrange[1:(i-1)])+2*(i-1) # where the k= should go
  }  
  text(textx,250,paste('k =',thisL),cex=.8,pos=4) # specify length on plot
  #error.bar(1:i,dat$x[,1],2*dat$x[,2])
}

# ### let's show the on the plot in the same way, to the left
# dat = filterDowners(.df.anl[.df.anl$cw=='movie',],5)
# points(dat$Group.1-8,dat$x[,1],
#        type=ltyp,lwd=lwd,ylim=c(miny,maxy),pch=pchLow,col='darkgreen')
# 
# dat = filterUppers(.df.anl[.df.anl$cw=='movie',],5)
# points(dat$Group.1-8,
#        dat$x[,1],type=ltyp,lwd=lwd,col='red',pch=pchHigh,lty='solid')
# thisx = sum(lrange[1:(i-1)])+2*(i-1)-.5
# points(c(thisx,thisx),c(0,400),type='l',col='gray')  
# text(-8,235,paste('\"movie\"'),cex=.8,pos=4)

title(xlab='Character position (1, ..., k)',line=1)

dev.off()
