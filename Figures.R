#The first function gives more control over boxplots in R
#The second function gives more control over density plots in R

##########
#BOXPLOTS#
##########

#obj.list is list of different objects containing numbers you wish to display
#pos is vector of positions of boxplots on plot, e.g., c(1,2,3)
#cols is vector of colors of boxplots, e.g. c("lightgreen","purple","coral")
#atx is vector of positions of x-axis labels, e.g., c(1,2,3), though you might imagine situations where one label accounts for two boxplots
#labs is vector of x-axis labels
#xtitle is x-axis title
#mtitle is main title; set to "" if no title desired
#add.n will add "n=" below each label; default is FALSE
#NOTE: Beneath each plot, an italicized n= is included to let the reader know how many observations went into each boxplot

boxp = function(obj.list, pos, cols, atx, labs, xtitle, ytitle, mtitle, ylim=NA, add.n=FALSE, rotx=NA, xcex, ad=FALSE, horz=FALSE, rangex=1) {
    obj.list = lapply(obj.list, function(x) x[!is.na(x)])
    if (is.na(sum(ylim))==TRUE) {ylim=range(as.numeric(unlist(obj.list)))}
    if (horz==FALSE) {
    boxplot(obj.list[[1]], at=pos[1], xlim=c(0,ceiling(max(pos))), ylim=ylim, xaxt="n", col=cols[1], xlab=xtitle, ylab=ytitle, main=mtitle, add=ad, horizontal=horz)
    if (length(cols)>1) {
    for (i in 2:length(obj.list)) {
        boxplot(obj.list[[i]], at=pos[i], xaxt="n", add=TRUE, horizontal=horz, col=cols[i])
    }}
    #define number per group
    neq = sapply(obj.list, function(x) paste("n=",length(x),sep=""))
    #add ticks
    axis(1, at=atx, label=rep("", length(atx)), tck=-0.02)
    #add labels
    if (is.na(rotx)) {axis(1, at=atx, label=labs, line=-0.25, lwd=0, cex.axis=xcex)}
    if (!is.na(rotx)) {text(x=atx, y=par()$usr[3]-0.035*(par()$usr[4]-par()$usr[3]), labels=labs, srt=rotx, adj=1, xpd=TRUE, cex=xcex)}
    #add n= to plot
    if (add.n==TRUE) {mtext(text=neq, side=1, at=atx, cex=0.75, line=1.6, font=3)}
    }
    else if (horz==TRUE) {
    l = rangex*min(as.numeric(lapply(obj.list, function(x) min(x, na.rm=TRUE))),na.rm=TRUE)
    u = rangex*max(as.numeric(lapply(obj.list, function(x) max(x, na.rm=TRUE))),na.rm=TRUE)
    if (is.na(ylim)) {ylim=c(l, u)}
    boxplot(obj.list[[1]], at=pos[1], ylim=ylim, xlim=c(0,ceiling(max(pos))), yaxt="n", col=cols[1], xlab=xtitle, ylab=ytitle, main=mtitle, add=ad, horizontal=horz)
    if (length(cols)>1) {
    for (i in 2:length(obj.list)) {
    boxplot(obj.list[[i]], at=pos[i], yaxt="n", add=TRUE, horizontal=horz, col=cols[i])
    }}
    #define number per group
    neq = sapply(obj.list, function(x) paste("n=",length(x),sep=""))
    #add ticks
    axis(2, at=atx, label=rep("", length(atx)), tck=-0.02)
    #add labels
    if (is.na(rotx)) {axis(2, at=atx, label=labs, line=-0.25, lwd=0, cex.axis=xcex)}
    if (!is.na(rotx)) {text(y=atx, x=par()$usr[1]-0.035*(par()$usr[4]-par()$usr[1]), labels=labs, srt=rotx, adj=1, xpd=TRUE, cex=xcex)}
    #add n= to plot
    if (add.n==TRUE) {mtext(text=neq, side=2, at=atx, cex=0.75, line=1.6, font=3)}
}
}


#Test it out
#obj.list=list(a=rnorm(100),b=rnorm(100),c=rnorm(100),d=rnorm(100))
#Vertical
#boxp(obj.list, pos=c(0.5,1,2.5,3.5), cols=c("lightgreen","coral","dodgerblue","moccasin"), atx=c(0.5,1,2.5,3.5), labs=c("a","b","c","d"), xtitle="", ytitle="Variable 1", mtitle="This is a boxplot", ylim=NA, add.n=TRUE, rotx=NA, xcex=0.8, ad=FALSE, horz=FALSE, rangex=1)
##Horizontal
#boxp(obj.list, pos=c(0.5,1,2.5,3.5), cols=c("lightgreen","coral","dodgerblue","moccasin"), atx=c(0.5,1,2.5,3.5), labs=c("a","b","c","d"), xtitle="", ytitle="Variable 1", mtitle="This is a boxplot", ylim=NA, add.n=TRUE, rotx=NA, xcex=0.8, ad=FALSE, horz=TRUE, rangex=1)
#One variable
#boxp(obj.list=list(a=rnorm(100)),pos=0.5,cols="lightgreen",atx=0.5,labs="a",xtitle="Variable 1",mtitle="This is a boxplot")


######################
#Kernel density plots#
######################

#obj.list is list of different objects containing numbers you wish to display
#y is upper limit of y-axis
#infl is expansion parameter of x axis range, default is 1, but may wish to expand for visual ease
#cols is vector of colors of densities, e.g., c("lightgreen","purple","coral"), with length equal to number of elements in obj.list
#ltys is vector of line types, e.g., c(1,2,3), with length equal to the number of elements in obj.list
#xtitle is x-axis title
#mtitle is main title; set to "" if no title desired
#leg is vector of character strings to include in legend, e.g., c("White","Black","Other")
#px is x-coordinate of legend
#py is y-coordinate of legend

#NOTE: default settings are used from R density function in making the density plots
#SECOND NOTE: *You* must tinker with the "xl" parameter (lower and upper limit to x-axis), "y" parameter (upper limit to y-axis) and also the legend placements "px" and "py" (x and y-coords, respectively)
#THIRD NOTE: The legend includes (n=) in parentheses to let the reader know how many observations went into each density

densp = function(obj.list,y,xl=NA,cols,ltys,xtitle,mtitle,leg,px,py) {
    obj.list = lapply(obj.list, function(x) x[!is.na(x)])
    if (is.na(xl)) {xl = range(as.numeric(unlist(obj.list)))}
    plot(density(obj.list[[1]]),xlim=xl,ylim=c(0,y),col=cols[1],lty=ltys[1],xlab=xtitle,main=mtitle,lwd=1.5)
    if (length(cols)>1) {
    for (i in 2:length(obj.list)) {
        lines(density(obj.list[[i]]),col=cols[i],lty=ltys[i],lwd=1.5)
    }}
    #define number per group
neq = sapply(obj.list,function(x) paste(paste("(n=",length(x),sep=""),")",sep=""))
leg2 = paste(leg,neq,sep=" ")
#axis(1,at=atx,labels=labs,tick=TRUE)
legend(px,py,inset=0.05,cex=0.8,lty=ltys,col=cols,leg2,bty="n",lwd=1.5)
}

#Test it out
#Multiple variables
#densp(obj.list,y=0.5,infl=1.5,cols=c("lightgreen","coral","dodgerblue","purple"),ltys=c(1,2,3,4),xtitle="Variable 1",mtitle="This is a density plot",leg=names(obj.list),2,0.4)
#One variable
#densp(obj.list=list(a=rnorm(100)),y=0.4,infl=1.5,cols=c("lightgreen"),ltys=c(1),xtitle="Variable 1",mtitle="This is a density plot",leg="a",2,0.3)


###########
#HEAT MAPS#
###########
library(lattice)
library(latticeExtra)
#Function to add numbers if desired
myPanel <- function(x, y, z, ...) {
    panel.levelplot(x,y,z,...)
    panel.text(x, y, round(z,2)) #round to 2 decimal places
}

#Heatmap function
#form is formula, e.g., Z~X*Y, where Z is third dimension, X is x-axis, Y is y-axis
#dta is data frame containing variables in form
#col1 is bottom of ramp-up color, white in middle, ramps to col2
#addnum=TRUE if numbers desired on plot
#xtitle and ytitle are titles for x and y axes, respectively
#addstar=TRUE if want significant correlations added at p<level

hm = function(matr, type="spearman", col1, col2, addnum=FALSE, xtitle, ytitle, rotx, mn, addstar=FALSE, psig=0.05) {
    mat1 = rcorr(as.matrix(matr), type=type)
    tt1r = mk.grid(mat1$r)
    mp = which(mat1$P<psig & !is.na(mat1$P), arr.ind=TRUE)
    mp[, 2] = mp[, 2]+1
    mp1 = mp[mp[, 1]!= mp[, 2], ]
    rgb.palette = colorRampPalette(c(col1, "white", col2), space="Lab")
    if (addnum==TRUE & addstar==FALSE) {
        levelplot(as.formula("Z~cols*rows"), data=tt1r, panel=myPanel, col.regions=rgb.palette(120), xlab=xtitle, ylab=ytitle, scales=list(tck=c(1,0), x=list(rot=rotx)), main=mn)
    } else if (addnum==FALSE & addstar==FALSE) {
        levelplot(as.formula("Z~cols*rows"), data=tt1r, col.regions=rgb.palette(120), xlab=xtitle, ylab=ytitle, scales=list(tck=c(1,0), x=list(rot=rotx)), main=mn)
    } else if (addnum==FALSE & addstar==TRUE) {
        levelplot(as.formula("Z~cols*rows"), data=tt1r, col.regions=rgb.palette(120), xlab=xtitle, ylab=ytitle, scales=list(tck=c(1,0), x=list(rot=rotx)), main=mn) + layer(panel.text(mp1[, 1], mp1[, 2],  "*"))
    }
}

#Example
#X = seq(1:10)
#Y = seq(1:12)
#grid <- expand.grid(X=X, Y=Y)
#grid$Z <- runif(12*10, -1, 1)

#hm("Z~X*Y",grid,"grey","lightcoral",addnum=FALSE,xtitle="x-axis",ytitle="y-axis") #no numbers
#hm("Z~X*Y",grid,"grey","lightcoral",addnum=TRUE,xtitle="x-axis",ytitle="y-axis") #numbers

#Go from table/dataframe to grid
mk.grid = function(ex) {
grid = expand.grid(rows=rownames(ex),cols=colnames(ex))
grid$Z = as.numeric(unlist(ex))
grid
}

#ex = as.data.frame(matrix(rbinom(120,1,0.5),12,10))
#tt = mk.grid(ex)
#hm("Z~cols*rows",tt,"grey","lightcoral",addnum=FALSE,xtitle="x-axis",ytitle="y-axis")


##############
#RADAR CHARTS#
##############
library(fmsb)

radar = function(dat,names.d,cols,n.axes,tit,lab.cex,axis.cex, pltys) {
    colnames(dat) = names.d
    radarchart(dat,pcol=cols,axistype=4,plty=pltys,seg=n.axes-1,plwd=2,cglty=1,cglcol="gray75",vlcex=lab.cex,calcex=axis.cex,axislabcol=1,caxislabels=round(seq(from=min(dat[1:2,]),to=max(dat[1:2,]),length=n.axes),2),title=tit)
}

#Example
#x = c(rep(4.5,7),rep(0,7),3.34, 3.28, 1.37, 1.12, 3.52, 4.07, 3.66)
#a = as.data.frame(matrix(x,nrow=3, ncol=7,byrow=T))
#radar(dat=a,cols="springgreen3",n.axes=4,tit="Example")


##############
#Scatterplots#
##############
scplot = function(dta, var.x, var.y, clr, xinf, yinf, xr=NA, yr=NA, xtitle, ytitle, mtitle, add.cor=FALSE, lpos="topleft", lcex=0.8, pchar=1, add.n=FALSE, lwd=1) {
    x = dta[, which(names(dta)==var.x)]
    y = dta[, which(names(dta)==var.y)]
    d1 = dta[!is.na(x) & !is.na(y), ]
    x2 = d1[, which(names(d1)==var.x)]
    y2 = d1[, which(names(d1)==var.y)]
    if (is.na(xr)) {xr = xinf*range(x2)} #problem is HERE; figure out names
    if (is.na(yr)) {yr = yinf*range(y2)}
    if (add.n==TRUE) {mtitle=paste(mtitle, paste(paste("(n=", dim(d1)[1], sep=""), ")", sep=""), sep=" ")}
    plot(x2, y2, xlab=xtitle, ylab=ytitle, main=mtitle, xlim=xr, ylim=yr, pch=pchar)
    lines(lowess(x2, y2), col=clr, lwd=lwd)

#Add correlations
if (add.cor==TRUE) {
#c1 = cor.test(x2, y2)
c2 = cor.test(x2, y2, method="spearman")

#e1 = round(c1$estimate, 2)
#p1 = round(c1$p.value, 3)

e2 = round(c2$estimate, 2)
p2 = round(c2$p.value, 3)

if (p2>0) {
    legend(lpos, legend=c(paste(paste("Rank Corr=", bquote(.(e2)), sep=""), paste("P=", bquote(.(p2)), sep=""), sep=", ")), cex=lcex, bty="n")
} else if (p2==0) {
    legend(lpos, legend=c(paste(paste("Rank Corr=", bquote(.(e2)), sep=""), "P<.001", sep=", ")), cex=lcex, bty="n")
}
}
}


scplot2 = function(dta, var.x, var.y, clr, xinf, yinf, xr=NA, yr=NA, xtitle, ytitle, mtitle, add.cor=FALSE, lpos="topleft", lcex=0.8) {
    x = dta[, which(names(dta)==var.x)]
    y = dta[, which(names(dta)==var.y)]
    d1 = dta[!is.na(x) & !is.na(y), ]
    x2 = d1[, which(names(d1)==var.x)]
    y2 = d1[, which(names(d1)==var.y)]
    if (is.na(xr)) {xr = xinf*range(x2)} #problem is HERE; figure out names
    if (is.na(yr)) {yr = yinf*range(y2)}
    plot(x2, y2, xlab=xtitle, ylab=ytitle, main=mtitle, xlim=xr, ylim=yr)
    lines(lowess(x2, y2), col=clr)

#Add correlations
if (add.cor==TRUE) {
c1 = cor.test(x2, y2)
c2 = cor.test(x2, y2, method="spearman")

e1 = round(c1$estimate, 2)
p1 = round(c1$p.value, 3)

e2 = round(c2$estimate, 2)
p2 = round(c2$p.value, 3)

legend(lpos, legend=c(paste(paste("Corr=", bquote(.(e1)), sep=""), paste("P=", bquote(.(p1)), sep=""), sep=", "), paste(paste("Rank Corr=", bquote(.(e2)), sep=""), paste("P=", bquote(.(p2)), sep=""), sep=", ")), cex=lcex, bty="n")
}
}


##Sample data
#head(mtcars)
#scplot(dta=mtcars, var.x="wt", var.y="mpg", clr="black", xinf=1, yinf=1, xr=c(1, 6), yr=c(10, 35), xtitle="Weight (tons)", ytitle="MPG", mtitle="", add.cor=TRUE, lpos="topright", lcex=0.8, pchar=1, add.n=FALSE, lwd=2)




















