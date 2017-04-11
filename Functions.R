#Function to compute multiple imputation CI
MI.conf.int = function(est , se) {
n = length(est)
Q.bar = mean(est , na.rm = TRUE)
W = mean(se^2 , na.rm = TRUE)
B = sum((est-Q.bar)^2 , na.rm = TRUE) / (1/(n-1))
T = W + (1+1/n) * B
gamma = (n-1) * ( (1 + W / ((1+1/n)*B) )^2 )
crit = qt(0.975 , gamma)
l = Q.bar - crit*sqrt(T)
u = Q.bar + crit*sqrt(T)
e = Q.bar
pres = paste(round(e,2),paste(paste(paste(paste("(",round(l,2),sep=""),", ",sep=""),round(u,2),sep=""),")",sep=""),sep=" ")
output.all = list(e=e,l=l,u=u,pres=pres)
return(output.all)
}


#Compute Wald test for hypotheses in regression model for H0: L beta = h
WaldTest = function(L,betahat,Vn,h=0)  {
      WaldTest = numeric(3)
      names(WaldTest) = c("W","df","p-value")
      r = dim(L)[1] #number of restrictions (df)
      W = t(L%*%betahat-h) %*% solve(L%*%Vn%*%t(L)) %*% (L%*%betahat-h) #Chi-square statistic
      W = as.numeric(W)
      pval = 1-pchisq(W,r) #p-value
      WaldTest[1] = W; WaldTest[2] = r; WaldTest[3] = pval
      WaldTest
  }

#Example (anova is sequential in R)
#m1 = lm(wt~cyl+disp+carb,data=mtcars)
#m2 = lm(wt~disp+carb,data=mtcars)
#m3 = lm(wt~carb,data=mtcars)
#anova(m2,m1)
#WaldTest(L=matrix(c(0,1,0,0),1,4),betahat=coef(m1),Vn=vcov(m1),h=0)
#anova(m3,m1)
#WaldTest(L=rbind(c(0,1,0,0),c(0,0,1,0)),betahat=coef(m1),Vn=vcov(m1),h=0) #similar to F-test in ANOVA without dividing by denom df and comparing to F


#Compute CI for linear combination of parameters
#j is model object
#a must be in matrix form, e.g., matrix(c(1,0,0,0),1,4)

comp.CI = function(j,a) {
v1 = vcov(j) #covariance matrix
var.s = a%*%v1%*%t(a)
est = a%*%summary(j)$coeff[,1] #summary(j)$coeff[,1] are the parameter estimates
e = as.numeric(est)
l = as.numeric(est-qnorm(0.975)*sqrt(var.s)) #lower 95% CI
u = as.numeric(est+qnorm(0.975)*sqrt(var.s)) #upper 95% CI
pres = paste(round(e,2),paste(paste(paste(paste("(",round(l,2),sep=""),", ",sep=""),round(u,2),sep=""),")",sep=""),sep=" ")
output.all = list(e=e,l=l,u=u,pres=pres)
return(output.all)
}


#Example
#Test on mtcars
#data(mtcars)
#names(mtcars)

#Simple model
#m1 = lm(wt~cyl+disp+carb,data=mtcars)

#CI for b0
#comp.CI(m1,a=matrix(c(1,0,0,0),1,4))

#CI for b0+b1
#comp.CI(m1,a=matrix(c(1,1,0,0),1,4))

#Function to convert Excel column names to numbers
exc = function(strg) {
    alph = c("abcdefghijklmnopqrstuvwxyz")
    alph2 = substring(alph,seq(1,nchar(alph),1),seq(1,nchar(alph),1))
    strg2 = substring(strg,seq(1,nchar(strg),1),seq(1,nchar(strg),1))
    strg3 = sapply(strg2,function(x) which(alph2==x))
    nch = nchar(strg)
    pow = -seq(-nch,-1,1)-1
    pow26 = sapply(pow,function(x) 26^x)
    sum(pow26*strg3)
}

#exc("aaj")

#Function to convert state FIPS codes to abbreviations
#AK 	02 	 	MS 	28
#AL 	01 	 	MT 	30
#AR 	05 	 	NC 	37
#AS 	60 		ND 	38
#AZ 	04 		NE 	31
#CA 	06 		NH 	33
#CO 	08 		NJ 	34
#CT 	09 	 	NM 	35
#DC 	11 	 	NV 	32
#DE 	10 		NY 	36
#FL 	12 		OH 	39
#GA 	13 	 	OK 	40
#GU 	66 		OR 	41
#HI 	15 	 	PA 	42
#IA 	19 		PR 	72
#ID 	16 	 	RI 	44
#IL 	17 	 	SC 	45
#IN 	18 	 	SD 	46
#KS 	20 	 	TN 	47
#KY 	21 	 	TX 	48
#LA 	22 	 	UT 	49
#MA 	25 	 	VA 	51
#MD 	24 	 	VI 	78
#ME 	23 	 	VT 	50
#MI 	26 	 	WA 	53
#MN 	27 	 	WI 	55
#MO 	29 	 	WV 	54
#  	  	  	WY 	56


st.fips = function(x) {
    x1 = NA
    if (is.na(x)) {x1=NA}
    else if (x==2) {x1="AK"}
    else if (x==1) {x1="AL"}
    else if (x==5) {x1="AR"}
    else if (x==60) {x1="AS"}
    else if (x==4) {x1="AZ"}
    else if (x==6) {x1="CA"}
    else if (x==8) {x1="CO"}
    else if (x==9) {x1="CT"}
    else if (x==11) {x1="DC"}
    else if (x==10) {x1="DE"}
    else if (x==12) {x1="FL"}
    else if (x==13) {x1="GA"}
    else if (x==66) {x1="GU"}
    else if (x==15) {x1="HI"}
    else if (x==19) {x1="IA"}
    else if (x==16) {x1="ID"}
    else if (x==17) {x1="IL"}
    else if (x==18) {x1="IN"}
    else if (x==20) {x1="KS"}
    else if (x==21) {x1="KY"}
    else if (x==22) {x1="LA"}
    else if (x==25) {x1="MA"}
    else if (x==24) {x1="MD"}
    else if (x==23) {x1="ME"}
    else if (x==26) {x1="MI"}
    else if (x==27) {x1="MN"}
    else if (x==29) {x1="MO"}
    else if (x==28) {x1="MS"}
    else if (x==30) {x1="MT"}
    else if (x==37) {x1="NC"}
    else if (x==38) {x1="ND"}
    else if (x==31) {x1="NE"}
    else if (x==33) {x1="NH"}
    else if (x==34) {x1="NJ"}
    else if (x==35) {x1="NM"}
    else if (x==32) {x1="NV"}
    else if (x==36) {x1="NY"}
    else if (x==39) {x1="OH"}
    else if (x==40) {x1="OK"}
    else if (x==41) {x1="OR"}
    else if (x==42) {x1="PA"}
    else if (x==72) {x1="PR"}
    else if (x==44) {x1="RI"}
    else if (x==45) {x1="SC"}
    else if (x==46) {x1="SD"}
    else if (x==47) {x1="TN"}
    else if (x==48) {x1="TX"}
    else if (x==49) {x1="UT"}
    else if (x==51) {x1="VA"}
    else if (x==78) {x1="VI"}
    else if (x==50) {x1="VT"}
    else if (x==53) {x1="WA"}
    else if (x==55) {x1="WI"}
    else if (x==54) {x1="WV"}
    else if (x==56) {x1="WY"}
    x1
}

#Test it out
#ee = c(72,50,34,37,13,25)
#unlist(lapply(ee,st.fips))

#Function to make table of parameter estimates and 95% CIs
#Regular CI function
CI = function(mat) {
 se = as.numeric(mat[2])
 e = as.numeric(mat[1])
 l = e-qnorm(0.975)*se #lower 95% CI
 u = e+qnorm(0.975)*se #upper 95% CI
 pres = paste(round(e,2),paste(paste(paste(paste("(",round(l,2),sep=""),", ",sep=""),round(u,2),sep=""),")",sep=""),sep=" ")
 return(pres)
}

#Logistic regression CI function - fix this?  Delta method needed?
exp.CI = function(mat) {
 se = as.numeric(mat[3]) #move to 2 for OR
 est = as.numeric(mat[1])
 e = exp(est)
 l = exp(est-qnorm(0.975)*se) #lower 95% CI
 u = exp(est+qnorm(0.975)*se) #upper 95% CI
 pres = paste(round(e,2),paste(paste(paste(paste("(",round(l,2),sep=""),", ",sep=""),round(u,2),sep=""),")",sep=""),sep=" ")
 return(pres)
}

#Housekeeping function
mtrx = function(x) {
x2 = x
if (sum(dim(x2))==0) {x2 = matrix(x,1,length(x))}
x2
}

#Table making function
table.eff = function(m1,name,e=FALSE) {
    if (e==FALSE) {j1 = apply(summary(m1)$coeff,1,CI)}
    else if (e==TRUE) {j1 = apply(summary(m1)$coeff,1,exp.CI)}
r = diag(length(j1))
pval = rep("",length(j1))
for (i in 1:length(name)) {
 pval[grep(name[i],names(j1))[1]] = round(WaldTest(mtrx(r[grepl(name[i],names(j1)),]),coef(m1),vcov(m1))[3],4)
}
tab = data.frame(eff=j1,pval=pval)
tab
}


#mtcars$cyl.F = factor(mtcars$cyl)
#m1 = lm(wt~cyl.F+disp+carb,data=mtcars)
#name = c("cyl.F","disp","carb")
#tab1 = table.eff(m1,name,e=FALSE)
#tab2 = table.eff(m1,name,e=TRUE)

#Record in word doc
#word.doc(obj.list=list(tab1,tab2),obj.title=c("Table 1: Model CIs with p-values","Table 2: Exp model CIs with p-values"),dest='/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables_ex33.docx',ftype="Arial",col.odd="white")


#Reverse Likert functions
lik1 = function(x) {
    x1 = NA
    if (is.na(x)) {x1=NA}
    else if (x=="a.StronglyAgree") {x1=1}
    else if (x=="b.Agree") {x1=2}
    else if (x=="c.Neutral") {x1=3}
    else if (x=="d.Disagree") {x1=4}
    else if (x=="e.StronglyDisagree") {x1=5}
    x1
}

lik2 = function(x) {
    x1 = NA
    if (is.na(x)) {x1=NA}
    else if (x=="a.VeryLikely") {x1=1}
    else if (x=="b.Likely") {x1=2}
    else if (x=="c.Neutral") {x1=3}
    else if (x=="d.Unlikely") {x1=4}
    else if (x=="e.VeryUnlikely") {x1=5}
    x1
}



