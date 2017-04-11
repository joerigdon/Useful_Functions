#Load packages
#install.packages("Gmisc")
library(Gmisc,verbose=FALSE)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("ReporteRs")
library(ReporteRs)

#Function to compute mean (SEM)
describeSEM = function (x, html = TRUE, digits = 1, number_first = TRUE, useNA = c("ifany", "no", "always"), useNA.digits = digits, percentage_sign = TRUE, plusmin_str, language = "en", ...)
{
    dot_args <- list(...)
    if ("show_missing_digits" %in% names(dot_args)) {
        useNA.digits <- dot_args$show_missing_digits
        dot_args$show_missing_digits <- NULL
        warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
    }
    if ("show_missing" %in% names(dot_args)) {
        if (missing(useNA)) {
            useNA <- convertShowMissing(dot_args$show_missing)
        }
        dot_args$show_missing <- NULL
        warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
    }
    useNA <- match.arg(useNA)
    if (missing(plusmin_str))
        if (html)
            plusmin_str <- "&plusmn;"
        else plusmin_str <- "\\pm"
    ret <- c(sprintf(sprintf("%%.%df (%s%%3.%df)", digits, plusmin_str,
        digits), mean(x, na.rm = T), sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))))
    if (html == FALSE)
        ret <- sprintf("$%s$", ret)
    if (useNA %in% c("ifany", "always") & sum(is.na(x)) > 0) {
        ret <- rbind(ret, descGetMissing(x = x, html = html,
            number_first = number_first, percentage_sign = percentage_sign,
            language = language, useNA.digits = useNA.digits,
            dot_args = dot_args))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else if (useNA == "always") {
        if (percentage_sign == TRUE)
            percentage_sign <- ifelse(html, "%", "\\%")
        else if (is.character(percentage_sign) == FALSE)
            percentage_sign = ""
        empty <- sprintf(ifelse(number_first, "0 (0%s)", "0%s (0)"),
            percentage_sign)
        ret <- rbind(ret, rep(empty, times = NCOL(ret)))
        rownames(ret) <- c("Mean (SEM)", "Missing")
    }
    else {
        names(ret) <- "Mean (SEM)"
    }
    return(ret)
}

################################
#FUNCTION TO AUTOMATE TABLE 1S"#
################################

#data is data frame
#var.names is list of data frame variables you wish to include as rows in the table
#ind.cat is 0/1 vector of same length as var.names equal to 0 if the variable is continuous and 1 if categorical
#group.name is the categorical variable you wish to display in the columns
#cfn is how you summarize continuous variables; describeMean if mean (sd) desired; describeMedian if median (IQR) desired
#miss is equal to "no" if no missing values to be displayed; "always" if always display (even if 0); "ifany" only display if any missing values
#NOTE: set miss="always" if you want both levels displayed for binary variables
#pval is equal to TRUE if want p-values; FALSE if not
#tot is equal to "last" if you want a column for totals and FALSE if not
#digit is the number of digits you want displayed, e.g., 1 if 12.1, 2 if 12.12, etc.
#NOTE: I recommend prefixing categories of categorical variables with alphabetized letters to ensure ordering in the table, e.g., for race, code it as "a.Black", "b.White", etc.

mktab = function(data,var.names,ind.cat,group.name,cfn,miss,pval,tot,digit) {
    n = names(data)
    cols = data[,which(n==group.name)]
    r = table(data[,which(n==group.name)])
    if (tot=="last") {r = matrix(c(r,sum(r)),1,length(r)+1)} #update r - this is why I was having trouble!!
    j = c(apply(r,1,function(x) paste("n=",x,sep="")))
    for (i in 1:length(var.names)) {
        if (ind.cat[i]==0) {
            outc = data[,which(n==var.names[i])]
            label(outc) = var.names[i]
            dd2 = getDescriptionStatsBy(outc,cols,html=TRUE,useNA=miss,statistics=pval,add_total_col=tot,continuous_fn=cfn,digits=digit)
            rownames(dd2)[1] = var.names[i]
        }
        else if (ind.cat[i]==1) {
            outc = data[,which(n==var.names[i])]
            label(outc) = var.names[i]
            dd1 = getDescriptionStatsBy(factor(outc),cols,html=TRUE,useNA=miss,statistics=pval,add_total_col=tot,digits=digit)
            dd2 = rbind(rep("",dim(dd1)[2]),dd1)
            rownames(dd2)[1] = var.names[i]
        }
      j = rbind(j,dd2)
    }
    #Clean up a few things
    k = t(apply(j,1,function(x) gsub("&plusmn;", "\\±", x)))
    k2 = t(apply(k,1,function(x) gsub("&lt; ", "<", x)))
    rownames(k2)[rownames(k2)=="j"] = ""
    rownames(k2) = lapply(rownames(k2),function(x) gsub("a[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("b[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("c[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("d[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("e[.]", " ", x))
    rownames(k2) = lapply(rownames(k2),function(x) gsub("f[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("a[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("b[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("c[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("d[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("e[.]", " ", x))
    colnames(k2) = lapply(colnames(k2),function(x) gsub("f[.]", " ", x))
    #Remove uninformative missing values if missing="ifany"
    if (miss=="always") {n0 = apply(k2,1,function(x) sum(x=="0 (0%)" | x=="0 (0.0%)" | x==""))
    if (pval==TRUE) {r = matrix(c(r,""),1,length(r)+1)}
    rmv = which(rownames(k2)=="Missing" & n0==length(r))
    if (length(rmv)>0) {k2 = k2[-as.numeric(rmv),]}}
    #Remove "n=" below P-value if pval=TRUE
    if (pval==TRUE) {k2[1,colnames(k2)=="P-value"] = ""}
    k2
}


#############################
#Function for two-way tables#
#############################

#data is data frame
#rowvar is the row variable to summarize
#colvar is the column variable to summarize
#disp.var is the display variable
#ind.bin is whether or not the display variable is binary (TRUE if yes, FALSE if no)
#digit is the number of digits to display in the statistics
mktab2 = function(data, rowvar, colvar, disp.var, ind.bin, tot=FALSE, digit) {
    row = names(table(data[, names(data)==rowvar]))
    col = names(table(data[, names(data)==colvar]))
    mat = matrix(NA, nrow=length(row), ncol=length(col))
    rownames(mat) = row
    colnames(mat) = col
    disp = as.numeric(data[, names(data)==disp.var]) #variable in table
    rtot = rep(NA, length(row)) #row totals
    ctot = rep(NA, length(col)) #column totals
    for (i in 1:length(row)) {
    for (j in 1:length(col)) {
        v1 = rownames(mat)[i]
        v2 = colnames(mat)[j]
        if (ind.bin==TRUE) {
            mat[i,j] = paste(sum(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            rtot[i] = paste(sum(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==rowvar]==v1)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            ctot[j] = paste(sum(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE) , paste(length(disp[which(!is.na(disp) & data[, names(data)==colvar]==v2)]), paste(paste("(", round(100*mean(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
        }
        else if (ind.bin==FALSE) {
            mat[i,j] = paste(round(mean(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==rowvar]==v1 & data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            rtot[i] = paste(round(mean(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==rowvar]==v1)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            ctot[j] = paste(round(mean(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), paste("(", paste(round(sd(disp[which(data[, names(data)==colvar]==v2)], na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")

    }
    }
}
    if (tot==TRUE) {
        mat = cbind(mat, rtot)
        if (ind.bin==TRUE) {
            ovtot = paste(sum(disp, na.rm=TRUE) , paste(length(disp), paste(paste("(", round(100*mean(disp, na.rm=TRUE), digit), sep=""), ")", sep="%"), sep=" "), sep="/")
            }
        else if (ind.bin==FALSE) {
            ovtot = paste(round(mean(disp, na.rm=TRUE), digit), paste("(", paste(round(sd(disp, na.rm=TRUE), digit), ")", sep=""), sep="±"), sep=" ")
            }
        ctot = c(ctot, ovtot)
        mat = rbind(mat, ctot)
        }
mat
}

##########################################
#Function to automate Word table creation#
##########################################

#obj.list is list of tables you created
#obj.title is list of character string titles corresponding to those tables
#dest is where you wish to save the word document
#ftype is font type, e.g., "Arial"
#NOTE: a Word document will be created that likely needs *YOUR* modification - you will have to add the categorical variable title names in the blank row left for them, and also adjust their level names for the reader's convenience (may also have to adjust continuous variable names too)
#SECOND NOTE: you may also want to play around with the formatting in Word if you like other settings (e.g., bolded variable names, remove striping in background, add colors, etc.)

word.doc = function(obj.list,obj.title,dest,ftype,col.odd) {
    doc = docx(title ='My document')
    options("ReporteRs-fontsize"=11) #font-size
    options("ReporteRs-default-font"=ftype) #font-type
    i = 1
    for (t in obj.list) {
    my_title = pot(obj.title[i],textProperties(font.size=14,font.weight="bold"))
    doc = addParagraph(doc,value=my_title,stylename="Normal") #Add title
    doc = addFlexTable(doc,setZebraStyle(vanilla.table(t,add.rownames=T),odd=col.odd,even='white'),par.properties=parCenter()) #Add simple table
    my_text = pot("",textProperties(font.size=14,font.weight="bold"))
    doc = addParagraph(doc,value=my_text,stylename="Normal") #Add white space
    my_text = pot("",textProperties(font.size=14,font.weight="bold"))
    doc = addParagraph(doc,value=my_text,stylename="Normal") #Add more white space
    i = i+1
}
writeDoc(doc,dest)
}

#col.odd='#eeeeee'

#Test it out on data set
#Make a binary variable with some missing data
mtcars$hp2 = mtcars$hp>100
mtcars$hp2b = mtcars$hp2
mtcars$hp2b[3] = NA

#No p-values with totals column
t1 = mktab(data=mtcars, var.names=c("mpg","qsec","carb","hp2","hp2b"),ind.cat=c(0,0,1,1,1), group.name="gear", cfn=describeSEM, miss="always", pval=FALSE, tot="last", digit=2) #note how hp2 and hp2b only show one level

#P-values, means, display both levels of binary
#t2 = mktab(mtcars,var.names=c("mpg","qsec","carb","hp2","hp2b"),ind.cat=c(0,0,1,1,1),group.name="gear",cfn=describeMean,miss="always",pval=TRUE,tot="last",digit=2) #now hp2 shows both levels as does hp2b [plus missing for hp2b]

#Can also record simple data frames or matrices that you make as tables
#t3 = matrix(round(rnorm(25),2),5,5)

#Can also record statistical model output
#m1 = lm(mpg~as.factor(am)+as.factor(gear),data=mtcars)
#t4 = round(summary(m1)$coeff,2)

#Or a two-way table
#t5 = mktab2(data=mtcars, rowvar="gear", colvar="vs", disp.var="mpg", ind.bin=FALSE, tot=TRUE, digit=2)

#t6 = mktab2(data=mtcars, rowvar="gear", colvar="vs", disp.var="am", ind.bin=TRUE, digit=2)

#Record in word doc
#word.doc(obj.list=list(t1,t2,t3,t4),obj.title=c("Table 1: Here are simple descriptive statistics","Table 2: Here are simple descriptive statistics with p-values","Table 3: This is a matrix","Table 4: Here is some model output"),dest='/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables_ex.docx',ftype="Arial",col.odd="white")

#word.doc(obj.list=list(t5,t6), obj.title=c("Table 5: Here is a two way table with a continuous variable","Table 6: Here is a two way table with a binary variable"), dest='/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables_ex22.docx', ftype="Arial", col.odd="white")


#NOTE: You will often have to go into the .docx file above and modify a few things; namely make columns left aligned, bold certain variables, etc.

#Record in word doc
#word.doc(obj.list=list(t1), obj.title=c("Table 1: Here are simple descriptive statistics"), dest='/Users/jrigdon/Box Sync/Rigdon/Useful Functions/Tables_ex10.docx', ftype="Arial", col.odd="white")












