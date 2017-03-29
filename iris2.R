install.packages("ggplot2")     #You can delete this one after installing packages. (You need an Internet connection)
install.packages("stats")
library("stats")
library("ggplot2")
TT=function(line)
{
  line=as.character(as.matrix(as.data.frame(strsplit(line, " ")))[,1])
  line=line[!(nchar(line)==0)]
  line=line[(!line=="##")]
  return(line)
}
help_Spirmen=function(table,ind)
{
  table=table[order(table[,ind]),]
  table[,ind+2]=1:nrow(table)
  line=table[,ind]
  for (i in as.numeric(names(table(line)[table(line)>1]))){
    sum=0
    for (j in which(line %in% i))
    {
      sum=sum+table[j,ind+2]
    }
    sum=sum/length(which(line %in% i))
    table[which(line %in% i),ind+2]=sum
  }
  return(table)
}
Spirmen=function(x1,x2)
{
  if(length(x1)!=length(x2))
  {
    return("Problem with the lengths of the vectors")
  }
  table=matrix(ncol=5,nrow=length(x1))
  table[,1]=x1
  table[,2]=x2
  table=help_Spirmen(table,1)
  table=help_Spirmen(table,2)
  table[,5]=(table[,3]-table[,4])**2
  sum=sum(table[,5])
  result=1-6*sum/(length(x1)*(length(x1)**2-1))
  return(result)  
}
med=function(elements)
{
  elements=elements[order(elements)]
  if(length(elements)%%2==0)
  {
    n=length(elements)%/%2
    element=(elements[n]+elements[n+1])/2
  }
  else
  {
    element=elements[length(elements)%/%2+1]
  }
  return(element)
}


# setwd("E://iris/")                                          #??? ???????? ???????, ??? ??? ?????? ???????????,
# table=file("iris.txt", open='r')                            #??? ????? ????. ? ????? ?????-?? ????????????
# table=readLines(table)                                      #??????? ? ??????????? ??, ??????? ??? ????? ??
# new.table=matrix(ncol=6,nrow=length(table))                 #?????? ???????????
# new.table[1,2:6]=TT(table[1]) 
# for (ind in 2:length(table)){
# new.table[ind,]=TT(table[ind])}
# iris=matrix(ncol=ncol(new.table)-1,nrow=nrow(new.table)-1)
# iris=new.table[2:51,2:6]
# colnames(iris)=new.table[1,2:6]
# rownames(iris)=new.table[2:51,1]
#############################################################
list.mean.col=list()
list.mean.row=list()
DNA=c()
dna_at=c()
vec.letters=c()
five.task=list()
last.task=c()
x.col=which(colnames(iris) %in% "Species")
for (i in setdiff(1:ncol(iris),x.col)){list.mean.col[i]=mean(as.numeric(iris[,i]))}
for (i in 1:nrow(iris)){list.mean.row[i]=mean(as.numeric(iris[i,-x.col]))}
for (i in 1:1000){DNA[i]=sample(c("A","C","G","T"),1)}
dna_at[1]=length(DNA[DNA=="A"])
dna_at[2]=length(DNA[DNA=="T"])
dna_at[3]=(dna_at[1]+dna_at[2])/length(DNA)
names(dna_at)=c("Count A","Count T","Proportion")
for (i in 1:10000){vec.letters[i]=sample(LETTERS[1:26],1)}
count.num=length(which(vec.letters %in% LETTERS[as.numeric(c("1","5","9","15","21","25"))]))
#The fifth task is too difficult for understanding. So I did it as I understood
for (i in names(table(iris[,x.col])))
{
  five.task.help=iris[which(iris[,x.col] %in% i),which(colnames(iris) %in% "Petal.Width")]
  five.task[[length(five.task)+1]]=c(summary(five.task.help))   
}
names(five.task)=names(table(iris[,x.col]))
Dia=diamonds[which(diamonds$price>1000),]
for (i in names(table(Dia$clarity))){last.task[length(last.task)+1]=mean(Dia[which(Dia$clarity %in% i),]$price)}
names(last.task)=names(table(Dia$clarity))
####################Regression and correlation############
MegaTable=read.csv("eddypro-1.csv",header=FALSE)
names.MT=c()
for (i in 1:ncol(MegaTable))
{
  names.MT[i]=as.character(MegaTable[2,i])
}
names.MT=gsub("-","",names.MT)
names.MT=gsub("  ","",names.MT)
colnames(MegaTable)=names.MT
MegaTable=MegaTable[-c(1:3),]
 
doy=as.data.frame(MegaTable[,2])[,1]
new.line=c()
for(i in doy){new.line[length(new.line)+1]=as.character(i)}
doy=as.matrix(as.data.frame(strsplit(new.line,"-")))[2,]

SmallTable=MegaTable[as.numeric(doy)>5,]
SmallTable=SmallTable[as.numeric(doy)<9,]

SmallTable[is.na(SmallTable)]=-9999

########Data####
Trans_help=function(zzz)
{
  table=matrix(ncol=ncol(zzz),nrow=nrow(zzz))
  for(i in 1:ncol(zzz))
  {
    table[,i]=TransformFactNumer(zzz[,i])
  }
  return(table)
}
TransformFactNumer=function(xxx)
{
  yyy=c()
  for(i in xxx){yyy[length(yyy)+1]=as.numeric(as.character(i))}
  return(yyy)
}

Table.for.anova=Trans_help(SmallTable)
colnames(Table.for.anova)=colnames(SmallTable)
Table.for.anova=Table.for.anova[,-which(is.na(Table.for.anova[1,]))]
Table.for.anova=Table.for.anova[,-1]
Table.for.anova=Table.for.anova[,-c(1,2)]


##########Analise
 x1=Table.for.anova[,10]
 x2=Table.for.anova[,2]
 x3=Table.for.anova[,3]
 x4=Table.for.anova[,4]
 funct1$coefficients
 funct1=lm(x1~x2+x3)
 funct2=lm(x1~x3*x4)
 funct3=lm(x1~x3+x2+x4)
 funct4=lm(x1~Table.for.anova[,c(2:5)])
 anova(funct4)
 anova(funct1,funct2,funct3,funct4)
 cor(x1,x3)
 summary(funct)

##########Monte Carlo method
VectInLine=function(vector.funct){
  line=vector.funct[1]
 for (i in vector.funct)
 {
   line=paste0(line,"$",i)
 }
  return(line)
}
iteration=100
Table.result.anova=matrix(ncol=3,nrow=iteration, dimnames = list(c(1:iteration),c("SPSS", "Numbers of Colons","Coefficients")))
ind=1
while (ind != iteration){
  Count.col.anova=sample(1:(ncol(Table.for.anova)-1),1)
  Numbers.of.col.anova=sample(c(1:9,9:ncol(Table.for.anova)),Count.col.anova,replace=FALSE)
  Numbers.of.col.anova=Numbers.of.col.anova[order(Numbers.of.col.anova)]
  funct=lm(Table.for.anova[,10]~Table.for.anova[,Numbers.of.col.anova])
  vector.funct=funct$coefficients
  vector.funct=paste0(names(vector.funct),"|",as.character(vector.funct))
  vector.funct= gsub("Numbers.of.col.anova]","",vector.funct)
  vector.funct=gsub("Table.for.anova","",vector.funct)
  vector.funct[2:length(vector.funct)]=substr(vector.funct[2:length(vector.funct)],4,length(vector.funct))
  
  Table.result.anova[ind,3]=VectInLine(vector.funct)
  Table.result.anova[ind,2]=VectInLine(as.character(Numbers.of.col.anova))
  Table.result.anova[ind,1]=anova(funct)[1,3]
  
  ind=ind+1
}

Table.result.anova=Table.result.anova[order(Table.result.anova[,1]),]
model.col.num=Table.result.anova[1,2]
model.col.num=as.matrix(as.data.frame(strsplit(as.character(model.col.num),"[$]")))[,1]
model.col.num=as.numeric(model.col.num)
funct=lm(Table.for.anova[,10]~Table.for.anova[,model.col.num])
anova.last=anova(funct)

######result
list.mean.col #FirstTask
list.mean.row #SecondTask
DNA           #A sample of the vector, which consist of the ATGC
dna_at        #Analysis of DNA
vec.letters   #Vector of random letters (task 4)
count.num     #Proportion of vocal letters (task 4)
five.task     #Five Task
last.task     #8 Task
med()         #Mediana; Argu - vector of numeric
Spirmen()     #Spirmen's method; Argu - two vectors of numeric with the same lengths
#########
summary(funct)#Coefficients of model
anova.last    #ANOVA results