1
iris
head(iris)
mean (iris$Sepal.Length)
mean (iris$Sepal.Width)
mean (iris$Petal.Length)
mean (iris$Petal.Width)
list(mean (iris$Sepal.Length), mean (iris$Sepal.Width), mean (iris$Petal.Length), mean (iris$Petal.Width))


2

ir = iris[c(1:4)] 
y = vector()
for(i in 1:length(iris$Sepal.Length)){
  y[i] = mean(t(ir[i,])) 
}
y

3

dna = factor(rep(c("T","G","C","A"))) 
sample(dna,size = 1000, replace = TRUE)
DNA = sample(dna,size = 1000, replace = TRUE)
ratio = summary(factor(DNA))
ratio
dna_at = dna[-c(2, 3)] 
ratio = summary(factor(dna_at))/length(DNA)
ratio

4
letters
sample(letters,size = 1000, replace = TRUE)
sampleabc = c(sample(letters, size = 1000, replace = TRUE))
summary(sampleabc)
glas = sampleabc[sampleabc %in% c("a","e","i","o","u")] 
length(glas)

5
x = factor(iris[order(iris$Petal.Length),]$Species)
x

6
median = function(x) {
  z=sort(x)
  if((length(z)%%2)!=0){
    result = z[(length(x)/2)+1]
  }
  else
    result = (z[length(x)/2]+z[length(x)/2+1])/2
  return(result)
}

7
library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_jitter(alpha = 0.6) + facet_grid(. ~ Species)

8

diamonds
levels(factor(diamonds$clarity))
x = levels(factor(diamonds$clarity))
y = vector()
for (i in 1:length(x)) {
  y[i]=mean(diamonds$price[(diamonds$price>1000) & diamonds$clarity==x[i]])
}
y

9

spirman = function(x, y) {
  if(length(x)==length(y) && is.vector(x)==TRUE && is.vector(y)==TRUE){
    p=1
    n=length(x)
    rx=rank(x)
    ry=rank(y)
    for(i in 1:n)
    {
      p = p - ((( rx[i] - ry[i] )^2)*6)/(n*(n^2-1))
    }
  }
  else{
    if(is.vector(x)==TRUE && is.vector(y)==TRUE)
      print()
    else
      print()
    p=-1
  }
  return(p)
}
spirman(iris$Sepal.Length,iris$Petal.Length)

10
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
