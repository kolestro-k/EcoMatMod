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

ratio=dna_at/length(DNA)
....
nucl <- c("A", "T", "G", "C") 
DNA = nucl[runif(1000, 1, 5)]
dna=summary(factor(DNA)) 
dna
dna_at=dna[-c(2, 3)] 
ratio=dna_at/length(DNA) #доля в общей цепочки
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
