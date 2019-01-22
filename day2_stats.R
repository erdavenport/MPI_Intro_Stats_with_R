#Load the iris dataset
data(iris)

#let's take a look at iris
head(iris)
summary(iris)

#Use the aggregate function to calculate the mean per species for each covariate
aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean)

#Use aggregate to calculate the standard deviation for each species per covariate
aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=sd)

#Let's take a look at the distribution of sepal width
plot(iris$Sepal.Width)

#when I plot I like to make filled circles
plot(iris$Sepal.Width, pch=20)

#Let's graph our species sepal widths side by side, one row three columns
par(mfrow=c(1,3))
plot(iris$Sepal.Width[which(iris$Species=="setosa")], pch=20)
plot(iris$Sepal.Width[which(iris$Species=="virginica")], pch=20)
plot(iris$Sepal.Width[which(iris$Species=="versicolor")], pch=20)

#Let's plot this to a file
pdf("~/Desktop/sepal_width.pdf", width = 12, height =6)
par(mfrow=c(1,3))
plot(iris$Sepal.Width[which(iris$Species=="setosa")], pch=20)
plot(iris$Sepal.Width[which(iris$Species=="virginica")], pch=20)
plot(iris$Sepal.Width[which(iris$Species=="versicolor")], pch=20)
dev.off()

#Let's make the same figure but with a labeled y-axis of "Sepal Width"
pdf("~/Desktop/sepal_width.pdf", width = 12, height =6)
par(mfrow=c(1,3))
plot(iris$Sepal.Width[which(iris$Species=="setosa")], pch=20, ylab="Sepal Width")
plot(iris$Sepal.Width[which(iris$Species=="virginica")], pch=20, ylab="Sepal Width")
plot(iris$Sepal.Width[which(iris$Species=="versicolor")], pch=20, ylab="Sepal Width")
dev.off()

#Let's make the y axes the same range
par(mfrow=c(1,3))
plot(iris$Sepal.Width[which(iris$Species=="setosa")], pch=20, ylab="Sepal Width", ylim = c(0,4.5))
plot(iris$Sepal.Width[which(iris$Species=="virginica")], pch=20, ylab="Sepal Width", ylim = c(0,4.5))
plot(iris$Sepal.Width[which(iris$Species=="versicolor")], pch=20, ylab="Sepal Width", ylim = c(0,4.5))

#Let's make a single graph with points colored by species
plot(iris$Sepal.Width, col = iris$Species, pch=20, ylab="Sepal Width", ylim = c(0,4.5))

#Let's compare sepal length and sepal width across species in a boxplot
par(mfrow=c(1,2))
boxplot(iris$Sepal.Length ~ iris$Species, main = "Sepal Length")
boxplot(iris$Sepal.Width ~ iris$Species, main = "Sepal Width")

#Let's plot the y-axis on a log10 scale
par(mfrow=c(1,2))
boxplot(iris$Sepal.Length ~ iris$Species, main = "Sepal Length")
boxplot(log10(iris$Sepal.Length) ~ iris$Species, main = "Sepal Length")

#Variance: Let's take a look at the distribution of sepal width for setosa
par(mfrow=c(1,1))
plot(iris$Sepal.Width[which(iris$Species=="setosa")])

#let's add a line for the mean of sepal width for setosa
abline( h=mean(iris$Sepal.Width[which(iris$Species=="setosa")]), col="green")

# We want to calculate the sum of squares for our data. Let's write a function to do this
SS = function(x){
  observedValues = x
  meanValue = mean(observedValues)
  squareResids = (observedValues - meanValue)^2
  return(sum(squareResids))
}

#let's calculate the Sum of Squares for setosa sepal width
SS(x=iris$Sepal.Width[which(iris$Species=="setosa")])

#We have now calculated the numerator for variance (the sum of squares), let's write a function to calculate variance
variance = function(x){
  sumOfSquares = SS(x)
  DoF = length(x) - 1
  return(sumOfSquares/DoF)
}

#Let's use the function we just wrote to calculate the variance of setosa Sepal Width
variance(iris$Sepal.Width[which(iris$Species=="setosa")])

#Use the R built in function for calculating variance to see if our function is correct (calculate variance for setosa sepal width using R built in function)
var(iris$Sepal.Width[which(iris$Species=="setosa")])
#Yes this gives us the same value!

#Intoduction to the t-test: This is the test to compare the means of two groups.

#Basic assumptions of the student's t-test is constancy of variance (this means that the different groups have the same variance)

#Question: Do different species have different sepal widths?
#Task 1: Are the variances significantly different?
var(iris$Sepal.Width[which(iris$Species=="setosa")])
var(iris$Sepal.Width[which(iris$Species=="virginica")])
var(iris$Sepal.Width[which(iris$Species=="versicolor")])

#How would we test whether these variances are signicantly different? The most commonly used test to compare variances is the F-test
var.test(iris$Sepal.Width[which(iris$Species=="virginica")], iris$Sepal.Width[which(iris$Species=="setosa")])

#These variances do not look significantly different from one another. But let's say they were significantly different, or we just want to be safe and have a reason to fear differences in variances. How would we compare the means of samples with different variances? Luckily, there is a t-test which has a correction for unequal variances. ***Importantly, this is the basic command in R***

#Is the mean sepal width different between virginica and setosa? This test is called "Welch's Approximate t-test"
t.test(iris$Sepal.Width[which(iris$Species=="setosa")], iris$Sepal.Width[which(iris$Species=="virginica")])

#Let's do the same analysis but with a test that assumes equal variance. This test is called the Two Sample t-test
t.test(iris$Sepal.Width[which(iris$Species=="setosa")], iris$Sepal.Width[which(iris$Species=="virginica")], var.equal = T)

#Let's figure out how to analyze data that is not normally distributed. Let's say our sepal width data was not normally distributed, then how can we compare the means of two groups? The most famous statistical test for this is the Wilcoxon rank-sum test.
wilcox.test(iris$Sepal.Width[which(iris$Species=="setosa")], iris$Sepal.Width[which(iris$Species=="virginica")])

#This analysis we just did was comparing the means of two groups. But what about when we have many groups (i.e. here we have three species to compare)? This is an appropriate scenario for Analysis of Variance (ANOVA)
#normally we would specify columns like this:
boxplot(iris$Sepal.Width ~ iris$Species, ylab= "Sepal Width")

#this can be rewritten as
boxplot(Sepal.Width ~ Species, data=iris)

#let's run an anova to compare the mean sepal width between all three groups
aov(iris$Sepal.Width ~ iris$Species)
summary(aov(iris$Sepal.Width ~ iris$Species))

#Let's move on now to looking at relationships between continuous variables. This is when we start talking about correlation coefficients and regression
#Correlation measures the independence (or dependence) of two continuos variables and the direction of their relationship (neg. or positive)
#Does having a longer sepal for setosa mean you have a longer petal?
setosa = iris[which(iris$Species=="setosa"),]
plot(x=setosa$Sepal.Length, y=setosa$Petal.Length, pch=20)

#Let's test the correlation between these two variables
cor.test(setosa$Sepal.Length, setosa$Petal.Length)

#Let's assign the output of the test to a variable
rval = cor.test(setosa$Sepal.Length, setosa$Petal.Length)

#to see how to access the pvalue and R value we can use the attributes function
attributes(rval)

#to access the pvalue we can use the dollar sign and p.value
rval$p.value

#to pull out the Rho value (or R value) and calculate R-squared do the following:
rval$estimate^2

#The Pearson's correlation has power to detect correlations between normally distributed variables. What about variables that are not normally distributed? The way to look at correlation between continuous variables that are not normally distributed is the Spearman rank correlation
cor.test(setosa$Sepal.Length, setosa$Petal.Length, method = "spearman")

#Introduction to Regression: y = bx + error
#The function for the most basic regression in R is lm
#Let's test how sepal length influences petal length in the setosa dataset
fit = lm(Petal.Length ~ Sepal.Length, data=setosa)
summary(fit)

#this regression is telling us that the Beta (b) relationship between y and x is 0.13 (this is the slope). However, the model fails to reject the null hypothesis which is that the slope is equal to 0 (P=0.06).

#Let's add some covariates
# Y= bx1 + cx2 + error
fit = lm(Petal.Length ~ Sepal.Length + Sepal.Width, data=setosa)
summary(fit)

#let's install ggplot2
install.packages('ggplot2')

#load ggplot2
library(ggplot2)

