#EXTRACT THE SALARY COLUMN FROM YOUR DATASET AND CALCULATE THE AVERAGE SALARY.
salary <- c(50000, 60000, 70000, 80000, 50000, 65000, 45000, 60000)
average_salary <- mean(salary)
average_salary

#Use a vector to store ages and find min and max ages.
age <- c(34, 29, 40, 30, 35, 27, 41, 30)
min_age <- min(age)
max_age <- max(age)
min_age
max_age

#Create a list to store one employee’s details and display 
employee <- list(Name="X", Department="HR", Age=34, Salary=50000)
employee

# Mean and standard deviation of Salary
mean_salary <- mean(salary)
sd_salary <- sd(salary)

# Correlation between Age and Salary
correlation <- cor(age, salary)
mean_salary
sd_salary
correlation





data("mtcars")
#visualizations using mtcars dataset
library(ggplot2)
#Scatter plot (Horsepower vs MPG)
data(mtcars)
plot(mtcars$hp, mtcars$mpg,
     main="Scatter Plot: Horsepower vs Miles Per Gallon",
     xlab="Horsepower (hp)", ylab="Miles per Gallon (mpg)",
     pch=19, col="blue")
abline(lm(mpg ~ hp, data=mtcars), col="red", lwd=2)

#Box plot (MPG vs Cylinders)
boxplot(mpg ~ cyl, data=mtcars,
        main="Box Plot: MPG by Number of Cylinders",
        xlab="Number of Cylinders", ylab="Miles per Gallon (mpg)",
        col=c("lightblue", "lightgreen", "lightpink"))

#Histogram (Distribution of Car Weights)
hist(mtcars$wt, 
     breaks=8, 
     main="Histogram: Distribution of Car Weights", 
     xlab="Weight (1000 lbs)", 
     col="lightcoral")

#violin plot (MPG by Gears)
ggplot(mtcars, aes(x=factor(gear), y=mpg, fill=factor(gear))) +
  geom_violin(trim=FALSE) +
  labs(title="Violin Plot: MPG by Number of Gears",
       x="Number of Gears", y="Miles per Gallon (mpg)") +
  theme_minimal()

#factor density plot (Distribution of MPG)
ggplot(mtcars, aes(x=mpg)) +
  geom_density(fill="lightblue", alpha=0.7) +
  labs(title="Density Plot: Distribution of Miles per Gallon (mpg)",
       x="Miles per Gallon (mpg)", y="Density") +
  theme_minimal()


