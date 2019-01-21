1 + 1

# Defining weights of subjects
weight_kg <- 55
weight_lb <- 2.2 * weight_kg
weight_kg <- 100

# Read survey data in R
surveys <- read.table(file = "surveys_complete.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Examine properties of data
dim(surveys)
str(surveys)
head(surveys)

# Adding another weight to our variable
weight_kg <- c(weight_kg, 52.3)
weight_kg

# Reference first element of weight_kg
weight_kg[1]

# Reference the second element of weight_kg
weight_kg[2]

# What kind of data is weight_kg?
class(weight_kg)

# What kind of data is surveys?
class(surveys)

# What kinds of data exists in surveys?
str(surveys)

head(surveys)

# Subsetting 2nd row, 3rd column of surveys
surveys[2, 3]

# Subset the 1st row, 5th column
surveys[1, 5]

# Pull out first row of surveys
surveys[1, ]

# Pull out the first column of surveys
surveys[ , 1]
head(surveys[ , 1])

head(surveys)

# Use name of column to reference
surveys$hindfoot_length

surveys$hindfoot_length

# The first 5 values of hindfoot_length
surveys$hindfoot_length[1:5]

# Look at the 1st, 4th, 5th, and 7th value of hindfoot_length
surveys$hindfoot_length[c(1, 4, 5, 7)]

# What is the mean hindfoot length?
mean(surveys$hindfoot_length)

# What is median hindfoot length?
median(surveys$hindfoot_length)

# What is the standard deviation of hindfoot length?
sd(surveys$hindfoot_length)

# What is the minimum and maximum value of hindfoot_length?
min(surveys$hindfoot_length)
max(surveys$hindfoot_length)

# How many of each sex are in this dataset?
table(surveys$sex)

# Which rows are female?
female_rows <- which(surveys$sex == "F")

length(female_rows)

# Mean hindfoot length of the ladies
mean(surveys$hindfoot_length[female_rows])

# What is the mean hindfoot length of the dudes?
male_rows <- which(surveys$sex == "M")
mean(surveys$hindfoot_length[male_rows])

# What is the summary of hindfoot_length?
summary(surveys$hindfoot_length)

# Make a new dataframe of survey data
surveys_new <- surveys

# Change hindfoot length from cm to mm
surveys_new$hindfoot_length <- surveys_new$hindfoot_length * 10
head(surveys)
head(surveys_new)

# Save surveys_new to computer
write.table(surveys_new, "surveys_new.csv", sep = ",", row.names = FALSE, quote = FALSE)
?read.table

# Write function to correct hindfoot length values by 10%

correcting_function <- function(hl) {
  hl*1.1
}

# Use function to correct example hindfoot length
correcting_function(50, 1.3)

# How do we apply this function to multiple entries? 
surveys$hindfoot_length_corrected <- sapply(surveys$hindfoot_length, correcting_function)
head(surveys)

# Make a scatter plot of weight and hindfoot length
pdf("hindfoot_length_vs_weight_plot.pdf")
plot(surveys$hindfoot_length, surveys$weight, 
     main = "Relationship between \n hindfoot length and weight",
     xlab = "hindfoot length (cm)",
     ylab = "weight (g)",
     col = "blue")
dev.off()
# Make the points blue!

# Boxplot of hindfoot lengths in males and females
boxplot(surveys$hindfoot_length ~ surveys$sex, 
        main = "Distribution of hindfoot length between sexes", 
        xlab = "sex", 
        ylab = "hindfoot length (cm)", 
        col = c("tomato", "cadetblue1")
)

# add x axis label, y axis label, and a title to your plot

library("swirl")
swirl()
