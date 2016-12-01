# Import R Packages and Modules
library("ggplot2")
library("dplyr")

# Import our datasets
arrests <- read.csv('data/arrests.csv', header = TRUE, stringsAsFactors = TRUE)

# Exploring data
head(arrests) # Displays the first 6 rows of the dataset
tail(arrests) # Display the last 6 rows of the datasets
edit(arrests) # Open data editor to edit the datasets
View(arrests) # View the datasets
summary(arrests) # Provides basic descriptive statistics and frequencies.

# Arrest by Gender
arrests$Sex <- as.character(arrests$Sex)
arrests$Sex[arrests$Sex == "M"] <- "Male"
arrests$Sex[arrests$Sex == "F"] <- "Female"
# Plot Bar Chart
ggplot(arrests, aes(Sex, fill=Sex) ) + geom_bar() + ggtitle("Graph showing arrests based on Gender.") + xlab("Gender") + ylab("Total Cases") + labs(fill = "Gender")
# Plot Pie Chart
ggplot(arrests, aes(x=factor(1), fill=factor(Sex)) ) + geom_bar(width = 1) + ggtitle("Graph showing arrests based on Gender.") + labs(fill = "Gender") + coord_polar(theta = "y") # + theme_void()
arrests$Sex <- as.factor(arrests$Sex)

# Arrest by Race
ggplot(arrests, aes(Race, fill=Race) ) + geom_bar() + ggtitle("Graph showing arrests based on Race.") + xlab("Race") + ylab("Total Cases") + labs(fill = "Race")

# Arrest by Districts
arrests$District <- as.character(arrests$District)
arrests$District[arrests$District == ""] <- NA
ggplot(arrests, aes(District, fill=District) ) + geom_bar() + ggtitle("Graph showing arrests based on District.") + xlab("District") + ylab("Total Cases") + labs(fill = "District")
arrests$District <- as.factor(arrests$District)

# Arrest by Age
arrests$Age_cat <- cut(arrests$Age, breaks = c(-Inf, 17, 24, 34, 44, 54, 64, Inf), labels = c("Below 18", "18 - 24", "25 - 34", "35 - 45", "45 - 54", "55 - 64", "65 Above"), right = FALSE)
ggplot(arrests, aes(Age_cat, fill=Age_cat) ) + geom_bar() + ggtitle("Graph showing arrests based on Age.") + xlab("Age Range") + ylab("Total Cases") + labs(fill = "Age Range:")

# Arrest by Year
# mutate(arrests, year = format(as.Date(ArrestDate, "%d/%m/%Y"), "%Y"))
ggplot(arrests, aes(factor(format(as.Date(ArrestDate, "%d/%m/%Y"), "%Y")), fill= factor(format(as.Date(ArrestDate, "%d/%m/%Y"), "%Y"))) ) + geom_bar() + ggtitle("Graph showing arrests based on year of arrest.") + xlab("Year of arrest") + ylab("Total Cases") + labs(fill = "Year")

# Determine the correlation between Age and the year of arrest
cor.test(arrests$Age, as.numeric( factor(format(as.Date(arrests$ArrestDate, "%d/%m/%Y"), "%Y"))))

# Arrest by hour of the day
arrests$ArrestTime_Rnd <- round(as.double(gsub(":",".", arrests$ArrestTime)))
ggplot(arrests, aes(ArrestTime_Rnd, fill= as.character(ArrestTime_Rnd)) ) + geom_bar() + ggtitle("Graph showing arrests based on hour of the day.") + xlab("Hour of arrest") + ylab("Total Cases") + labs(fill = "Hour")

View(arrests)
