#EDA AND VISUALISATION ASSIGNMENT #2
#DONE BY: ZHENG YI TAO

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Importing csv file with ONLY the columns required.
dataset <- read.csv("C:\\Users\\yitao22\\Dropbox\\DSCT\\Assignment 2\\GCI_dataset.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
#dataset <- read.csv("D:\\DSCT\\Assignment 2\\GCI_dataset.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Check for each variable how many have missing records
sapply(dataset, function(x) sum(is.na(x)))

#Check for each variable how many unique values there are
sapply(dataset, function(x) length(unique(x)))

#Taking out rows that are not used
dataset <- dataset[!(dataset$Series.code == 10.03 |
                       dataset$Series.code == 3.02 |
                       dataset$Series.code == 3.04 |
                       dataset$Series.code == 4.06 |
                       dataset$Series.code == 6.09 |
                       dataset$Series.code == 6.13 |
                       dataset$Series.code == 6.14 |
                       dataset$Series.code == 10.04 |
                       dataset$Series.code == 0.02 |
                       dataset$Series.code == 0.04
                     ),]

#Because some values is "<0.1" for this column, change it to 0
dataset[dataset=="<0.1"] <- 0

#Tidy Data Set
dataset <- dataset %>%
  gather(Country, Elements, KHM:VNM, na.rm = TRUE) %>%
  spread(Attribute, Elements) %>%
  arrange(Edition, Series.code, Country)

#Convert value to 2 decimal places
dataset$Value = round(as.numeric(dataset$Value), 2)

#Convert series.code and Edition into categorical
dataset$Series.code <- as.factor(dataset$Series.code)
dataset$Edition <- as.factor(dataset$Edition)
levels(dataset$Edition) = c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014-2015")

#Convert Rank to numeric
dataset$Rank <- as.numeric(as.character(dataset$Rank))

#******************************************Question 1********************************************

#Domestic Market dataset
domestic <- dataset %>% 
  filter(grepl("Domestic", Series)) %>%
  arrange(Edition, Series, Country)

#Foreign Market dataset
foreign <- dataset %>% 
  filter(grepl("Foreign", Series)) %>%
  arrange(Edition, Series, Country)

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

#Create Line plot for foreign market size indices
fLinePlot <- ggplot(foreign, aes(x = Edition, y = Value)) +
  geom_line(aes(group = Country, color = Country), size = 1) +
  geom_vline(xintercept = 3, linetype = "longdash", color = "Blue", size = 1) +
  geom_vline(xintercept = 2, linetype = "longdash", color = "Blue", size = 1) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Foreign Market size indices") +
  scale_colour_manual(values = cbbPalette)

#Create Line plot for domestic market size indices
dLinePlot <- ggplot(domestic, aes(x = Edition, y = Value)) +
  geom_line(aes(group = Country, color = Country), size = 1) +
  geom_vline(xintercept = 3, linetype = "longdash", color = "Blue", size = 1) +
  geom_vline(xintercept = 2, linetype = "longdash", color = "Blue", size = 1) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Domestic Market size indices") +
  scale_colour_manual(values = cbbPalette)

#Figure 1.1 - Combine both fLinePlot and dLinePlot
dfCombinedPlot1 <- grid.arrange(fLinePlot, dLinePlot, ncol=2)
dfCombinedPlot1

#Create Line plot for foreign market size indices
fLinePlot <- ggplot(foreign, aes(x = Edition, y = Value)) +
  geom_line(aes(group = Country, color = Country), size = 1) +
  geom_vline(xintercept = 3, linetype = "longdash", color = "Blue", size = 1) +
  geom_vline(xintercept = 5, linetype = "longdash", color = "Blue", size = 1) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Foreign Market size indices") +
  scale_colour_manual(values = cbbPalette)

#Create Line plot for domestic market size indices
dLinePlot <- ggplot(domestic, aes(x = Edition, y = Value)) +
  geom_line(aes(group = Country, color = Country), size = 1) +
  geom_vline(xintercept = 3, linetype = "longdash", color = "Blue", size = 1) +
  geom_vline(xintercept = 5, linetype = "longdash", color = "Blue", size = 1) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Domestic Market size indices") +
  scale_colour_manual(values = cbbPalette)

#Figure 1.2 - Combine both fLinePlot and dLinePlot
dfCombinedPlot2 <- grid.arrange(fLinePlot, dLinePlot, ncol=2)
dfCombinedPlot2

#******************************************Question 2********************************************

#GDP Per Capita
GDP_per_Capita <- dataset[dataset$Series.code %in% "0.03",]

#GDP (US$ Billions)
GDP_US_Billion <- dataset[dataset$Series.code %in% "0.01",]

#Quality of Education System
Quality_EduSys <- dataset[dataset$Series.code %in% "5.03",]
Quality_EduSys <- Quality_EduSys[!Quality_EduSys$Edition %in% c("2006", "2007", "2014-2015"),]

#Figure 2.1 - Line Plot of GDP Per Capita and Quality of Edu System
ggplot(GDP_per_Capita, aes(Rank, x = Edition)) +
  geom_line(aes(group = Country, color = "GDP Per Capita"), size = 1) +
  geom_line(data = Quality_EduSys, aes(Rank, group = Country, color = "Quality of Edu System", 
                x = Edition), size = 1) + 
  facet_wrap(~ Country) +
  ggtitle("Quality of Education System in comparison with GDP Per Capita in Rank") +
  labs(color = "Series")

#Figure 2.2 - Line Plot of GDP (US$ Billions) and Quaity of Edu System
ggplot(GDP_US_Billion, aes(Rank, x = Edition)) +
  geom_line(aes(group = Country, color = "GDP (US$ Billions)"), size = 1) +
  geom_line(data = Quality_EduSys, aes(Rank, group = Country, color = "Quality of Edu System", 
                x = Edition), size = 1) + 
  facet_wrap(~ Country) +
  ggtitle("Quality of Education System in comparison with GDP (US$ Billions) in Rank") +
  labs(color = "Series")

#Generating Bar Plot for Ranking comparison
#GDP Per Capital and Education System Bar plot
GDP_Pc_EduSys <- dataset[dataset$Series.code %in% c("0.01", "5.03"),]
GDP_Pc_EduSys <- GDP_Pc_EduSys[!GDP_Pc_EduSys$Edition %in% c("2006", "2007", "2014-2015"),]

#Figure 2.3
ggplot(GDP_Pc_EduSys, aes(Edition, y = Rank)) +
  geom_bar(aes(group = Series.code, fill = Series.code), stat = "identity", position = "dodge") +
  facet_wrap(~ Country) +
  scale_y_continuous(breaks=seq(0,130,10)) +
  ggtitle("Quality of Education System in comparison with GDP Per Capita in Rank") +
  scale_fill_discrete(name="Series", labels=c("GDP Per Capita", "Education System"))

#GDP US Billions and Education System Bar plot
GDP_US_EduSys <- dataset[dataset$Series.code %in% c("0.03", "5.03"),]
GDP_US_EduSys <- GDP_US_EduSys[!GDP_US_EduSys$Edition %in% c("2006", "2007", "2014-2015"),]

#Figure 2.4
ggplot(GDP_US_EduSys, aes(Edition, y = Rank)) +
  geom_bar(aes(group = Series.code, fill = Series.code), stat = "identity", position = "dodge") +
  facet_wrap(~ Country) +
  ggtitle("Quality of Education System in comparison with GDP (US  BIllions) in Rank") +
  scale_y_continuous(breaks=seq(0,130,10)) +
  scale_fill_discrete(name="Series", labels=c("GDP US (Billions)", "Education System"))

#******************************************Question 3********************************************

#Extract relevant data
HIV <- dataset[dataset$Series.code %in% "4.05",]
HIV <- HIV[HIV$Edition %in% c("2006", "2014-2015"),]

Primary_enroll <- dataset[dataset$Series.code %in% "4.1",]
Primary_enroll <- Primary_enroll[Primary_enroll$Edition %in% c("2006", "2014-2015"),]

Secondary_enroll <- dataset[dataset$Series.code %in% "5.01",]
Secondary_enroll <- Secondary_enroll[Secondary_enroll$Edition %in% c("2006", "2014-2015"),]

Tertiary_enroll <- dataset[dataset$Series.code %in% "5.02",]
Tertiary_enroll <- Tertiary_enroll[Tertiary_enroll$Edition %in% c("2006", "2014-2015"),]

#Generating bar plot for each education level and HIV prevalence
priBarPlot <- ggplot(Primary_enroll, aes(x = Country, Value)) +
  geom_bar(aes(fill = Edition), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "% of Primary Education", breaks = seq(0,100,2)) +
  scale_x_discrete(name = "Countries") +
  coord_cartesian(ylim = c(80, 100)) +
  ggtitle("Primary Education level (2006 & 2015)")

secBarPlot <- ggplot(Secondary_enroll, aes(x = Country, Value)) +
  geom_bar(aes(fill = Edition), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "% of Secondary Education", breaks = seq(10,100,10)) +
  scale_x_discrete(name = "Countries") +
  ggtitle("Secondary Education level (2006 & 2015)")

terBarPlot <- ggplot(Tertiary_enroll, aes(x = Country, Value)) +
  geom_bar(aes(fill = Edition), stat = "identity", position = "dodge") +
  scale_y_continuous(name = "% of Tertiary Education", breaks = seq(0,80,10)) +
  scale_x_discrete(name = "Countries") +
  ggtitle("Tertiary Education level (2006 & 2015)")

hivBarPlot <- ggplot(HIV, aes(x = Country, Value)) +
  geom_bar(aes(fill = Edition), stat = "identity", position = "dodge") +
  scale_fill_hue(l=30) +
  scale_y_continuous(name = "% of HIV", breaks=seq(0,3,0.2)) +
  scale_x_discrete(name = "Countries") +
  ggtitle("HIV Prevalence by Countries (2006 & 2015)")

#Figure 3.1 - Combine bar plots
pri_Hiv_CombinedPlot <- grid.arrange(hivBarPlot, priBarPlot, secBarPlot, terBarPlot)

#******************************************Question 4********************************************
#1.03 Diversion of public funds, [1 = very commonly occurs; 7 = never occurs]
#1.05 Irregular payments and bribes, 1 [very common] to 7 [never occurs]
#1.16 Reliability of police services, [1 = cannot be relied upon at all; 7 = can be completely relied upon]

#Extract Relevant Data
Qn4_data <- dataset[dataset$Series.code %in% c("1.03", "1.05", "1.16"),]
Qn4_data <- Qn4_data[Qn4_data$Edition %in% c("2010", "2011", "2012", "2013", "2014-2015"),]

#Bunch of code to get the average of Series code 1.03 and 1.05
Reliability <- Qn4_data[Qn4_data$Series.code %in% "1.16",]
Reliability$Series <- NULL
Reliability$Rank <- NULL
Diversion <- Qn4_data[Qn4_data$Series.code %in% "1.03",]
Bribe <- Qn4_data[Qn4_data$Series.code %in% "1.05",]
Diversion$BribeVal <- Bribe$Value
Diversion$Average <- apply(Diversion[,6:7], 1, mean)
Mean_Diversion_Bribe <- data.frame(Diversion$Edition, Diversion$Series.code, Diversion$Country, Diversion$Average)
colnames(Mean_Diversion_Bribe) <- c("Edition", "Series.code", "Country", "Value")
Qn4_Working_data <- rbind(Reliability, Mean_Diversion_Bribe)

#Figure 4.1 - Plot Bar Chart
ggplot(Qn4_Working_data, aes(y = Value, Edition)) +
  geom_bar(stat = "identity", aes(fill = Series.code), position = "dodge", color = "Black") +
  scale_y_continuous(name = "Occurrences Scale", breaks = seq(1,7,1)) +
  scale_x_discrete(name = "Years") + 
  scale_fill_hue(l=30, 
                 labels = c("Average of Diversion of Public Funds & \nIrregular Payments and Bribes",
                            "Reliability of Police Services"),
                 name = "Series") +
  ggtitle("Whether Diversion of Public Funds and Irregular Payments and Bribes affect Reliability of Police Services\n") +
  facet_wrap(~ Country)

#Figure 4.2 - Plot Points with Line Chart
ggplot(Qn4_Working_data, aes(y = Value, Edition)) +
  geom_point(aes(group = Series.code, color = Series.code)) +
  geom_line(aes(group = Series.code, color = Series.code)) +
  scale_y_continuous(name = "Occurrences Scale", breaks = seq(1,7,1)) +
  scale_x_discrete(name = "Years") + 
  scale_color_hue(l=70, 
                 labels = c("Average of Diversion of Public Funds & \nIrregular Payments and Bribes",
                            "Reliability of Police Services"),
                 name = "Series") +
  ggtitle("Whether Diversion of Public Funds and Irregular Payments and Bribes affect Reliability of Police Services\n") +
  facet_wrap(~ Country)


#******************************************Question 5********************************************
#8.06 Soundness of banks, 
#[1 = extremely low-banks may require recapitalization; 
#7 = extremely high-banks are generally healthy with sound balance sheets]

#3.03 Inflation
#For inflation rates between 0.5 and 2.9 percent, a country
#receives the highest possible score of 7. Outside this range,
#scores decrease linearly as they move away from these values.

#Extract Relevant Data
Qn5_data <- dataset[dataset$Series.code %in% c("3.03", "8.06", "0.03"),]
Qn5_data <- Qn5_data[!Qn5_data$Edition %in% c("2006", "2007", "2014-2015"),]

#Dataset to compare
Inflation <- Qn5_data[Qn5_data$Series.code %in% "3.03",]
Soundness <- Qn5_data[Qn5_data$Series.code %in% "8.06",]
Soundness_GDP <- Qn5_data[Qn5_data$Series.code %in% c("0.03", "8.06"),]
Inflation_GDP <- Qn5_data[Qn5_data$Series.code %in% c("0.03", "3.03"),]

#Figure 5.1 - Looking at just inflation
ggplot(Inflation, aes(Value, x = Edition, fill = "Red")) +
  geom_bar(stat = "identity", position = "dodge", colour = "Black") +
  facet_wrap(~Country) +
  geom_hline(yintercept=c(0.5, 2.9), color = "Blue", size = 1, linetype = "longdash") +
  geom_text(aes(0, x=0, y=2.9,label = 2.9, vjust = -0.5, hjust = -0.5), size = 3, color = "Blue") +
  geom_text(aes(0, x=0, y=0.5,label = 0.5, vjust = -0.5, hjust = -0.5), size = 3, color = "Blue") +
  scale_y_continuous(name = "Inflation (in %)", breaks=seq(-2,25,2)) +
  scale_x_discrete(name = "Years") + 
  ggtitle("Inflation in different Countries (2008-2013)") +
  guides(fill = FALSE)

#Figure 5.2 - Comparing Inflation and GDP
ggplot(Inflation_GDP, aes(Rank, x = Edition, fill = Series.code)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country) +
  scale_y_continuous(name = "Rank (Lower is better)", breaks=seq(0,120,20)) +
  scale_x_discrete(name = "Years") + 
  scale_fill_hue(l=30, 
                 labels = c("GDP Per Capita",
                            "Inflation"),
                 name = "Series") +
  ggtitle("Inflation and GDP Per Capita comparison")

#Figure 5.3 - Comparing Soundness and GDP
ggplot(Soundness_GDP, aes(y = Rank, x = Edition, fill = Series.code)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country) +
  scale_y_continuous(name = "Rank (Lower is better)", breaks=seq(0,120,20)) +
  scale_x_discrete(name = "Years") + 
  scale_fill_hue(l=30, 
                 labels = c("GDP Per Capita",
                            "Soundness of Banks"),
                 name = "Series") +
  ggtitle("Soundness of Banks and GDP Per Capita comparison")
