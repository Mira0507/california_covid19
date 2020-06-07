library(ggplot2)
library(tidyverse)

g <- glimpse  
s <- summary   
h <- head   

#################################### Data Cleaning #################################

cov <- read_csv("covid-19-cases.csv")


names(cov) <- c("County",
                "Date",
                "Confirmed",
                "Deaths", 
                "Positive_Patients",
                "Suspected_Patients",
                "ICU_Positive_Patients",
                "ICU_Suspected_Patients")

cov1 <- cov[complete.cases(cov), ]

cov2 <- gather(cov1, Category, Count, -c(County, Date)) %>%
        filter(Category == "Confirmed")

cov2_inspect <- cov2 %>% 
        group_by(County) %>%
        summarize(Count_by_County = sum(Count)) %>%
        arrange(desc(Count_by_County))

cov3 <- cov1 %>% 
        filter(County != "Los Angeles") 

cov3_inspect <- gather(cov3, Category, Count, -c("County",
                                                "Date",
                                                "Deaths")) %>%
        filter(!str_detect(Category, "Suspected"))

#################################### Modeling #################################### 




#################################### Plotting #######################################

# Determine outlier county 
plot_outlier_county <- 
        ggplot(cov2_inspect, aes(y = Count_by_County)) +
        geom_boxplot(fill = "#CC79A7", alpha = 0.5) +
        theme_bw() + 
        ggtitle("Distribution of Daily Confirmed Case Number over Counties in California") + 
        ylab("Daily Count per County") + 
        theme(axis.text.x = element_blank()) # Outlier "Los Angeles" county is found 



# Plotting of Number of Deaths over Confirmed vs ICU positive patients vs Positive patients 
plot_predictors <- 
        ggplot(cov3_inspect, aes(x = Count,
                                 y = Deaths,
                                 color = Category)) + 
        geom_jitter(alpha = 0.3) + 
        geom_smooth(method = "lm",
                    se = FALSE) + 
        theme_bw() + 
        xlab("Number of Cases") +
        ylab("Number of Deaths") + 
        ggtitle("Relationship between Daily Number of Deaths and COVID-19 Cases")
                 