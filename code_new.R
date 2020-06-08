library(ggplot2)
library(tidyverse)
library(lubridate)

g <- glimpse  
s <- summary   
h <- head   

#################################### Data Cleaning #################################

cov <- read_csv("covid19data.csv")


names(cov) <- c("County",
                "Date",
                "Confirmed",
                "Deaths", 
                "Positive_Patients",
                "Suspected_Patients",
                "ICU_Positive_Patients",
                "ICU_Suspected_Patients")

cov1 <- cov[complete.cases(cov), ] %>% 
        mutate(Date = mdy(Date))

cov2 <- cov1 %>%
        filter(County != "Los Angeles") %>%
        select(-c(Suspected_Patients, ICU_Suspected_Patients)) %>%
        gather(Category, Count, c(Confirmed, ICU_Positive_Patients))

cov3 <- spread(cov2, Category, Count)


#################################### Modeling ##################################

fm1 <- Deaths ~ Confirmed
fm2 <- Deaths ~ Confirmed + Confirmed:ICU_Positive_Patients

library(vtreat)

set.seed(1265)

splitPlan1 <- kWayCrossValidation(nrow(cov3), 4, NULL, NULL)

cov3$pred1 <- 0
cov3$pred2 <- 0

for (i in 1:4) {
        split <- splitPlan1[[i]]
        model <- lm(fm1, data = cov3[split$train, ])
        cov3$pred1[split$app] <- predict(model, newdata = cov3[split$app, ])
}

set.seed(563)

splitPlan2 <- kWayCrossValidation(nrow(cov3), 4, NULL, NULL)

for (j in 1:4) {
        split <- splitPlan2[[j]]
        model <- lm(fm1, data = cov3[split$train, ])
        cov3$pred2[split$app] <- predict(model, newdata = cov3[split$app, ])
}

cov3 <- cov3 %>%
        mutate(resid1 = Deaths - pred1,
               resid2 = Deaths - pred2,
               relerr1 = resid1 / Deaths,
               relerr2 = resid2 / Deaths)

cov3_ver1 <- gather(cov3, Prediction, Predicted_Value, c(pred1, pred2)) %>%
        mutate(Prediction = ifelse(Prediction == "pred1", 
                                   "Model 1",
                                   "Model 2"))

cov4 <- cov3 %>%
       summarize(SD = sd(Deaths),
                 RMSE1 = sqrt(mean(resid1^2)),
                 RMSE2 = sqrt(mean(resid2^2)),
                 R_Squared1 = cor(pred1, Deaths)^2,
                 R_Squared2 = cor(pred2, Deaths)^2, 
                 RMS_relerr1 = sqrt(mean(relerr1^2)),
                 RMS_relerr2 = sqrt(mean(relerr2^2)))

#################################### Plotting ##################################





# Inspection Plots

plot_inpect1 <- ggplot(subset(cov1, 
                             Date == "2020-06-06"),
                      aes(y = Confirmed)) + 
        geom_boxplot(alpha = 0.5, 
                     fill = "#660066") +
        theme_bw() + 
        ylab("Number of Confirmed Cases") + 
        theme(axis.text.x = element_blank()) + 
        ggtitle("Inspection for Determining Outliers")
                          
plot_inpect2 <- ggplot(cov2,
                       aes(x = Count, 
                           y = Deaths,
                           color = Category)) + 
        geom_jitter(alpha = 0.2) + 
        geom_smooth(method = "lm",
                    se = FALSE) + 
        theme_bw() + 
        xlab("Counted Cases") + 
        ylab("Counted Deaths") + 
        ggtitle("Number of Deaths over Number of Confirmed or ICU COVID19-Positive Patients")

plot_inpect3 <- ggplot(cov2,
                       aes(x = Count, 
                           y = Deaths,
                           color = Category)) + 
        geom_jitter(alpha = 0.2) + 
        facet_grid(.~ Category,
                   scales = "free_x") + 
        geom_smooth(method = "lm",
                    se = FALSE) + 
        theme_bw() + 
        xlab("Counted Cases") + 
        ylab("Counted Deaths") + 
        ggtitle("Number of Deaths over Number of Confirmed or ICU COVID19-Positive Patients")


# Prediction vs Outcome

plot_pred_vs_outcome <- 
        ggplot(cov3_ver1, 
               aes(x = Predicted_Value,
                   y = Deaths,
                   color = Prediction)) + 
        geom_jitter(alpha = 0.2) + 
        geom_smooth(method = "lm", se = FALSE) + 
        facet_grid(Prediction ~.) +
        xlab("Predicted Deaths") + 
        theme_bw() + 
        theme(legend.position = "none") + 
        ggtitle("Relationship between Prediction and Outcome")




# Residual Plots

plot_residual1 <- 
        ggplot(cov3,
               aes(x = pred1, 
                   y = resid1)) + 
        geom_point(alpha = 0.2,
                   color = "#336633") + 
        theme_bw() + 
        geom_smooth(method = "lm", se = FALSE) + 
        xlab("Prediction 1") + 
        ylab("Residual 1") + 
        ggtitle("Residual Created by Model 1")

plot_residual2 <- 
        ggplot(cov3,
               aes(x = pred2, 
                   y = resid2)) +
        geom_point(alpha = 0.2,
                   color = "#666600") +                                     
        theme_bw() + 
        geom_smooth(method = "lm", se = FALSE) + 
        xlab("Prediction 2") + 
        ylab("Residual 2") + 
        ggtitle("Residual Created by Model 2")




# Gain Curve 
library(WVPlots)

gaincurve_model1 <- GainCurvePlot(cov3, 
                                  "pred1", 
                                  "Deaths", 
                                  "Gain Curve with Model 1") + 
        theme_bw() +
        xlab("Fraction Items in Sort Order") + 
        ylab("Fractional Total Sum Deaths")


gaincurve_model2 <- GainCurvePlot(cov3, 
                                  "pred2", 
                                  "Deaths", 
                                  "Gain Curve with Model 2") + 
        theme_bw() +
        xlab("Fraction Items in Sort Order") + 
        ylab("Fractional Total Sum Deaths")