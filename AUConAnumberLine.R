## AUC on a Number Line

library(randomForest)
library(ggplot2)

data("iris")

head(iris)

train_sample <- sample(1:150, 100)
train_set <- iris[train_sample, ]

test_set <- iris[-train_sample, ]

# IS setosa or not?

train_set$target <- ifelse(train_set$Species == "setosa", TRUE, FALSE)

## random choice

test_set$random <- runif(50)

# Logistic Regression

log_reg <- glm(target ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
               data = train_set, family = "binomial")
summary(log_reg)

test_set$log_reg <- predict.glm(log_reg, test_set[, 1:4], type = "response")

# Random Forest

rf <- randomForest(as.factor(target) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                   data = train_set)

test_set$rand_for <- predict(rf, test_set[, 1:4], type = "prob")


# Random Choice Viz

rand_ <- test_set[order(test_set$random, decreasing = TRUE), ]
rand_$answer <- ifelse((rand_$Species == "setosa" & rand_$random >= (2/3)) | 
                           (rand_$Species != "setosa" & rand_$random < (2/3)), 
                       "correct", "incorrect")

rand_$x <- rand_$random
rand_$y <- 0

ggplot(data = rand_, aes(x = x, y = y, color = answer)) +
    geom_point() + 
    scale_y_continuous(limits = c(0, 0.01)) + 
    ylab(label = element_blank()) + 
    xlab(label = "Random Probability") + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), 
          legend.position = "bottom", panel.background = element_blank(), 
          legend.title = element_blank(), legend.key = element_blank())

# Logistic Regression Viz

lr <- test_set[order(test_set$log_reg, decreasing = TRUE), ]
lr$answer <- ifelse((lr$Species == "setosa" & lr$log_reg >= 0.5) | 
                        (lr$Species != "setosa" & lr$log_reg < 0.5),
                    "correct", "incorrect")
lr$x <- lr$log_reg
lr$y <- 0

ggplot(data = lr, aes(x = x, y = y, color = answer)) +
    geom_point() + 
    scale_y_continuous(limits = c(0, 0.01)) + 
    ylab(label = element_blank()) + 
    xlab(label = "Logistic Regression Probability") + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), 
          legend.position = "bottom", panel.background = element_blank(), 
          legend.title = element_blank(), legend.key = element_blank())


# Random Forest Vis

test_set$rand_for <- test_set$rand_for[, 2]

test_set$adjust <- runif(50, 0, 0.10)
test_set$rand_for2 <- ifelse(test_set$rand_for >= 0.5, 
                            test_set$rand_for - test_set$adjust, 
                            test_set$rand_for + test_set$adjust)

rf <- test_set[order(test_set$rand_for2, decreasing = TRUE), ]
rf$answer <- ifelse((rf$Species == "setosa" & rf$rand_for2 >= 0.5) | 
                        (rf$Species != "setosa" & rf$rand_for2 < 0.5),
                    "correct", "incorrect")
rf$x <- rf$rand_for2
rf$y <- 0

ggplot(data = rf, aes(x = x, y = y, color = answer)) +
    geom_point() + 
    scale_y_continuous(limits = c(0, 0.01)) + 
    ylab(label = element_blank()) + 
    xlab(label = "Random Forest Probability") + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), 
          legend.position = "bottom", panel.background = element_blank(), 
          legend.title = element_blank(), legend.key = element_blank())
