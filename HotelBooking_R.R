### Hotel Booking Analysis 
## By Sezgi Cobanbas, Zeynep Sila Kaya, Emel Dar Omar Safarini 
## Statistical Learning ModB Project 


library(magrittr) 
library(dplyr)    
library(xtable)
library(readr)
library(plotrix)
library(corrplot)
library(rsample)
library(pROC)
library(olsrr)
library(MASS)
library(glmnet)
library(stepAIC)


booking <- read.table(file="hotel_booking.csv", header=TRUE, sep=',')


booking <- booking[, -which(names(booking) %in% c("adr","arrival_date_day_of_month","arrival_date_week_number",
                                                  "credit_card","email","name","phone-number",
                                                  "reservation_status_date"))]


names(booking)

is_cancelled_counts <- table(booking$is_canceled)
print(is_cancelled_counts)

reservation_status_counts <- table(booking$reservation_status)
print(reservation_status_counts)

booking <- booking[, -which(names(booking) %in% c("reservation_status"))]
names(booking)



booking_na <- booking
booking_na[booking_na == ""] <- NA
anyNA(booking_na)
colSums(is.na(booking_na))
booking_na<- booking_na[, -which(names(booking) %in% c("agent","company"))]


booking_clean <- na.omit(booking_na)
anyNA(booking_clean)

booking_clean$staysin <- rowSums(booking_clean[, c("stays_in_weekend_nights", "stays_in_week_nights")])
booking_clean <- booking_clean[!(booking_clean$staysin == 0), ]

booking_clean <- booking_clean[, -which(names(booking_clean) %in% c("is_repeated_guest", "previous_cancellations", "previous_bookings_not_canceled", "total_of_special_requests"))]

booking_clean <- booking_clean[!(booking_clean$adults == 0),]
booking_clean <- booking_clean[!(booking_clean$adults == 0 & booking_clean$children == 0 & booking_clean$babies == 0), ]



names(booking_clean)
attach(booking_clean)

summary(booking_clean)
View(booking_clean)

###

###

#1
bar_plot <- table(booking_clean$meal)
percentages <- round((bar_plot / sum(bar_plot)) * 100, 1)
barplot_heights <- barplot(bar_plot, 
                           main = "Meal Distribution", 
                           xlab = "Meal Type", 
                           ylab = "Frequency", 
                           col = "lightsteelblue",
                           font.main = 2)
grid()

#2
freq.tb.iscanceled <- table(is_canceled)
freq.tb.iscanceled
prop.tb.iscanceled <- prop.table(freq.tb.iscanceled)
prop.tb.iscanceled <- round(prop.tb.iscanceled * 100, 2)
labelpie <- c("Not Canceled", "Canceled")
labels <- paste(labelpie, "\n", format(prop.tb.iscanceled, nsmall = 2), "%")
a<-prop.table(freq.tb.iscanceled)
pie(a, labels = labels, main = "Pie Chart of Cancel Ratio", col = c("lightsteelblue", "mistyrose"),cex = 1,
    font.main = 2)


#3
booking_clean$total_guests <- rowSums(booking_clean[, c("adults", "children", "babies")])
freq.total_guests <- table(booking_clean$total_guests)
barplot_heights <-barplot(freq.total_guests, 
                          main = "Distribution of  Guests",
                          xlab = "Total Guests",
                          ylab = "Frequency",
                          col = "lightsteelblue",
                          ylim = c(0, max(freq.total_guests))) 

# Optional: Add grid lines for better readability
grid()
booking_clean <- booking_clean[, -which(names(booking_clean) %in% c("total_guests"))]

#4
hotel_names <- c("City Hotel" , "Resort Hotel")
booking_counts <- table(booking_clean$hotel, booking_clean$is_canceled)
barplot(booking_counts, beside = TRUE, 
        col = c("lightsteelblue", "mistyrose"), 
        main = "Reservation Counts by Hotel and Cancellation Status",
        xlab = "Hotel", ylab = "Reservation Count", 
        legend = c("Not Canceled", "Canceled"), 
        names.arg = hotel_names)

#5
reservation_summary <- table(booking_clean$is_canceled, booking_clean$arrival_date_month)
barplot(reservation_summary, beside = TRUE, col = c("lightsteelblue", "mistyrose"),
        legend = c("Not Canceled", "Canceled"),
        main = "Reservation Status Per Month",
        xlab = "Month",
        ylab = "Frequency",
        ylim = c(0, max(reservation_summary) * 1.1))


#6
freq.total_staysin <- table(booking_clean$staysin)
barplot(freq.total_staysin, 
        main = "Bar Chart of Total Nights Spent",
        xlab = "Total Nights Spent",
        ylab = "Frequency",
        col = "lightsteelblue",
        ylim = c(0, max(freq.total_staysin)),
        xlim = c(0,12)) 

booking_clean <- booking_clean[, -which(names(booking_clean) %in% c("staysin"))]


#7
families <- booking_clean$children > 0 | booking_clean$babies > 0
true_families_count <- sum(families)
true_families_count

families_with_children_or_babies <- subset(booking_clean, children > 0 | babies > 0)
head(families_with_children_or_babies)
freq.canceled <- table(families_with_children_or_babies$is_canceled)
prop.canceled <- prop.table(freq.canceled)
labelpie <- c("Not Canceled", "Canceled")
percentages <- round(prop.canceled * 100, 2)
labels <- paste(labelpie, "\n", percentages, "%", sep="")
colors <- c("lightsteelblue", "mistyrose")
pie(prop.canceled, labels = labels, main = "Pie Chart of Cancellation Ratio of Adults with Children", col = colors)

#8
boxplot(lead_time ~ is_canceled, data = booking_clean,
        main = "Box Plot of Lead Time and Cancellation Rate",
        xlab = "Is Cancelled",
        ylab = "Lead Time (days)",
        names = c("Not Cancelled", "Cancelled"),
        col = c("lightsteelblue", "mistyrose"))



#9
numeric <- booking_clean[, sapply(booking_clean, is.numeric)]
numeric<-na.omit(numeric)

korelasyon_matris <- cor(numeric)

corrplot(korelasyon_matris, method = "number", type = "upper", 
         tl.col = "black", tl.srt = 45,
         tl.pos = "lt"
)


#10

country_counts <- table(booking_clean$country)
top_10_countries <- head(sort(country_counts, decreasing = TRUE), 10)
barplot(top_10_countries, main = "Booking Distribution by Top 10 Countries", xlab = "Country",
        ylab = " Count", col = "lightsteelblue", ylim = c(0, max(top_10_countries) * 1.1))


#11
channel_counts <- table(booking_clean$distribution_channel)
bar_colors <- c("lightsteelblue", "mistyrose", "thistle", "antiquewhite",?"rosybrown")

barplot(channel_counts, main = "Reservation Distribution by Distribution Channel",
        ylab = "Count", xlab = "Distribution Channel", col = bar_colors,
        ylim = c(0, max(channel_counts) * 1.2), 
        beside = TRUE)

options(scipen = 999)


#13
same_count <- sum(booking_clean$assigned_room_type == booking_clean$reserved_room_type)
different_count <- sum(booking_clean$assigned_room_type != booking_clean$reserved_room_type)
total <- nrow(booking_clean)

same_ratio <- same_count / total * 100
different_ratio <- different_count / total * 100

labels <- c("Same", "Different")
ratios <- c(same_ratio, different_ratio)
colors <- c("lightsteelblue", "mistyrose")
pie(ratios, labels = labels, col = colors, main = "Assigned Room Type vs Reserved Room Type")
legend_labels <- paste(labels, round(ratios, 1), "%")
legend("topright", legend = legend_labels, fill = colors)

##LOG?ST?C MODEL####

logistic_model <- glm(is_canceled~ . , data = booking_clean, family = binomial)
summary(logistic_model)
glm.prob_lm <- predict(logistic_model, type="response")
roc.out_lm <- roc(booking_clean$is_canceled, glm.prob_lm)
plot(roc.out_lm, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate",main="Roc Curve for Logistic Model")



forward_model <- stepAIC(logistic_model, direction = "forward")
summary(forward_model)
glm.prob_f <- predict(forward_model, type="response")
roc.out_f <- roc(booking_clean$is_canceled, glm.prob_f)
plot(roc.out_f, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate",main="Roc Curve for Forward Model")



model <- glm(formula = is_canceled ~ hotel + lead_time + arrival_date_year + 
               arrival_date_month + stays_in_weekend_nights + stays_in_week_nights + 
               adults + children + babies + meal + market_segment + 
               distribution_channel + reserved_room_type + 
               booking_changes + deposit_type + days_in_waiting_list + customer_type + 
               required_car_parking_spaces, 
             family = binomial, data = booking_clean)
summary(model)



glm.prob <- predict(model, type="response")
roc.out <- roc(booking_clean$is_canceled, glm.prob)
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main= "Roc Curve for New Logistic Model")




set.seed(123)
n <- nrow(booking_clean)
train_indices <- sample(1:n, size = 0.8*n, replace = FALSE)
train_data <- booking_clean[train_indices, ]
test_data <- booking_clean[-train_indices, ]

train_data$country <- factor(train_data$country)
test_data$country <- factor(test_data$country, levels = levels(train_data$country))

train_data$country <- droplevels(train_data$country)
test_data$country <- droplevels(test_data$country)



model2 <- glm(train_data$is_canceled ~ ., data = train_data, family = binomial)
summary(model2)

glm.prob_train <- predict(model2, newdata = train_data, type = "response")
roc.out2 <- roc(train_data$is_canceled, glm.prob_train)
plot(roc.out2, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main= "Roc curve for Training Data")



glm.prob_test <- predict(model2, newdata = test_data, type = "response")
roc.out3 <- roc(test_data$is_canceled, glm.prob_test)
plot(roc.out3, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main= "Roc curve for Test Data")



# Confusion matrix
conf_matrix_test <- table(test_data$is_canceled, glm.prob_test > 0.5)

# True Positives (TP)
TP_Test <- conf_matrix_test[2, 2]

# True Negatives (TN)
TN_Test <- conf_matrix_test[1, 1]

# False Positives (FP)
FP_Test <- conf_matrix_test[1, 2]

# False Negatives (FN)
FN_Test <- conf_matrix_test[2, 1]

# Accuracy
accuracy_Test <- (TP_Test + TN_Test) / sum(conf_matrix_test)

# Precision
precision_Test <- TP_Test / (TP_Test + FP_Test)

# Recall
recall_Test <- TP_Test / (TP_Test + FN_Test)

# F1-Score
f1_score_Test <- 2 * (precision_Test * recall_Test) / (precision_Test + recall_Test)

# Print the metrics
print(paste("Accuracy for Test :", accuracy_Test))
print(paste("Precision Test:", precision_Test))
print(paste("Recall for Test:", recall_Test))
print(paste("F1-Score for Test:", f1_score_Test))



###LASSO Regression###

X <- booking_clean[, !names(booking_clean) %in% "is_canceled"]

y <- booking_clean$is_canceled

X$hotel <- as.factor(X$hotel)
X$meal <- as.factor(X$meal)
X$country <- as.factor(X$country)

lasso_model <- glmnet(X, y, alpha = 1)
plot(lasso_model, xvar = "lambda", label = TRUE)
X1 <- model.matrix(~ . - 1, data = X) 
lasso_model1 <- glmnet(X1, y, alpha = 1)
# Cross-validation choose lambda 
cv_fit <- cv.glmnet(X1, y, alpha = 1)
best_lambda <- cv_fit$lambda.min
lasso_model_best <- glmnet(X1, y, alpha = 1, lambda = best_lambda)

predictions <- predict(lasso_model_best, newx = X1, s = best_lambda, type = "response")
roc.outp <- roc(y, predictions)
plot(roc.outp, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main= "Roc curve for LASSO")

# Confusion matrix
conf_matrix_ <- table(y, predictions > 0.5)

# True Positives (TP)
TP_ <- conf_matrix_[2, 2]

# True Negatives (TN)
TN_ <- conf_matrix_[1, 1]

# False Positives (FP)
FP_ <- conf_matrix_[1, 2]

# False Negatives (FN)
FN_ <- conf_matrix_[2, 1]

# Accuracy
accuracy_ <- (TP_ + TN_) / sum(conf_matrix_)

# Precision
precision_ <- TP_ / (TP_ + FP_)

# Recall
recall_ <- TP_ / (TP_ + FN_)

# F1-Score
f1_score_ <- 2 * (precision_ * recall_) / (precision_ + recall_)

# Print the metrics
print(paste("Accuracy for Lasso :", accuracy_))
print(paste("Precision for Lasso:", precision_))
print(paste("Recall for Lasso:", recall_))
print(paste("F1-Score for Lasso:", f1_score_))




predictions_lasso <- predict(lasso_model_best, newx = X1, s = best_lambda, type = "response")
predictions_logit <- predict(model2, newdata = test_data, type = "response")
roc_lasso <- roc(y, predictions_lasso)
roc_logit <- roc(test_data$is_canceled, predictions_logit)
plot(roc_lasso, col = "blue", legacy.axes = TRUE, print.auc = FALSE,
     main = "ROC Curves for Lasso and Logistic Regression", lwd = 2)
plot(roc_logit, col = "red", legacy.axes = TRUE, print.auc = FALSE, add = TRUE, lwd = 2)
legend("bottomright", legend = c(paste("Lasso (AUC =", round(auc(roc_lasso), 2), ")"),
                                 paste("Logistic Regression (AUC =", round(auc(roc_logit), 2), ")")),
       col = c("blue", "red"), lwd = 2, cex = 0.8)

