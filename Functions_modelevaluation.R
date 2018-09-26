#----------------------------------------------
# Customer   : xxx
# Project    : xxx
# Evaluation : general functions
#----------------------------------------------

library(colorRamps)
library(plotly)

# ===================== Probability to Class =====
prob_to_class  = function(y, c) { # raw predicted vaules ,cutoff rate
  y[y < c] <- 0                  # assign 0 depending from cutoff rate
  y[y >=c] <- 1                  # assign 1 depending from cutoff rate
  return (y)
}

# ===================== Function Lift Chart ======
library(dplyr)
library(ggplot2)
library(stringr)
library(colorRamps)
library(plotly)
library(ROCR)      # plot ROC curve
library(caret)     # machine learning suites

Lift_Chart <- function (yhat_proba, ytrue, headline, color) {
  # Creating the data frame
  df_lift <- data.frame(yhat_proba, ytrue)
  
  # Ordering the dataset
  df_lift <- df_lift[order(df_lift$yhat_proba, decreasing = TRUE),]
  
  # Creating the cumulative density
  df_lift$cumden <- cumsum(df_lift$ytrue/sum(df_lift$ytrue))
  
  # Creating the % of population
  df_lift$perpop <- (seq(nrow(df_lift))/nrow(df_lift))*100
  
  # Ploting
  plot(df_lift$perpop,df_lift$cumden,type="l",
       xlab="% of Cases",ylab="% of Default's", 
       col=color, main=headline, cex.main=0.75)
  
  y = function(yhat_proba) { return ( yhat_proba/100 ) }
  plot(y, 0, 100, add = TRUE) 
}

# ===================== Function ROC Chart =======
ROC_Chart <- function (yhat_proba, ytrue, headline) {
  pred_obj <- prediction(yhat_proba, ytrue)
  perf     <- performance(pred_obj, 'tpr', 'fpr')
  
  #calcualte AUC (Area under Curve-ROC)
  auc_value <- performance(pred_obj, 'auc')@y.values[[1]]
  cat ("Area under Curve-ROC:",auc_value)
  plot(perf, colorize=T,
       xlab="1 - Specificity", ylab="Sensitivity", cex.main=0.75,
       main = toString (c(headline, " AUC", round(auc_value, digits = 4))))
}