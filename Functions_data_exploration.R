#----------------------------------------------
# Customer   : xxx
# Project    : xxx
# Exploration: general functions
#----------------------------------------------


# ====================== Function ================
# Print of boxplots, histograms and summaries for numerical variables
plot_num <- function(df,desc) {
  par(mfrow=c(1,2), mai=c(0.5,0.5,0.5,0.5), pty="m", cex=0.8)
  #'mai' gives the number of lines of margin
  # to be specified on the four sides of the plot. 
  for(i in 1:ncol(df)) {
    if (all (is.na(df_num[,i])) == TRUE) {
      print("--------------------------------------------------------------------------------------------")
      print(paste("Field name:", names(df[i])), col="green")
      print(paste("Comment   : all values na ",var_desc$Comment.Data.Expl))
    }    
    else {
      # Boxplots
      boxplot(df[,i], main=names(df[i]), type="l")
      text(y=fivenum(df[,i]), labels =fivenum(round(df[,i], 3)), x=1.25)
      # Histograms
      hist(df[,i], main=names(df[i]), xlab=NULL)
      # Summaries
      print(summary(df[i]))
      print(paste("# of na    :", colSums(is.na(df[i])), "  in %", colSums(is.na(df[i]))/nrow(df[i])*100))
      var_desc <-(desc[names(df[i])==desc[,1],]) # get descriptions of the field
      print(paste("Description:", var_desc[2]))  # field description
      print(paste("Comment    :", var_desc$Comment.Data.Expl)) # field comment
    }
  }
}
# ====================== Function ================
# Prints summaries and plots non numerical variables
summary_non_num <- function(df,desc) {
  par(mfrow=c(1,1), mai=c(0.5,2.5,0.5,0.5), pty="m", cex=0.8)
  for(i in 1:ncol(df)) { 
    var_ <- df[i]
    var_desc <- desc[names(df[i])==desc[,1],] # get descriptions of the field
    
    if (all (is.na(df_not_num[,i])) == TRUE) {
      print("--------------------------------------------------------------------------------------------")
      print(paste("Field name:", names(df[i])), col="green")
      print(paste("Comment   : all values na ",var_desc$Comment.Data.Expl))
    }    
    else {
      # factorize "non date" fields
      if (is.character(df[,i])) {
        var_ <- as.factor (unlist(df[i]))
        barplot(table(var_), xlab = "Frequency of Level Occurrence", main=names(df[i]), horiz=TRUE, las=1)
      }
      else {
        boxplot(df[,i], main=names(df[i]), type="l")
        #text(y=fivenum(df_not_num[,i]), labels =fivenum(round(df_not_num[,i], 3)), x=1.25)
      }
      # Summaries  
      print(paste("Field name:", names(df[i])), col="green")
      print(summary(var_, max=10))
      print(paste("# of na    :", colSums(is.na(df[i])), "  in %", colSums(is.na(df[i]))/nrow(df[i])*100))
      print(paste("Description:", var_desc[2]))     
      print(paste("Comment    :", var_desc$Comment.Data.Expl))  
    }
    #print("--------------------------------------------------------------------")
  }
}

# ====================== Function ================
# Check collinearity and produce correlation matrix
check_collinearity <- function(df) {
  library(corrplot)
  if (printpdf) pdf(outputfile)
  cor_m <- cor(df)
  corrplot::corrplot.mixed(cor_m)
  if (printpdf) dev.off()    # reset device to screen plot
  
  # get index of positively correlated variables from correlation matrix
  cor_index <- which (cor_m >= 0.7 & cor_m < 1)
  row_index <- cor_index %/% nrow(cor_m) + 1
  column_index <- cor_index %% nrow(cor_m)
  print("--------------------------------------------------------------------")  
  print ("Positively correlated variable pairs, r >= 0,7")
  if (length(cor_index) > 0) {
    for (i in 1:length(cor_index)) {
      print(paste(names(df_num[row_index[i]]),"/", names(df_num[column_index[i]]), ":",
                  round(cor_m[cor_index[i]], digits = 2)))
    }
  }
  
  # get index of negatively correlated variables from correlation matrix
  cor_index <- which (cor_m <= -0.7 & cor_m > -1)
  row_index <- cor_index %/% nrow(cor_m) + 1
  column_index <- cor_index %% nrow(cor_m)
  print("--------------------------------------------------------------------")   
  print ("Negatively correlated variable pairs, r <= -0,7")
  if (length(cor_index) > 0) {
    for (i in 1:length(cor_index)) {
      print(paste(names(df_num[row_index[i]]),"/", names(df_num[column_index[i]]), ":",
                  round(cor_m[cor_index[i]], digits = 2)))
    }
  }
}
