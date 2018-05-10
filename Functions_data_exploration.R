#----------------------------------------------
# Customer   : xxx
# Project    : xxx
# Exploration: general functions
#----------------------------------------------

#library(tidyverse)
#library(hexbin)

# ====================== Function ================
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ====================== Function ================
# Print of boxplots, histograms and summaries for numerical variables
plot_num <- function(df,desc) {
  #par(mfrow=c(1,2), mai=c(0.5,0.5,0.5,0.5), pty="m", cex=0.8)
  #'mai' gives the number of lines of margin
  # to be specified on the four sides of the plot.
  for(i in 1:ncol(df)) {
    if (all (is.na(df_num[,i])) == TRUE) {
      print("--------------------------------------------------------------------------------------------")
      print(paste("Field name:", names(df[i])), col="green")
      print(paste("Comment   : all values na ",var_desc$Comment.Data.Expl))
    }    
    else {
      # --- Boxplots ---
      boxp_5_num <- fivenum(df[,i]) # gives the 5 numbers
      p1 <- ggplot(data = df) +
        geom_boxplot(mapping = aes(x = "", y = df[,i]), na.rm = TRUE) +
        labs(x = names(df[i]), y = "") +
        geom_text(data = as.data.frame(boxp_5_num), aes(x = 1.25, boxp_5_num, label = boxp_5_num))
      #boxplot(df[,i], main=names(df[i]), type="l")
      #text(y=fivenum(df[,i]), labels =fivenum(round(df[,i], 3)), x=1.25)
      
      # --- Histograms ---
      #hist(df[,i], main=names(df[i]), xlab=NULL)
      if (max(na.omit(df_num[i])) - min(na.omit(df_num[i])) == 0) {
        bw <- 0.1
      }
      else {
        bw <- (max(na.omit(df_num[i])) - min(na.omit(df_num[i])))/30
      }
      p2 <- ggplot(data = df) +
        geom_histogram(mapping = aes(x = df[,i]), binwidth = bw, na.rm = TRUE) +
        labs(x = names(df[i]))
      
      # --- plot ---
      multiplot(p1, p2, cols=2)
      #gridextra::gridarrange (p1, p2, cols=2)
      # Summaries
      print(summary(df[i]))
      print(paste("# of na in %:", round(colSums(is.na(df[i]))/nrow(df[i])*100)), 1)
      print(paste("Variance    :", var(na.omit(df_num[,i]))))     
      print(paste("Standard dev:", sqrt(var(na.omit(df_num[,i])))))    
      var_desc <-(desc[names(df[i])==desc[,1],]) # get descriptions of the field
      print(paste("Description :", var_desc[2]))  # field description
      print(paste("Comment     :", var_desc$Comment.Data.Expl)) # field comment
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

