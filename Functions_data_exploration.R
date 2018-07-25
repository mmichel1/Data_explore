#----------------------------------------------
# Customer   : xxx
# Project    : xxx
# Exploration: general functions
#----------------------------------------------

#library(tidyverse)
#library(hexbin)

library (dplyr)
library (rlang)

# ====================== Function ================
read_datadesc <- function(data_control_file_name) {
  desc <-read_xlsx (data_control_file_name, # sheet = "datadesc",    once control file written sheet name disappears unfortunately 
                       col_types = c(
                         "text",      # Field Name
                         "text",      # Field Description
                         "text",      # Marker for Clean Data Frame
                         "text",      # Scale of Mesurement
                         "text",      # Binary Default 0
                         "text",      # Comment on outlier checking
                         "text",      # comment on data exploration result
                         "text",      # Create dummy var if "x" 
                         "text",      # Reduce which Dummy
                         "text"))  # % of na values
  warnings()
  desc[is.na(desc)] <- ''
  return(desc)
}


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
    var_desc <-(desc[names(df[i])==desc[ ,1], ]) # get descriptions of the field with matching var name
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
      
      # --- 3 most typical values ---- für einzelwerte as.table() nötig
      top_3 <- table(df[i]) %>% sort(decreasing = TRUE) %>% as.table() %>% as.data.frame()
      if (sum(top_3$Freq) != 0) {
        top_3$perc <- round(top_3$Freq/(sum(top_3$Freq))*100, 2)
      }
      else top_3$perc <- 0
      
      # Summaries
      print(summary(df[i]))
      print(paste("# of na: ", round(colSums(is.na(df[i]))/nrow(df[i]), digits = 5)*100, "%"))      
      print("3 most typical values:")
      print(paste(top_3[1:3,1], ""))
      print(paste(top_3[1:3,2], "#"))
      print(paste(top_3[1:3,3], "%"))
      print(paste("Variance    :", round(var(na.omit(df_num[,i])), digits =2)))     
      print(paste("Standard dev:", round(sqrt(var(na.omit(df_num[,i]))), digits =2)))    
      if (var_desc$`Field Description` != "")  print(paste("Description :", var_desc$`Field Description`))  # field description
      if (var_desc$Comment.Data.Expl != "")  print(paste("Comment     :", var_desc$Comment.Data.Expl)) # field comment
      cat("\n")
    }
  }
}
# ====================== Function ================
# Prints summaries and plots non numerical variables
summary_non_num <- function(df,desc) {
  par(mfrow=c(1,1), mai=c(0.5,2.5,0.5,0.5), pty="m", cex=0.8)
  for(i in 1:ncol(df)) { 
    var_ <- df[i]
    var_desc <- desc[names(df[i])==desc[,1],] # get descriptions of the field with matching var name
    
    if (all (is.na(df_not_num[,i])) == TRUE) {
      print("--------------------------------------------------------------------------------------------")
      print(paste("Field name:", names(df[i])), col="green")
      print(paste("Comment   : all values na ",var_desc$Comment.Data.Expl))
    }    
    
    else {
      # factorize "non date" fields
      #if (names(df[i])=='SERGEBNIS') browser()
      if (is.character(df[,i]) || is.factor(df[,i])) {
        var_ <- as.factor (unlist(df[i]))
        barplot(table(var_), xlab = "Frequency of Level Occurrence", main=names(df[i]), horiz=TRUE, las=1)
      }

      else {
        # factorize "non date" fields
        if (is.character(df[,i])) {
         #var_ <- as.factor (unlist(df[i]))
         #barplot(table(var_), xlab = "Frequency of Level Occurrence", main=names(df[i]), horiz=TRUE, las=1)
          if (length (levels(as.factor(df[,i]))) < 20)
            {
            p1 <- ggplot(data = df) +
              geom_bar(mapping = aes(x = df[,i])) +
              labs(x = names(df[i]), y = "")
            
            # --- plot ---
            multiplot(p1, cols=1)            
          }
        }
        else {
          boxplot(df[,i], main=names(df[i]), type="l")
          #boxp_5_num <- fivenum(df[,i]) # gives the 5 numbers
          #ggplot(data = df) +
          #  geom_boxplot(mapping = aes(x = "", y = df[,i]), na.rm = TRUE) +
          #  labs(x = names(df[i]), y = "") +
          #  geom_text(data = as.data.frame(boxp_5_num), aes(x = 1.25, boxp_5_num, label = boxp_5_num))
          #text(y=fivenum(df_not_num[,i]), labels =fivenum(round(df_not_num[,i], 3)), x=1.25)
        }
      }  
      # Summaries  
      print(paste("Field name:", names(df[i])), col="green")
      print(summary(var_, max=10))
      print(paste("# of na    :", colSums(is.na(df[i])), "  in %", colSums(is.na(df[i]))/nrow(df[i])*100))
      print(paste("Description:", var_desc[2]))     
      print(paste("Comment    :", var_desc$Comment.Data.Expl))  
      cat("\n")
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
  warnings()
  corrplot::corrplot.mixed(cor_m, number.cex = 1, tl.cex = .7)
  if (printpdf) dev.off()    # reset device to screen plot
  
  # get index of positively correlated variables from correlation matrix
  cor_index <- which (cor_m >= 0.7 & cor_m < 1)
  row_index <- cor_index %/% nrow(cor_m) + 1
  column_index <- cor_index %% nrow(cor_m)
  cat("\n--------------------------------------------------------------------\n")  
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
  print('--------------------------------------------------------------------')   
  print ("Negatively correlated variable pairs, r <= -0,7")
  if (length(cor_index) > 0) {
    for (i in 1:length(cor_index)) {
      print(paste(names(df_num[row_index[i]]),"/", names(df_num[column_index[i]]), ":",
                  round(cor_m[cor_index[i]], digits = 2)))
    }
  }
}


# =========================================================================== 
# 3. D a t a   p r e p a r a t i o n
# =========================================================================== 

# ====================== Function ================
# function returns a number which specifies frequent used facotrs beyond a common threshold
# factor needs to be ordered by frequency
fac_max <- function (fac, th){
  fac_acc <- 0
  for (i in 1:length(fac)) {
    fac_acc <- fac_acc + fac[i]
    if (fac_acc/sum(fac) > th) return (i)
  }
}

# =================================================
# 3.1 Transformation of variables
# =================================================
transformation <- function(df, datadesc) {
  for(i in 1:ncol(df)) {
    if ((datadesc$`Clean Data Frame`[i] != '') && (datadesc$`Clean Data Frame`[i] != 'exclude') ) {
      print (paste("... transforming", names(df[i])))
      
      # transform right-skewed data to logarithmic scale
      if (datadesc$`Clean Data Frame`[i]=="log") {
        # mit dplyer ziemlicher Aufwand, weil dynamische Spaltenzuweisung notwendig - Paket rlang! 
        # dann kann man mit doppel Ausrufezeichen die Spaltennamenvariablen referenzieren
        # https://stackoverflow.com/questions/47440812/dplyr-mutate-dynamically-named-variables-using-other-dynamically-named-variabl
        #col_log_name <- rlang::sym(paste0(names(df[i]), '_log'))
        col_orig_name <- rlang::sym(names(df[i]))
        df <- df %>% mutate(!!col_orig_name := round(log(!!col_orig_name), 2)) %>% as.data.frame() #%>% mutate(!!col_orig_name := NULL)
        #      df[i] <- log(df[i])      nicht wirklich einfacher :-(
        # immer noch kein infinite handling
        df[i] <- replace(unlist(df[i]), is.infinite(unlist(df[i])), 0)  #replacement of infinite values NA by 0
        # field name austauschen und control file neu abspeichern
        #datadesc$`Field Name`[i] <- as.character(col_log_name)
        #writexl::write_xlsx(datadesc, path = paste0(rel_fpath, filename, '_data_control', endung))
      }
      
      # transform into binary (0,1) variable
      if (datadesc$`Clean Data Frame`[i]=="binary") {
        var_desc <- datadesc[names(df[i])==datadesc[,1],] # get descriptions of the field
        var_default_0 <- as.character(var_desc[4])              # get default 0 value for the variable
        df[i] <- replace(df[i],df[i] != var_default_0,"1_yes")
        df[i] <- replace(df[i],df[i] == var_default_0,"0_no")
      }
      
      # binning of a variable
      if (datadesc$`Clean Data Frame`[i]=="binning") {
        map_pos <- which(names(df[i])==names(datamap[1,]), TRUE) # column index mapping var
        map_table <- datamap[1:which(datamap[,map_pos]=="other#"),map_pos:(map_pos+1)]   # load mapping table     
        l <- as.vector(unlist(map_table[2:(nrow(map_table)-1),1])) 
        b <- as.vector(unlist(map_table[1:(nrow(map_table)-1),2]))  
        df[i] <- as.character(cut(unlist(df[i]), breaks=b, labels=l))
      }  
      
      # reduce categories
      if (datadesc$`Clean Data Frame`[i]=="reduce") { 
        fac <- summary(factor(as.factor (unlist(df[i]))))
        fac <- fac[order(fac, decreasing = TRUE)]
        for (ii in 1:nrow(df)) {
          df[ii,i] <- replace(df[ii,i], 
                              !df[ii,i] %in% names(fac[1:fac_max(fac,0.95)]),
                              "other") # threshold 90%
        }  
      }                      
      
      # mapping of values to viewer # of categories
      if ((datadesc$`Clean Data Frame`[i]=="map") | (datadesc$`Clean Data Frame`[i]=="map_exact") ) { 
        map_pos <- which(names(df[i])==names(datamap[1,]), TRUE) # column index mapping var
        map_table <- datamap[1:which(datamap[,map_pos]=="other#"),map_pos:(map_pos+1)]   # load mapping table
        var_temp <- df[i]
        df[i] <- (rep(map_table[nrow(map_table),2],nrow(df)) )# fill var. with "other" value
        # therein mapping of character string
        if (datadesc$`Clean Data Frame`[i]=="map") {
          for (ii in 1:(nrow(map_table)-1)){
            map_idx <- grepl(map_table[ii,1], unlist(var_temp))     # create lookup index
            df[i] <- replace(unlist(df[i]), map_idx, as.character( map_table[ii,2])) # replace with mapping value
          }
        }
        else {                 # exact mapping of character string
          for (ii in 1:(nrow(map_table)-1)){
            df[var_temp==as.character(map_table[ii,1]),i] <- map_table[ii,2]
          }
        }
      }
      
    }
  }
  return (df)
}  


# =================================================
# 3.3 Construct Data
# =================================================

create_dummy <- function(df, datadesc) {
  
  # =================================================
  # 3.3.1 Create dummy varialbes
  # =================================================
  #open pdf output file and
  ## examine one-hot-encoder representation via comparison 
  #show top rows of concatenated one-hot-encoder and its original 
  #head(cbind(new_x_df, x))
  #   R E P L A C E   specify variables to be transformed to dummy varialbes in "change_var" vector
  change_var <- unlist(datadesc[datadesc$Dummy =="x",1])
  change_var <- change_var[!is.na(change_var)]
  if (length(change_var)) {
    for (i in 1:length(change_var)) {
      print (paste("... create dummies", change_var[i]))
      
      x <- unlist(df[,change_var[i]])
      uniq_levels <- unique(x) 
      n_obs       <- length(x) 
      n_levels    <- length(uniq_levels) 
      
      # create container to store dummy variables 
      new_x_df <- data.frame(matrix(0, nrow=n_obs, ncol=n_levels)) 
      # loop through each level and detect appearance of a level 
      for(ii in 1:length(uniq_levels)) { 
        # the level for examination 
        the_level <- uniq_levels[ii] 
        one_idx   <- x == the_level 
        new_x_df[one_idx, ii] <- 1 
      } 
      names (new_x_df) <- as.character (uniq_levels)
      names (new_x_df) <- paste (names (new_x_df), change_var[i])
      # concat dataframes
      df <- data.frame (c(df, (new_x_df)))
    }
    
    # =================================================
    # 3.3.2 Delete variables and rename variables by short names
    # =================================================
    # delete original varialbes of dummy 
    df <- df [ , !(names(df) %in% change_var)]
    # delete one for each dummy 
    change_var <- unlist(datadesc$Reduce_which_Dummy)
    change_var <- change_var[!is.na(change_var)]
    df <- df [ , !(names(df) %in% change_var)]
    # rename varialbes by short names - gibts bis dato nicht
    # if (shortnames==T) names(df) <- unlist(datanames$Short_name)

  }
  return (df)
}

