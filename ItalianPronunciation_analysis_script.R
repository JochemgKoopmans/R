# Importing useful packages:
library(data.table)
library(psych)
library(irr)

#Analysis parameters
file_names <- c(1,2,3,4,5,6,7,8,9,10)
word_list <- c('armadio','balcone','banana','barca','batteria','bilia','delfino','forbici','formica','gamba',
               'giraffa','gonna','lampada','martello','maschera','melone','naso','nastro','nuvola','pietra',
               'pistola','rana','rossetto','sandalo','sega','tigre','uovo','vulcano')
word_numbers <- 1:length(word_list)

#Supporting variables
file_name_lead <- "U:\\Documents\\Experiments\\Italian_pronunciation\\data\\"
file_name_tail <- ".csv"

#Supporting functions
add_word_columns <- function(frame_var,list) {
  for (x in list) {
    frame_var[,paste(toString(x), sep = "")] <- NA
  }
  return (frame_var <- frame_var)
}

#Creating empty dataframes
word_pron_scores <- data.frame(
  pronouncer = rep(c(1:30,'f','m'),each=28),
  word = rep(c('sandalo','batteria','naso','pistola','melone','delfino','tigre','giraffa','banana','sega','balcone','vulcano','maschera','lampada','bilia','uovo','gamba','nastro','formica','rana','armadio','martello','forbici','nuvola','gonna','pietra','rossetto','barca'),times=32),
  rating_mean = NA,
  rating_sd = NA
)
for (x in 1:length(file_names)) {
  word_pron_scores[x+4] <- NA
  colnames(word_pron_scores)[x+4] <- paste('r',as.character(x),sep="")
}

rater_info <- data.frame(
  rater = c(1:length(file_names)) #number of raters that will be analysed
)

rater_reliability <- data.frame(
  rater = c(1:length(file_names)),
  native_mean = NA,
  nonnative_mean = NA,
  all_mean = NA
)

mean_drift <- data.frame(rater = c(1:length(file_names)))
mean_drift <- add_word_columns(mean_drift,word_numbers)

sd_drift <- data.frame(rater = c(1:length(file_names)))
sd_drift <- add_word_columns(sd_drift,word_numbers)

word_eval_native_mean <- data.frame(rater = c(1:length(file_names)))
word_eval_native_mean <- add_word_columns(word_eval_native_mean,word_list)

word_eval_native_sd <- data.frame(rater = c(1:length(file_names)))
word_eval_native_sd <- add_word_columns(word_eval_native_sd,word_list)

word_eval_nonnative_mean <- data.frame(rater=c(1:length(file_names)))
word_eval_nonnative_mean <- add_word_columns(word_eval_nonnative_mean,word_list)

word_eval_nonnative_sd <- data.frame(rater=c(1:length(file_names)))
word_eval_nonnative_sd <- add_word_columns(word_eval_nonnative_sd,word_list)

word_repeats <- data.frame(rater=c(1:length(file_names))) #Over time
word_repeats <- add_word_columns(word_repeats,word_numbers)

trial_repetitions <- array(c(0), dim = c(28, 32, length(file_names))) #,dimnames = list('words','pronouncers','raters'))

mean_pronouncer_eval <- data.frame(rater=c(1:length(file_names)))
mean_pronouncer_eval <- add_word_columns(mean_pronouncer_eval,c(1:30,'m','f'))

sd_pronouncer_eval <- data.frame(rater=c(1:length(file_names)))
sd_pronouncer_eval <- add_word_columns(sd_pronouncer_eval,c(1:30,'m','f'))

rater_correlations <- data.frame(
  r = rep(c(0), each = length(file_names)),
  t = rep(c(0), each = length(file_names)),
  p = rep(c(NA), each = length(file_names))
)

for (rater_no in 1:length(file_names)) {
  file_name <- paste(file_name_lead,as.character(file_names[rater_no]),file_name_tail,sep="")
  data_file <- fread(file_name)
  
  word_no <- 1
  
  #rater_info[rater_no,3] <- data_file$response[data_file$trial_type=="survey-text"]
  
  q_separate <- strsplit(paste(data_file$response[data_file$trial_type=="survey-html-form"][2]),",")
  q_age <- strsplit(q_separate[[1]][1],"[\"]")
  q_occ <- strsplit(q_separate[[1]][2],"[\"]")
  q_gen <- strsplit(q_separate[[1]][3],"[\"]")
  q_edu <- strsplit(q_separate[[1]][4],"[\"]")
  fb <- strsplit(paste(data_file$response[data_file$trial_type=="survey-text"]),",")
  fb_1 <- strsplit(fb[[1]][1],"[\"]")
  fb_2 <- strsplit(fb[[1]][2],"[\"]")
  
  rater_info$age[rater_no] <- as.double(q_age[[1]][7])
  rater_info$occupation[rater_no] <- q_occ[[1]][7]
  rater_info$gender[rater_no] <- q_gen[[1]][7]
  rater_info$education[rater_no] <- as.double(q_edu[[1]][7])
  rater_info$grow_italy[rater_no] <- grepl("grow_up_italy",data_file$response[data_file$trial_type=="survey-html-form"][2])
  rater_info$one_parent[rater_no] <- grepl("1parent_speech",data_file$response[data_file$trial_type=="survey-html-form"][2])
  rater_info$two_parent[rater_no] <- grepl("2parent_speech",data_file$response[data_file$trial_type=="survey-html-form"][2])
  rater_info$primary_school[rater_no] <- grepl("primary_school",data_file$response[data_file$trial_type=="survey-html-form"][2])
  rater_info$secondary_school[rater_no] <- grepl("secondary_school",data_file$response[data_file$trial_type=="survey-html-form"][2])
  rater_info$fb1[rater_no] <- fb_1[[1]][7]
  rater_info$fb2[rater_no] <- fb_2[[1]][7]
  rater_info$total_duration[rater_no] <- data_file$time_elapsed[length(data_file$time_elapsed)]/60000
  rater_info$rating_duration[rater_no] <- (data_file$time_elapsed[length(data_file$time_elapsed)-2] - data_file$time_elapsed[data_file$trial_index[data_file$trial_type == 'preload'][1]])/60000
  
  ratings <- subset(data_file,button_response > -1 & word_order >0, select=c('word_order','word','pronouncer','native','button_response'))
  repeats <- subset(data_file,button_response == -1 & word_order >0, select=c('word_order','word','pronouncer','native','button_response'))
  
  catch_values <- matrix(nrow=2,ncol=3) #1st row: native catch trial difference, 2nd row nonnative catch trial difference
  c_native <- 1
  c_nonnative <- 1
  for (pronounced_word in word_list) {
    word_subset <- subset(ratings,word==pronounced_word)
    #Identifying catch-trials
    trial_repeats <- as.data.frame(table(data_file$pronouncer[data_file$word == pronounced_word & data_file$button_response>-1]))
    if (max(trial_repeats[,2])==2) { #The trial was repeated
      catch_trials <- word_subset[word_subset$pronouncer %in% trial_repeats$Var1[trial_repeats$Freq>1],]
      if (catch_trials$native[1] == TRUE) {
        catch_values[1,c_native] <- abs(catch_trials$button_response[1]-catch_trials$button_response[2])
        c_native <- c_native +1
      } else {
        catch_values[2,c_nonnative] <- abs(catch_trials$button_response[1]-catch_trials$button_response[2])
        c_nonnative <- c_nonnative +1
      }
      word_pron_scores[word_pron_scores$pronouncer==catch_trials$pronouncer[1] & word_pron_scores$word==catch_trials$word[1],rater_no+4] <- (catch_trials$button_response[1]+catch_trials$button_response[2])/2 +1
    }
    #Filling word pronunciation evaluation dataframe
    for (x in 1:nrow(word_subset)) {
      if (is.na(word_pron_scores[word_pron_scores$pronouncer==word_subset$pronouncer[x] & word_pron_scores$word==word_subset$word[x],rater_no+4])) {
        word_pron_scores[word_pron_scores$pronouncer==word_subset$pronouncer[x] & word_pron_scores$word==word_subset$word[x],rater_no+4] <- word_subset$button_response[x]+1
      }
    }
    #Identifying drift information
    w_ord <- word_subset$word_order[1] +1
    mean_drift[rater_no,w_ord] <- mean(word_subset$button_response)+1
    sd_drift[rater_no,w_ord] <- sd(word_subset$button_response)
    
    #Identifying word evaluations
    word_index <- word_no+1
    word_eval_native_mean[rater_no,word_index] <- mean(word_subset$button_response[word_subset$native==TRUE])+1
    word_eval_native_sd[rater_no,word_index] <- sd(word_subset$button_response[word_subset$native==TRUE])
    word_eval_nonnative_mean[rater_no,word_index] <- mean(word_subset$button_response[word_subset$native==FALSE])+1
    word_eval_nonnative_sd[rater_no,word_index] <- sd(word_subset$button_response[word_subset$native==FALSE])
    
    #Identifying word repeats: over time, per word/pronouncer/rater
    word_repeats[rater_no,word_index] <- sum(repeats$button_response[repeats$word == pronounced_word]==-1)
    if (sum(repeats$button_response[repeats$word == pronounced_word]==-1)>0) { #If participant repeated any trial in this block
      for (x in 1:length(repeats$pronouncer[repeats$word==pronounced_word])) {
        if (repeats$pronouncer[repeats$word==pronounced_word][x]=='f') {
          trial_repetitions[word_no,32,rater_no] <- trial_repetitions[word_no,32,rater_no] +1
          next
        } else if (repeats$pronouncer[repeats$word==pronounced_word][x]=='m') {
          trial_repetitions[word_no,31,rater_no] <- trial_repetitions[word_no,31,rater_no] +1
        } else {
          trial_repetitions[word_no,as.double(repeats$pronouncer[repeats$word==pronounced_word][x]),rater_no] <- trial_repetitions[word_no,as.double(repeats$pronouncer[repeats$word==pronounced_word][x]),rater_no] +1
        }
      }
    }
    word_no <- word_no +1
  }
  rater_reliability[rater_no,2] <- mean(catch_values[1,])
  rater_reliability[rater_no,3] <- mean(catch_values[2,])
  rater_reliability[rater_no,4] <- mean(catch_values)
  
  #Identifying pronouncer evaluations
  for (x in 1:32) {
    pronouncer_index <- x+1
    if (x>31) {
      pronouncer_index <- 'f'
      mean_pronouncer_eval[rater_no,pronouncer_index] <- mean(data_file$button_response[data_file$pronouncer==pronouncer_index & data_file$button_response>-1], na.rm = TRUE)+1
      sd_pronouncer_eval[rater_no,pronouncer_index] <- sd(data_file$button_response[data_file$pronouncer==pronouncer_index & data_file$button_response>-1], na.rm = TRUE)
      next
    } else if (x>30) {
      pronouncer_index <- 'm'
      mean_pronouncer_eval[rater_no,pronouncer_index] <- mean(data_file$button_response[data_file$pronouncer==pronouncer_index & data_file$button_response>-1], na.rm = TRUE)+1
      sd_pronouncer_eval[rater_no,pronouncer_index] <- sd(data_file$button_response[data_file$pronouncer==pronouncer_index & data_file$button_response>-1], na.rm = TRUE)
      next
    }
    mean_pronouncer_eval[rater_no,pronouncer_index] <- mean(data_file$button_response[data_file$pronouncer==toString(x) & data_file$button_response>-1], na.rm = TRUE)+1
    sd_pronouncer_eval[rater_no,pronouncer_index] <- sd(data_file$button_response[data_file$pronouncer==toString(x) & data_file$button_response>-1], na.rm = TRUE)
  }
}
word_pron_scores$rating_mean <- rowMeans(word_pron_scores[,5:ncol(word_pron_scores)], na.rm = TRUE) #works only with multiple raters (length(file_names>1))
word_pron_scores$rating_sd <- apply(word_pron_scores[,5:ncol(word_pron_scores)],1,sd, na.rm = TRUE) #same

#write.csv(word_pron_scores,"C:\\Users\\s4529901\\Documents\\word_pronunciation_scores.csv")

# Interrater correlations
for (i in 1:length(file_names)) {
  col_vector <- c(5:14)[!c(5:14) %in% c(i+4)]
  other_rater_mean <- rowMeans(word_pron_scores[col_vector[!col_vector %in% c(i+4)]])
  rater_correlations[i,1] = cor.test(word_pron_scores[i+4][!is.na(word_pron_scores[i+4])],other_rater_mean[!is.na(other_rater_mean)])$estimate
  rater_correlations[i,2] = cor.test(word_pron_scores[i+4][!is.na(word_pron_scores[i+4])],other_rater_mean[!is.na(other_rater_mean)])$statistic
  rater_correlations[i,3] = cor.test(word_pron_scores[i+4][!is.na(word_pron_scores[i+4])],other_rater_mean[!is.na(other_rater_mean)])$p.value
  
}
interrater_correlation <- ICC(word_pron_scores[!is.na(word_pron_scores[5]),5:14])
kendall_W <- kendall(word_pron_scores[!is.na(word_pron_scores[5]),5:14], correct = TRUE)

paired_rater_correlations <- matrix(c(NA), nrow = 10, ncol = 11)
for (i in 1:length(file_names)) {
  raters_list <- c(5:14)[!c(5:14) %in% c(i+4)]
  for (j in raters_list) {
    paired_rater_correlations[i,j-4] = cor.test(word_pron_scores[!is.na(word_pron_scores[i+4]),i+4],word_pron_scores[!is.na(word_pron_scores[j]),j], method = "spearm")$estimate
  }
  paired_rater_correlations[i,11] = mean(paired_rater_correlations[i,1:10], na.rm=TRUE)
}

#Rater evaluation spread overall
raters_sds <- rep(c(0), times=10)
for (i in 1:length(file_names)) {
  raters_sds[i] = sd(word_pron_scores[!is.na(word_pron_scores[5]),i+4])
}

#PLOTS
colours <- c('purple','blue','aquamarine','green','yellow','peachpuff','orange','red','darkred','pink') #in case I want to show each rater in a different colour
#1: Rater information: Visually inspect rater_info (unless further specified)

#2: Rater reliability:
par(mfrow = c(1,1))
plot(rep(c(1),each=nrow(rater_reliability)), rater_reliability[,2],
     main = "Rater reliability on catch trials",
     xlab = "", xlim=c(0,4), ylab = "Mean rating difference", ylim = c(0,max(rater_reliability[,2:4])),
     col='blue',las = 1,xaxt = "n")
points(rep(c(2),each=nrow(rater_reliability)), rater_reliability[,3], col = 'red')
points(rep(c(3),each=nrow(rater_reliability)), rater_reliability[,4], col = 'purple')
axis(1, at = c(1,2,3), labels=c('native','non-native','combined'))
#For more raters, this should be combinable into one plot?
par(mfrow = c(1,1))
plot(c(1,2,3), rater_reliability[1,2:4],
     main = "Rater reliability on catch trials",
     xlab = "", ylab = "Mean rating difference", ylim = c(min(rater_reliability[,2:4]),max(rater_reliability[,2:4])),
     col = colours[1], type="b", las = 1, xaxt = "n")
for (x in 2:length(file_names)) {
  points(c(1,2,3), rater_reliability[x,2:4], col = colours[x], type = "b")
}
legend(x = "topleft", legend = c("Each line represents one rater"), cex = .8, xpd = TRUE)
axis(1,at = c(1,2,3), labels = c('native','non-native','combined'))

#3. Rating drift over blocks (perhaps create two plots in the same figure)
par(mfrow = c(2,1))
plot(c(1:28), mean_drift[1,2:29],
     main = "Evaluation mean per block",
     xlab = "Word number" , ylab = "mean", ylim = c(min(mean_drift[,2:29]),max(mean_drift[,2:29])),
     col = colours[1], type = "b", las = 1)
for (x in 2:length(file_names)) {
  points(c(1:28), mean_drift[x,2:29], col = colours[x], type = "b")
}
plot(c(1:28), sd_drift[1,2:29],
     main = "Evaluation spread per block",
     xlab = "Word number", ylab = "sd", ylim = c(min(sd_drift[,2:29]),max(sd_drift[,2:29])),
     col = colours[1], type = "b", las = 1)
for (x in 2:length(file_names)) {
  points(c(1:28), sd_drift[x,2:29], col = colours[x], type = "b")
}

#4. Word evaluation
par(mfrow=c(2,1))
plot(c(1:28), word_eval_native_mean[1,2:29],
     main = "Evaluation mean per word",
     xlab = "Word", ylab = "mean", ylim = c(min(word_eval_nonnative_mean[,2:29]),max(word_eval_native_mean[,2:29])),
     col = "blue", type = "b", las = 1, xaxt = "n")
points(c(1:28), word_eval_nonnative_mean[1,2:29], col = "red", type = "b")
for (x in 2:length(file_names)) {
  points(c(1:28),word_eval_native_mean[x,2:29], col = "blue", type = "b")
  points(c(1:28),word_eval_nonnative_mean[x,2:29], col = "red", type = "b")
}
axis(1,at = c(1:28), labels = word_list)
legend(x = "bottomright", inset = c(0,1), legend = c("native", "non-native"), lty = c(1,1), col = c('blue', 'red'), cex = .8, xpd = TRUE)

plot(c(1:28),word_eval_native_sd[1,2:29],
     main = "Evaluation SD per word",
     xlab = "Word", ylab = "sd", ylim = c(min(word_eval_native_sd[,2:29]),max(word_eval_nonnative_sd[,2:29])),
     col = "blue",type = "b", las = 1, xaxt = "n")
points(c(1:28), word_eval_nonnative_sd[1,2:29], col = "red", type = "b")
for (x in 2:length(file_names)) {
  points(c(1:28), word_eval_native_sd[x,2:29], col = "blue", type = "b")
  points(c(1:28), word_eval_nonnative_sd[x,2:29], col = "red", type = "b")
}


#5. Word repeats
  #5.1: Per rater, per block
par(mfrow = c(3,1))
plot(c(1:28),word_repeats[1,2:29],
     main = "Trials repeated per block",
     xlab = "block", ylab = "trial repeats", ylim=c(0,max(word_repeats[,2:29])),
     col = colours[1], type = "b",las = 1)
for (x in 2:length(file_names)) {
  points(c(1:28), word_repeats[x,2:29], col = colours[x], type = "b")
}
  #5.2: Per word
plot(c(1:28), rowSums(trial_repetitions, na.rm = FALSE, 1),
     main = "Trials repeated per word",
     xlab = "Word", ylab = "trial repeats", ylim = c(0,max(rowSums(trial_repetitions, na.rm=FALSE, 1))),
     type = "b", xaxt = "n"
)
axis(1,at = c(1:28), labels = word_list)
points(c(1:28), rowSums(trial_repetitions, na.rm = FALSE, 2)[1:28,9], col = "blue", type = "b")
points(c(1:28), rowSums(trial_repetitions, na.rm = FALSE, 2)[1:28,22], col = "green", type = "b")
points(c(1:28), rowSums(trial_repetitions, na.rm = FALSE, 2)[1:28,23], col = "yellow", type = "b")
points(c(1:28), rowSums(trial_repetitions, na.rm = FALSE, 2)[1:28,24], col = "red", type = "b")

  #5.3: Per pronouncer
plot(c(1:32), rowSums(colSums(trial_repetitions, 1)),
     main = "Trials repeated per pronouncer",
     xlab = "Pronouncer", ylab = "trial repeats", ylim = c(0,max(rowSums(colSums(trial_repetitions,1)))),
     type = "b"
)

#6. Pronouncer evaluations
par(mfrow = c(2,1))
plot(c(1:32), mean_pronouncer_eval[1,2:33],
     main = "Evaluation mean per pronouncer",
     xlab = "Pronouncer", ylab = "mean", ylim = c(min(mean_pronouncer_eval[,2:33]),max(mean_pronouncer_eval[,2:33])),
     col = colours[1], type = "b", las = 1, xaxt = "n")
for (x in 2:length(file_names)) {
  points(c(1:32), mean_pronouncer_eval[x,2:33], col = colours[x], type = "b")
}
points(c(1:32), colMeans(mean_pronouncer_eval[2:33], dims = 1), col = "black", type = "b", pch = 15)
axis(1, at = c(1:32), labels = c(1:30,'m','f'))

plot(c(1:32),sd_pronouncer_eval[1,2:33],
     main = "Evaluation SD per pronouncer",
     xlab = "Pronouncer", ylab = "sd", ylim = c(min(sd_pronouncer_eval[,2:33]),max(sd_pronouncer_eval[,2:33])),
     col = colours[1], type = "b", las = 1, xaxt = "n")
for (x in 2:length(file_names)) {
  points(c(1:32), sd_pronouncer_eval[x,2:33], col = colours[x], type = "b")
}
axis(1, at = c(1:32), labels = c(1:30,'m','f'))

#7. Pronouncer spread
par(mfrow = c(1,1))
plot(c(1), raters_sds[1],
     main = "Rating spread per rater",
     xlab = "", ylab = "SD",
     col = colours[1], las = 1, xaxt = "n")
for (x in 2:length(file_names)) {
  points(c(1), raters_sds[x], col = colours[x])
}
#Printing values in the console:
print(rater_info)
print(rater_correlations)
print(interrater_correlation)
print(kendall_W)
print(paired_rater_correlations)