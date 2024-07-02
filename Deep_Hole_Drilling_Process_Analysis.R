install.packages("tuneR")
library("tuneR")

# Adjust the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Custom Function to read the header files
read.gx1.hdr <- function(hdr.file, to.numeric = c("RATE", "NUM_SERIES", "SLOPE", "X_OFFSET", "Y_OFFSET", "NUM_SAMPS")) { 
  hdr.lines <- readLines(hdr.file, warn = FALSE)
  par.name <- suppressWarnings(sapply(strsplit(hdr.lines, " "), "[", 1))
  DATA <- grep("^DATA$", par.name)
  hdr.lines <- hdr.lines[1:(DATA-1)]
  par.name <- par.name[1:(DATA-1)]
  hdr.lines <- substring(hdr.lines, nchar(par.name)+2)
  splitted <- strsplit(hdr.lines, ",")
  splitted <- lapply(splitted, function(x) gsub("^[[:space:]]*([[:graph:]])", "\\1", x))
  splitted <- lapply(splitted, function(x) gsub("([[:graph:]])[[:space:]]*$", "\\1", x))
  names(splitted) <- par.name
  to.numeric <- which(par.name %in% to.numeric)
  for(i in to.numeric){
    splitted[[i]] <- as.numeric(splitted[[i]])
  }
  return (splitted)
}

# Custom Function to read the data files
read.gx1 <- function(filestem, toWaveMC=FALSE, ...){
  header <- read.gx1.hdr(paste(filestem, ".hdr", sep =""), ...)
  con <- file(paste(filestem, ".dat", sep=""), open="rb")
  on.exit(close(con))
  NUM_SERIES <- header[["NUM_SERIES"]]
  dat <- readBin(con, what="integer", size = 2, endian = "little", signed=TRUE, 
                 n = header[["NUM_SAMPS"]] * NUM_SERIES)
  dat <- matrix(dat, nrow = NUM_SERIES)
  dat <- t(dat * header[["SLOPE"]] + header[["Y_OFFSET"]])
  colnames(dat) <- header[["SERIES"]]
  if(toWaveMC && require("tuneR"))
    dat <- WaveMC(dat, samp.rate = header[["RATE"]], bit=1)
  else dat <- as.data.frame(dat)
  for (i in names(header))
    attr(dat, i) <- header[[i]]
  return(dat)
}

# --------------- Read data for all experiments------------------

drill2 <- read.gx1("V2_00001", toWaveMC = TRUE)
drill6 <- read.gx1("V6_00001", toWaveMC = TRUE)
drill10 <- read.gx1("V10_0001", toWaveMC = TRUE)
drill17 <- read.gx1("V17_0001", toWaveMC = TRUE)
drill20 <- read.gx1("V20_0001", toWaveMC = TRUE)
drill24 <- read.gx1("V24_0001", toWaveMC = TRUE)
drill25a <- read.gx1("V25a_001", toWaveMC = TRUE)
drillD04 <- read.gx1("D0400001", toWaveMC = TRUE)
drillD06 <- read.gx1("D0600001", toWaveMC = TRUE)
drillD08 <- read.gx1("D0800001", toWaveMC = TRUE)


par(mfrow=c(2,1))
plot(drill10[,"CH1_Moment"], main = "Experiment V10: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill10[,"CH2_Kraft"], main = "Experiment V10: Kraft", center = FALSE, ylab = "Kraft (N)", xlab = "Time (sec)")
plot(drill10[,"CH4_akustik"], main = "Experiment V10: Akustik", center = FALSE, ylab = "Akustik (Pa)", xlab = "Time (sec)")
plot(drill10[,"CH3_SyncSig"], main = "Experiment V10: SyncSig", center = FALSE, ylab = "SyncSig (V)", xlab = "Time (sec)")
plot(drill10[,"CH5_a1_WSAS"], main = "Experiment V10: WSAS", center = FALSE, ylab = expression(WSAS (  ~ m/s^2 )), xlab = "Time (sec)")
plot(drill10[,"CH6_a2_WSAF"], main = "Experiment V10: WSAF", center = FALSE, ylab = expression(WSAF (  ~ m/s^2 )), xlab = "Time (sec)")
plot(drill10[,"CH7_a3_BOZA"], main = "Experiment V10: BOZA", center = FALSE, ylab = expression(BOZA (  ~ m/s^2 )), xlab = "Time (sec)")

## listen the sound
drill10_sound <- as(normalize(drill10[,"CH4_akustik"], unit = "16"), "Wave")
drill10_sound
play(drill10_sound)

par(mfrow=c(2,2))
plot(drill2[,"CH1_Moment"], main = "Experiment V2: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill6[,"CH1_Moment"], main = "Experiment V6: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill17[,"CH1_Moment"], main = "Experiment V17: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill20[,"CH1_Moment"], main = "Experiment V20: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill24[,"CH1_Moment"], main = "Experiment V24: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drill25a[,"CH1_Moment"], main = "Experiment V25a: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drillD04[,"CH1_Moment"], main = "Experiment D04: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drillD06[,"CH1_Moment"], main = "Experiment D06: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")
plot(drillD08[,"CH1_Moment"], main = "Experiment D08: Moment",  center = FALSE, ylab = "Moment (Nm)", xlab = "Time (sec)")

par(mfrow=c(2,1))
# Begin -> Contact
plot(drill10[(28*20000):(34*20000),"CH1_Moment"] , center=FALSE, xaxt = "n", main = "First Contact Phase", ylab = "Moment (in Nm)", xlab = "Time (in sec)") 
axis(1, at=0:6, labels=c(28:34))
# Contact -> I
plot(drill10[(45*20000):(51*20000),"CH1_Moment"], center=FALSE, xaxt = "n", main = "Start of Problematic Phase (I)", ylab = "Moment (in Nm)", xlab = "Time (in sec)") 
axis(1, at=0:6, labels=c(45:51))

#Before the start of problematic phase (II)
plot(drill10[(90.1*20000):(90.12*20000),"CH1_Moment"], xaxt = "n",center = FALSE, main="Before the start of problematic phase (II)", ylab = "Moment (in Nm)", xlab = "Time (in sec)")
axis(1, at = seq(0, 0.02, by = 0.005), labels=c(90.1,90.105,90.110,90.115,90.120))
#After the start of problematic phase (II)
plot(drill10[(91.2*20000):(91.22*20000),"CH1_Moment"], xaxt = "n", center = FALSE, main="After the start of problematic phase (II)", ylab = "Moment (in Nm)", xlab = "Time (in sec)")  
axis(1, at = seq(0, 0.02, by = 0.005), labels=c(91.2,91.205,90.210,90.215,90.220))


##### Function to detect transition into problematic phases #######
detect_changes <- function(ts, plot_pdg = FALSE, plot_ts = FALSE, 
                           wd_length = 1000, is_test = FALSE)  {
  
  # Create a data frame to store the flags
  df = data.frame(matrix(vector(),
                         nrow =  round(length(ts)/wd_length -1), 
                         ncol = 3 , 
                         dimnames = list(c(), c("Window",
                                                "ProblemStarts_1",
                                                "ProblemStarts_2"))),
                  stringsAsFactors=F)
  
  # Run through windows
  for (i in (0:(round(length(ts)/wd_length)-2))) {
    # Subset window
    wd <- ts[((i)*wd_length+1):((i+1)*wd_length)]
    
    # Plot time series plot of the current window
    if(plot_ts){
      plot(wd, cex.lab = 1.3, cex.axis=1.2, cex.main=1.3)
    }
    
    # Reset the Flags
    ProblemStartsFlag_1 <- 0
    ProblemStartsFlag_2 <- 0
    
    # Calculate values for the periodogram
    #per <- spectrum(wd, method = "pgram", log="no", plot=FALSE)
    per <- spectrum(wd, method = "pgram", log="no", plot=FALSE, taper = 0, kernel = NULL,
                    detrend = FALSE, spans = NULL, fast = FALSE )
    
    # Scale the spec values between 0 and 1
    spec <- per$spec
    spec_scaled <- spec/sum(spec)
    # Get the maximum value for the scaled spec
    max_spec_scaled <- max(spec_scaled)
    # Represent frequencies in Hertz (By multiplying with sampling rate 20000)
    freq <- per$freq*20000
    # Get the most dominant frequency
    max_freq <- freq[which.max(spec)]
    # Get top five dominant frequencies
    top_five_freqs <- freq[which(spec >= sort(spec, decreasing=T)[5],
                                 arr.ind=TRUE)]
    # Round frequency values to integers
    top_five_freqs <- as.integer(top_five_freqs)   
    
    # Plot the scaled periodogram for the current window
    if(plot_pdg) {
      plot(freq, spec_scaled, type = "l", ylim=c(0,1), ylab="Power (Scaled)",
           xlab="Frequency (in Hertz)", main=paste0("Peridogram : Window ", i+1),
           cex.lab = 1.3, cex.axis=1.2, cex.main=1.3)
    }
    
    # Find dominant frequencies
    count_spikes <- length(sort(spec_scaled[spec_scaled > 0.02*max_spec_scaled]))
    
    # Find the variance and mean
    wd <- as.vector(wd)
    wd_var <- var(wd)
    wd_mean <- mean(wd)
    
    # !!!test!!!
    if(is_test){
      cat("Window   : " , i+1, "\n")
      cat("Length   : " , length(wd), "\n")
      cat("max_freq : " , max_freq, "\n")
      cat("spikes   : " , count_spikes, "\n")
      cat("wd_var   : " , wd_var, "\n")
      cat("wd_mean  : " , wd_mean, "\n")
    }
    # !!!test!!!
    
    # Set vectors to store the values from the previous windows
    if(i==0) {
      last_vars <- vector(length=3)
      last_means <- vector(length=3)
      last_top_five_freqs <- vector(length=5)
      VarianceIncreased <- vector(length=1) 
    }
    
    # Set Flags for the current window 
    if(i>=2){
      VarianceIncreased <- wd_var > 1.5*mean(last_vars)
      MeanIncreased <- wd_mean > 2*abs(mean(last_means))
      SpikesFew <- count_spikes < 30
      
      # Set the flags if a transition into a problematic phase is happening
      if(SpikesFew & !MeanIncreased & VarianceIncreased & last_VarianceIncreased){
        if( (1180 %in% top_five_freqs) & (1180 %in% last_top_five_freqs) ){
          ProblemStartsFlag_1 <- 1
          if(is_test)
            cat("<<<<<<<<<<<  PROBLEM STARTS PHASE 1  >>>>>>>>>>>>", "\n")
        }
        else if(  ((720 %in% top_five_freqs) & (720 %in% last_top_five_freqs))
                  | ((240 %in% top_five_freqs) & (240 %in% last_top_five_freqs)) 
        ){
          ProblemStartsFlag_2 <- 1
          if(is_test)
            cat("<<<<<<<<<<<  PROBLEM STARTS PHASE 2  >>>>>>>>>>>>", "\n")
        } 
        
        # !!!test!!!
        if(is_test){
          cat("Variance Increased       : ", VarianceIncreased, "\n")
          cat("Mean Increased           : ", MeanIncreased, "\n")
          cat("Spikes Few               : ", SpikesFew, "\n")
          cat("Top five frequencies     : ", top_five_freqs, "\n")
          cat("Last top five frequencies : ", last_top_five_freqs, "\n")
          cat("Last Variance Increased  : ", last_VarianceIncreased, "\n")
        }
        # !!!test!!!
        
      }
    }  
    
    # Update the values for the upcoming window
    last_vars[((i+1)%%3)+1] <- wd_var
    last_means[((i+1)%%3)+1] <- wd_mean
    last_top_five_freqs <- top_five_freqs
    last_VarianceIncreased <- VarianceIncreased
    
    # Store the flags for the current window
    df[i+1, "Window"] <- i+1
    df[i+1, "ProblemStarts_1"] <- ProblemStartsFlag_1
    df[i+1, "ProblemStarts_2"] <- ProblemStartsFlag_2
    
    # !!!test!!!
    if(is_test)
      cat("----------------------", "\n")
    # !!!test!!!
    
  }
  
  return(df)
}

###### Function to plot the time series together with the detected transition phases into problematic phases ####
plot_change_points <- function(drill, channel = "CH1_Moment",
                               is_test=FALSE, plot_pdg = FALSE,
                               plot_axes = TRUE, plot_ylab = TRUE,
                               ylabel = "Moment (in Nm)") {
  # Subset only the specified sensor data
  ts <- drill[,channel]
  
  # Call the custom change detection function
  ts_flags <- detect_changes(ts, is_test = is_test, plot_pdg = plot_pdg)
  
  # Plot the axes of the time series 
  if(plot_axes){
    plot(ts, xaxt = "n" ,center = FALSE, 
         main = paste0("Experiment: ", attributes(drill)[[7]]),
         ylab = ylabel, xlab = "Time (in sec)",
         cex.lab = 1.3, cex.axis=1.2, cex.main=1.3)
    axis(1, at=seq(from = 0, to = 6, by = 0.1), labels=seq(from = 45, to = 51, by = 0.1))
  }
  else {
    plot(ts, center = FALSE, xaxt = "n",
         main = paste0("Experiment: ", attributes(drill)[[7]]), 
         ylab = ylabel, xlab = "Time (in sec)")
  }
  
  # Plot the detected transition phases from normal to problematic phase
  abline(v=ts_flags[ts_flags$ProblemStarts_1 == 1,
                    "Window"]/20, col="red" , lwd = 1)  
  abline(v=ts_flags[ts_flags$ProblemStarts_2 == 1, 
                    "Window"]/20, col="yellow" , lwd = 0.2) 
  
  ChangePoints <-  ts_flags[(ts_flags$ProblemStarts_1 == 1
                             | ts_flags$ProblemStarts_2 == 1), ]
  
  return(ChangePoints)
}


par(mfrow=c(2,2))
#(V10) 1180 Hz <-> 240 Hz
plot_change_points(drill10[(45*20000):(51*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=TRUE) # Contact -> I
plot_change_points(drill10[(89*20000):(95*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=TRUE) # I -> II

#(V2) 720 Hz 
plot_change_points(drill2[(197*20000):(203*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=TRUE)


#(V6) 1180 Hz <-> 240 Hz
plot_change_points(drill6[(45*20000):(51*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE) # Contact -> I
plot_change_points(drill6[(132*20000):(138*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE) # I -> II

#(V17) 1180 Hz (33-39 sec) <-> 240 Hz (41-47 sec) Hz 
plot_change_points(drill17[(33*20000):(39*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE) # Contact -> I
plot_change_points(drill17[(41*20000):(47*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE) # I -> II


#(V20) 1180 Hz <-> 240 Hz
plot_change_points(drill20[(49*20000):(55*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE)# Contact -> I
plot_change_points(drill20[(94*20000):(100*20000),"CH1_Moment"],is_test = TRUE , plot_pdg=TRUE)# I -> II

#(V24) 1180 Hz (19-25 sec) <-> 240 Hz (61-67 sec)
plot_change_points(drill24[(19*20000):(25*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=TRUE)  # Contact -> I
plot_change_points(drill24[(61*20000):(67*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=TRUE) # I -> II

#(V25a) 1180 Hz (19-25 sec) <-> 700 Hz (61-67 sec)
plot_change_points(drill25a[(20*20000):(50*20000),"CH1_Moment"])
plot_change_points(drill25a[(0*20000):(250*20000),"CH1_Moment"])
plot_change_points(drill25a[(21*20000):(25*20000),"CH1_Moment"], is_test = TRUE)
plot_change_points(drill25a[(114*20000):(121*20000),"CH1_Moment"], is_test = TRUE)
plot_change_points(drill25a[(117*20000):(119*20000),"CH1_Moment"], is_test = TRUE, plot_pdg=TRUE)

#change detection for all experiments
plot_change_points(drill2[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill6[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill10[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill17[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill20[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill24[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drill25a[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drillD04[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drillD06[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)
plot_change_points(drillD08[,"CH1_Moment"], is_test = FALSE , plot_pdg=FALSE)


# Comparison of early detection based on different sensors
par(mfrow=c(2,1))
plot_change_points(drill10[(45*20000):(51*20000),"CH1_Moment"], is_test = TRUE , plot_pdg=FALSE, channel = "CH1_Moment")
plot_change_points(drill10[(45*20000):(51*20000),"CH2_Kraft"], ylabel = "Kraft (in N)" ,is_test = TRUE , plot_pdg=FALSE, channel = "CH2_Kraft")
plot_change_points(drill10[(45*20000):(51*20000),"CH3_SyncSig"], ylabel = "yncSig (in V)", is_test = TRUE , plot_pdg=FALSE, channel = "CH3_SyncSig")
plot_change_points(drill10[(45*20000):(51*20000),"CH4_akustik"], ylabel = "akustik (in Pa)", is_test = TRUE , plot_pdg=FALSE, channel = "CH4_akustik")
plot_change_points(drill10[(45*20000):(51*20000),"CH5_a1_WSAS"], ylabel = expression(WSAS (  ~ m/s^2 )), is_test = TRUE , plot_pdg=FALSE, channel = "CH5_a1_WSAS")
plot_change_points(drill10[(45*20000):(51*20000),"CH6_a2_WSAF"], ylabel = expression(WSAF (  ~ m/s^2 )), is_test = TRUE , plot_pdg=FALSE, channel = "CH6_a2_WSAF")
plot_change_points(drill10[(45*20000):(51*20000),"CH7_a3_BOZA"], ylabel = expression(BOZA (  ~ m/s^2 )), is_test = TRUE , plot_pdg=FALSE, channel = "CH7_a3_BOZA")

