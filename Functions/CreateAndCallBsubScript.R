##############################################
#
# General description:
#
#   The following function creates and calls a qsub script

# Input:
#
#     file: the path to save the script file to
#     SlurmHeaderLines: header lines of pbs script
#     SlurmCommandLines: command lines of pbs script
#     scriptName: name to be used to identify run
#     Args: external input arguments

# Output:
#   


# Comments:
#    

##############################################


CreateAndCallSlurmScript <- function(file,
   BsubHeaderLines = c('#BSUB -J NoName', 
                       '#BSUB -n 1', 
                       '#BSUB -q 6-hours', 
                       '#BSUB -oo NoName.o%J', 
                       '#BSUB -eo NoName.e%J', 
                       '#BSUB -m node10', 
                       '#BSUB -N', 
                       '#BSUB -u hb54@aub.edu.lb', 
                       '#BSUB -R "span[ptile=16]"'
                        
   ),
   CommandLines, 
   scriptName = 'NoName', 
   Args = "", blnIntern = TRUE,
   blnWait = F,
   blnStopIfNotSubmitted = F){
  
  # Replace name, time and memory in header lines
  BsubHeaderLines[grep("#BSUB -J", SlurmHeaderLines)] <- 
    paste('#BSUB -J', scriptName)
  BsubHeaderLines[grep("#BSUB -oo", SlurmHeaderLines)] <- 
    paste('#BSUB -oo ', scriptName, 'o%J', sep = '')
  BsubHeaderLines[grep("#BSUB -eo", SlurmHeaderLines)] <- 
    paste('#BSUB -eo ', scriptName, 'e%J', sep = '')
  
  # Save script
  writeLines(c(BsubHeaderLines, CommandLines), file)
  
  # Run script
  RunCmd <- paste("bsub <", file, Args)
  RunMessage <- system(command = RunCmd, wait = blnWait, intern = blnIntern)
  
  if (length(grep("is submitted to queue", RunMessage)) == 1){
    RunID <- strsplit(RunMessage, " is submitted to queue")[[1]][1]
  } else {
    RunID <- NA
    if(blnStopIfNotSubmitted){
      stop("Job not properly launched")
    }
  }
  list(RunMessage = RunMessage, RunID = RunID)
}
