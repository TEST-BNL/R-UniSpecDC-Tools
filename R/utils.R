#--------------------------------------------------------------------------------------------------#
# A set of helper functions
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' Read settings file for spectra import and processing
##' 
##' @name settings
##' @title parse settings file used for spectra file import and processing
##' @param input.file settings file containing information needed for spectra processing
##'
##' @examples
##' \dontrun{
##' settings <- settings()
##' settings <- settings('/home/$USER/settings.xml')
##' }
##'
##' @export
##'
##' @author Shawn P. Serbin
##'
settings <- function(input.file=NULL){
  settings.xml <- NULL
  
  ### Parse input settings file
  if (!is.null(input.file) && file.exists(input.file)) {
    settings.xml <- xmlParse(input.file)  
    # convert the xml to a list
    settings.list <- xmlToList(settings.xml)
    
  } else {
    print("***** WARNING: no settings file defined *****")
  }
  
  # make sure something was loaded
  if (is.null(settings.xml)) {
    #log.error("Did not find any settings file to load.")
    stop("Did not find any settings file to load.")
  }
  
  ### Remove comment or NULL fields
  settings.list <- settings.list[settings.list !="NULL" ]
  
  # Return settings file as a list
  #invisible(settings.list) # invisible option
  return(settings.list)
  
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name extract.metadata.SPU
##' @title Get header metadata from individula SPU files and put into a flat .csv file
##'
##' @param file.dir File directory or filename of single spectra for processing
##' @param out.dir Output directory for metadata information file 
##' @param header.lines Number of header lines in each .SPU file. Current default is 11
##' @param spec.file.ext [Optional] Input spectra file extension. E.g. .spu
##' @param output.file.ext [Optional] Output file extension of meta-data information file. 
##' Default .csv
##' @param tz [Optional] Set the timezone of the spectra file collection.  Used to covert spectra collection 
##' time to UTC.  If unused it is assumed that the correct timezone is the current system timezone.
##' 
##' @return output Returns output dataframe of SPU metadata information
##'
##'
extract.metadata.SPU <- function(file.dir=NULL,out.dir=NULL,spec.file.ext=NULL,header.lines=11,
                              output.file.ext=NULL,tz=NULL) {
  ### Set platform specific file path delimiter.  Probably will always be "/"
  dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
  
  # Check for custom spectra file extension
  if (is.null(spec.file.ext)) {
    spec.file.ext <- ".spu"
  } else {
    spec.file.ext <- spec.file.ext
  }
  #print(spec.file.ext)
  
  # Determine if running on single file or directory
  check <- file.info(file.dir)
  if (check$isdir) {
    spu.files.names <- list.files(path=file.dir,pattern=spec.file.ext,full.names=FALSE)
    spu.files.names <- unlist(strsplit(spu.files.names,spec.file.ext))
    spu.files <- list.files(path=file.dir,pattern=spec.file.ext,full.names=TRUE)
    out.file.name <- "Spectra"
    
  } else {
    spu.files <- file.dir
    out.file.name <- unlist(strsplit(file.dir,dlm))
    out.file.name <- out.file.name[length(out.file.name)]                
    out.file.name <- unlist(strsplit(out.file.name,spec.file.ext))
    spu.files.names <- unlist(strsplit(out.file.name,spec.file.ext))
  }
  #print(spu.files.names)
  
  
  
  # Build empty metadata dataframe
  output.metadata <- array(data=NA,dim=c(length(spu.files),2))
  
  # Run metadata extraction
#  for (i in 1:length(spu.files)){
  for (i in 1:length(spu.files)){
    file.head <- readLines(spu.files[i],n=11)
    
    
    file.head <- read.table(spu.files[i],nrows = 11,sep="\t",
               fill = FALSE, header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE)
    
    
    
    
    
    file.head <- scan(spu.files[i], skip = 1, quiet = TRUE)
    file.head <- read.table(spu.files[i],sep = "\t",)
    
    
    file.head <- read.delim2(spu.files[i],sep = "\t",quote = "\"t",nrows=11,
                             colClasses = "character")
    
    read.delim2(file, header = TRUE, sep = "\t", quote = "\"",
                dec = ",", fill = TRUE, comment.char = "", ...)
    
    output.metadata[1,i] <- gsub(" ","",(strsplit(file.head[2],"Time:")[[1]])[2])
                                   
                                   
                                   
    output.metadata[1,i] <- format(output.metadata[1,i],"%YY-%mm-%dd")
    
    
    
    
    
    
  }
  print(file.head)

  # Create output dataframe header
  #out.head <- data.frame(variable = c("Remarks"), values = c())
  
  return(output.metadata)
  
} ### End of function
#==================================================================================================#

## EOF