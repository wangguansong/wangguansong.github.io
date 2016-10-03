########################################################################
ReplaceTag <- function(htmlfile, htmlpart, tag, whichone=1, id) {
  # Replace a tag (a big building block) in a html file
  # It cannot deal with a tree structure of the same tag.

  if (length(htmlfile)==1) {
    if (!file.exists(htmlfile)) {
      stop(paste("File \"", htmlfile , "\" doesn't exist.", sep=""))
    } else {
      filetext <- readLines(htmlfile, encoding="UTF-8")
    }
  } else {
    filetext <- htmlfile
  }

  if (length(htmlpart)==1 && nchar(htmlpart)>0){
    if (!file.exists(htmlpart)) {
      stop(paste("File \"", htmlpart, "\" doesn't exist.", sep=""))
    } else {
      parttext <- readLines(htmlpart, encoding="UTF-8")
    }
  } else {
    parttext <- htmlpart
  }

  if (missing(tag)) {
    tag <- gsub("<([^ ]+)[ >]", "\\1", parttext[1])
  }

  opentag <- paste("<", tag, "[^>]*>", sep="")
  startline <- grep(opentag, filetext)
  endtag <- paste("</", tag, "[^>]*>", sep="")
  endline <- grep(endtag, filetext)

  if (!missing(id)) {
    whichone <- grep(paste("<[^<]*id[ ]*=[ ]*",
                           "\"", id, "\"", "[^>]*>", sep = ""),
                     filetext[startline])
  }


  if (length(startline)>1) {
    startline <- startline[whichone]
    endline <- endline[whichone]
  }

  if (length(htmlfile)==1) {
    writeLines(c(filetext[1:(startline-1)],
                 parttext,
                 filetext[(endline+1):length(filetext)]), htmlfile)
    return()
  } else {
    return(c(filetext[1:(startline-1)], parttext,
             filetext[(endline+1):length(filetext)]))
  }
}

