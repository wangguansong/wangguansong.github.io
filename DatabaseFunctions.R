# Scan blog pages

# Build album pages database

ScanAllTags <- function(rowIds, photoDFfile = "photoDF.csv") {
  # scan all tags in photoDF.
  # if photoDF doesn't exist, read it from dbFile
  if (!exists("photoDF", mode = "list")) {
    x <- read.csv(photoDFfile)
    photoDF <- read.csv(photoDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(photoDF)
  }
  allTags <- unlist(strsplit(photoDF$tags[rowIds], ","))
  ####################### delete spaces
  allTags <- unique(allTags)
  tagsDF <- data.frame(tag = allTags)
  tagsDF$date <- apply(tagsDF, 1, FUN = function(x) {
      return(max(photoDF$date[grepl(x, photoDF$tags[rowIds])]))
    })
  return(tagsDF)
}


UpdateAlbumDatabase <- function(rowIds,
                                photoDFfile = "photoDF.csv",
                                albumDFfile = "albumDF.csv") {
  # if albumDFfile exists, update it; if not, create one.
  # albumDFfile:  tag, date, zhTitle, enTitle, zhDesc, enDesc, type

  # load photoDF if not loaded
  if (!exists("photoDF", mode = "list")) {
    photoDF <- read.csv(photoDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(photoDF)
  }

  albumDFnew <- ScanAllTags(rowIds, photoDFfile = photoDFfile)

  # if dbFile exists, load it
  if (file.exists(albumDFfile)) {
    albumDF <- read.csv(albumDFfile, stringsAsFactors = FALSE)
  } else {
    albumDF <- data.frame(tag = character(0),
                          date = character(0),
                          zhTitle = character(0),
                          enTitle = character(0),
                          zhDesc = character(0),
                          enDesc = character(0),
                          type = character(0))
  }

  colnames(albumDFnew) <- c("tag", "dateNew")
  albumDF <- merge(albumDF, albumDFnew, by = "tag", all = TRUE)

  updateRows <- which(!is.na(albumDF$dateNew) &
                      (albumDF$dateNew > albumDF$date |
                      albumDF$date == "" |
                      is.na(albumDF$date)))
  albumDF$date[updateRows] <- albumDF$dateNew[updateRows]

  updateTag <- albumDF$tag[updateRows]
  if (length(updateTag) > 0) {
    write.csv(albumDF[, - which(colnames(albumDF)=="dateNew")],
              file = albumDFfile, row.names = FALSE)
    cat(paste("Updated following tags: ",
              paste(updateTag, collapse = ", "), "\n"))
  } else {
    cat("There is no update of albumDF.\n")
  }

  return(invisible(updateTag))

}


# Build index pages database
