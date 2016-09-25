

########################################################################
# Database functions

UpdatePhotoDF <- function(rootDir, subDir,
                          photoDFfile = "database/photoDF.csv") {
  if (!grepl("/$", rootDir)) rootDir <- paste(rootDir, "/", sep = "")
  if (!grepl("/$", subDir)) subDir <- paste(subDir, "/", sep = "")
  photoFiles <- list.files(paste(rootDir, subDir, sep = ""))
  addPhotoDF <- data.frame(filePath = paste(subDir, photoFiles,
                                            sep = ""),
                           date = "",
                           tags = "",
                           zhTitle = "",
                           enTitle = "",
                           zhDesc = "",
                           enDesc = "",
                           favorite = 0,
                           hide = 0)
  write.table(addPhotoDF, file = photoDFfile, append = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE, quote = FALSE,
              fileEncoding = "utf-8")
  return(invisible(addPhotoDF))
}

# Update the file of albumDF (usually albumDF.csv) if there is any new
# tag or updated date.
# Return updated tag (if asked for), or character(0) if no update.
# Side effect: write albumDFfile (albumDF.csv)
UpdateAlbumDatabase <- function(rowIds,
                                photoDFfile = "database/photoDF.csv",
                                albumDFfile = "database/albumDF.csv") {
  # if albumDFfile exists, update it; if not, create one.
  # albumDFfile:  tag, date, type, zhTitle, enTitle, zhDesc, enDesc

  # load photoDF if not loaded
  if (!exists("photoDF", mode = "list")) {
    photoDF <- read.csv(photoDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(photoDF)
  }

  albumDFnew <- ScanPhotoTags(rowIds, photoDFfile = photoDFfile)

  # if dbFile exists, load it
  if (file.exists(albumDFfile)) {
    albumDF <- read.csv(albumDFfile, stringsAsFactors = FALSE)
  } else {
    albumDF <- data.frame(tag = character(0),
                          date = character(0),
                          type = character(0),
                          zhTitle = character(0),
                          enTitle = character(0),
                          zhDesc = character(0),
                          enDesc = character(0),
                          stringsAsFactors = FALSE)
  }

  colnames(albumDFnew) <- c("tag", "dateNew")
  albumDF <- merge(albumDF, albumDFnew, by = "tag", all = TRUE)

  updateRows <- which(!is.na(albumDF$dateNew) &
                      (albumDF$dateNew > albumDF$date |
                      albumDF$date == "" |
                      is.na(albumDF$date)))
  albumDF$date[updateRows] <- albumDF$dateNew[updateRows]

  updateTags <- albumDF$tag[updateRows]
  if (length(updateTags) > 0) {
    write.csv(albumDF[, - which(colnames(albumDF)=="dateNew")],
              file = albumDFfile, row.names = FALSE)
    cat(paste("Updated following tags of photos: ",
              paste(updateTags, collapse = ", "), "\n"))
  } else {
    cat("There is no update of albumDF.\n")
  }

  return(invisible(updateTags))

}


# Scan for tags for given rows by "rowIds", return tagsDF:tag,date.
ScanPhotoTags <- function(rowIds,
                          photoDFfile = "database/photoDF.csv") {
  # scan all tags in photoDF of given rowIds.
  # if photoDF doesn't exist, read it from dbFile
  if (!exists("photoDF", mode = "list")) {
    photoDF <- read.csv(photoDFfile, stringsAsFactors = FALSE)
  }
  # if rowIds not given, scan all the photos
  if (missing(rowIds)) {
    rowIds <- 1:nrow(photoDF)
  }
  allTags <- unlist(strsplit(photoDF$tags[rowIds], ","))
  allTags <- trimws(allTags)    # R >= 3.2 required
  allTags <- unique(allTags)
  tagsDF <- data.frame(tag = allTags)
  tagsDF$date <- apply(tagsDF, 1, FUN = function(x) {
      return(max(photoDF$date[grepl(x, photoDF$tags[rowIds])]))
    })
  return(tagsDF)
}


########################################################################
# Build index pages database
# name of df: photoDF

BuildAllAlbumPages <- function(lang,
                               indexFileName = "albums_index.html") {
  for (i in which(!is.na(albumDF$date))) {
    BuildAlbumPage(tags = albumDF$tag[i],
                   lang = lang,
                   fileName = albumDF$tag[i],
                   update = albumDF[i, "date"],
                   title = albumDF[i, paste(lang, "Title", sep = "")],
                   desc = albumDF[i, paste(lang, "Desc", sep = "")])
  }

  BuildAlbumPage(lang = lang,
                 fileName = "all",
                 title = "All Photo",
                 update = max(photoDF$date),
                 desc = "All of them")

}
UpdateAlbumPages <- function() {
}

BuildAlbumPage <- function(tags, lang, dates, fileName,
                           title, desc, update, iconSize = "small",
                           favorite = FALSE, hide = TRUE,
                           maxPhoto = 50) {
  albumRows <- rep(TRUE, nrow(photoDF))
  if (!missing(dates)) {
    albumRows <- albumRows & photoDF$date %in% dates 
  }
  if (!missing(tags)) {
    tagsList <- strsplit(photoDF$tags, ",")
    albumRows <- albumRows & unlist(lapply(tagsList,
                                           FUN = function(x) {
                                             return(all(tags %in% x))
                                           }))
  }
  if (favorite) {
    albumRows <- albumRows & photoDF$favorite
  }
  if (hide) {
    albumRows <- albumRows & !photoDF$hide
  }
  albumRows <- which(albumRows)

  # destination file
  destFile <- paste(lang, "/photos/", fileName,
                    ifelse(grepl("\\.html$", fileName),
                           "", ".html"),
                    sep = "")

  # header
  headerHTML <- "<header>"
  if (!missing(title)) {
    headerHTML <- c(headerHTML,
                    paste("  <h2>", title, "</h2>", sep = ""))
  }
  if (!missing(update)) {
    headerHTML <- c(headerHTML,
                    paste("  <p>", update, "</p>", sep = ""))
  }
  if (!missing(desc)) {
    headerHTML <- c(headerHTML,
                    paste("  <p>", desc, "</p>", sep = ""))
  }
  headerHTML <- c(headerHTML, "</header>")

  # page divider
  totalPages <- length(albumRows) %/% maxPhoto + 1
  destFileList <- paste(gsub("\\.html$", "", destFile),
                        "_page", ifelse(1:totalPages<10, "0", ""),
                        1:totalPages, ".html",
                        sep = "")
  pagesHTML <- paste("<a href=\"/", destFileList, "\">[",
                     1:totalPages, "]</a>",
                     sep = "")
  pagesHTML <- c("Pages:", pagesHTML)

  for (p in 1:totalPages) {
    # generate an album page by copying the template
    if (lang == "zh") {
      file.copy("zh/photos/album_template.html", destFileList[p],
                overwrite = TRUE)
    } else if (lang == "en") {
      file.copy("en/photos/album_template.html", destFileList[p],
                overwrite = TRUE)
    }

    # write header
    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = headerHTML,
               tag = "header",
               whichone = 2)

    # write page index
    tempPagesHTML <- pagesHTML
    tempPagesHTML[p+1] <- paste("[", p, "]", sep = "")
    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = c("<section>",
                            tempPagesHTML,
                            "</section>"),
               tag = "section",
               whichone = 1)

    # write album table
    tempAlbumRows <- albumRows[1:maxPhoto + (p-1)*maxPhoto]
    tempAlbumRows <- tempAlbumRows[!is.na(tempAlbumRows)]

    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = BuildAlbumTable(tempAlbumRows,
                                          lang = lang),
               tag = "table",
               whichone = 1)
  }
  return(invisible(destFileList))
}



BuildAlbumTable <- function(rowIds, lang, iconSize = "small") {
  # Build an HTML table of an album (part of the photo data frame)
  tableHTML <- "<table>"
  for (i in rowIds) {
    tags <- unlist(strsplit(photoDF$tags[i], ","))
    tags <- trimws(tags)    # R >= 3.2 required
    tagsDF <- albumDF[which(albumDF$tag %in% tags), ]

    date <- photoDF$date[i]
    title <- photoDF[i, paste(lang, "Title", sep = "")]
    desc <- photoDF[i, paste(lang, "Desc", sep = "")]

    location <- tagsDF[which(tagsDF$type == "location"),
                       paste(lang, "Title", sep = "")]
    location <- paste(location, collapse = ", ")
    theme <- tagsDF[which(tagsDF$type == "theme"),
                    paste(lang, "Title", sep = "")]
    tagsTitle <- tagsDF[which(tagsDF$type == "tag"),
                        paste(lang, "Title", sep = "")]
    tableHTML <- c(tableHTML,
                   "  <tr>",
                   "    <td>",
                   paste("      <a href=\"",
                         PhotoLink(photoDF$filePath[i], "original"),
                         "\">",
                         sep = ""),
                   paste("      <img src=\"",
                         PhotoLink(photoDF$filePath[i], iconSize),
                         "\" />",
                         "      </a>",
                         sep = ""),
                   "    </td>",
                   "    <td>",
                   paste("      <h4>", title, "</h4>"),
                   "      <ul>",
                   paste("        <li>", date, "</li>", sep = ""),
                   paste("        <li>", location, "</li>", sep = ""),
                   paste("        <li>", desc, "</li>", sep = ""),
                   "      </ul>",
                   "    </td>",
                   "  </tr>")
  }
  tableHTML <- c(tableHTML, "</table>")
  return(tableHTML)
}
########################################################################
PhotoLink <- function(filePath, filter,
                      host = "http://images.guansong.wang/") {
  # Generate link to a photo
  # "filter" is the photo manipulate keyword of aliyun OSS.
  # Now it has "small" for icon, "original" for large.
  if (filter == "") {
    filterSep <- ""
  } else {
    filterSep <- "@!"
  }
  return(paste(host, filePath, filterSep, filter, sep = ""))
}


TestHTML <- function(htmlPart, lang="zh", filePath="test.html") {
  HTML <- c("<!DOCTYPE HTML>",
            "<html lang=\"zh-cmn-Hans\">",
            "<head>",
            "  <meta charset=\"UTF-8\">",
            "  <title>All Photos</title>",
            "</head>",
            "<body>",
            htmlPart,
            "</body>",
            "</html>")
  writeLines(HTML, filePath)

}


