source("SiteFunctions.R")
########################################################################
# Build/Update front and album pages

UpdatePictureFrontPage <-function(lang) {
  pagePath <- paste(lang, "/pictures.html", sep = "")
  typeList <- c("theme", "location", "tag", "year")
  if (lang == "en") {
    typeWord <- c("Theme", "Location", "Tag", "Year")
  } else {
    typeWord <- c("专题", "地理位置", "标签", "年")
  }

  for (i in 1:length(typeList)) {
    sectionHTML <- c(paste("<section id = \"", typeList[i], "\">",
                           sep = ""),
                     paste("  <h2>", typeWord[i], "</h2>", sep = ""),
                     "  <table>")
    sectionTags <- albumDF$tag[albumDF$type == typeList[i]]

    for (j in which(albumDF$type == typeList[i])) {
      tagLink <- paste("/", lang, "/", "photos", "/",
                       albumDF$tag[j], "_page01.html",
                       sep = "")
      sectionHTML <- c(sectionHTML, "    <tr>")
      sectionHTML <- c(sectionHTML,
                       paste("      <td>",
                             paste("<a href=\"", tagLink, "\">",
                                   sep = ""),
                             ifelse(lang=="en",
                                    albumDF$enTitle[j],
                                    albumDF$zhTitle[j]),
                             "</a>",
                             "</td>",
                             sep = ""),
                       paste("      <td>",
                             albumDF$date[j],
                             "</td>",
                             sep = ""),
                       paste("      <td>",
                             ifelse(lang=="en",
                                    albumDF$enDesc[j],
                                    albumDF$zhDesc[j]),
                             "</td>",
                             sep = ""),
                       "    </tr>")

    }

    sectionHTML <- c(sectionHTML, "  </table>", "</section>")
    ReplaceTag(htmlfile = paste(lang, "/pictures.html", sep = ""),
               htmlpart = sectionHTML,
               tag = "section",
               id = typeList[i])
  }
}

UpdateAlbumPages <- function(lang, tags,
                             updateAll = FALSE,
                             updateFront = TRUE) {
  # Update album (given by tags) pages.
  # If tags not given, update the album database and build or update
  # for updated tags.

  currentTags <- unique(albumDF$tag)

  if (missing(tags)) {
    updateTags <- UpdateAlbumDatabase()
    albumDF <- read.csv(albumDFfile, stringsAsFactors = FALSE)
    tags <- updateTags
  } else {
    updateTags <- tags
  }

  for (i in which(!is.na(albumDF$date))) {
    if (albumDF$tag[i] %in% tags) {
      BuildAlbumPage(tags = albumDF$tag[i],
                     lang = lang,
                     fileName = albumDF$tag[i],
                     update = albumDF[i, "date"],
                     title = albumDF[i, paste(lang, "Title", sep = "")],
                     desc = albumDF[i, paste(lang, "Desc", sep = "")])
      cat("Built album page: tag = \"", albumDF$tag[i], "\" ",
          "lang = \"", lang, "\"\n", sep = "")
    }
  }

  if (updateAll) {
    BuildAlbumPage(lang = lang,
                   fileName = "all",
                   title = "All Photo",
                   update = max(photoDF$date),
                   desc = "All of them")
  }
  if (updateFront) {
    if (length(setdiff(updateTags, currentTags))>0) {
      UpdatePictureFrontPage(lang = lang)
    }
  }

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

  albumRows <- albumRows[AlbumOrder(photoDF[albumRows,])]

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

    # change title
    if (!missing(title)) {
      ReplaceTag(htmlfile = destFileList[p],
                 htmlpart = c("<title>",
                              paste(ifelse(lang=="zh",
                                           "相册：", "Album:"),
                                    title, "-",
                                    ifelse(lang=="zh",
                                           "王冠嵩", "WGS")),
                              "</title>"),
                 tag = "title",
                 whichone = 1)
    }


    # write header
    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = headerHTML,
               tag = "header",
               whichone = 2)

    # write top page index
    tempPagesHTML <- pagesHTML
    tempPagesHTML[p+1] <- paste("[", p, "]", sep = "")
    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = c("<section id=\"top_pages_index\">",
                            tempPagesHTML,
                            "</section>"),
               tag = "section",
               id = "top_pages_index")

    # write album table
    tempAlbumRows <- albumRows[1:maxPhoto + (p-1)*maxPhoto]
    tempAlbumRows <- tempAlbumRows[!is.na(tempAlbumRows)]

    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = BuildAlbumTable(tempAlbumRows,
                                          lang = lang),
               tag = "table",
               id = "album_table")

    # write bottom page index
    tempPagesHTML <- pagesHTML
    tempPagesHTML[p+1] <- paste("[", p, "]", sep = "")
    ReplaceTag(htmlfile = destFileList[p],
               htmlpart = c("<section id=\"bottom_pages_index\">",
                            tempPagesHTML,
                            "</section>"),
               tag = "section",
               id = "bottom_pages_index")

  }
  return(invisible(destFileList))
}



BuildAlbumTable <- function(rowIds, lang, iconSize = "small") {
  # Build an HTML table of an album (part of the photo data frame)
  # Return the <table> element

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
                   paste("      <img src=\"",
                         PhotoLink(photoDF$filePath[i], iconSize), "\"",
                         sep = ""),
                   "           style=\"cursor:pointer\"",
                   paste("           onclick=\"showImage('",
                         PhotoLink(photoDF$filePath[i], "original"),
                         "');\">", sep = ""),
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
# Database functions
# UpdatePhotoDatabase(rootDir, subDir, photoDFfile)
# UpdateAlbumDatabase <- function(rowIds, photoDFfile, albumDFfile)

UpdatePhotoDatabase <- function(rootDir, subDir,
                                photoDFfile = "database/photoDF.csv") {
  # Look for all photos inside 'rootDir/subDir', attach new photos on
  # the bottom of the database photoDF.
  # Currently only filePath is collected.
  # Args:
  #   rootDir   The root directory of my photo files. Tell the function
  #             where to look.
  #   subDir    The path to the actual photos, after the rootDir. The
  #             online storage has the same structure as local. So this
  #             is used for constructing links.
  # Ret:
  #   When asked for, the function returns the newly added section of
  #   photoDF.
  # Side effect: write photoDFfile.

  if (!grepl("/$", rootDir)) rootDir <- paste(rootDir, "/", sep = "")
  if (!grepl("/$", subDir)) subDir <- paste(subDir, "/", sep = "")
  photoFiles <- list.files(paste(rootDir, subDir, sep = ""))
  photoFiles <- photoFiles[ ! photoFiles %in% c("Private")]
  photoFiles <- photoFiles[ ! grepl("(mp4|mpg|avi)$", photoFiles,
                                    ignore.case = TRUE) ]

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
  if (require("exif")) {
    addPhotoDF$date <-
      tryCatch(read_exif(paste(rootDir,
                               addPhotoDF$filePath,
                               sep = ""))$origin_timestamp,
               error = function(e) {return(NA)})
    addPhotoDF$date <- strptime(addPhotoDF$date,
                                "%Y:%m:%d %H:%M:%S")
  }

  write.table(addPhotoDF, file = photoDFfile, append = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE, quote = FALSE,
              fileEncoding = "utf-8")
  assign("photoDF",
         read.csv(photoDFfile, stringsAsFactors = FALSE),
         envir = .GlobalEnv)
  cat("Data frame \"photoDF\" is loaded from database/photoDF.csv.\n")
  return(invisible(addPhotoDF))
}

UpdateAlbumDatabase <- function(rowIds,
                                photoDFfile = "database/photoDF.csv",
                                albumDFfile = "database/albumDF.csv") {
  # Update the file of albumDF (usually albumDF.csv) if there is any new
  # tag or updated date. Return updated tag (if asked for), or
  # character(0) if no update.
  # Side effect: write albumDFfile (albumDF.csv)
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

  cat("Data frame \"albumDF\" is loaded from database/albumDF.csv.\n")
  assign("albumDF", albumDF, envir = .GlobalEnv)
  return(invisible(updateTags))

}



########################################################################
# Helper functions
# PhotoLink(filePath, filter, host)
# ScanPhotoTags(rowIds, photoDFfile)

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

ScanPhotoTags <- function(rowIds,
                          photoDFfile = "database/photoDF.csv") {
  # Scan for tags for given rows by "rowIds", return tagsDF:tag,date.
  # Return a data frame: tag, date

  photoDF <- read.csv(photoDFfile, stringsAsFactors = FALSE)
  # if rowIds not given, scan all the photos
  if (missing(rowIds)) {
    rowIds <- 1:nrow(photoDF)
  }
  allTags <- unlist(strsplit(photoDF$tags[rowIds], ","))
  allTags <- trimws(allTags)    # R >= 3.2 required
  allTags <- unique(allTags)
  tagsDF <- data.frame(tag = allTags)
  tagsDF$date <- apply(tagsDF, 1, FUN = function(x) {
    tagregex <- paste("\\<", x, "\\>", sep = "")
    return(max(photoDF$date[grepl(tagregex, photoDF$tags[rowIds])]))
  })
  return(tagsDF)
}

AlbumOrder <- function(x) {
  # Sort by date in reverse order first, then by time
  onlyDate <- ! grepl(" ", x$date)
  xposixct <- as.POSIXct(x$date, tz = "CST")
  xdates <- as.numeric(as.Date(xposixct))
  xtime <- format(xposixct, "%H%M%S")
  xtime[onlyDate] <- NA
  return(order(-xdates, xtime, basename(x$filePath)))

}

