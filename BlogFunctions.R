########################################################################
# Procedure for adding a new blog:
# - NewBlogPage(path)
# - source("ScanPages.R")
# - UpdateSidebars()
# - UpdateBlogFrontPage()


########################################################################
# Building pages

UpdateBlogFrontPage <- function(lang) {

}

UpdateTagPages <- function(lang, tags,
                           updateAll = TRUE,
                           updateFront = TRUE) {
}


BuildTagPage <- function(tags, lang, dates, fileName,
                         type, title, desc) {
  # Build a collection page of blogs, selected by tags, lang, dates.

  if (! all(tags %in% tagDF$tag)) {
    stop("UpdateTagPage: one or more tags not found in database.")
  }

  tagRows <- rep(TRUE, nrow(blogDF))
  #tagRows <- tagRows & blogDF$type == "blog"
  tagRows <- tagRows & blogDF$lang == lang
  if (!missing(dates)) {
    tagRows <- tagRows & blogDF$date %in% dates
  }
  if (!missing(tags)) {
    tagsList <- strsplit(blogDF$tags, ",")
    tagRows <- tagRows & unlist(lapply(tagsList,
                                       FUN = function(x) {
                                         return(all(tags %in% x))
                                       }))
  }
  tagRows <- which(tagRows)
  tagRows <- tagRows[order(blogDF$date[tagRows], decreasing = TRUE)]

  blognum <- length(tagRows)
  linkLine <- paste("      <a href=\"/",
                    blogDF$filePath[tagRows], "\">",
                    blogDF$title[tagRows], "</a>", sep="")
  dateLine <- paste("      <span class=\"datestamp\">",
                    blogDF$date[tagRows], "</span>",
                    sep="")
  descLine <-paste("      ",
                   blogDF$desc[tagRows],
                   sep="")

  sectionhtml <- character(length=blognum*9)
  sectionhtml[(1:blognum-1)*9 + 1] <- "  <section>"
  sectionhtml[(1:blognum-1)*9 + 2] <- "    <h3>"
  sectionhtml[(1:blognum-1)*9 + 3] <- linkLine
  sectionhtml[(1:blognum-1)*9 + 4] <- dateLine
  sectionhtml[(1:blognum-1)*9 + 5] <- "    </h3>"
  sectionhtml[(1:blognum-1)*9 + 6] <- "    <p class=\"summary\">"
  sectionhtml[(1:blognum-1)*9 + 7] <- descLine
  sectionhtml[(1:blognum-1)*9 + 8] <- "    </p>"
  sectionhtml[(1:blognum-1)*9 + 9] <- "  </section>"

  articlehtml <-
    c("<article>",
      "  <header>",
      paste("    <h2>", title, "</h2>", sep=""),
      "    <p class=\"summary\">",
      paste("      ", desc, sep=""),
      "    </p>",
      "  </header>",
      sectionhtml,
      "</article>")

  fileName <- paste(lang, "/", type, "s/", fileName,
                    ".html", sep = "")
  file.copy(paste(lang, "/", type, "s/", type,
                  "_template.html", sep = ""),
            fileName,
            overwrite = TRUE)
  # copy template
  ReplaceTag(fileName,
             c("<title>", title, "</title>"),
             "title", 1)
  ReplaceTag(fileName, articlehtml, "article", 1)
  return(invisible(articlehtml))

}

UpdateBlogFrontPage <- function(lang,
                                blogDFfile = "database/blogDF.csv") {
  # Update the blog front page in zh/en

  if (!exists("blogDF", mode = "list")) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  }

  rowIds <- which(blogDF$type == "blog" & blogDF$lang == lang)
  rowIds <- rowIds[order(blogDF$date[rowIds], decreasing = TRUE)]
  blognum <- length(rowIds)
  linkLine <- paste("      <a href=\"/",
                    blogDF$filePath[rowIds], "\">",
                    blogDF$title[rowIds], "</a>", sep="")
  dateLine <- paste("      <span class=\"datestamp\">",
                    blogDF$date[rowIds], "</span>",
                    sep="")
  descLine <-paste("      ",
                   blogDF$desc[rowIds],
                   sep="")

  sectionhtml <- character(length=blognum*9)
  sectionhtml[(1:blognum-1)*9 + 1] <- "  <section>"
  sectionhtml[(1:blognum-1)*9 + 2] <- "    <h3>"
  sectionhtml[(1:blognum-1)*9 + 3] <- linkLine
  sectionhtml[(1:blognum-1)*9 + 4] <- dateLine
  sectionhtml[(1:blognum-1)*9 + 5] <- "    </h3>"
  sectionhtml[(1:blognum-1)*9 + 6] <- "    <p class=\"summary\">"
  sectionhtml[(1:blognum-1)*9 + 7] <- descLine
  sectionhtml[(1:blognum-1)*9 + 8] <- "    </p>"
  sectionhtml[(1:blognum-1)*9 + 9] <- "  </section>"

  articlehtml <-
    c("<article>",
      sectionhtml,
      "</article>")
  ReplaceTag(paste(lang, "blog.html", sep="/"),
             articlehtml, "article", 1)
  return(invisible(articlehtml))

}

UpdateSidebars <- function(lang,tags, blogFront = TRUE,
                           tagDFfile = "database/tagDF.csv") {
  # Update the sidebars of tag pages and blog front page

  if (!exists("tagDF", mode = "list")) {
    tagDF <- read.csv(tagDFfile, stringsAsFactors = FALSE)
  }
  if (missing(tags)) {
    tags <- tagDF$tag[tagDF$lang == lang]
  }

  rowIds <- which(tagDF$type == "tag" & tagDF$lang == lang)
  tagsidebar <-
    c("<aside>",
      paste("  <h3>", ifelse(lang=="en", "Tag: ", "标签："), "</h3>",
            sep=""),
      "  <ul>",
      paste("    <li><a href=\"/", lang, "/tags/",
            tagDF$name[rowIds], ".html\">",
            tagDF$tag[rowIds], "</a></li>", sep=""),
      "  </ul>", "</aside>")

  rowIds <- which(tagDF$type == "project" & tagDF$lang == lang)
  projectsidebar <-
    c("<aside>",
      paste("  <h3>", ifelse(lang=="en", "Project: ", "项目："),
            "</h3>", sep=""),
      "  <ul>",
      paste("    <li><a href=\"/", lang, "/projects/",
            tagDF$name[rowIds], ".html\">",
            tagDF$tag[rowIds], "</a></li>", sep=""),
      "  </ul>", "</aside>")

  for (tag in tags) {
    tagRowId <- which(tagDF$tag == tag & tagDF$lang == lang)
    fileName <- paste(lang, "/", tagDF$type[tagRowId], "s/",
                      tagDF$name[tagRowId], ".html", sep = "")
    if (tagDF$type[tagRowId] == "tag") {
      ReplaceTag(fileName,
                 ToggleActive(tagsidebar,
                              paste("/", fileName, sep="")),
                 "aside", 1)
    } else if (tagDF$type[tagDF$tag==tag] == "project") {
      ReplaceTag(fileName,
                 ToggleActive(projectsidebar,
                              paste("/", fileName, sep="")),
                 "aside", 1)
    }
  }

  if (blogFront) {
    ReplaceTag(paste(lang, "blog.html", sep="/"),
               projectsidebar, "aside", 1)
    ReplaceTag(paste(lang, "blog.html", sep="/"),
               tagsidebar, "aside", 2)
  }
  
}


########################################################################
# Database functions

BuildBlogDatabase <- function(blogDFfile = "database/blogDF.csv") {
  # Scan and parse blog html files, build a data frame of blogs.
  blogFileList <- character()
  for (lang in c("en", "zh")) {
    blogFileList <-  c(blogFileList,
                       list.files(paste(lang, "blogs", sep="/"),
                                  recursive=TRUE, full.names=TRUE,
                                  pattern="*.html$"))
  }
  blogFileList <- blogFileList[!grepl("~$", blogFileList)]
  blogFileList <- blogFileList[!grepl("yyyymmdd.*template", blogFileList)]

  character(length(blogFileList)) -> langList -> titleList ->
    dateList -> tagsList -> summaryList
  langList[grep("zh/", blogFileList)] <- "zh"
  langList[grep("en/", blogFileList)] <- "en"

  for (i in 1:length(blogFileList)) {
    basicinfo <- ParseBlogHTML(blogFileList[i])
    titleList[i] <- basicinfo$title
    tagsList[i] <- paste(basicinfo$tags, collapse=",")
    dateList[i] <- basicinfo$datestamp
    summaryList[i] <- basicinfo$summary
  }
  blogDF <- data.frame(filePath = blogFileList,
                       lang = as.factor(langList),
                       title = titleList,
                       tags = tagsList,
                       date = as.Date(dateList, format="%Y-%m-%d"),
                       desc = summaryList,
                       hide = 0,
                       stringsAsFactors=FALSE)

  write.csv(blogDF, file = blogDFfile, row.names = FALSE)

  assign("blogDF", blogDF, envir = .GlobalEnv)
  cat("Data frame \"blogDF\" is loaded from database/blogDF.csv.\n")

  return(invisible(blogDF))
}

UpdateTagDatabase <- function(rowIds,
                              blogDFfile = "database/blogDF.csv",
                              tagDFfile = "database/tagDF.csv") {
  # Build or update the tag database from blog database.
  # If asked, return updated tags.
  if (!exists("blogDF", mode = "list")) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(blogDF)
  }

  enRowIds <- rowIds[blogDF$lang[rowIds]=="en"]
  zhRowIds <- rowIds[blogDF$lang[rowIds]=="zh"]
  enAllTags <- unlist(strsplit(blogDF$tags[enRowIds], ","))
  enAllTags <- trimws(enAllTags)    # R >= 3.2 required
  enAllTags <- unique(enAllTags)
  zhAllTags <- unlist(strsplit(blogDF$tags[zhRowIds], ","))
  zhAllTags <- trimws(zhAllTags)    # R >= 3.2 required
  zhAllTags <- unique(zhAllTags)

  tagDFNew <- data.frame(tag = c(enAllTags, zhAllTags),
                         lang = c(rep("en", length(enAllTags)),
                                   rep("zh", length(zhAllTags))),
                         stringsAsFactors = FALSE)
  tagDFNew$dateNew <- apply(tagDFNew, 1, FUN = function(x) {
      tagRows <- which(grepl(x[1], blogDF$tags[rowIds]) &
                       blogDF$lang[rowIds] == x[2])
      return(max(blogDF$date[rowIds][tagRows], na.rm=TRUE))
    })


  if (file.exists(tagDFfile)) {
    tagDF <- read.csv(tagDFfile, stringsAsFactors = FALSE)
  } else {
    tagDF <- data.frame(tag = character(0),
                        lang = character(0),
                        date = character(0),
                        type = character(0),
                        title = character(0),
                        desc = character(0),
                        name = character(0),
                        stringsAsFactors = FALSE)
  }

  tagDF <- merge(tagDF, tagDFNew, by = c("tag", "lang"), all = TRUE)
  print(tagDF)

  updateRows <- which(!is.na(tagDF$dateNew) &
                      (tagDF$dateNew > tagDF$date |
                       tagDF$date == "" |
                       is.na(tagDF$date) |
                       tagDF$lang == "" |
                       is.na(tagDF$lang)))
  tagDF$date[updateRows] <- tagDF$dateNew[updateRows]
  tagDF <- tagDF[, - which(colnames(tagDF) %in%
                           c("langNew", "dateNew"))]

  updateTags <- tagDF$tag[updateRows]
  if (length(updateTags) > 0) {
    write.csv(tagDF, file = tagDFfile, row.names = FALSE)
    cat(paste("Updated following tags of blogs: ",
              paste(updateTags, collapse = ", "), "\n"))
  } else {
    cat("There is no update of tagDF.\n")
  }

  cat("Data frame \"tagDF\" is loaded from database/tagDF.csv.\n")
  assign("tagDF", tagDF, envir = .GlobalEnv)
  return(invisible(updateTags))
}



########################################################################
# Helper functions.
# ScanBlogTags(rowIds, blogDFfile)
# ParseBlogHTML(path)
# ToggleActive(htmlfile, link)

ScanBlogTags <- function(rowIds, blogDFfile = "database/blogDF.csv") {
  if (!exists("blogDF", mode = "list")) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(blogDF)
  }
  enRowIds <- rowIds[blogDF$lang[rowIds]=="en"]
  zhRowIds <- rowIds[blogDF$lang[rowIds]=="zh"]
  enAllTags <- unlist(strsplit(blogDF$tags[enRowIds], ","))
  enAllTags <- trimws(enAllTags)    # R >= 3.2 required
  enAllTags <- unique(enAllTags)
  zhAllTags <- unlist(strsplit(blogDF$tags[zhRowIds], ","))
  zhAllTags <- trimws(zhAllTags)    # R >= 3.2 required
  zhAllTags <- unique(zhAllTags)

  tagsDF <- data.frame(tag = c(enAllTags, zhAllTags),
                       lang = c(rep("en", length(enAllTags)),
                                rep("zh", length(zhAllTags))),
                       stringsAsFactors = FALSE)
  tagsDF$date <- apply(tagsDF, 1, FUN = function(x) {
      tagRows <- which(grepl(x[1], blogDF$tags[rowIds]) &
                       blogDF$lang[rowIds] == x[2])
      return(max(as.character(blogDF$date)[rowIds][tagRows], na.rm=TRUE))
    })
  return(tagsDF)

}


ParseBlogHTML <- function(path) {
  # Given a path to a blog page, return tags, (url of tags), date stamp,
  # title and summary line.

  rawhtml <- readLines(con=path, encoding="UTF-8")
  articleopen <- grep("<article id=\"content\">", rawhtml)
  headeropen <- grep("<header>", rawhtml[-(1:articleopen)])
  headerclose <- grep("</header>", rawhtml[-(1:articleopen)])[1]
  headerblock <- rawhtml[headeropen:headerclose + articleopen]

  tagsopen <- grep("<span class=\"tags\">", headerblock)
  tagsclose <- grep("</span>", headerblock[-(1:tagsopen)])[1]
  tagsblock <- headerblock[tagsopen + 0:tagsclose]
  tagline <- grep("<a href=", tagsblock)
  tags <- gsub(".*<a href=[^>]*>(.*)</a>.*", "\\1", tagsblock[tagline])
  #tagsurl <- gsub(".*<a href=\"/([^>]*)\">[^<].*</a>.*",
  #                "\\1", tagsblock[tagline])

  dateline <- grep("<span class=\"datestamp\">", headerblock)[1]
  datestamp <- gsub(".*<span class=\"datestamp\">(.*)</span>.*",
                    "\\1", headerblock[dateline])

  summaryopen <- grep("<p class=\"summary\">", headerblock)
  summaryclose <- grep("</p>", headerblock[-(1:summaryopen)])[1]
  summary <- headerblock[summaryopen + 1:(summaryclose-1)]
  summary <- gsub("^[ ]*(.*)[ ]*$", "\\1", summary)

  titleline <- grep("<h2>(.*)</h2>", headerblock)[1]
  title <- gsub(".*<h2>(.*)</h2>.*", "\\1", headerblock[titleline])

  return(list(tags=tags, datestamp=datestamp, #tagsurl=tagsurl,
              title=title, summary=summary))
}

ToggleActive <- function(htmlfile, link) {
  # Toggle the class to "active" in a navigation list.
  # Search by link.

  if (length(htmlfile)==1) {
    if (!file.exists(htmlfile)) {
      stop(paste("File \"", htmlfile, "\" doesn't exist.", sep=""))
    }
    filetext <- readLines(htmlfile, encoding="UTF-8")
    if (missing(link)) {
      link <- basename(htmlfile)
    }
  } else {
    filetext <- htmlfile
  }

  linkline <- grep(paste("href=\"", link, "\"", sep=""), filetext)
  newline <- gsub("<li>", "<li class=\"active\">", filetext[linkline])
  if (length(htmlfile)==1) {
    writeLines(c(filetext[1:(linkline-1)], newline,
                 filetext[(linkline+1):length(filetext)]),
               htmlfile)
    return()
  } else {
    return(c(filetext[1:(linkline-1)], newline,
             filetext[(linkline+1):length(filetext)]))
  }
}
########################################################################
# obsolete functions

NewTagPage <- function(tag, filename="", lang="", type="", summ="",
                       title="") {
  # Create a new tag page, by the given information, or user input.
  # Depend on: ReplaceTag(), and tag_template.html

  if (! filename=="") {
    filename <- strsplit(filename, "/")[[1]]
    lang <- filename[1]
    type <- filename[2]
    filename <- filename[3]
  }
  cat(rep("-", 30), "\n")
  cat("Creating HTML page for tag: ", tag, "\n", sep="")
  while (type=="") {
    type <- readline("Enter type, either tag, t, project or p: ")
    type <- ifelse(type %in% c("tag", "project", "t", "p"), type, "")
  }
  if (type=="t") type <- "tag"
  if (type=="p") type <- "project"
  while (lang=="") {
    lang <- readline("Enter language, either en, e, zh or z: ")
    lang <- ifelse(lang %in% c("en", "zh", "e", "z"), lang, "")
  }
  if (lang=="e") lang <- "en"
  if (lang=="z") lang <- "zh"
  while (filename=="") {
    filename <- readline("Enter the file name: tag/project_")
  }
  if (! grepl(".html$", filename)) {
    filename <- paste(filename, ".html", sep="")
  }
  while (title=="") {
    title <- readline("Enter the title of the tag: ")
  }
  while (summ=="") {
    summ <- readline("Enter one line of HTML as summary: ")
  }

  path <- paste(lang, type, filename, sep="/")
  templatepath <- list.files(dirname(path), pattern="template.*html$",
                             full.names=TRUE)
  file.copy(templatepath, path)
  articlehtml <-
    c("<article>",
      "  <header>",
      paste("    <h2>", title, "</h2>", sep=""),
      "    <p class=\"summary\">",
      paste("      ", summ, sep=""),
      "    </p>",
      "  </header>",
      "</article>")
  asidehtml <-
    c("<aside>",
      paste("  <h3>", ifelse(lang=="en", "Tags:", "标签："), "</h3>",
            sep=""),
      "  <ul>",
      paste("    <li class=\"active\">", "<a href=\"/", path, "\">",
            tag, "</a></li>", sep=""),
      "  </ul>",
      "</aside>")
  ReplaceTag(path, articlehtml, "article", 1)
  ReplaceTag(path, asidehtml, "aside", 1)
  return(invisible(path))
}

NewBlogPage <- function(path) {
  # Given the path to a new blog page, that's not in htmldf:
  #   - scan tags, title, date, summary
  #   - if there are tags not in htmldf

  if (! file.exists(path)) {
    stop("NewBlogPage: file does not exist.")
  }
  if (! exists("htmldf", mode="list")) load("SiteMeta.RData")
  if (sum(htmldf$PATH==path)>0) {
    stop("NewBlogPage: file already in database.")
  }
  basicinfo <- ParseBlogHTML(path)
  lang <- substr(path, 1, 2)
  tags <- basicinfo$tags
  newtags <- setdiff(tags,
                     htmldf$TAGS[htmldf$TYPE %in% c("tag", "project") &
                                 htmldf$LANG==lang])
  if (length(newtags)>0) {
    for (i in 1:length(newtags)) {
      NewTagPage(newtags[i],
                 filename=basicinfo$tagsurl[which(tags==newtags[i])])
    }
  }
  sectionhtml <-
    c("  <section>",
      "    <h3>",
      paste("      <a href=\"/", path, "\">", basicinfo$title, "</a>",
            sep=""),
      paste("      <span class=\"datestamp\">", basicinfo$datestamp,
            "</span>", sep=""),
      "    </h3>",
      "    <p class=\"summary\">",
      paste("      ", basicinfo$summary, sep=""),
      "    </p>",
      "  </section>")
  for (i in 1:length(tags)) {
    tagshtml <- readLines(basicinfo$tagsurl[i])
    articleopen <- grep("<article[^>]*>", tagshtml)
    headerclose <- grep("</header>", tagshtml[-(1:articleopen)])[1] +
                   articleopen
    tagshtml <- c(tagshtml[1:headerclose],
                  sectionhtml,
                  tagshtml[(headerclose+1):length(tagshtml)])
    writeLines(tagshtml, basicinfo$tagsurl[i])
  }
  return(invisible(path))
}


ParseTagHTML <- function(path) {
  # Given a path to a tag page, return the tag, title and summary line.

  rawhtml <- readLines(con=path, encoding="UTF-8")
  articleopen <- grep("<article>", rawhtml)
  headeropen <- grep("<header>", rawhtml[-(1:articleopen)])
  headerclose <- grep("</header>", rawhtml[-(1:articleopen)])[1]
  headerblock <- rawhtml[headeropen:headerclose + articleopen]
  titleline <- grep("<h2>(.*)</h2>", headerblock)[1]
  title <- gsub(".*<h2>(.*)</h2>.*", "\\1", headerblock[titleline])

  tagline <- grep("<li class=\"active\"[^>]*>", rawhtml)
  tag <- gsub(".*<li[^>]*><a[^>]*>(.*)</a></li>.*", "\\1",
              rawhtml[tagline])

  summaryopen <- grep("<p class=\"summary\">", headerblock)
  summaryclose <- grep("</p>", headerblock[-(1:summaryopen)])[1]
  summary <- headerblock[summaryopen + 1:(summaryclose-1)]
  summary <- gsub("^[ ]*(.*)[ ]*$", "\\1", summary)

  return(list(tags=tag, title=title, summary=summary))

}

