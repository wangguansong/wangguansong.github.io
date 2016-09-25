########################################################################
# Database functions

BuildBlogDatabase <- function(blogDFfile = "database/blogDF.csv") {
  htmlfilelist <- character()
  for (lang in c("en", "zh")) {
    htmlfilelist <-  c(htmlfilelist,
                       list.files(paste(lang, "blogs", sep="/"),
                                  recursive=TRUE, full.names=TRUE,
                                  pattern="*.html$"))
  }
  blogDF <- ParseFiles(htmlfilelist)
  write.csv(blogDF, file = blogDFfile, row.names = FALSE)
  return(invisible(blogDF))
}

UpdateTagDatabase <- function(rowIds,
                              blogDFfile = "database/blogDF.csv",
                              tagDFfile = "database/tagDF.csv") {
  if (!exists("blogDF", mode = "list")) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  }
  if (missing(rowIds)) {
    rowIds <- 1:nrow(blogDF)
  }

  tagDFnew <- ScanBlogTags(rowIds, blogDFfile = blogDFfile)

  if (file.exists(tagDFfile)) {
    tagDF <- read.csv(tagDFfile, stringsAsFactors = FALSE)
  } else {
    tagDF <- data.frame(tag = character(0),
                        lang = character(0),
                        date = character(0),
                        type = character(0),
                        title = character(0),
                        desc = character(0),
                        stringsAsFactors = FALSE)
  }

  colnames(tagDFnew) <- c("tag", "langNew", "dateNew")
  tagDF <- merge(tagDF, tagDFnew, by = "tag", all = TRUE)

  updateRows <- which(!is.na(tagDF$dateNew) &
                      (tagDF$dateNew > tagDF$date |
                       tagDF$date == "" |
                       is.na(tagDF$date) |
                       tagDF$lang == "" |
                       is.na(tagDF$lang)))
  tagDF$date[updateRows] <- tagDF$dateNew[updateRows]
  tagDF$lang[updateRows] <- tagDF$langNew[updateRows]

  updateTags <- tagDF$tag[updateRows]
  if (length(updateTags) > 0) {
    write.csv(tagDF[, - which(colnames(tagDF) %in%
                              c("langNew", "dateNew"))],
              file = tagDFfile, row.names = FALSE)
    cat(paste("Updated following tags of blogs: ",
              paste(updateTags, collapse = ", "), "\n"))
  } else {
    cat("There is no update of tagDF.\n")
  }

  return(invisible(updateTags))
}


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
      return(max(blogDF$date[grepl(x[1], blogDF$tags[rowIds])],
                 na.rm=TRUE))
    })
  return(tagsDF)

}

########################################################################
ParseFiles <- function(pathlist) {
  # Given a path list, find its language, type, title, tags, etc.
  # Return a data.frame.
  # Depend on: ParseBlogHTML() and ParseTagHTML().

  pathlist <- pathlist[!grepl("yyyymmdd.*template", pathlist)]

  character(length(pathlist)) -> langlist -> typelist -> titlelist ->
    datelist -> tagslist -> summarylist
  langlist[grep("zh/", pathlist)] <- "zh"
  langlist[grep("en/", pathlist)] <- "en"

  typelist[grep("(zh|en)/blogs/", pathlist)] <- "blog"
  typelist[grep("(zh|en)/tags/", pathlist)] <- "tag"
  typelist[grep("(zh|en)/projects/", pathlist)] <- "project"
  typelist[grep("yyyymmdd.*template", pathlist)] <- "template"
  typelist[grep("(tag|project)_template", pathlist)] <- "template"

  for (i in 1:length(pathlist)) {
    if (typelist[i]=="template") {
      basicinfo <- list(title="Template", tags=NA,
                        datestamp=NA, summary=NA)
    } else if (typelist[i] %in% c("tag", "project")) {
      basicinfo <- ParseTagHTML(pathlist[i])
      basicinfo$datestamp <- NA
    } else if (typelist[i]=="blog") {
      basicinfo <- ParseBlogHTML(pathlist[i])
    }
    titlelist[i] <- basicinfo$title
    tagslist[i] <- paste(basicinfo$tags, collapse=",")
    datelist[i] <- basicinfo$datestamp
    summarylist[i] <- basicinfo$summary
  }
  return(data.frame(filePath = pathlist,
                    lang = as.factor(langlist),
                    type = as.factor(typelist),
                    title = titlelist,
                    tags = tagslist,
                    date = as.Date(datelist, format="%Y-%m-%d"),
                    desc = summarylist,
                    stringsAsFactors=FALSE))
}


ParseBlogHTML <- function(path) {
  # Given a path to a blog page, return tags, url of tags, date stamp,
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
  tagsurl <- gsub(".*<a href=\"/([^>]*)\">[^<].*</a>.*",
                  "\\1", tagsblock[tagline])
  dateline <- grep("<span class=\"datestamp\">", headerblock)[1]
  datestamp <- gsub(".*<span class=\"datestamp\">(.*)</span>.*",
                    "\\1", headerblock[dateline])

  summaryopen <- grep("<p class=\"summary\">", headerblock)
  summaryclose <- grep("</p>", headerblock[-(1:summaryopen)])[1]
  summary <- headerblock[summaryopen + 1:(summaryclose-1)]
  summary <- gsub("^[ ]*(.*)[ ]*$", "\\1", summary)

  titleline <- grep("<h2>(.*)</h2>", headerblock)[1]
  title <- gsub(".*<h2>(.*)</h2>.*", "\\1", headerblock[titleline])

  return(list(tags=tags, datestamp=datestamp, tagsurl=tagsurl,
              title=title, summary=summary))
}







########################################################################
# Procedure for adding a new blog:
# - NewBlogPage(path)
# - source("ScanPages.R")
# - UpdateSidebars()
# - UpdateBlogFrontPage()

########################################################################
WriteDataBase <- function() {
  # Save htmldf to files: SiteMeta.RData/SiteMeta.csv

  if (!exists("htmldf", mode="list")) {
    stop("WriteDataBase: Cannot find data frame htmldf to write.")
  }
  save(htmldf, file="SiteMeta.RData")
  write.csv(htmldf, file="SiteMeta.csv", row.names=FALSE)
}

########################################################################

########################################################################
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
########################################################################
########################################################################
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
UpdateTagPage <- function(tag, lang) {
  # Update the page of a tag in zh/en, based on htmldf.

  if (! exists("htmldf", mode="list")) load("SiteMeta.RData")
  tagid <- which(htmldf$TAGS==tag &
                 htmldf$TYPE %in% c("tag", "project") &
                 htmldf$LANG==lang)
  if (! length(tagid)==1) {
    stop("UpdateTagPage: tag not found or has multiple hits.")
  }
  rowid <- which(htmldf$TYPE=="blog" & grepl(tag, htmldf$TAGS) &
                 htmldf$LANG==lang)
  rowid <- rowid[order(htmldf$DATE[rowid], decreasing=TRUE)]
  blognum <- length(rowid)
  linkline <- paste("      <a href=\"/", htmldf$PATH[rowid], "\">",
                    htmldf$TITLE[rowid], "</a>", sep="")
  dateline <- paste("      <span class=\"datestamp\">",
                    format(htmldf$DATE[rowid], "%Y-%m-%d"), "</span>",
                    sep="")
  sectionhtml <- character(length=blognum*9)
  sectionhtml[(1:blognum-1)*9 + 1] <- "  <section>"
  sectionhtml[(1:blognum-1)*9 + 2] <- "    <h3>"
  sectionhtml[(1:blognum-1)*9 + 3] <- linkline
  sectionhtml[(1:blognum-1)*9 + 4] <- dateline
  sectionhtml[(1:blognum-1)*9 + 5] <- "    </h3>"
  sectionhtml[(1:blognum-1)*9 + 6] <- "    <p class=\"summary\">"
  sectionhtml[(1:blognum-1)*9 + 7] <- paste("      ",
                                            htmldf$SUMMARY[rowid],
                                            sep="")
  sectionhtml[(1:blognum-1)*9 + 8] <- "    </p>"
  sectionhtml[(1:blognum-1)*9 + 9] <- "  </section>"

  articlehtml <-
    c("<article>",
      "  <header>",
      paste("    <h2>", htmldf$TITLE[tagid], "</h2>", sep=""),
      "    <p class=\"summary\">",
      paste("      ", htmldf$SUMMARY[tagid], sep=""),
      "    </p>",
      "  </header>",
      sectionhtml,
      "</article>")
  ReplaceTag(htmldf$PATH[tagid], articlehtml, "article", 1)
  return(invisible(articlehtml))

}
########################################################################
UpdateBlogFrontPage <- function(lang) {
  # Update the blog front page in zh/en, based on htmldf.

  if (! exists("htmldf", mode="list")) load("SiteMeta.RData")
  rowid <- which(htmldf$TYPE=="blog" & htmldf$LANG==lang)
  rowid <- rowid[order(htmldf$DATE[rowid], decreasing=TRUE)]
  blognum <- length(rowid)
  linkline <- paste("      <a href=\"/", htmldf$PATH[rowid], "\">",
                    htmldf$TITLE[rowid], "</a>", sep="")
  dateline <- paste("      <span class=\"datestamp\">",
                    format(htmldf$DATE[rowid], "%Y-%m-%d"), "</span>",
                    sep="")
  sectionhtml <- character(length=blognum*9)
  sectionhtml[(1:blognum-1)*9 + 1] <- "  <section>"
  sectionhtml[(1:blognum-1)*9 + 2] <- "    <h3>"
  sectionhtml[(1:blognum-1)*9 + 3] <- linkline
  sectionhtml[(1:blognum-1)*9 + 4] <- dateline
  sectionhtml[(1:blognum-1)*9 + 5] <- "    </h3>"
  sectionhtml[(1:blognum-1)*9 + 6] <- "    <p class=\"summary\">"
  sectionhtml[(1:blognum-1)*9 + 7] <- paste("      ",
                                            htmldf$SUMMARY[rowid],
                                            sep="")
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

########################################################################
UpdateSidebars <- function() {
  # Update the sidebars of tag pages and blog front page

  if (! exists("htmldf", mode="list")) load("SiteMeta.RData")

  for (lang in c("en", "zh")) {
  #for (lang in c("zh")) {
    rowid <- which(htmldf$TYPE=="tag" & htmldf$LANG==lang)
    tagsidebar <-
      c("<aside>",
        paste("  <h3>", ifelse(lang=="en", "Tag: ", "标签："), "</h3>",
              sep=""),
        "  <ul>",
        paste("    <li><a href=\"/", htmldf$PATH[rowid], "\">",
              htmldf$TAGS[rowid], "</a></li>", sep=""),
        "  </ul>", "</aside>")
    for (i in rowid) {
      print(htmldf$PATH[i])
      ReplaceTag(htmldf$PATH[i],
                 ToggleActive(tagsidebar,
                              paste("/", htmldf$PATH[i], sep="")),
                 "aside", 1)
    }

    rowid <- which(htmldf$TYPE=="project" & htmldf$LANG==lang)
    projectsidebar <-
      c("<aside>",
        paste("  <h3>", ifelse(lang=="en", "Project: ", "项目："),
              "</h3>", sep=""),
        "  <ul>",
        paste("    <li><a href=\"/", htmldf$PATH[rowid], "\">",
              htmldf$TAGS[rowid], "</a></li>", sep=""),
        "  </ul>", "</aside>")
    for (i in rowid) {
      print(htmldf$PATH[i])
      ReplaceTag(htmldf$PATH[i],
                 ToggleActive(projectsidebar,
                              paste("/", htmldf$PATH[i], sep="")),
                 "aside", 1)
    }

    ReplaceTag(paste(lang, "blog.html", sep="/"),
               projectsidebar, "aside", 1)
    ReplaceTag(paste(lang, "blog.html", sep="/"),
               tagsidebar, "aside", 2)

  }
  
}
########################################################################
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

########################################################################
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
