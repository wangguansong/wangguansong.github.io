#########################################################################
## Procedure for adding a new blog:
## - NewBlogPage(path)
## - source("ScanPages.R")
## - UpdateSidebars()
## - UpdateBlogFrontPage()
#
#
#########################################################################
## Building pages

BuildTagPage <- function(tag, lang,
                         blogDFfile = "database/blogDF.csv",
                         tagDFfile = "database/tagDF.csv") {
  # Build a collection page of blogs, selected by tags, lang, dates.
  blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  tagDF <- read.csv(tagDFfile, stringsAsFactors = FALSE)

  tagId <- which(tagDF$tag == tag & tagDF$lang == lang)
  if (length(tagId) != 1) {
    stop(paste("The tag \"", tag, "\" is not found in database.",
               sep = ""))
  }

  tagRows <- rep(TRUE, nrow(blogDF))
  #tagRows <- tagRows & blogDF$type == "blog"
  tagRows <- tagRows & blogDF$lang == lang

  tagsList <- strsplit(blogDF$tags, ",")
  tagRows <- tagRows &
    unlist(lapply(tagsList, FUN = function(x) {
                   return(tag %in% x)
                 }))
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
      paste("    <h2>", tagDF$title[tagId], "</h2>", sep=""),
      "    <p class=\"summary\">",
      paste("      ", tagDF$desc[tagId], sep=""),
      "    </p>",
      "  </header>",
      sectionhtml,
      "</article>")

  # copy template
  fileName <- paste(tagDF$lang[tagId], "/", tagDF$type[tagId], "s/",
                    tagDF$name[tagId], ".html", sep = "")
  file.copy(paste("template/", tagDF$type[tagId], "_template_",
                  tagDF$lang[tagId], ".html", sep = ""),
            fileName,
            overwrite = TRUE)
  ReplaceTag(fileName,
             c("<title>", tagDF$title[tagId], "</title>"),
             "title", 1)
  ReplaceTag(fileName, articlehtml, "article", 1)
  return(invisible(articlehtml))

}

UpdateBlogFrontPage <- function(lang,
                                blogDFfile = "database/blogDF.csv") {
  # Update the blog front page in zh/en
  blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)

  rowIds <- which(blogDF$lang == lang & blogDF$hide == 0)
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
#
UpdateSidebars <- function(lang, tags, blogFront = TRUE,
                           tagDFfile = "database/tagDF.csv") {
  # Update the sidebars of tag pages and blog front page

  tagDF <- read.csv(tagDFfile, stringsAsFactors = FALSE)
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


#########################################################################
## Database functions
UpdateBlogDatabase <- function(newBlogPaths,
                               blogDFfile = "database/blogDF.csv") {
  # Add new blogs into the database.
  # The paths of new blogs are given by a character vector.
  # If not given, search for new html files in zh|en/blogs directories.
  if (file.exists(blogDFfile)) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
    flagCreateNew <- FALSE 
  } else {
    flagCreateNew <- TRUE
  }

  if (missing(newBlogPaths)) {
    newBlogPaths <- character()
    for (lang in c("en", "zh")) {
      newBlogPaths <-  c(newBlogPaths,
                         list.files(paste(lang, "blogs", sep="/"),
                                    recursive=TRUE, full.names=TRUE,
                                    pattern="*.html$"))
    }
    newBlogPaths <- newBlogPaths[!grepl("~$", newBlogPaths)]
    newBlogPaths <- newBlogPaths[!grepl("yyyymmdd.*template",
                                        newBlogPaths)]

  }

  if (! all(file.exists(newBlogPaths))) {
    error("Some blog given by path does not exist")
  }
  if (!flagCreateNew) {
    newBlogPaths <- setdiff(newBlogPaths, blogDF$filePath)
  }
  if (length(newBlogPaths) == 0) {
    print("No new blogs or blogs given by paths already in database.")
    return()
  }

  character(length(newBlogPaths)) -> langList -> titleList ->
    dateList -> tagsList -> summaryList
  langList[grep("zh/", newBlogPaths)] <- "zh"
  langList[grep("en/", newBlogPaths)] <- "en"
  for (i in 1:length(newBlogPaths)) {
    basicinfo <- ParseArticle(newBlogPaths[i])
    titleList[i] <- basicinfo$title
    tagsList[i] <- paste(basicinfo$tags, collapse=",")
    dateList[i] <- basicinfo$datestamp
    summaryList[i] <- basicinfo$summary
  }
  newBlogDF <- data.frame(filePath = newBlogPaths,
                          lang = as.factor(langList),
                          title = titleList,
                          tags = tagsList,
                          date = as.Date(dateList, format="%Y-%m-%d"),
                          summary = summaryList,
                          hide = 0,
                          stringsAsFactors=FALSE)

  if(flagCreateNew) {
    write.csv(newBlogDF, file = blogDFfile, row.names = FALSE,
              fileEncoding = "UTF-8")
  } else {
    write.table(newBlogDF, file = blogDFfile, append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE, quote = TRUE,
                fileEncoding = "UTF-8")
  }
  #assign("blogDF",
  #       read.csv(blogDFfile, stringsAsFactors = FALSE),
  #       envir = .GlobalEnv)
  #cat("Data frame \"blogDF\" is loaded from ", blogDFfile, "\n")
  return(invisible(newBlogDF))

}



UpdateTagDatabase <- function(blogDF,
                              blogDFfile = "database/blogDF.csv",
                              tagDFfile = "database/tagDF.csv") {
  # Build or update the tag database from blog database.
  # If asked, return updated tags.

  if (missing(blogDF)) {
    blogDF <- read.csv(blogDFfile, stringsAsFactors = FALSE)
  }

  enRowIds <- blogDF$lang == "en"
  zhRowIds <- blogDF$lang == "zh"
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
  dateNew <- character(nrow(tagDFNew))
  blogTagList <- strsplit(blogDF$tags, ",")
  tagDFNew$dateNew <- apply(tagDFNew, 1, FUN = function(x) {
    tagregex <- paste("\\<", x["tag"], "\\>", sep = "")
    tagRows <- which(grepl(x["tag"], blogTagList) &
                     blogDF$lang == x["lang"])
    return(max(blogDF$date[tagRows]))
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

  return(invisible(updateTags))
}



ScanBlogTags <- function(blogDF) {
  # Scan for tags of blogs, return tagsDF:tag,date.
  # Return a data frame: tag, date

  allTags <- unlist(strsplit(blogDF$tags, ","))
  allTags <- trimws(allTags)    # R >= 3.2 required
  allTags <- unique(allTags)
  tagsDF <- data.frame(tag = allTags,
                       stringsAsFactors = FALSE)
  tagsDF$date <- apply(tagsDF, 1, FUN = function(x) {
    tagregex <- paste("\\<", x["tag"], "\\>", sep = "")
    return(max(blogDF$date[grepl(tagregex, blogDF$tags)]))
  })
  return(tagsDF)
}
#########################################################################
## Helper functions.
## ScanBlogTags(rowIds, blogDFfile)
## ParseBlogHTML(path)
## ToggleActive(htmlfile, link)
#
ParseArticle <- function(path) {
  # Given a path to a blog page, return tags, (url of tags), date stamp,
  # title and summary line.

  require("rvest")
  rawhtml <- read_html(path, encoding = "UTF-8")

  title <- html_node(rawhtml, "#article_title") %>% html_text
  tags <- html_nodes(rawhtml, "#article_tags a") %>% html_text
  datestamp <- html_node(rawhtml, "#article_datestamp") %>% html_text
  summ <- html_node(rawhtml, "#article_summary") %>% html_text


  return(list(tags=tags, datestamp=datestamp, #tagsurl=tagsurl,
              title=title, summary=summ))
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
