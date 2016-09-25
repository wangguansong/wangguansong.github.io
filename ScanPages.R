# This script works only for my website pages, because:
# - I put newlines after some tags (span for tags), but not all tags (
#   h2, span for date)
# - The pages' HTML is simple enough.
# - I didn't consider other possible senarios when I wrote this.

source("SiteFunctions.R", encoding="utf-8")

#if (file.exists("SiteMeta.csv")) {
#  htmlolddf <- read.csv("SiteMeta.csv", stringsAsFactors=FALSE)
#  htmlolddf$LANG <- as.factor(htmlolddf$LANG)
#  htmlolddf$TYPE <- as.factor(htmlolddf$TYPE)
#}

# Scan all pages
htmlfilelist <- character()
for (lang in c("en", "zh")) {
  htmlfilelist <-  c(htmlfilelist,
                     list.files(paste(lang, "blogs", sep="/"),
                                recursive=TRUE, full.names=TRUE,
                                pattern="*.html$"),
                     list.files(paste(lang, "tags", sep="/"),
                                recursive=TRUE, full.names=TRUE,
                                pattern="*.html$"),
                     list.files(paste(lang, "projects", sep="/"),
                                recursive=TRUE, full.names=TRUE,
                                pattern="*.html$"))
}
htmldf <- ParseFiles(htmlfilelist)

save(htmldf, file="SiteMeta.RData")
write.csv(htmldf, file="SiteMeta.csv", row.names=FALSE)


## Check number of Tags and Projects
allblogtags <- paste(htmldf$TAGS[htmldf$TYPE=="blog"], collapse=",")
allblogtags <- unique(strsplit(allblogtags, ",")[[1]])
allpagetags <- htmldf$TAGS[htmldf$TYPE %in% c("project", "tag")]
newtags <- setdiff(allblogtags, allpagetags)
if (length(newtags)>0) {
  stop("Need to create tag/project page for: ", newtags)
}

