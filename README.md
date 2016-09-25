# Summary of Most Used Procedures
## Add a New Blog
## Add New Photos

# Database: Structures
## photoDF
* File:
/database/photoDF.csv
* Variables:
filePath, date, tags,
zhTitle, enTitle, zhDesc, enDesc,
favorite, hide

## albumDF
* File:
/database/albumDF.csv
* Variables:
tag, date, type,
zhTitle, enTitle, zhDesc, enDesc

## blogDF
* File:
/database/blogDF.csv
* Variables:
filePath, lang, date, tags, title, desc


## tagDF
* File:
/database/tagDF.csv
* Variables:
tag, lang, date, type, title, desc

# Database: Building and Updating
## photoDF
* Collect all the photos of interest,
zip the raw files for archive,
shrink the photos using "jpegoptim" into a subdirectory (to be safe),
check for defected photos,
save photos of family and friends to a private directory,
pick the photos to show,
then upload those to aliyun OSS.
* Update the photoDF.csv using UpdatePhotoDF().
For example, to update new photos placed in a subdirectory:
```R
UpdatePhotoDF(rootDir = "/absolute/path/to/photo/collections/",
              subDir = "2016/20160915_MidAutumnDay/")
```
Then I have to mannually put in all other variables in photoDF.

## albumDF
## blogDF
## tagDF
