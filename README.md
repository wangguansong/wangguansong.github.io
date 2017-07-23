# Summary of Most Used Procedures
## Add a New Blog

## Add New Photos
  * Collect all photo files into a directory.
  * Make an archive and save the zip file.
  * Compress the photo files into smaller ones:
```bash
mkdir test
for i in *.JPG *.jpg; do jpegoptim --dest=test --size=1024 $i; done
```
  * Select photos to keep and to keep private.
  * Update the photo database. In R:
```R
UpdatePhotoDatabase(rootDir = "/media/butters/MyHDD/Photos/",
                    subDir = "2016/20160101_HappyNewYear")
```
  * Open the actual csv file, input all dates, tags, title, etc.
  * Update the album database. In R:
```R
currentTags <- unique(albumDF$tag)
updateTags <-
  UpdateAlbumDatabase(rowIds = which(is.na(photoDF$date)))
```
  * Open the albumDF csv file, input descriptions.
  * Reload albumDF.
  * Generate / update album pages, in R:
```R
  UpdateAlbumPages("zh", updateTags)
  UpdateAlbumPages("en", updateTags)
```
  * Update picture front page if new tags are added:
```R
if (length(setdiff(updateTags, currentTags))>0) {
  UpdatePictureFrontPage("zh")
  UpdatePictureFrontPage("en")
}
```

## Re-build all album pages:
```R
UpdateAlbumPages("zh", albumDF$tags)
UpdateAlbumPages("en", albumDF$tags)
```


# Database: Structures
## photoDF
  * File: /database/photoDF.csv
  * Variables: filePath, date, tags, zhTitle, enTitle,
               zhDesc, enDesc, favorite, hide

## albumDF
  * File: /database/albumDF.csv
  * Variables: tag, date, type, zhTitle, enTitle, zhDesc, enDesc

## blogDF
  * File: /database/blogDF.csv
  * Variables: filePath, lang, date, tags, title, desc, hide

## tagDF
  * File: /database/tagDF.csv
  * Variables: tag, lang, date, type, title, desc, name

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


# To Do
