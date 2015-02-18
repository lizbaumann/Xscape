################################################################################
#  + decide whether to mix things up more. which do we get paid more for?
# GOALS:
# from main Nordstroms and Dillards pages, read in html
# NOTE: Nordstroms, have to delete stuff before / after dresses, could improve later...
# output list of all dresses being shown, list elements:
# nbr, name, dressurl, photo1url, photo2url, price
# probably do not use price - it can change and would probably hurt sales not help
################################################################################

library(XML)
library(RCurl)

setwd("~/Liz/_Freelance/_XscapeDresses/html")
rm(list=ls())

################################################################################
# DILLARDS 
# main html must be saved to working directory and reduced for dresses parts only.
# individual dress page urls are used and read systematically.
################################################################################
# read main file, parse html code, put in data frame.
#html0 <- readLines("dxs_test.txt")   # for testing
html0 <- readLines("dxs_dresses.txt")   # ALL dresses, ONLY dresses html

ndoc <- htmlParse(html0, useInternalNodes=TRUE)
name <- ndoc["//span[@class='productName']"]
photourls <- ndoc["//img/@data-src_medium"]    # always 2 of these, though they are same if only one photo...
dressahref <- ndoc["//a/@href"] # for url... note, only want ones without # or with class='title'...

# a hrefs have urls not just of dresses, the dresses do not have % in url so remove those
remove <- grep("%", dressahref)
dressurl <- dressahref[-remove]
dressurl <- unique(dressurl)

# photos: parse out the 2 from each other, and remove the '$searchCatMedium$' from ends
# odd are photo1, even are photo2
photo1url <- vector()
photo2url <- vector()
for (i in 1:length(photourls)) {
        removetext <- '$searchCatMedium$'
        photourl <- substr(photourls[[i]][1], 1, nchar(photourls[[i]][1]) - nchar(removetext))
                if (i %% 2 == 1) { photo1url <- c(photo1url, photourl) }
                else { photo2url <- c(photo2url, photourl) }
}

# start the empty data frame
numdresses <- length(name)
dresses <- matrix(, nrow=numdresses, ncol=11)
colnames(dresses) <- c("nbr", "name", "dressurl", "photo1url", "photo2url", "price",
                       "desc", "madein", "sizes", "colors", "store")
dresses <- as.data.frame(dresses)

dxs3 <- vector()  # initialize a vector to house the code for wordpress

# now script the html code for each dress and add it to the wordpress code vector.
# within this, download html of url for each dress to get more dress info like color and size.
for (i in 1:numdresses) {
        dresses$store[i] <- "Dillards"
        dresses$name[i] <- xmlValue(name[[i]])
        dresses$dressurl[i] <- paste0("http://dillards.com", dressurl[[i]][1])
        dresses$nbr[i] <- sub(".*df=([0-9]+).*", "\\1", dressurl[[i]][1])
        dresses$photo1url[i] <- photo1url[i]
        if (photo2url[i] == photo1url[i]) { dresses$photo2url[i] <- NA }
        else { dresses$photo2url[i] <- photo2url[i] }
        dresses$price[i] <- NA

        # Now, read URL to get dress info: desc, madein, sizes, colors: 
        #url <- "http://shop.nordstrom.com/s/xscape-crystal-back-jersey-gown/3662704?origin=category"
        #dtlfile <- download.file(url, destfile = "dressurl.txt")
        destfile = paste0("./dressescode/d", dresses$nbr[i], ".txt")
        if (!file.exists(destfile)) { 
                dtlfile <- download.file(dresses$dressurl[i], destfile=destfile)
        }
        dtl <- readLines(destfile, warn=FALSE) # warn=FALSE because get 'incomplete final line' a lot
        ddoc <- htmlParse(dtl, useInternalNodes=TRUE)
        
        # description and made in usa info
        desctext <- ddoc["//div[@id='description']"]
        desctext <- xmlValue(desctext[[1]])
        desctext <- gsub("\n{2}", ". ", desctext)
        desctext <- gsub("\n", ", ", desctext)
        desctext <- gsub(" ,", ", ", desctext)
        desctext <- gsub("[\t]+", " ", desctext)
        desctext <- gsub(" +", " ", desctext)
        desctext <- gsub(": ?,", ":", desctext)
        desctext <- gsub(" . ", ". ", desctext)
        desctext <- gsub("[., ]*$", ".", desctext)
        desctext <- gsub("^, ", "", desctext)
        desc <- gsub("Description: (.*) ([Mm]ade|[Ii]mport).*", "\\1", desctext)
        madein <- gsub(".*(([Mm]ade|[Ii]mport)[^.,;]+)[.,;](.*)", "\\1", desctext)
        madein <- paste0(toupper(substr(madein,1,1)), substr(madein,2,nchar(madein)), ".")
        
        # sizes and colors - cannot get from html, it is read via javascript code
        
        ################################################################################
        # OUTPUTING:
        ################################################################################
        # Now generate the html code for the dress.
        
        dxs1 <- vector()
        length(dxs1) <- 22

        # photo placement will alternate between float left and right
        if (i %% 2 == 0) { floattxt <- "float:left; margin-right:" }
        else { floattxt <- "float:right; margin-left:" }

        dxs1[1] <- "<br clear=\"all\" />"
        dxs1[2] <- "<br />"
        dxs1[3] <- paste0("<a href=\"", dresses$dressurl[i], "\" title=\"", 
                         dresses$name[i], "\" target=\"_blank\">")
        dxs1[4] <- paste0("<img src=\"", dresses$photo1url[i], "\" alt=\"", dresses$name[i], 
                         "\" style=\"height:180px; ", floattxt, "\" />")
        dxs1[5] <- "</a>"
        dxs1[6] <- dxs1[3]  # url
        if (!is.na(dresses$photo2url[i])) { 
                dxs1[7] <- paste0("<img src=\"", dresses$photo2url[i], "\" alt=\"", dresses$name[i],  
                                 "\" style=\"height:180px; ", floattxt, "\" />") }
        else { dxs1[7] <- "" }
        dxs1[8] <- "</a>"
        dxs1[9] <- "<strong>"
        dxs1[10] <- paste0("PERSONALTITLE", sprintf("%02d", i)) # special title
        dxs1[11] <- "</strong>"
        dxs1[12] <- "<br />"
        dxs1[13] <- paste0(desc, " ", madein, " <strong>Sizes</strong>: GETSIZES", sprintf("%02d", i), 
                          ". <strong>Colors</strong>: GETCOLORS", sprintf("%02d", i), ". ")
        dxs1[14] <- "<br />"
        dxs1[15] <- paste0("<em>An ", dresses$name[i], " by Joanna Chen.</em>")
        dxs1[16] <- "<br />"
        dxs1[17] <- "<strong>"
        # Note: before, 18 and 19 could have multiple colors and links... no longer trying to combine.
        dxs1[18] <- dxs1[3]  # url
        dxs1[19] <- paste0("BUY FROM ", toupper(dresses$store[i]))
        dxs1[20] <- "</a>"
        dxs1[21] <- "</strong>"
        dxs1[22] <- ""
        
        # some clean up...
        for (i in 1:22) {
                dxs1[i] <- gsub("\n","",dxs1[i])
                dxs1[i] <- gsub("( )+"," ",dxs1[i])
        }
        
        dxs2 <- paste(dxs1, collapse="")
        
        # output
        if (i == 1) { dxs3 <- dxs2 }
        else { dxs3 <- c(dxs3, " ", dxs2) }
}

dxs <- dxs3

################################################################################
# PersonaLIZation: give each dress a catchy title, to be unique 
# (and more importantly, in case it is noticed by search engines as more original)
################################################################################
dxs4 <- dxs3

dxs4 <- gsub("PERSONALTITLE01", "Svelte, Sheer with Beaded Sleeves", dxs4)
dxs4 <- gsub("PERSONALTITLE02", "Bewitching Beaded Boat Neckline and Sweeping Length", dxs4)
dxs4 <- gsub("PERSONALTITLE03", "Glamorous Gown with Sexy Sweetheart Bodice", dxs4)
dxs4 <- gsub("PERSONALTITLE04", "Sweet Silky Goodness", dxs4)
dxs4 <- gsub("PERSONALTITLE05", "Tempting in Tantalizing Taupe", dxs4)
dxs4 <- gsub("PERSONALTITLE06", "Ornate and Orignal Neckline on Knee-length Dress", dxs4)
dxs4 <- gsub("PERSONALTITLE07", "Sweepingly Sleeveless with V-neck and V-back", dxs4)
dxs4 <- gsub("PERSONALTITLE08", "Alluringly Original Neckline and Smart Styling", dxs4)
dxs4 <- gsub("PERSONALTITLE09", "Alluring Neckline, Lovely Lacy Sleeves, Sheer Skirt", dxs4)

dxs4 <- gsub("GETSIZES01", "2, 4, 6, 10, 12, 14", dxs4)
dxs4 <- gsub("GETSIZES02", "4, 12", dxs4)
dxs4 <- gsub("GETSIZES03", "4, 6, 8, 10, 12, 14", dxs4)
dxs4 <- gsub("GETSIZES04", "6, 8, 10, 12, 14, 16", dxs4)
dxs4 <- gsub("GETSIZES05", "10", dxs4)
dxs4 <- gsub("GETSIZES06", "4, 6, 8, 10", dxs4)
dxs4 <- gsub("GETSIZES07", "4, 6, 8, 10, 12, 14", dxs4)
dxs4 <- gsub("GETSIZES08", "4, 6, 8, 10, 12, 14", dxs4)
dxs4 <- gsub("GETSIZES09", "4, 6, 8, 10, 12, 14", dxs4)

dxs4 <- gsub("GETCOLORS01", "Plum", dxs4)
dxs4 <- gsub("GETCOLORS02", "Navy", dxs4)
dxs4 <- gsub("GETCOLORS03", "Charcoal", dxs4)
dxs4 <- gsub("GETCOLORS04", "Ivory", dxs4)
dxs4 <- gsub("GETCOLORS05", "Pure Taupe", dxs4)
dxs4 <- gsub("GETCOLORS06", "Black", dxs4)
dxs4 <- gsub("GETCOLORS07", "Black", dxs4)
dxs4 <- gsub("GETCOLORS08", "Navy", dxs4)
dxs4 <- gsub("GETCOLORS09", "Black", dxs4)

dxs <- dxs4

# save to txt file. copy the txt text to wordpress and review it.
writeLines(dxs, "dxscode.txt")


################################################################################
# GENERAL INFO AND FOR TROUBLESHOOTING:
################################################################################
troubleshoot <- function () {
        print(paste0("nrow(dresses): ", nrow(dresses)))
        print(paste0("length(name): ", length(name), ". NAs: ", sum(is.na(name))))
        print(paste0("length(dressurl): ", length(dressurl), ". NAs: ", sum(is.na(dressurl))))
        print(paste0("length(photo1url): ", length(photo1url), ". NAs: ", sum(is.na(photo1url))))
        print(paste0("length(photo2url): ", length(photo2url), ". NAs: ", sum(is.na(photo2url))))
        print(paste0("length(dxs) (should be 2x dresses, for blank lines): ", length(dxs)))
}
troubleshoot()
