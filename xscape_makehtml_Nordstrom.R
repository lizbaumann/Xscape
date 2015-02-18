################################################################################
# TO DO:
#  + decide whether to mix things up more. which do we get paid more for?
# GOALS:
# from main Nordstroms and Dillards pages, read in html
# NOTE: Nordstroms, have to delete stuff before / after dresses, could improve later...
# output list of all dresses being shown, list elements:
# nbr, name, dressurl, photo1url, photo2url, price
# probably do not use price - it can change and would probably hurt sales not help
# Updates: 10/1/14
################################################################################

library(XML)
library(RCurl)

setwd("~/Liz/_Freelance/_XscapeDresses/html_201502")
rm(list=ls())

################################################################################
# NORDSTROM 
# main html must be saved to working directory and reduced for dresses parts only.
# individual dress page urls are used and read systematically.
################################################################################
# read main file, parse html code, put in data frame.
#html0 <- readLines("nxs_test.txt")   # for testing
html0 <- readLines("nxs_dresses.txt")   # ALL dresses, ONLY dresses html

ndoc <- htmlParse(html0, useInternalNodes=TRUE)
name <- ndoc["//img/@alt"]
photo1url <- ndoc["//img/@data-original"]
dressahref <- ndoc["//a/@href"] # for url... note, only want ones without # or with class='title'...
price <- ndoc["//span[@class='price regular']"]

# photos: note there is not always an alt photo, otherwise would have gotten like above.
pht <- grep("[^<]*?<div data-fashion-image-url=\"([^\"]*)\"( data-alt-image-url=\"([^\"]*)\")? class=\"fashion-photo\">",
               html0)
photos <- html0[pht]
photo1suffix <- gsub("[^<]*?<div data-fashion-image-url=\"([^\"]*).*", "\\1", photos)
photo2suffix <- gsub(".*?data-alt-image-url=\"([^\"]*).*", "\\1", photos)
# if alt photo not found, it will just pull the whole thing, tell this from the length
for (i in 1:length(photo2suffix)) {
        if (nchar(photo2suffix[i]) > 20) { photo2suffix[i] <- NA }
}

# a hrefs have urls not just of dresses, the dresses do not have # in url so remove those
remove <- grep("#", dressahref)
dressurl <- dressahref[-remove]

# start the empty data frame
numdresses <- length(name)
dresses <- matrix(, nrow=numdresses, ncol=11)
colnames(dresses) <- c("nbr", "name", "dressurl", "photo1url", "photo2url", "price",
                       "desc", "madein", "sizes", "colors", "store")
dresses <- as.data.frame(dresses)

nxs3 <- vector()  # initialize a vector to house the code for wordpress

# now script the html code for each dress and add it to the wordpress code vector.
# within this, download html of url for each dress to get more dress info like color and size.
for (i in 1:numdresses) {
        dresses$store[i] <- "Nordstrom"
        dresses$name[i] <- name[[i]][1]
        dresses$dressurl[i] <- paste0("http://shop.nordstrom.com", dressurl[[i]][1])
        dresses$nbr[i] <- sub("[^(0-9)]+([0-9]+)[^(0-9)]+", "\\1", dressurl[[i]][1])
        dresses$photo1url[i] <- photo1url[[i]][1]
        if (!is.na(photo2suffix[i])) {dresses$photo2url[i] <- paste0(substr(photo1url[[i]][1], 1, 
                nchar(photo1url[[i]][1]) - nchar(photo1suffix[i])), photo2suffix[i])}
        dresses$price[i] <- xmlValue(price[[i]][1]$text)

        # Now, read URL to get dress info: desc, madein, sizes, colors: 
        #url <- "http://shop.nordstrom.com/s/xscape-crystal-back-jersey-gown/3662704?origin=category"
        #dtlfile <- download.file(url, destfile = "dressurl.txt")
        destfile = paste0("./dressescode/n", dresses$nbr[i], ".txt")
        if (!file.exists(destfile)) { 
                dtlfile <- download.file(dresses$dressurl[i], destfile=destfile)
        }
        dtl <- readLines(destfile, warn=FALSE) # warn=FALSE because get 'incomplete final line' a lot
        ddoc <- htmlParse(dtl, useInternalNodes=TRUE)
        
        # description and made in usa info
        desctext <- ddoc["//section[@id='details-and-care']"]
        desctext <- xmlValue(desctext[[1]])
        desctext<- gsub("[\n\t]+", " ", desctext)
        desctext <- gsub(" +", " ", desctext)
        desc <- gsub(" Details & Care (.*) By Xscape(.*)", "\\1", desctext)
        madein <- gsub(" Details & Care (.*) By Xscape(.*)", "\\2", desctext)
        madein <- gsub("[.,;] ((made|import)[^.,;]+)[.,;](.*)", "\\1", madein)
        madein <- paste0(toupper(substr(madein,1,1)), substr(madein,2,nchar(madein)), ".")
        
        # sizes and colors
        # "size": "2", "color": "Black",
        sclines <- grep("\"size\": \"[^\"]+\", \"color\": \"[^\"]+\"", dtl, value=T)
        sizes <- gsub(".*\"size\": \"([^\"]+)\", \"color\": \"([^\"]+)\".*", "\\1", sclines)
        colors <- gsub(".*\"size\": \"([^\"]+)\", \"color\": \"([^\"]+)\".*", "\\2", sclines)
        colors <- gsub("/ +", "/", colors)
        colors <- gsub(" +/", "/", colors)
        sizes <- unique(sizes)
        colors <- unique(colors)
        sizes <- paste(sizes, collapse=", ")
        colors <- paste(colors, collapse=", ")
        
        ################################################################################
        # OUTPUTING:
        ################################################################################
        # Now generate the html code for the dress.
        
        nxs1 <- vector()
        length(nxs1) <- 22

        # photo placement will alternate between float left and right
        if (i %% 2 == 1) { floattxt <- "float:left; margin-right:" }
        else { floattxt <- "float:right; margin-left:" }

        nxs1[1] <- "<br clear=\"all\" />"
        nxs1[2] <- "<br />"
        nxs1[3] <- paste0("<a href=\"", dresses$dressurl[i], "\" title=\"", 
                         dresses$name[i], "\" target=\"_blank\">")
        nxs1[4] <- paste0("<img src=\"", dresses$photo1url[i], "\" alt=\"", dresses$name[i], 
                         "\" style=\"height:180px; ", floattxt, "\" />")
        nxs1[5] <- "</a>"
        nxs1[6] <- nxs1[3]  # url
        if (!is.na(dresses$photo2url[i])) { 
                nxs1[7] <- paste0("<img src=\"", dresses$photo2url[i], "\" alt=\"", dresses$name[i],  
                                 "\" style=\"height:180px; ", floattxt, "\" />") }
        else { nxs1[7] <- "" }
        nxs1[8] <- "</a>"
        nxs1[9] <- "<strong>"
        nxs1[10] <- paste0("PERSONALTITLE", sprintf("%02d", i)) # special title
        # nxs1[10] <- paste0("TITLE_", sprintf("%02d", i), "_n", dresses$nbr) # special title
        nxs1[11] <- "</strong>"
        nxs1[12] <- "<br />"
        nxs1[13] <- paste0(desc, " ", madein, " <strong>Sizes</strong>: ", sizes, 
                          ". <strong>Colors</strong>: ", colors, ". ")
        nxs1[14] <- "<br />"
        nxs1[15] <- paste0("<em>An ", dresses$name[i], " by Joanna Chen.</em>")
        nxs1[16] <- "<br />"
        nxs1[17] <- "<strong>"
        # Note: before, 18 and 19 could have multiple colors and links... no longer trying to combine.
        nxs1[18] <- nxs1[3]  # url
        nxs1[19] <- paste0("BUY FROM ", toupper(dresses$store[i]))
        nxs1[20] <- "</a>"
        nxs1[21] <- "</strong>"
        nxs1[22] <- ""
        
        # some clean up...
        for (i in 1:22) {
                nxs1[i] <- gsub("\n","",nxs1[i])
                nxs1[i] <- gsub("( )+"," ",nxs1[i])
        }
        
        nxs2 <- paste(nxs1, collapse="")
        
        # output
        if (i == 1) { nxs3 <- nxs2 }
        else { nxs3 <- c(nxs3, " ", nxs2) }
}

nxs <- nxs3

################################################################################
# PersonaLIZation: give each dress a catchy title, to be unique 
# (and more importantly, in case it is noticed by search engines as more original)
################################################################################
nxs4 <- nxs3

nxs4 <- gsub("PERSONALTITLE01", "Comfortable Yet Elegant Polished Stretch Jersey", nxs4)
nxs4 <- gsub("PERSONALTITLE02", "Curvy Crystalline Confident Crimson", nxs4)
nxs4 <- gsub("PERSONALTITLE03", "Youthful, Flowing and Sparkly", nxs4)
nxs4 <- gsub("PERSONALTITLE04", "Casually Styled with Pleasing Pleats", nxs4)
nxs4 <- gsub("PERSONALTITLE05", "Playful Like a Princess Satiny Two-Piece", nxs4)
nxs4 <- gsub("PERSONALTITLE06", "Silvery Beaded Sweetheart", nxs4)
nxs4 <- gsub("PERSONALTITLE07", "Split Sleeves with Sexy Skirt", nxs4)
nxs4 <- gsub("PERSONALTITLE08", "Single Shoulder Beaded Wonder", nxs4)
nxs4 <- gsub("PERSONALTITLE09", "Casual Elegance in Flowing Foiled Gown", nxs4)
nxs4 <- gsub("PERSONALTITLE10", "Exciting Emerald with Neat Neckline", nxs4)
nxs4 <- gsub("PERSONALTITLE11", "Red and Crystalled Jersey Style Top and Flowing Gown", nxs4)
nxs4 <- gsub("PERSONALTITLE12", "Black and Crystalled Jersey Style Top and Flowing Gown", nxs4)
nxs4 <- gsub("PERSONALTITLE13", "Beaded Beautiful Blue", nxs4)
nxs4 <- gsub("PERSONALTITLE14", "Strap Cutout Back with Side Embellishments, Side Slit", nxs4)
nxs4 <- gsub("PERSONALTITLE15", "Airy Chiffon with Flirty Corset-Style Back Lacing", nxs4)
nxs4 <- gsub("PERSONALTITLE16", "Striking and Comfortable Floor Length Gown", nxs4)
nxs4 <- gsub("PERSONALTITLE17", "Scalloped Lacy Looking Beaded Shoulders in Little Black Sarong Style Dress", nxs4)
nxs4 <- gsub("PERSONALTITLE18", "Scalloped and Sheer Beaded Shoulders in Little Black Sarong Style Dress", nxs4)
nxs4 <- gsub("PERSONALTITLE19", "Faux-Wrapped Skirt and Comfy Bodice", nxs4)
nxs4 <- gsub("PERSONALTITLE20", "Exciting Jeweled Cuffs with Split Sleeves with Sexy Skirt", nxs4)
nxs4 <- gsub("PERSONALTITLE21", "Scalloped Sheer Beaded Shoulders in Plum Sarong Style Dress", nxs4)
nxs4 <- gsub("PERSONALTITLE22", "Comfortably Draped Bodice with Split Sleeves and Sensible Skirt", nxs4)
nxs4 <- gsub("PERSONALTITLE23", "Cold Shoulder Comfy Cocktail", nxs4)
nxs4 <- gsub("PERSONALTITLE24", "Crisscrossed Neckline in Comfy Stretch Jersey", nxs4)
nxs4 <- gsub("PERSONALTITLE25", "Cool Cerise Color and Crystals on Long Regal Gown", nxs4)
nxs4 <- gsub("PERSONALTITLE26", "Ring-Necked Bodice with Shiny Lengthy Skirt", nxs4)

nxs <- nxs4

# save to txt file. copy the txt text to wordpress and review it.
writeLines(nxs, "nxscode.txt")


################################################################################
# GENERAL INFO AND FOR TROUBLESHOOTING:
################################################################################
troubleshoot <- function () {
        print(paste0("nrow(dresses): ", nrow(dresses)))
        print(paste0("length(name): ", length(name), ". NAs: ", sum(is.na(name))))
        print(paste0("length(dressurl): ", length(dressurl), ". NAs: ", sum(is.na(dressurl))))
        print(paste0("length(photo1url): ", length(photo1url), ". NAs: ", sum(is.na(photo1url))))
        print(paste0("length(photo1suffix): ", length(photo1suffix), ". NAs: ", sum(is.na(photo1suffix))))
        print(paste0("length(photo2suffix): ", length(photo2suffix), ". NAs: ", sum(is.na(photo2suffix))))
        print(paste0("length(price): ", length(price), ". NAs: ", sum(is.na(price))))
        print(paste0("length(nxs) (should be 2x dresses, for blank lines): ", length(nxs)))
}
troubleshoot()
