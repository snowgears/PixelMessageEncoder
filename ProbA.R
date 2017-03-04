library(pixmap)

# Encoder
secretencoder <- function(imagefilenmame, msg, startpix, stride, consec=NULL)
{
    # Check if file exists
    if (!file.exists(imagefilenmame)) {
        print("File does not exist.")
        stop()
    }
    
    # Read image
    img <- read.pnm(imagefilenmame)
    plot(img)
    grey_img <- img@grey
    
    # Split string and convert to doubles
    # Vectorized since utf8ToInt() returns a vector if string is inputed
    str_val <- utf8ToInt(msg) / 128
    
    # Encode
    j <- 1
    for (i in seq(startpix, startpix * length(str_val), stride)) {
        grey_img[i] <- str_val[j]
        j <- j + 1
    }
    
    # Overwrite grey in image and save to disc
    img@grey <- grey_img
    write.pnm(img)
    
    
    
    return(img)
}


# Decoder
secretdecoder <- function(imagefilename, startpix, stride, consec=NULL)
{
}