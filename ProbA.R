# Required libries
library(pixmap)

# Encoder
secretencoder <- function(imagefilename, msg, startpix, stride, consec=NULL)
{
    # Check if file exists
    if (!file.exists(imagefilename)) {
        print("File does not exist.")
        stop()
    }
    
    # Read image
    img <- read.pnm(imagefilename)
    grey_img <- img@grey
    
    # Split string and convert to doubles
    # Vectorized since utf8ToInt() returns a vector if string is inputed
    str_val <- as.double(utf8ToInt(msg) / 128)
    str_val <- c(str_val, 0)
    
    # Encode
    j <- 1
    for (i in seq(startpix, startpix + (stride * length(str_val)), stride)) {
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
    img <- read.pnm(imagefilename)
    msg_mat <- img@grey
    
    # Does one loop iteration outside of while loop before starting loop
    idx <- startpix
    val <- msg_mat[idx]
    msg_vals <- val # Start vector with numeric values from the matrix
    
    # Keep looping until 0 is found (0 is null terminator)
    while (val != 0) {
        idx <- idx + stride
        val <- msg_mat[idx]
        msg_vals <- c(msg_vals, val)
    }
    
    
    # Revert from floats back to ints for Utf8 conversion
    # While loop which picks up message takes the null terminator,
    # so I remove it by doing msg_vals[,-1]
    msg_vals <- as.integer(msg_vals[-length(msg_vals)] * 128)
    
    # FLOATING POINT BUG? FIX LATER
    msg <- intToUtf8(msg_vals, multiple=TRUE)
    print(msg)
    
    return(msg)
}