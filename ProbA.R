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
    str_val <- utf8ToInt(msg) / 128
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
    size <- length(msg_mat)
    
    idx_arr <- seq(startpix, size, stride)
    msg_vals <- msg_mat[idx_arr]
    nul_idx <- which(msg_vals == 0)
    
    # nul_idx can have more than one index, so since everything
    # in R is a vector, I can access the first instance of 0 reguardless
    # of how many 0's I pick up
    print(nul_idx)
    msg_vals <- msg_vals[1:(nul_idx[1] - 1)]
    msg_vals <- msg_vals * 128
    
    msg <- intToUtf8(as.integer(msg_vals), multiple=TRUE)
    print(msg)
}