# Required libries
library(pixmap)


# Helper function for computing GCD using euclid's algorithm with recursion
# Algorithm based on pseudocode from
# CLRS, Introduction to Algorithms 3rd. Edition, pg. 935
euclid <- function(a, b)
{
    if (b == 0)
        return(a)
    else
        return(euclid(b, a %% b))
}


# Encoder
secretencoder <- function(imagefilename, msg, startpix, stride, consec=NULL)
{
    # Check if file exists
    if (!file.exists(imagefilename)) {
        warning("File does not exist!")
        stop()
    }
    
    # Read image
    img <- read.pnm(imagefilename)
    grey_img <- img@grey
    
    # Check if stride is relatively prime to image size
    # Relatively prime means 2 ints have gcd(a, b) = 1.
    size <- ncol(grey_img) * nrow(grey_img)
    #row <- nrow(grey_img)
    
    if (euclid(stride, size) != 1) {
        warning("Stride not relatively prime to image size!")
        stop()
    }
    
    # Split string and convert to doubles
    # Vectorized since utf8ToInt() returns a vector if string is inputed
    str_val <- as.double(utf8ToInt(msg)) / 128.0
    str_val <- c(str_val, 0) # Add null terminator
    
    # Generate a sequencce of indices to store into grey_img.
    # Note that the lengh(str_val) - 1 is needed becaise seq
    # generates [start, end], so I need 1 less
    max_idx <- startpix + (stride * length(str_val) - 1)
    idx <- seq(startpix, max_idx, stride)
    # ifelse() used to create a set of indices which wraps around the matrix
    idx <- ifelse(idx < size, idx, idx - size * floor(idx / size))
    
    if (is.null(consec)) {
        # Encode
        grey_img[idx] <- str_val
    } # consec is NULL
    else {
        # Check if consec is positive
        if (consec <= 0) {
            warning("Consec should be positive!")
            stop()
        }
        
        # Make a vector of bools to check if a specific pixel
        # has been overwritten or not
        written <- vector(length = size)
        new_idx <- vector('numeric')
        
        for (i in idx) {
            if (!written[i]) {
                new_idx <- c(new_idx, i)
                written[i] <- TRUE
            } # Check if element is written
            else {
                # Iterate to the unwritten value
                while(written[i])
                    i <- i + stride
                
                new_idx <- c(new_idx, i)
                written[i] <- TRUE
            }
        }
        
        # Set the values
        grey_img[new_idx] <- str_val
        
            #l_bound <- seq(i, i - row * consec, -row)
            #r_bound <- seq(i, i + row * consec, row)
            #u_bound <- startpix:(i - consec)
            #d_bound <- startpix:(i + consec)
            #
            #l_bound <- ifelse(l_bound <= 0, l_bound + size, l_bound)
            #r_bound <- ifelse(r_bound > size, r_bound - size, r_bound)
            #u_bound <- ifelse(u_bound <= 0, u_bound + size, u_bound)
            #d_bound <- ifelse(d_bound > size, d_bound - size, d_bound)
        
    } # consec is not NULL
    
    # Overwrite grey in image and save to disc
    img@grey <- grey_img
    write.pnm(img)
    
    # Returns a pixmapGrey class
    return(img)
}


# Decoder
secretdecoder <- function(imagefilename, startpix, stride, consec=NULL)
{
    # Check if file exists
    if (!file.exists(imagefilename)) {
        warning("File does not exist!")
        stop()
    }
    
    # Read image
    img <- read.pnm(imagefilename)
    msg_mat <- img@grey
    size <- ncol(msg_mat) * nrow(msg_mat)
    
    if (is.null(consec)) {
        # Does one loop iteration outside of while loop before starting loop
        idx <- startpix
        val <- msg_mat[idx]
        msg_vals <- val # Start vector with numeric values from the matrix
        
        # Keep looping until 0 is found (0 is null terminator)
        while (val != 0) {
            idx <- idx + stride
            # If statement to wrap around the image when needed
            if (idx > size) 
                idx <- idx - size
            
            val <- msg_mat[idx]
            msg_vals <- c(msg_vals, val)
        }
    } # consec is NULL
    else {
        # Check if consec is positive
        if (consec <= 0) {
            print("Consec should be positive!")
            stop()
        }
        
        written <- vector(length=size)
        
        # Get the first value
        idx <- startpix
        val <- msg_mat[idx]
        msg_vals <- val # Start vector with numeric values from the matrix
        written[idx] <- TRUE
        
        while (val != 0) {
            # Advance idx to next location
            idx <- idx + stride 
            if (idx > size) 
                idx <- idx - size
            
            # If written is true, than we already read the pixel, so we
            # need to advance to the next stride
            while (written[idx]) {
                idx <- idx + stride 
                if (idx > size)
                    idx <- idx - size
            }
            
            val <- msg_mat[idx]
            msg_vals <- c(msg_vals, val)
            written[idx] <- TRUE
        }
    } # consec is not NULL
    
    # Revert from floats back to ints for Utf8 conversion. Rounding is necessary
    # While loop which picks up message takes the null terminator,
    # so I remove it by doing msg_vals[,-1]
    msg_vals <- round(msg_vals[-length(msg_vals)] * 128)
    
    # Decode message.
    if (length(msg_vals) == 1) # Special case, encode 1 chracter
        msg <- intToUtf8(msg_vals)
    else
        msg <- intToUtf8(msg_vals, multiple=TRUE)
    
    # Returns a vector of chars
    return(msg)
}