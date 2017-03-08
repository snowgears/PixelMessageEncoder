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
    
    # Check if image loaded properly
    if (is.null(img)) {
        warning("Image not loaded correctly!")
        stop()
    }
    
    # Check if stride is relatively prime to image size
    # Relatively prime means 2 ints have gcd(a, b) = 1.
    size <- ncol(grey_img) * nrow(grey_img)
    row <- nrow(grey_img)
    
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
        
        # Create a vector for consec
        consec_idx <- vector('numeric')
        written <- vector(length = size)
        
        # Loop checks 1) double writing
        #             2) if a pixel is within consec of another
        for (i in idx) {
            # Counter is needed to stop code if infinite loop
            # is encountered while trying to encode the message
            ctr <- 0
            while(written[i]) {
                i <- i + stride
                ctr <- ctr + stride
                if (i > size) # Loop back to beginning
                    i <- i - size
                if (ctr > size * stride) {
                    # While loop gives stride sweeps through the matrix before
                    # Exiting with a warning
                    warning("Exceeded stride number of sweeps through matrix")
                    warning("Not enough room for message!")
                    stop()
                }
            }
            
            consec_idx <- c(consec_idx, i)
            
            # Generate invalid positions
            l_bound <- seq(i, i - row * consec, -row)
            r_bound <- seq(i, i + row * consec, row)
            u_bound <- startpix:(i - consec)
            d_bound <- startpix:(i + consec)
            
            ## Handle edge cases
            l_bound <- ifelse(l_bound <= 0, l_bound + size, l_bound)
            r_bound <- ifelse(r_bound > size, r_bound - size, r_bound)
            u_bound <- ifelse(u_bound <= 0, u_bound + size, u_bound)
            d_bound <- ifelse(d_bound > size, d_bound - size, d_bound)
            
            # Append consec bounds to written array
            true_idx  <- c(l_bound, r_bound, u_bound, d_bound)
            written[true_idx] <- TRUE
        }
        
        # Set the values
        grey_img[consec_idx] <- str_val
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
    row <- nrow(msg_mat)
    
    # Check if image loaded properly
    if (is.null(img)) {
        warning("Image not loaded correctly!")
        stop()
    }
    
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
        
        # Vector which tracks positions which are read
        read <- vector(length=size)
        
        # Get the first value
        # Needed to do outside of loop in order to get things going
        idx <- startpix
        val <- msg_mat[idx]
        msg_vals <- val # Start vector with numeric values from the matrix
        
        while (val != 0) {
            # Generate invalid positions from the previous iteration
            l_bound <- seq(idx, idx - row * consec, -row)
            r_bound <- seq(idx, idx + row * consec, row)
            u_bound <- startpix:(idx - consec)
            d_bound <- startpix:(idx + consec)
            
            ## Handle edge cases
            l_bound <- ifelse(l_bound <= 0, l_bound + size, l_bound)
            r_bound <- ifelse(r_bound > size, r_bound - size, r_bound)
            u_bound <- ifelse(u_bound <= 0, u_bound + size, u_bound)
            d_bound <- ifelse(d_bound > size, d_bound - size, d_bound)
            
            # Append to read vector
            true_idx <- c(l_bound, r_bound, u_bound, d_bound)
            read[true_idx] <- TRUE
            
            # Advance idx to next location
            idx <- idx + stride 
            if (idx > size) 
                idx <- idx - size
            
            # If written is true, than we already read the pixel, so we
            # need to advance to the next stride
            while (read[idx]) {
                idx <- idx + stride 
                if (idx > size)
                    idx <- idx - size
            }
            
            # Append to message vector
            val <- msg_mat[idx]
            msg_vals <- c(msg_vals, val)
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