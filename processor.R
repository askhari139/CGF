library(tidyverse)
library(stringr)

cgf_merger <- function(cgf_file, migration_file, folder = "Labelled"){
    ### Inputs ----
    # 1. cgf_file - The path to the file containing cgf data
    # 2. migration_file - The path to the file containing migrating cell data
    # 3. folder - The path to the folder where the merged file will be saved. 
    
    fileName <- cgf_file %>% str_remove(".*/") %>% str_remove(".csv")
    
    # Read the data ----
    cgf_data <- read.csv(cgf_file, stringsAsFactors = F)
    coord_data <- read.csv(migration_file, stringsAsFactors = F)
    
    # The count of the number of cells tracked extracted from the file ----
    n_cells <- coord_data[1,1] %>% str_split(" ") %>% unlist %>% 
        magrittr::extract2(length(.)) %>% as.integer
    
    # Cleaning the coordinate data ----
    proc_cf <- coord_data %>% 
        filter(!row_number() %in% 1) %>% # remove first row
        select(-contains("Flag")) %>% # remove columns with "Flag"
        mutate_if(is.numeric, round) %>% # Round the coordinates to integers
        mutate(Slice = Frame) %>% select(-Frame) %>% # rename the Frame column to Slice to facilitate merging with cgf file
        mutate(Slice = as.integer(as.character(Slice))) # Converting Slice from factor to integer
    
    # Resize the coordinate file so that we have 3 columns: Slice, X, Y ----
    resize_cf <- proc_cf %>% select(contains("X")) %>% # Get the X coordinates
        unlist %>% # Vectorize them
        cbind.data.frame( # Combine column wise with 
            proc_cf %>% select(contains("Y")) %>% 
                unlist # Vectorized Y coordinates
        ) %>% 
        cbind.data.frame(rep(proc_cf$Slice,n_cells)) %>% # adding a slice column. 
        # The number of rows corresponding to each slice will be the number of cells tracked in total, hence we repeat slice column n_cells times
        set_names(c("X", "Y", "Slice")) %>%
        mutate(Status = "M") %>% filter(!is.na(X), !is.na(Y))
    
    
    # merge with cgf ----
    df <- cgf_data %>% mutate(X = round(X), Y = round(Y)) %>% # round the coordinates in CGF file
        merge(resize_cf, by = c("Slice", "X", "Y"), all = T)
    df$Status[is.na(df$Status)] <- "NM"
    if (nrow(df) != nrow(cgf_data))
    {
        print(fileName)
        print(nrow(resize_cf))
        print(nrow(df) - nrow(cgf_data))
    }
    if (!dir.exists(folder))
        dir.create(folder)
    write.csv(df, paste0(folder, "/", fileName, ".csv"), row.names = F) # save the data in a different folder "Labelled"
    return(df)
}

# set working directory to where you want to save the merged files.
cgf_folder <- "Whole Population Data" #path to Folder containing cgf files
coordinate_folder <- "Migrating Cell Co-ordinates" # path to Folder containing coordinates
setwd(cgf_folder)
file_list <- list.files(".", pattern = "\\d.csv") # Get all the file names
setwd("..")

merger <- sapply(file_list, function(x){
    cgf_file <- paste0(cgf_folder, "/", x)
    migration_file <- paste0(coordinate_folder, "/", "Migration_", x)
    cgf_merger(cgf_file, migration_file)
})

