## code to pull eDNA output from the eDNA-for-MPAs repository. 
## note that this requires that the output in the specified directories are up to date with the lastest changes pulled from github

sync_folders <- function(src_dir, dest_dir) {
  # List all files in the source directory recursively
  source_files <- list.files(src_dir, recursive = TRUE, full.names = TRUE)
  
  # Loop through each file in the source directory
  for (file in source_files) {
    # Get the relative path (remove source directory from the full file path)
    rel_path <- gsub(normalizePath(src_dir), "", normalizePath(file), fixed = TRUE)
    
    # Construct the destination file path
    dest_file <- file.path(dest_dir, rel_path)
    
    # Check if the destination file exists
    if (!file.exists(dest_file)) {
      # If the file doesn't exist in the destination folder, copy it
      dir.create(dirname(dest_file), recursive = TRUE, showWarnings = FALSE)  # Ensure subdirectories are created
      file.copy(file, dest_file)
      cat("Copied new file:", file, "\n")
    } else {
      # If the file exists, compare modification times and update if necessary
      if (file.info(file)$mtime > file.info(dest_file)$mtime) {
        file.copy(file, dest_file, overwrite = TRUE)
        cat("Updated file:", file, "\n")
      }
    }
  }
}

sync_changed_files <- function(src_dir, dest_dir) {
  # Normalize source and destination directories to avoid path issues
  src_dir <- normalizePath(src_dir, winslash = "/")
  dest_dir <- normalizePath(dest_dir, winslash = "/")
  
  # List all files in source and destination directories (recursive)
  source_files <- list.files(src_dir, recursive = TRUE, full.names = TRUE)
  destination_files <- list.files(dest_dir, recursive = TRUE, full.names = TRUE)
  
  # Function to get relative path for comparison (remove base path)
  relative_path <- function(files, base_dir) {
    gsub(paste0("^", normalizePath(base_dir, winslash = "/")), "", normalizePath(files, winslash = "/"))
  }
  
  # Get relative paths for source and destination files
  rel_source_files <- relative_path(source_files, src_dir)
  rel_dest_files <- relative_path(destination_files, dest_dir)
  
  # Identify files that are new or updated
  changed_files <- c()  # To store files that have changed
  
  for (i in seq_along(source_files)) {
    rel_path <- rel_source_files[i]
    
    # Check if the file exists in the destination
    dest_file_path <- file.path(dest_dir, rel_path)
    
    if (!file.exists(dest_file_path)) {
      # File doesn't exist in destination, so it's a new file
      changed_files <- c(changed_files, source_files[i])
    } else {
      # File exists in destination, compare modification times
      if (file.info(source_files[i])$mtime > file.info(dest_file_path)$mtime) {
        # Source file is more recent, so it's an updated file
        changed_files <- c(changed_files, source_files[i])
      }
    }
  }
  
  # Copy changed files to destination
  for (file in changed_files) {
    # Determine the destination path
    rel_path <- gsub(paste0("^", normalizePath(src_dir, winslash = "/")), "", normalizePath(file, winslash = "/"))
    dest_file_path <- file.path(dest_dir, rel_path)
    
    # Ensure the destination directory exists
    dir.create(dirname(dest_file_path), recursive = TRUE, showWarnings = FALSE)
    
    # Copy the file
    file.copy(file, dest_file_path, overwrite = TRUE)
    cat("Copied file:", file, "to", dest_file_path, "\n")
  }
  
  if (length(changed_files) == 0) {
    cat("No files to copy. The destination folder is up to date.\n")
  }
}


#To do the intial sync
sync_folders("c:/Users/stanleyr/Documents/Github/eDNA-for-MPAs/data/Musquash/", 
             "c:/Users/stanleyr/Documents/Github/musquash_mpa/data/edna/")

#Run after any new 'pulls' from https://github.com/NickJeff13/eDNA-for-MPAs
sync_changed_files("c:/Users/stanleyr/Documents/Github/eDNA-for-MPAs/data/Musquash/", 
                   "c:/Users/stanleyr/Documents/Github/musquash_mpa/data/edna/")
