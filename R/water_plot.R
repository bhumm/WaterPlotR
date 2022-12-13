# WaterPlotR
#
# This is a function that takes a directory (as a string) with bedgraphs and plots them all on waterfall plots.
# Each line is normalized to itself, rather than the group. All bedgraph files are assumed to have a header.
# The output is a single .png file.
#
# Dependencies: ggplot2, dplyr
#
# Author: blake hummer
# Date: 2022-12-13
# Report all bugs to: hummer.blake@gmail.com


water_plot <- function(y) {
  toString(y)
  filesToProcess <- list.files(y, pattern = "*.bedgraph|*.bedGraph", full.names = TRUE) # List files to process
  listOfFiles <- lapply(filesToProcess, function(x) read.table(x, header = FALSE, skip = 1)) # Read in files

  names(listOfFiles) <- filesToProcess # Add names of the files to the list

  # Loop over dfs in the list and modify
  listOfFiles <- lapply(names(listOfFiles), function(z) {
    listOfFiles[[z]] <- listOfFiles[[z]] %>% mutate("MeanNorm" = V4/mean(listOfFiles[[z]]$V4)) # Add in mean normalization for each df in the list
    dfname <- strsplit(z, split = "_") # Split file name to get library name
    listOfFiles[[z]] <- listOfFiles[[z]] %>% mutate("LibraryName" = dfname[[1]][1]) # Add in library name as a column
    listOfFiles[[z]] <- listOfFiles[[z]][, c("MeanNorm", "LibraryName")] # Cut down to just columns of interest
    return(listOfFiles[[z]]) # Return dataframe
  })

  BigDF <- do.call("rbind", listOfFiles) # Bind all the rows from the dfs in the list into one dataframe

  ggplot(BigDF, aes(x=MeanNorm)) + geom_line(aes(y = 1 - ..y.., colour=LibraryName), stat='ecdf') + coord_cartesian(xlim = c(0,3)) # Plot with ecdf
  ggsave("Waterfall_plot.png", width = 6, height = 4) # Save out
}
