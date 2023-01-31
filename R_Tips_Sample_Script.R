### How to find a specific word in a column, and what file it came from. 
    ## DataFrameName[which(DataFrameName$ColumnName == "WordToSearchFor"),]
## If you only want to return certain columns, include them after last comma
    ## DataFrameName[which(DataFrameName$ColumnName == "WordToSearchFor"),c(143,34)]
## In above example, 143 and 34 are the numbers of the columns you want to see.
## To identify columns, use colnames(DataFrameName)

## When you want to look at the entire dataset, use:
    ## View(DataFrameName)

## To check out a specific column while running through script, use:
    ## unique(DataFrameName$ColumnName)

## To save a file as a .csv, use:
    ## write.csv(DataFrameName, file.path(main_dir,"AdditionalPathAfterMain",
    ## paste0("FileName_", Sys.Date(), ".csv")),row.names = F)
