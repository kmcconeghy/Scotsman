FileNames <- readxl::read_excel('~\\GitHub\\Scotty\\data-raw\\FileNames.xlsx',
                                col_names=FALSE)

save(FileNames, file='~\\GitHub\\Scotty\\data\\FileNames.Rda')