tr <- read.transactions("C:/Users/Burbu/Documents/Projects/Ubiqum/Tutorial/Apples, Milk, Bread.csv", 
                         format = c("basket", "single"), rm.duplicates = FALSE, sep = ",")

pacman::p_load(shiny)

tr@itemInfo
inspect(tr)
Categories <- c("fruits", "bakery", "fruits", "liquids")
trcat <- aggregate(tr, Categories)
trcat@itemInfo



install.packages("devtools")
library("devtools")
library("usethis")
source_gist(id='706a28f832a33e90283b')
arulesApp(trcatdf)

#trcatdf <- as.matrix(trcat@data)

trcatdf <- as(trcat, "matrix")
trcatdf <- as.data.frame(trcatdf)
trcatdf$bakery <- as.numeric(trcatdf$bakery)
trcatdf$fruits <- as.numeric(trcatdf$fruits)
trcatdf$liquids <- as.numeric(trcatdf$liquids)
trcatdf$client <- ifelse(trcatdf$fruits==1&trcatdf$bakery==1, "prime", "get out")

#buscar funcion which
#|=or
#&=and

trcatdf$basket2 <- rowSums(trcatdf)
primeclient <- trcatdf[trcatdf$basket2 ==3]
