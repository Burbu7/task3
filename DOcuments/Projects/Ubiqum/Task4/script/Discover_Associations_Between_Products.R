# Discover Associations Between Products ####
#Load packages ####
#Load the required packages for this task
library("Matrix")
library("grid")
library("arules")
library("arulesViz")
library("devtools")
library("usethis")
source_gist(id='706a28f832a33e90283b')

#Friendly reminder ####
#View association rules as interesting discoveries or sources of insight, NOT as predictions.

#Import the data set ####
#I will call the csv file with the last month transactions from my PC, but in a different way because it have to read transactions

EMT <- read.transactions("C:/Users/Burbu/Documents/Projects/Ubiqum/Task4/data/ElectronidexTransactions2017.csv", 
                         format = c("basket", "single"), rm.duplicates = FALSE, sep = ",", header = FALSE) #I'm indicating that the csv are separete by commas, to basket the transactions and don't remove duplicates
#EMT = Electronidext Month Transactions

#Remove empty rows####
Emptyrows <- which(size(EMT) ==0)
Emptyrows
EMT <- EMT[-Emptyrows]
#There's also that way to remove the empty rows: EMT <- subset(EMT, size(EMT) > 0)


#Explore transactional data ####
#Now, I will explore and analize the transacional data in its new format
str(EMT) #see the structure
inspect(EMT) #for view all transactions
length(EMT) #it shows you the number of transactions
options(max.print = 10000)
size(EMT) #it brings you the number of items per transaction
LIST(EMT) #Lists the transactions by conversion
itemLabels(EMT) #shows you the item labels (which item is each column)
#Label items are in alphabetic order. That's to change the labeling by product type later
sum(size(EMT)) #There are 43104 items bought in our dataset (we count Keyboard+Mouse Combo as 1 item)

#Visualize the Dataset ####

#how visualize the items within my dataset
itemFrequencyPlot(EMT, type = "absolute", topN=15)
#we need to fix and improve this plot. There's a TIP in the plan of attack. We only will show the highest frequencies? find a way
#we see that iMac is the TOP ranked product. We observe that Apple products are the most bought (4 items in the TOP10)
#7 of the TOP15 products are Laptops or Desktop Camputers. We can find a pattern here. 


#how visualize the transactions within my dataset
image(sample(EMT, 100),  xlab = "Items (Columns)", 
      ylab = "Transactions (Rows)")
#Here we have an example of 100 random rows of transactions in our dataset

#I want to visualize the transactions with more than 15 items ("big transactions")
EMTmoreitems <- subset(EMT, size(EMT) >= 15)
inspect(EMTmoreitems)
image(EMTmoreitems,  xlab = "Items (Columns)", 
      ylab = "Transactions (Rows)")
sum(size(EMTmoreitems))
#The transactions with more than 15 items (188 transacions, the 1,91% of the dataset) suppose the 7.58% of the items bought (3268 of 43104). 


#I also want to visualize the transactions with more than 10 items ("huge transactions")
EMThugetransactions <- subset(EMT, size(EMT) >= 10)
inspect(EMThugetransactions)
image(EMThugetransactions,  xlab = "Items (Columns)", 
      ylab = "Transactions (Rows)")
sum(size(EMThugetransactions))
#The transactions with more than 10 items (874 transacions, the 8,89% of the dataset) suppose the 25,65% of the items bought (11056 of 43104). 
#I start from the idea that "normal customers (BTC)" normally don't buy more than 10 items, but this kind of transactions are a quarter of our sales. 
#This should indicate that part of the transactions with a lot of items are made by companies (BTB). 


#First iteration with all data####

#I do a first interation to find some rules with the whole dataset
FirstRules <- apriori(EMT, 
                 parameter = list(supp = 0.1, conf = 0.8))
summary(FirstRules)


#I have not found rules, so I changed the parameters and executed again

FirstRules <- apriori(EMT, 
                 parameter = list(supp = 0.025, conf = 0.3))
summary(FirstRules)
inspect(FirstRules)
#Finally, I found 24 rules

#Here I can see the rules sorted by confidence
inspect(sort(FirstRules, by = "confidence"))
#Here I can see the rules sorted by support
inspect(sort(FirstRules, by = "support"))
#Here I can see the rules sorted by lift
inspect(sort(FirstRules, by = "lift"))


#I'll try to show only the TOP 10 rules by confidence
first.top.support <- sort(FirstRules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(first.top.support, 10))

#I noticed that only one rule does not involve iMac and HP Laptop ({Samsung Monitor}=>{CYBERPOWER Gamer Desktop})
#So, let's see the rules sorted by item to help in our analysis
#Specific item's rules
FirstiMacRules <- subset(FirstRules, items %in% "iMac")
FirstHPLaptopRules <- subset(FirstRules, items %in% "HP Laptop")
inspect(FirstiMacRules)
inspect(FirstHPLaptopRules)

#Here I can see the frequencies of that items in our dataset. 
sum(size(EMT[,70])) 
sum(size(EMT[,64]))
sum(size(EMT[,35]))

#As I saw in the last codelines (and in the item frequency plot), these rules only involve TOP3 bestsellers products of our dataset.

#Right now, I know that the rules found didn't give the desired information to analyze Electronidex in the right way
#Anyway, I will finish this iteration finding if is there any redundant rule and ploting our results

#Redundant rules 
is.redundant(FirstRules) #There isn't redundant rules

#Plot rules

#scatter box visualization
plot(FirstRules, jitter = 0)
#?plot to see the parameters I can add. 

#graph method
plot(FirstRules, method = "graph", control = list(type = "items"))

#To summerize, I have found some rules with a high value of lift (important rules), but low confidence and support.
#The information adquired is not relevant to make the business anaalysis needed. 
#I need to make changes in the dataset to improve the analysis. 

#Make new categories - Product Type####

#I will analyze the associations between product types, because it didn't work in a more specific category like Products. 

#I will investigate accesories category 
?itemFrequencyPlot
itemFrequencyPlot(EMT[, c(26,33, 79, 97)], type = "absolute")

#We have 17 predetermined categories. To adapt these to Blackwell's portfolio, I split "Accesories" in 3 new categories: "Sofware (Microsoft Office Home and Student 2016)", "Computer Games (Computer Game)", and "Accesories (Belkin Mouse Pad & Large Mouse Pad)"


EMT@itemInfo #Here I observe which are the Items given
#I created a vector (length=125), in which every Item is labeled by product type.
#I could do that because they were sort alphabetically.

Categories <- c("Harddrive", "Harddrive", "Mice", "Harddrive", "Harddrive", "Laptop", "Desktop", 
                "Monitor", "ComputerHeadphones", "Laptop", "Monitor", "ActiveHeadphones", "ActiveHeadphones", 
                "Laptop", "Laptop", "Keyboard", "SmartHomeDevices", "Keyboard", "Keyboard", "Monitor", "Laptop", 
                "Desktop", "Monitor", "ComputerCords", "Keyboard", "Accesories", "Speakers", "Printer", "Toner", 
                "Speakers", "Toner", "Printer", "Computer Games", "Speakers", "Desktop", "Desktop", "Desktop", 
                "ComboKeyboardMice", "Laptop", "Monitor", "Keyboard", "Speakers", "Printer", "Toner", 
                "ComboKeyboardMice", "Laptop", "Toner", "Printer", "ComputerCords", "ComputerCords", "Tablet", 
                "SmartHomeDevices", "MonitorStand", "Mice", "Mice", "SmartHomeDevices", "MonitorStand", "MonitorStand", 
                "ComputerCords", "ComputerCords", "MonitorStand", "Toner", "Desktop", "Laptop", "Monitor", "Laptop", 
                "Keyboard", "Mice", "Printer", "Desktop", "Desktop", "Tablet", "Tablet", "ComputerCords", "Speakers", 
                "ComputerHeadphones", "Tablet", "ComputerHeadphones", "Accesories", "Desktop", "Monitor", "Laptop", 
                "Mice", "ComputerHeadphones", "ComboKeyboardMice", "Keyboard", "ComboKeyboardMice", "ComboKeyboardMice", 
                "ComboKeyboardMice", "Speakers", "ComputerHeadphones", "Keyboard", "Mice", "Speakers", "Mice", 
                "ComputerHeadphones", "Software", "ComboKeyboardMice", "ComboKeyboardMice", "ActiveHeadphones", 
                "MonitorStand", "ActiveHeadphones", "ActiveHeadphones", "ComputerHeadphones", "ComputerHeadphones", 
                "ActiveHeadphones", "Mice", "ComboKeyboardMice", "Keyboard", "Speakers", "SmartHomeDevices", 
                "ComputerCords", "Tablet", "Monitor", "Monitor", "Harddrive", "Mice", "SmartHomeDevices", "Speakers", 
                "ComputerCords", "ComputerCords", "Monitor", "Mice", "ComputerHeadphones", "ComputerHeadphones")

EMTpt <- aggregate(EMT, Categories) #I changed the Items, substituting it per product type
EMTpt@itemInfo
str(EMTpt) 
inspect(EMTpt)


itemFrequencyPlot(EMTpt, type = "absolute")
#Here I can see how the categories are distributed

#Second iteration with Product Type ####

SecondRules <- apriori(EMTpt, 
                      parameter = list(supp = 0.1, conf = 0.8))
summary(SecondRules)
inspect(SecondRules)

#I have found only one rule, so I changed the parameters and executed again
#I also noticed that the rule is irrelevant (obvious)

SecondRules2 <- apriori(EMTpt, 
                      parameter = list(supp = 0.025, conf = 0.8))
summary(SecondRules2)
inspect(SecondRules2)
#Finally, I found 47 rules

#Here I can see the rules sorted by confidence
inspect(sort(SecondRules2, by = "confidence"))
#Here I can see the rules sorted by support
inspect(sort(SecondRules2, by = "support"))
#Here I can see the rules sorted by lift
inspect(sort(SecondRules2, by = "lift"))

#Redundant rules 
is.redundant(SecondRules2) #There isn't redundant rules

#I'll try to show only the TOP 10 rules by confidence
second.top.support <- sort(SecondRules2, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(second.top.support, 10))

#The rules show that people should buy desktops with a lot of other accesories. Among these we can find the Laptops.
#That isn't a normal customer buy, That's a company buy (B2B)


#So, let's see the rules sorted by item to help in our analysis
#Specific product types's rules
DesktopRules <- subset(SecondRules2, items %in% "Desktop")
LaptoppRules <- subset(SecondRules2, items %in% "Laptop")
inspect(DesktopRules)
inspect(LaptoppRules)
#I should iterate again changing the parameters of the rules, because the rules are'nt good enough

#Split the dataset - B2B&B2C ####

EMTdf <- t(as.matrix(EMT@data))
EMTdf <- as.data.frame(EMTdf)
str(EMTdf)
colnames(EMTdf) <- Categories
colnames(EMTdf)
 for (i in 1:ncol(EMTdf)){
   EMTdf[,i] <- as.integer(EMTdf[,i])
 }


transactionValue <- c()
transactionValue$Accesories <- NA
transactionValue$ActiveHeadphones <- NA
transactionValue$ComboKeyboardMice <- NA
transactionValue$ComputerGames <- NA
transactionValue$ComputerCords <- NA
transactionValue$ComputerHeadphones <- NA
transactionValue$Desktop <- NA
transactionValue$Harddrive <- NA
transactionValue$Keyboard <- NA
transactionValue$transactionValue$Laptop <- NA
transactionValue$Mice <- NA
transactionValue$Monitor <- NA
transactionValue$MonitorStand <- NA
transactionValue$Printer <- NA 
transactionValue$SmartHomeDevices <- NA
transactionValue$Software <- NA
transactionValue$Speakers <- NA
transactionValue$Tablet <- NA
transactionValue$Toner <- NA
#probar con un loop


for(j in 1:nrow(EMTdf)){
  
  
  AccesoriesCount <-0
  ActiveHeadphonesCount <-0
  ComboKeyboardMiceCount <-0
  ComputerGamesCount <-0
  ComputerCordsCount <-0
  ComputerHeadphonesCount <-0
  DesktopCount <-0
  HarddriveCount <-0
  KeyboardCount <-0
  LaptopCount <-0
  MiceCount <-0
  MonitorCount <-0
  MonitorStandCount <-0
  PrinterCount <-0
  SmartHomeDevicesCount <-0
  SoftwareCount <-0
  SpeakersCount <-0
  TabletCount <-0
  TonerCount <-0
  
for( i in 1:ncol(EMTdf)){
  
  if(EMTdf[j,i]==1){
  
  categoryName <- colnames(EMTdf[i])
  
  if(categoryName == "Accesories"){
    
    AccesoriesCount =  AccesoriesCount +1 }
    
  if(categoryName == "ActiveHeadphones"){
    
    ActiveHeadphonesCount= ActiveHeadphonesCount+1
  }
  
  if(categoryName == "ComboKeyboardMice"){
    
    ComboKeyboardMiceCount= ComboKeyboardMiceCount+1
  }
  
  if(categoryName == "Computer Games"){
    
    ComputerGamesCount= ComputerGamesCount+1
  }
  
  if(categoryName == "ComputerCords"){
    
    ComputerCordsCount= ComputerCordsCount+1
  }
  
  if(categoryName == "ComputerHeadphones"){
    
    ComputerHeadphonesCount= ComputerHeadphonesCount+1
  }
  
  if(categoryName == "Desktop"){
    
    DesktopCount= DesktopCount+1
  }
  
  if(categoryName == "Harddrive"){
    
    HarddriveCount= HarddriveCount+1
  }
  
  if(categoryName == "Keyboard"){
    
    KeyboardCount= KeyboardCount+1
  }
  
  if(categoryName == "Laptop"){
    
    LaptopCount = LaptopCount +1 }
  
  if(categoryName == "Mice"){
    
    MiceCount= MiceCount+1
  }
  
  if(categoryName == "Monitor"){
    
    MonitorCount= MonitorCount+1
  }
  
  if(categoryName == "MonitorStand"){
    
    MonitorStandCount= MonitorStandCount+1
  }
  
  if(categoryName == "Printer"){
    
    PrinterCount= PrinterCount+1
  }
  
  if(categoryName == "SmartHomeDevices"){
    
    SmartHomeDevicesCount= SmartHomeDevicesCount+1
  }
  
  if(categoryName == "Software"){
    
    SoftwareCount= SoftwareCount+1
  }
  
  if(categoryName == "Speakers"){
    
    SpeakersCount= SpeakersCount+1
  }
  
  if(categoryName == "Tablet"){
    
    TabletCount= TabletCount+1
  }
  
  if(categoryName == "Toner"){
    
    TonerCount= TonerCount+1
  }
  
  
  
}
  
  transactionValue$Accesories[j] <- AccesoriesCount
  transactionValue$ActiveHeadphones[j] <- ActiveHeadphonesCount
  transactionValue$ComboKeyboardMice[j] <- ComboKeyboardMiceCount
  transactionValue$ComputerGames[j] <- ComputerGamesCount
  transactionValue$ComputerCords[j] <- ComputerCordsCount
  transactionValue$ComputerHeadphones[j] <- ComputerHeadphonesCount
  transactionValue$Desktop[j] <- DesktopCount
  transactionValue$Harddrive[j] <- HarddriveCount
  transactionValue$Keyboard[j] <- KeyboardCount
  transactionValue$transactionValue$Laptop[j] <- LaptopCount
  transactionValue$Mice[j] <- MiceCount
  transactionValue$Monitor[j] <- MonitorCount
  transactionValue$MonitorStand[j] <- MonitorStandCount
  transactionValue$Printer[j] <- PrinterCount
  transactionValue$SmartHomeDevices[j] <- SmartHomeDevicesCount
  transactionValue$Software[j] <- SoftwareCount
  transactionValue$Speakers[j] <- SpeakersCount
  transactionValue$Tablet[j] <- TabletCount
  transactionValue$Toner[j] <- TonerCount


}
}

transactionValue <- as.data.frame(transactionValue)


B2B <- EMT[which(transactionValue$Accesories>2 | transactionValue$Mice>2 | transactionValue$Harddrive>2 | transactionValue$Monitor>2| transactionValue$ComputerHeadphones>2|
                        transactionValue$ActiveHeadphones>2|transactionValue$Keyboard>2|transactionValue$SmartHomeDevices>2|transactionValue$ComputerCords>2|transactionValue$Speakers>2|
                        transactionValue$Printer>1|transactionValue$Toner>4|transactionValue$ComboKeyboardMice>2|transactionValue$Tablet>2|transactionValue$MonitorStand>2|
                        transactionValue$Software>1|transactionValue$Laptop>1|transactionValue$Desktop>1|transactionValue$ComboKeyboardMice==1&transactionValue$Mice==1|
                        transactionValue$ComboKeyboardMice==1&transactionValue$Keyboard==1|transactionValue$Desktop==1&transactionValue$Laptop==1)]
B2C <- EMT[-which(transactionValue$Accesories>2 | transactionValue$Mice>2 | transactionValue$Harddrive>2 | transactionValue$Monitor>2| transactionValue$ComputerHeadphones>2|
                        transactionValue$ActiveHeadphones>2|transactionValue$Keyboard>2|transactionValue$SmartHomeDevices>2|transactionValue$ComputerCords>2|transactionValue$Speakers>2|
                        transactionValue$Printer>1|transactionValue$Toner>4|transactionValue$ComboKeyboardMice>2|transactionValue$Tablet>2|transactionValue$MonitorStand>2|
                        transactionValue$Software>1|transactionValue$Laptop>1|transactionValue$Desktop>1|transactionValue$ComboKeyboardMice==1&transactionValue$Mice==1|
                        transactionValue$ComboKeyboardMice==1&transactionValue$Keyboard==1|transactionValue$Desktop==1&transactionValue$Laptop==1)]



#Third iteration with all data####

#I do a first interation to find some rules with the new datasets (B2B and B2C)
B2BRules <- apriori(B2B, 
                      parameter = list(supp = 0.1, conf = 0.8))

B2CRules <- apriori(B2C, 
                    parameter = list(supp = 0.1, conf = 0.8))
summary(B2BRules)
summary(B2CRules)

#I have not found rules, so I changed the parameters and executed again

B2BRules <- apriori(B2B, 
                    parameter = list(supp = 0.005, conf = 0.8))

B2CRules <- apriori(B2C, 
                    parameter = list(supp = 0.005, conf = 0.5))
summary(B2BRules)
summary(B2CRules)

#Finally, I found 24 rules

#Here I can see the rules sorted by confidence
inspect(sort(FirstRules, by = "confidence"))
#Here I can see the rules sorted by support
inspect(sort(FirstRules, by = "support"))
#Here I can see the rules sorted by lift
inspect(sort(FirstRules, by = "lift"))


#I'll try to show only the TOP 10 rules by confidence
first.top.support <- sort(FirstRules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(first.top.support, 10))

#I noticed that only one rule does not involve iMac and HP Laptop ({Samsung Monitor}=>{CYBERPOWER Gamer Desktop})
#So, let's see the rules sorted by item to help in our analysis
#Specific item's rules
FirstiMacRules <- subset(FirstRules, items %in% "iMac")
FirstHPLaptopRules <- subset(FirstRules, items %in% "HP Laptop")
inspect(FirstiMacRules)
inspect(FirstHPLaptopRules)

#Here I can see the frequencies of that items in our dataset. 
sum(size(EMT[,70])) 
sum(size(EMT[,64]))
sum(size(EMT[,35]))

#As I saw in the last codelines (and in the item frequency plot), these rules only involve TOP3 bestsellers products of our dataset.

#Right now, I know that the rules found didn't give the desired information to analyze Electronidex in the right way
#Anyway, I will finish this iteration finding if is there any redundant rule and ploting our results

#Redundant rules 
is.redundant(FirstRules) #There isn't redundant rules

#Plot rules

#scatter box visualization
plot(FirstRules, jitter = 0)
#?plot to see the parameters I can add. 

#graph method
plot(FirstRules, method = "graph", control = list(type = "items"))

#To summerize, I have found some rules with a high value of lift (important rules), but low confidence and support.
#The information adquired is not relevant to make the business anaalysis needed. 
#I need to make changes in the dataset to improve the analysis. 






