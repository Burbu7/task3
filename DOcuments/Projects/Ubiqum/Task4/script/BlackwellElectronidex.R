BlackwellProducts <- read.csv("C:/Users/Burbu/Downloads/existingproductattributes2017.csv")

BlackwellProducts$Margin <- BlackwellProducts$ProfitMargin * BlackwellProducts$Price
BlackwellProducts$Revenue <- BlackwellProducts$Price * BlackwellProducts$Volume
BlackwellProducts$Benefits <- BlackwellProducts$ProfitMargin * BlackwellProducts$Price * BlackwellProducts$Volume

Averagetransaction <- sum(BlackwellProducts$Revenue)/sum(BlackwellProducts$Volume)
Averagetransaction
sum(BlackwellProducts$Revenue)
BlackwellProducts$Revenue
sum(BlackwellProducts$Volume)

PCs <- subset(BlackwellProducts, BlackwellProducts$ProductType == "PC")
mean(PCs$Benefits)

Laptops <- subset(BlackwellProducts, BlackwellProducts$ProductType == "Laptop")
mean(Laptops$Benefits)

Accesories <- subset(BlackwellProducts, BlackwellProducts$ProductType == Accesories")
mean(Accesories$Benefits)

Game <- subset(BlackwellProducts, BlackwellProducts$ProductType == "GameConsole")
mean(Game$Benefits)

Tablet <- subset(BlackwellProducts, BlackwellProducts$ProductType == "Tablet")
mean(Tablet$Benefits)

Software <- subset(BlackwellProducts, BlackwellProducts$ProductType == "Software")
mean(Software$Benefits)

Printer <- subset(BlackwellProducts, BlackwellProducts$ProductType == "Printer")
mean(Printer$Benefits)

BlackwellProducts <- subset(BlackwellProducts, BlackwellProducts$ProductType != "PC" & BlackwellProducts$ProductType != "Laptop" & BlackwellProducts$ProductType != "Printer" & BlackwellProducts$ProductType != "Tablet")
BlackwellProducts$ProductType


Averagetransaction <- sum(BlackwellProducts$Revenue)/sum(BlackwellProducts$Volume)
Averagetransaction
sum(BlackwellProducts$Revenue)
BlackwellProducts$Revenue
sum(BlackwellProducts$Volume)
