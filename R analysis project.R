A <- c("Phytoplankton", "Diatom", "Zooplankton") 
B<-c(1,-2,3)
C <- c(22,12,46)
#table sorting

Table <- data.frame(A, B, C) 
colnames(Table) <- c("Groups","Summer","Winter")
rownames(Table) <- c("Station 1","Station 2","Station 3")

Table$Autumn<-c(1.3,1.5,1.7) # $ is called hooks index

W<-Table[,c(2,3)]  #[row,column]and inside(),these are column number....this is basically done for selecting or sorting specific column
#similarly for rows
U<-Table[c(2,3),]
#selecting both columns and rows
OO<-Table[c(1,2),c(2,3)]
#Rearranging column
RR<-Table[,c(4,3,1,2)]

#Sum
Table$Total<-rowSums(Table[,c(2,3,4)],na.rm=T) #here rowSum is the command for summing up the row, na.rm=T(N/A remove) is used for blank entities so that it don't create any problem in summing up process

#Average
Table$Average<-rowMeans(Table[,c(2,3,4)],na.rm=T) #here rowMeans command for getting average/mean value

#subtraction of column values
Table$Winter_Autumn<-Table$Winter-Table$Autumn


#Deleting Columns####
#subset is indicating to the main table

E1= subset(Table, select = -c(Total,Average,Winter_Autumn) )

#separating column in new data table####
E2 = subset(Table, select = c(Total,Average,Winter_Autumn) )

#Replacing data
Table[2,3]=111 #here[row,column]


#default data loaded in R####
data()

#selecting random data set
View(mtcars) 

#exporting table from R
setwd("C:/Users/Admin/Desktop/R programming") #it's the directory which is used for locating files

write.csv(Table,"Table.csv", row.names=FALSE) #row.names=FALSE is used to remove row heading while exporting, similarly TRUE is used for vice-versa

#data filtering
library(dplyr) # we can call library by clicking tic on packages as well

BA=read.csv("Biol_data.csv")
View(BA)

BC<-filter(BA,Company=='A')
BY<-filter(BA,Level==c(1,4))
attach(BY)
NN<-BY[order(Level),]

library(devtools) #devtolls for selecting multiple characters
GG <- filter(BA, Company %in% c('A', 'B')) 

library(table1) #for showing summary in a table

table1::label(BA$Rice)
table1::label(BA$Oil)

table1::table1(~Rice+Oil|Level,data=BA)

library(ggplot2)
ggplot(BA,aes(Company,Rice))+  #aes stands for aesthetics and it is used for directing which variable will sit which axis.....aes(X axis,Y axis)
  geom_bar(stat="identity") #it's done for giving command of X and Y axis for the desired variable

#removing background of barplot

theme_set(theme_classic())

ggplot(BA,aes(Company,Rice))+
  geom_bar(stat ="identity",width = 0.9,fill="tomato2")+
  labs(title="Rice Abundances",
       subtitle = "Concentration in different companies",
       caption = "Source=Biol_Data",
       y="Rice(thousand mt",
       x="Name of the companies")+
  theme(axis.text.x = element_text(angle=65,vjust=0.6))+ #vjust means vertical adjustment, it describes distance of elements(A,B,C....) from X axis

  theme(axis.text.y = element_text(angle=65,hjust=0.6))  #hjust means horizontal adjustment, it describes distance of elements from y axis

setwd("C:/Users/Admin/Desktop/R programming")
getwd()
BA=read.csv("Biol_data.csv")
View(BA)
library(ggplot2)
#Dot plot as bar style
ggplot(BA,aes(Stations, Phytoplankton))+
  geom_point() #dot plot is essential to see all the values of a bar

#stacked bar plot-       this is to see data in comparison with another vatriable
ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="stack", stat="identity")

#Grouped Stacked Bar Plot
P <- ggplot(BA, aes(fill=Zones, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="stack", stat="identity")
P

#Multiple Grouped Bar#### 
ggplot(BA,aes(fill=Zones,y=Phytoplankton,x=Stations))+
  geom_bar(position="stack",stat="identity")+
  facet_wrap(~Depth)

ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~Zones)
# (%) Stacked Bar Plot

Q <- ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  geom_bar(position="fill", stat="identity")
Q

#Pie Plot

#Pie Plot####
R <- ggplot(BA, aes(fill=Zones, y=Phytoplankton,x="" )) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0)+ #for making circle
  theme_void()    #for removing background
R
#Circular Barplot
ggplot(BA, aes(fill=Depth, y=Phytoplankton,x=Stations)) +
  geom_bar(stat="identity") +
  ylim(-800,800)+
  coord_polar(start=0)+theme_void()

#Circular Grouped Bar
ggplot(BA, aes(fill=Zones, y=Phytoplankton,x=Stations)) +
  geom_bar(stat="identity") +
  ylim(-800,800)+
  coord_polar(start=0)+theme_void()

#Multiple Graphs in same sheet
library(ggpubr)

ggarrange(P, Q + font("x.text", size = 10),
          ncol = 1, nrow = 2)  #ncol=number of columns and nrow=number of rows

ggarrange(P, Q + font("x.text", size = 10),
          ncol = 2, nrow = 1)

#Horizontal Graph Arrangement

ggarrange(P,Q,R + font("x.text", size = 10),
          ncol = 1, nrow = 4,
          labels = c("(A)", "(B)", "C"))

#Vertical Arrangement####
ggarrange(P, Q, R,Q + font("x.text", size = 8),
          ncol = 2, nrow = 2, 
          labels = c("(A)", "(B)", "C","D"))


#Default -scatter plots####

#Cutting table in sub-table###  
BO <- BA[,c(-1:-5)]
BB <- BA[,c(6:17)]
#View(BB)
plot(BB)

#Scatter plots with Biological Parameters



#Scatter plot of Biological Parameters
BX=BB[,c(1,2,3,4)]
BX
plot(BX)

#Scatter plot of Environmental Parameters
BY=BB[,c(-1,-2,-3,-4)]

plot(BY)

#Single-scatterplot

#Single-scatterplot####

scatter.smooth(x=BB$Phytoplankton, y=BB$Diatoms,
               col="red",
               main="Phytoplankton~Diatoms",
               xlab = "Phytoplankton",ylab = "Diatoms")

#Best Scatter plot

#Best Scatter plot####

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggExtra")
library(ggExtra)
theme_set(theme_bw())  #pre-set the bw theme.

g <- ggplot(BA,aes(Phytoplankton,Diatoms, 
                   color=Phytoplankton,size=Depth))+
  geom_point(alpha=0.5)+
  theme_classic()+ 
  labs(y="Phytoplankton (cells/L)", 
       x="Diatoms", 
       title="Scatterplot", 
       subtitle="Phytoplankton Vs Diatoms",
       caption = "Source: Biol_data")
plot(g)
ggMarginal(g, type = "histogram", fill="Pink")

#Multiple scatter plot

#Multiple scatter plot####

library(tidyverse)
library(modelr)

ggplot(BA, aes(Temperature,Phytoplankton, 
               colour = Depth,size=Phytoplankton)) + 
  geom_point() + 
  facet_grid(BA$Zones~BA$Depth)+theme()

#segmenting bar plot with 2 groups

ggplot(BA, aes(Stations, Phytoplankton))+
  geom_bar(stat="identity", width = 0.7, fill="tomato2") + 
  facet_grid(BA$Zones~BA$Depth)+theme()

#Sized Scatter-Plot####

ggplot(BA,aes(Zones,Stations, color=Depth,size=Phytoplankton))+
  geom_point(alpha=0.5)+
  theme_classic()

#inverse Scatter Plot####

ggplot(BA,aes(Depth,Stations, color=Zones,size=Phytoplankton))+
  geom_point(alpha=0.5)+
  theme_classic()

#3d Scatter Plot####
library(tidyverse)
library(rgl)

plot3d(x=BA$Temperature,
       z=BA$Phytoplankton, 
       y=BA$Salinity,
       xlab="Temperature",
       ylab = "Salinity",
       zlab = "Phytoplankton",
       col=1:5,type="s",
       size =2)

rgl.snapshot("3dTS.png")

























