getwd()
library(csvread)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringi)
comcast <- read.csv("Comcast Telecom Complaints data.csv",header = TRUE)
View(comcast)
str(comcast)
summary(comcast)
comcast$Date <- gsub("/","-",comcast$Date, fixed = TRUE)
comcast$Date <- as.Date(comcast$Date,format = "%d-%m-%Y")
arrange(comcast,comcast$Date)
str(comcast)

x = table(comcast$Date)
x
plot(x, type = "l", xlab = "Date", ylab = "No of Complaints", main = "Daily Complaints Trend")

monthlytrend = format(comcast$Date, "%m-%Y")
y = table(monthlytrend)
plot(y, type = 'l', main = "Monthly Complaints Trend", xlab = "Month", ylab="No' of Complaints")

comcast %>% 
  group_by(Customer.Complaint) %>% 
  summarise(no_of_complaints = n()) %>% 
  arrange(desc(no_of_complaints))

network.tkts<- contains(comcast$Customer.Complaint,match = 'network',ignore.case = TRUE)
comcast$Customer.Complaint[network.tkts]<- "Network Issues"

internet.tkts<- contains(comcast$Customer.Complaint,match = 'internet',ignore.case = TRUE)
comcast$Customer.Complaint[internet.tkts]<- "Internet Issues"

billing.tkts<- contains(comcast$Customer.Complaint,match = 'bill',ignore.case = TRUE)
comcast$Customer.Complaint[billing.tkts]<- "Billing Issues"

email.tkts<- contains(comcast$Customer.Complaint,match = 'email',ignore.case = TRUE)
comcast$Customer.Complaint[email.tkts]<- "Email Issues"

charges.tkts<- contains(comcast$Customer.Complaint,match = 'charge',ignore.case = TRUE)
comcast$Customer.Complaint[charges.tkts]<- "Issues Regarding Charges"

speedrelated.tkts<- contains(comcast$Customer.Complaint,match = 'speed',ignore.case = TRUE) 
comcast$Customer.Complaint[speedrelated.tkts]<- "Speed Related Issues"

general.tkts<- contains(comcast$Customer.Complaint,match = 'comcast', ignore.case = TRUE)
comcast$Customer.Complaint[general.tkts]<- "Comcast General Complaints"

datacaps.tkts<- contains(comcast$Customer.Complaint,match = 'Data caps', ignore.case = TRUE)
comcast$Customer.Complaint[datacaps.tkts]<-"Data Caps Related Issues"

comcast$Customer.Complaint[-c(network.tkts, internet.tkts,billing.tkts,email.tkts,charges.tkts,speedrelated.tkts,general.tkts,datacaps.tkts)]<- "Others"
Complaint_Types_Frequency=table(comcast$Customer.Complaint)
Complaint_Types_Frequency

#plot(Frequecy_Complaint.Types, type = 'l', main = "Frequency of Complaints Types", xlab = "Complaint Types", ylab="Frequency")

open.complaints<- (comcast$Status == "Open"| comcast$Status =="Pending")
closed.complaints<-(comcast$Status == "Closed"| comcast$Status =="Solved")
comcast$ComplaintStatus[open.complaints]<-"Open" 
comcast$ComplaintStatus[closed.complaints]<- "Closed"
comcast$ComplaintStatus = as.factor(comcast$ComplaintStatus)
comcast$ComplaintStatus
str(comcast)

comcast$ComplaintStatus = as.character(comcast$ComplaintStatus)
comcast<- group_by(comcast,State,ComplaintStatus)
Display.Data<- summarise(comcast,Count = n())
Stacked.Chart <- as.data.frame(Display.Data)
ggplot(Stacked_Chart, mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 1)+
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        title = element_text(size = 12),
        plot.title = element_text(hjust =  1))+
  labs(title = "Statewise Complaint Status Stacked Bar Chart",
       x = "States",y = "Number of Complaints",
       fill= "Complaint Status")

TotalComplaintsReceived <- comcast %>% group_by(State) %>%
  summarise(no_complaints = n()) %>% arrange(desc(no_complaints))
TotalComplaintsReceived
Total.Open.Complaints <- Display.Data%>% filter(ComplaintStatus=="Open")
Total.Open.Complaints[Total.Open.Complaints$Count == max(Total.Open.Complaints$Count), c(1,3)]


Total.Open.Complaints = filter(Stacked.Chart, ComplaintStatus == "Open")
Total.Open.Complaints <- (Total.Open.Complaints%>%mutate(Percentage=paste0(round(Count/sum(Count)*100,2),"%")))
Total.Open.Max.Percent <- Total.Open.Complaints[Total.Open.Complaints$Count == max(Total.Open.Complaints$Count),c(1,4)]
Total.Open.Max.Percent

comcast$Received.Via <- as.factor(comcast$Received.Via)
levels(comcast$Received.Via)
NewStatus.ReceivedVia <- table(comcast$Received.Via, comcast$ComplaintStatus)
NewStatus.ReceivedVia <- cbind(NewStatus.ReceivedVia, Total = rowSums(NewStatus.ReceivedVia))
NewStatus.ReceivedVia

values <- c(864, 255)
names <- c("Closed", "Open")
pct <- round(values/sum(values)*100)
names <- paste(names, pct) # add percents to labels
names <- paste(names,"%",sep="") # ad % to labels
pie(values,labels = names, col=rainbow(length(names)),
    main="Percentage Share of Complaints Status Received Via Call")

values1 <- c(843, 262)
names1 <- c("Closed", "Open")
pct1 <- round(values1/sum(values1)*100)
names1 <- paste(names1, pct1) # add percents to labels
names1 <- paste(names1,"%",sep="") # ad % to labels
pie(values1,labels = names1, col=rainbow(length(names1)),
    main="Pie Chart of Received Via Internet")





