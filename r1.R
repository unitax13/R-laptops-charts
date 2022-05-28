### Data source:  kaggle/2022-march-laptop-data


setwd("C:/Users/Jacek/Downloads")
data = read.csv("Cleaned_Laptop_data.csv", header = TRUE)
x = data.frame(data)
str(data)
head(data)
hist(data$latest_price,)

library(ggplot2)  
c <- ggplot(data)
stat_bin(data=data)
c(stat_bin())

##### ten śmieszny scatter
plot.new()
plot.window(c(1,7), c(200, 1000))
points(data$latest_price, data$display_size,col='black')
box()
axis(1,)

ggplot(data=x, mapping = aes(x = latest_price, y=display_size)) + geom_point(aes(color = processor_brand)) + labs(title="siema")

unique(data$display_size)

d_sizes = c("15.6",  "14.1",  "14" ,   "13.3", "17.3",  "14.96", "15", "14.2",  "16.2",  "14.9", "13.4",  "13" ,   "15.3",  "16",    "16.1",  "13.5","12.2","12");

x[x$display_size %in% c(d_sizes)]
library(dplyr)
x2 <- x %>% filter(display_size %in% d_sizes)
x2
ggplot(data=x2, mapping = aes(x = latest_price, y=display_size)) + geom_point(aes(color = processor_brand)) + labs(title="Wielkość ekranu a cena i producent procesora",y="Wielkość ekranu [cale]", x="Cena [INR]")

processor_brands = unique(x2$processor_brand)
#x3 <- x2 %>% filter(processor_brand %in% processor_brands)

#x4 = x3 %>% group_by(processor_brand) %>% summarise()
##### producenci procesorów
pie_values = NULL
for (brand1 in processor_brands) {
  print(brand1)
  pie_values[brand1] = as.numeric(sum(data$processor_brand == brand1))
}
#pie_values <- append(pie_values, as.numeric(sum(data$processor_brand == processor_brands[1])))
#pie_values <- append(pie_values, as.numeric(sum(data$processor_brand == processor_brands[2])))
#pie_values <- append(pie_values, as.numeric(sum(data$processor_brand == processor_brands[3])))
#pie_values2 <- c(pie_values)
pie_values = sort(pie_values, decreasing = TRUE)


barplot((pie_values), names.arg = names(pie_values), main="Producenci procesorów", ylab="Ilość",col = "#1b98e0");
?barplot


gens_values = 0
processor_gens = unique(x3$processor_gnrtn)
processor_gens_str = c( "12th", "11th",    "10th",    "9th",     "8th" ,    "7th");
for (gen in processor_gens_str) {
  print(gen)
  gens_values[gen] = as.numeric(sum(data$processor_gnrtn == gen))  
}
gens_values = sort(gens_values, decreasing = TRUE)

barplot(as.numeric(gens_values), names.arg = names(gens_values), main="Generacje procesorów Intela", ylab="Ilość",col = "#1b98e0");
##### marki unikalne
brands = unique(x$brand)
brands
brands_values = 0
for (br in unique(x$brand)) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
  brands_values[br] = as.numeric(sum(data$brand == br))
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Producenci laptopów", ylab="Ilość", las=2,col = "#1b98e0");

##### wielkości ekranu
brands = unique(x$display_size)
brands
brands_values = 0
for (br in unique(x$display_size)) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
  brands_values[br] = as.numeric(sum(data$display_size == br))
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Wielkości ekranów", ylab="Ilość", las=2,col = "#1b98e0");

##### wielkości ramu
brands = unique(x$ram_gb)
brands
brands = c("4", "8", "16", "32", "5", "15.6");
brands_values = NULL
for (br in unique(x$ram_gb)) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
  if (!(br %in% brands)) {
    brands_values["Other"] = as.numeric(sum(data$ram_gb == br))
  }else {
  brands_values[br] = as.numeric(sum(data$ram_gb == br))
  }
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Ilości RAMu [GB]", ylab="Ilość", las=1,col = "#1b98e0");

x

### typy dysków
ssds = unique(x$ssd)
hdds = unique(x$hdd)

drives = c("SSD", "HDD", "both")
drives_values = 0

for (val in drives) {
  drives_values[val] = 0
}

for (i in 1:nrow(x)) {
  if (x$ssd[i] > 0 && x$hdd[i] > 0) {
    drives_values["both"] = drives_values["both"] + 1;
  } else if (x$ssd[i] > 0) {
    drives_values["SSD"] = drives_values["SSD"] + 1;
  } else if (x$hdd[i] > 0 ) {
    drives_values["HDD"] = drives_values["HDD"] + 1;
  }
}
vals = drives_values[2:4]
barplot(as.numeric(vals), names.arg = names(drives_values)[2:4], main="Typ dysku", ylab="Ilość",las=1,col = "#1b98e0");


##### wielkości SSD
brands1 = toString(unique(x$ssd)[2:length(unique(x$ssd))])
brands = as.vector(strsplit(brands1, ", "))[1]
#brands = c("4", "8", "16", "32", "5", "15.6");
brands = brands[[1]]
brands_values = NULL
for (br in unique(x$ssd[x$ssd != 0])) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
  if (!(br %in% brands)) {
    brands_values["bd. / HDD"] = as.numeric(sum(data$ssd == br))
  }else {
    brands_values[toString(br)] = as.numeric(sum(data$ssd == br))
  }
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Wielkość dysku SSD [GB]", ylab="Ilość", las=2,col = "#1b98e0");


##### wielkości HDD
brands1 = toString(unique(x$hdd))
brands = as.vector(strsplit(brands1, ", "))[1]
#brands = c("4", "8", "16", "32", "5", "15.6");
brands = brands[[1]]
brands_values = NULL
for (br in unique(x$hdd[x$hdd != 0])) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
  if (!(br %in% brands)) {
    brands_values["bd. / HDD"] = as.numeric(sum(data$hdd == br))
  }else {
    brands_values[toString(br)] = as.numeric(sum(data$hdd == br))
  }
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Wielkość dysku HDD [GB]", ylab="Ilość", las=2,col = "#1b98e0");


##### gwarancja
brands1 = toString(unique(x$warranty))
brands = as.vector(strsplit(brands1, ", "))[1]
#brands = c("4", "8", "16", "32", "5", "15.6");
brands = brands[[1]]
brands_values = NULL
for (br in unique(x$warranty)) {
  print(br)
  # brands_values = append(brands_values, as.numeric(sum(data$brand == br)))
    brands_values[toString(br)] = as.numeric(sum(data$warranty == br))
}
brands_values2 <- sort(brands_values,decreasing = TRUE )
class(brands_values)

barplot(as.numeric(brands_values2), names.arg = names(brands_values2), main="Długość gwarancji [lata]", ylab="Ilość", las=1,col = "#1b98e0");


### os plots
os = data$os

library(ggplot2)

os_table = table(os)
os_freq_df = data.frame(os_table)
os_freq_df$Freq = os_freq_df$Freq/sum(os_freq_df$Freq)
os_freq_df$labels = sprintf("%0.2f", os_freq_df$Freq *100)

ggplot(os_freq_df, aes(x = "", y = Freq, fill = os)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  labs(title = "System operacyjny",
              caption = "Źródło: kaggle/2022-march-laptop-data")+
  theme(plot.title = element_text(hjust = 0.5))

p<-ggplot(data, aes(x=os, y=latest_price, fill=os)) +
  geom_violin(trim=FALSE)+
    labs(title = "Cena komputera",
              caption = "Źródło:  kaggle/2022-march-laptop-data")+
  theme(plot.title = element_text(hjust = 0.5))
p

p<-ggplot(data, aes(x=os, y=star_rating, fill=os)) +
  geom_violin(trim=FALSE)+
    labs(title = "Ocena komputera (gwiazdki)",
              caption = "Źródło:  kaggle/2022-march-laptop-data")+
  theme(plot.title = element_text(hjust = 0.5))
p

p<-ggplot(data, aes(x=os, y=ratings, fill=os)) +
    geom_boxplot(trim=FALSE)+
    labs(title = "Ilość ocen (popularność)",
              caption = "Źródło:  kaggle/2022-march-laptop-data")+
  theme(plot.title = element_text(hjust = 0.5))
p

