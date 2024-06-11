library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(rvest)
library(data.table)
library(usethis)

setwd("C:/Users/adamp/OneDrive/Documents")
zmeny <- read_excel("dyplomka prawa/komplet/zmeny_ve_skutkove_podstate-2009-fin.xlsx", skip = 1)
names(zmeny)[16] <- "ucinnost"

zmeny <- data.table(zmeny)
# Define character vectors for replacement, space for _ in the end
czech_chars <- c("á", "č", "ď", "é", "ě", "í", "ň", "ó", "ř", "š", "ť", "ú", "ů", "ý", "ž"," ")
english_chars <- c("a", "c", "d", "e", "e", "i", "n", "o", "r", "s", "t", "u", "u", "y", "z","_")

# Loop through each Czech character and replace it with the corresponding English character
for (i in seq_along(czech_chars)) {
  zmeny <- zmeny %>%
    rename_all(~ gsub(czech_chars[i], english_chars[i], .))
}

# Convert Excel date format (numeric) to Date format, exclude dates before 1990
zmeny <- zmeny[!(as.Date(zmeny$ucinnost) < as.Date("1990-01-01"))]
zmeny[, ucinnost := as.Date(ucinnost)]


# Create change indicator (1 for stricter punishment, -1 for more lenient)
zmeny[,smer := ifelse(substr(Typ_zmeny,1,1)=="v",1,-1)]

# Create rating of the article by summing all change directions for the article
zmeny[, rating_paragrafu := sum(smer), by=.(Paragraf_novy, Novela)]
zmeny[, smer_paragrafu := ifelse(smer > 0, 1,-1)]

# sum the total to see the general direction
zmeny_unique <- unique(zmeny[, .(Paragraf_novy, Novela, smer_paragrafu)])
sum(zmeny_unique$smer_paragrafu)


# add "hlava" for future grouping
zmeny <- zmeny %>% mutate(par_cislo = as.numeric(gsub("[^0-9]","",Paragraf_novy))) %>% 
  mutate(hlava = case_when(
    par_cislo %in% 140:167 ~ 1,
    par_cislo %in% 168:184 ~ 2,
    par_cislo %in% 185:193 ~ 3,
    par_cislo %in% 194:204 ~ 4,
    par_cislo %in% 205:232 ~ 5,
    par_cislo %in% 233:271 ~ 6,
    par_cislo %in% 272:292 ~ 7,
    par_cislo %in% 293:308 ~ 8,
    par_cislo %in% 309:322 ~ 9,
    par_cislo %in% 323:368 ~ 10,
    par_cislo %in% 369:374 ~ 11,
    par_cislo %in% 400:418 ~ 12,
    par_cislo %in% 419:421 ~ 13,
    TRUE ~ 0 # Keep the original value if no condition is met
  ))

dataset1 <- zmeny 
### barchart changes by hlava ###
zmeny[,sum_hlava := sum(smer_paragrafu), by= .(hlava)]
hlavy <- unique(zmeny[,.(hlava, sum_hlava)])
hlavy <- hlavy[order(hlavy$hlava), ]
barplot(hlavy$sum_hlava, col = 'blue', names.arg = hlavy$hlava, xlab = 'hlava', ylab = 'Cumulative Sum')


zmeny <- zmeny[order(hlava, ucinnost)]  # Ensure the data is ordered by hlava and ucinnost

# Calculate cumulative sums by hlava and Novela
zmeny[, kumulativne_hlava_paragrafy := cumsum(smer_paragrafu), by = .(hlava, Novela)]
zmeny[, kumulativne_hlava_odstavce := cumsum(smer), by = .(hlava)]

# Calculate cumulative sums by Novela
zmeny[, kumulativne_odstavce := cumsum(smer), by = Novela]
zmeny[, kumulativne_paragrafy := cumsum(smer_paragrafu), by = Novela]

# Filter for specific hlava values
#filtered_zmeny <- zmeny[hlava %in% c(1, 2, 3, 4, 5, 6, 7, 9) & ucinnost != as.Date("2010-01-01")]
filtered_zmeny <- zmeny

# Plot kumulativne_hlava_odstavce for each hlava with ucinnost on x-axis using step plot
ggplot(filtered_zmeny, aes(x = ucinnost, y = kumulativne_hlava_odstavce, color = factor(hlava))) +
  geom_step() +
  labs(title = "Cumulative Sum of Odstavce per Selected Hlava",
       x = "Ucinnost",
       y = "Cumulative Sum of Odstavce",
       color = "Hlava") +
  theme_minimal()

for (hlava_val in unique(filtered_zmeny$hlava)) {
  # Subset data for the current hlava
  hlava_data <- filtered_zmeny[hlava == hlava_val]
  
  # Plot kumulativne_hlava_odstavce for the current hlava
  p <- ggplot(hlava_data, aes(x = ucinnost, y = kumulativne_hlava_odstavce)) +
    geom_step(color = "blue") +
    labs(title = paste("Cumulative Sum of Odstavce for Hlava", hlava_val),
         x = "Ucinnost",
         y = "Cumulative Sum of Odstavce")
  
  # Save the plot as a PNG file
  ggsave(paste0("C:/Users/adamp/OneDrive/Documents/dyplomka prawa/zmeny_hlava_", hlava_val, ".png"),
         plot = p,
         width = 8, height = 6, units = "in", dpi = 300)
}

  
### kumulativne za celek ###
zmeny[,score_novela := sum(smer_paragrafu), by = .(Novela)]
novely <- unique(zmeny[,.(Novela, score_novela, ucinnost)])
novely <- arrange(novely, ucinnost)
novely[, kumulativne := cumsum(score_novela)]
novely[, soubor := "zmeny_v_SP"]

ggplot(data = novely, aes(x = ucinnost, y = kumulativne)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Cumulative Value", title = "Nice Line Chart") +
  theme_minimal()


po_odstavcich <- zmeny[,score_odstavec := sum(smer), by = .(Novela)]
po_odstavcich <- po_odstavcich[is.na(po_odstavcich$relevavtni_zmena) == TRUE] #dropping irrelevant changes
odstavce <- unique(po_odstavcich[,.(Novela, score_odstavec, ucinnost)])
odstavce <- arrange(odstavce, ucinnost)
odstavce[, kumulativne := cumsum(score_odstavec)]
odstavce[, soubor := "zmeny_v_SP"]
ggplot(data = odstavce, aes(x = ucinnost, y = kumulativne)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Cumulative Value", title = "Nice Line Chart") +
  theme_minimal()
  
zmeny2 <- zmeny






################ dekriminalizace ###################

dekrim <- read_excel("dyplomka prawa/komplet/Dekriminalizace-2009-fin.xlsx", skip = 1)
names(dekrim)[9] <- "ucinnost"


#View(dekrim)
dekrim <- data.table(dekrim)
# Define character vectors for replacement, space for _ in the end
czech_chars <- c("á", "č", "ď", "é", "ě", "í", "ň", "ó", "ř", "š", "ť", "ú", "ů", "ý", "ž"," ")
english_chars <- c("a", "c", "d", "e", "e", "i", "n", "o", "r", "s", "t", "u", "u", "y", "z","_")

 # Loop through each Czech character and replace it with the corresponding English character
for (i in seq_along(czech_chars)) {
  dekrim <- dekrim %>%
    rename_all(~ gsub(czech_chars[i], english_chars[i], .))
}

# Convert Excel date format (numeric) to Date format
  dekrim[, ucinnost := as.Date(as.numeric(ucinnost), origin = "1899-12-30")]
  dekrim <- dekrim[!(as.Date(dekrim$ucinnost) < as.Date("1990-01-01"))]


# Create change indicator (1 for stricter punishment, -1 for more lenient)
dekrim[,smer := ifelse(substr(Typ_zmeny,1,1)=="z",-1,1)]

# Create rating of the article by summing all change directions for the article
dekrim[, rating_paragrafu := sum(smer), by=.(Paragraf, ucinnost)]
dekrim[, smer_paragrafu := ifelse(smer > 0, 1,-1)]

# sum the total to see the general direction
dekrim_unique <- unique(dekrim[, .(Paragraf, Novela, smer_paragrafu)])
sum(dekrim_unique$smer_paragrafu)


# add "hlava" for future grouping
dekrim <- dekrim %>% mutate(par_cislo = as.numeric(gsub("[^0-9]","",Paragraf))) %>% 
  mutate(hlava = case_when(
    par_cislo %in% 140:167 ~ 1,
    par_cislo %in% 168:184 ~ 2,
    par_cislo %in% 185:193 ~ 3,
    par_cislo %in% 194:204 ~ 4,
    par_cislo %in% 205:232 ~ 5,
    par_cislo %in% 233:271 ~ 6,
    par_cislo %in% 272:292 ~ 7,
    par_cislo %in% 293:308 ~ 8,
    par_cislo %in% 309:322 ~ 9,
    par_cislo %in% 323:368 ~ 10,
    par_cislo %in% 369:374 ~ 11,
    par_cislo %in% 400:418 ~ 12,
    par_cislo %in% 419:421 ~ 13,
    TRUE ~ 0 # Keep the original value if no condition is met
  ))

### group by § ###
dekrim[,sum_hlava := sum(smer_paragrafu), by= .(hlava)]
hlavy <- unique(dekrim[,.(hlava, sum_hlava)])
plot(hlavy)


dataset2 <- dekrim 
### barchart changes by hlava ###
dekrim[,sum_hlava := sum(smer_paragrafu), by= .(hlava)]
hlavy <- unique(dekrim[,.(hlava, sum_hlava)])
hlavy <- hlavy[order(hlavy$hlava), ]
barplot(hlavy$sum_hlava, col = 'blue', names.arg = hlavy$hlava, xlab = 'hlava', ylab = 'Cumulative Sum')


dekrim <- dekrim[order(hlava, ucinnost)]  # Ensure the data is ordered by hlava and ucinnost

# Calculate cumulative sums by hlava and Novela
dekrim[, kumulativne_hlava_paragrafy := cumsum(smer_paragrafu), by = .(hlava, Novela)]
dekrim[, kumulativne_hlava_odstavce := cumsum(smer), by = .(hlava)]

# Calculate cumulative sums by Novela
dekrim[, kumulativne_odstavce := cumsum(smer), by = Novela]
dekrim[, kumulativne_paragrafy := cumsum(smer_paragrafu), by = Novela]

# Filter for specific hlava values
filtered_dekrim <- dekrim[hlava %in% c(1, 2, 3, 4, 5, 6, 7, 9) ]#& ucinnost != as.Date("2010-01-01")]
#filtered_dekrim <- dekrim

# Plot kumulativne_hlava_odstavce for each hlava with ucinnost on x-axis using step plot
ggplot(filtered_dekrim, aes(x = ucinnost, y = kumulativne_hlava_odstavce, color = factor(hlava))) +
  geom_step() +
  labs(title = "Cumulative Sum of Odstavce per Selected Hlava",
       x = "Ucinnost",
       y = "Cumulative Sum of Odstavce",
       color = "Hlava") +
  theme_minimal()

for (hlava_val in unique(filtered_dekrim$hlava)) {
  # Subset data for the current hlava
  hlava_data <- filtered_dekrim[hlava == hlava_val]
  
  # Plot kumulativne_hlava_odstavce for the current hlava
  p <- ggplot(hlava_data, aes(x = ucinnost, y = kumulativne_hlava_odstavce)) +
    geom_step(color = "blue") +
    labs(title = paste("Cumulative Sum of Odstavce for Hlava", hlava_val),
         x = "Ucinnost",
         y = "Cumulative Sum of Odstavce")
  
  # Save the plot as a PNG file
  ggsave(paste0("C:/Users/adamp/OneDrive/Documents/dyplomka prawa/dekrim_hlava_", hlava_val, ".png"),
         plot = p,
         width = 8, height = 6, units = "in", dpi = 300)
}

dekrim[,score_novela := sum(smer_paragrafu), by = .(Novela)]
novely2 <- unique(dekrim[,.(Novela, score_novela, ucinnost)])
novely2 <- arrange(novely2, ucinnost)
novely2[, kumulativne := cumsum(score_novela)]
novely2[, soubor := "de/kriminalizace"]

data_zmeny <- zmeny[, .(Novela, hlava,
                          score_novela, 
                          smer, 
                          smer_paragrafu,
                          soubor = "zmeny")]

data_dekrim <- dekrim[, .(Novela, hlava, 
                          score_novela, 
                          smer, 
                          smer_paragrafu,
                          soubor = "de/kriminalizace")]
alldata <- rbind(data_zmeny, data_dekrim)

# Calculate cumulative sums by hlava and Novela
alldata[, kumulativne_hlava_paragrafy := cumsum(smer_paragrafu), by = .(hlava, Novela)]
alldata[, kumulativne_hlava_odstavce := cumsum(smer), by = .(hlava)]

# Calculate cumulative sums by Novela
alldata[, kumulativne_odstavce := cumsum(smer), by = Novela]
alldata[, kumulativne_paragrafy := cumsum(smer_paragrafu), by = Novela]

# Filter for specific hlava values
filtered_alldata <- alldata[hlava %in% c(1, 2, 3, 4, 5, 6, 7, 9) ]#& ucinnost != as.Date("2010-01-01")]
#filtered_alldata <- alldata

# Plot kumulativne_hlava_odstavce for each hlava with ucinnost on x-axis using step plot
ggplot(filtered_dekrim, aes(x = ucinnost, y = kumulativne_hlava_odstavce, color = factor(hlava))) +
  geom_step() +
  labs(title = "Cumulative Sum of Odstavce per Selected Hlava",
       x = "Ucinnost",
       y = "Cumulative Sum of Odstavce",
       color = "Hlava") +
  theme_minimal()

for (hlava_val in unique(filtered_alldata$hlava)) {
  # Subset data for the current hlava
  hlava_data <- filtered_dekrim[hlava == hlava_val]
  
  # Plot kumulativne_hlava_odstavce for the current hlava
  p <- ggplot(hlava_data, aes(x = ucinnost, y = kumulativne_hlava_odstavce)) +
    geom_step(color = "blue") +
    labs(title = paste("Cumulative Sum of Odstavce for Hlava", hlava_val),
         x = "Ucinnost",
         y = "Cumulative Sum of Odstavce")
  print(p)
  # Save the plot as a PNG file
  ggsave(paste0("C:/Users/adamp/OneDrive/Documents/dyplomka prawa/all_hlava_", hlava_val, ".png"),
         plot = p,
         width = 8, height = 6, units = "in", dpi = 300)
}


novely$ucinnost<- as.Date(novely$ucinnost)
plotting_data <- rbind(novely, novely2)

# Create the line chart
ggplot(data = plotting_data, aes(x = ucinnost, y = kumulativne, color = soubor)) +
  geom_step(size = 1.3) +
  labs(x = "Date", y = "Level of Strictness (pocet zprisneni - pocet zmirneni)", title = "Changes in Criminal Code") +
  theme_minimal()
ggsave("C:/Users/adamp/OneDrive/Documents/dyplomka prawa/graf_nevazeny.png")

ggplot(data = novely2, aes(x = ucinnost, y = kumulativne)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Cumulative Value", title = "Nice Line Chart") +
  theme_minimal()


#### merging datasets ####

zmeny[, soubor := "zmeny_v_SP"]
dekrim[, soubor := "de/kriminalizace"]























### CLUSTERING ###
# retrieving the target web page 
document <- read_html("https://www.zakonyprolidi.cz/cs/1961-140")
# selecting the list of product HTML elements 
# Select elements with class = "L5"
html_products <- document %>%
  html_nodes("p.L4, p.L3, p.L5, p.L6, p.L2, p.L7")
text_from_elements <- html_products %>%
  html_text()
text_from_elements <- as.list(text_from_elements)
long_string <- paste(text_from_elements, collapse = " ")
split_strings <- strsplit(long_string, "§")
split_strings <- as.data.frame(split_strings)

# Define the function to replace Czech characters with English characters
replace_chars <- function(string) {
  for (i in seq_along(czech_chars)) {
    string <- gsub(czech_chars[i], english_chars[i], string)
  }
  return(string)
}

# Define character vectors for replacement, space for _ in the end
czech_chars <- c("á", "č", "ď", "é", "ě", "í", "ň", "ó", "ř", "š", "ť", "ú", "ů", "ý", "ž")
english_chars <- c("a", "c", "d", "e", "e", "i", "n", "o", "r", "s", "t", "u", "u", "y", "z")
# Apply the function to each string in split_strings
split_strings <- lapply(split_strings, replace_chars)

### PRIDAT jina zavazna ujma, jina ujma velkeho rozsahu - 124b - komplet nevyznamne, je jenom tam
selected_strings <- split_strings[[1]][grep("jiny zvlast zavazny nasledek", split_strings[[1]], ignore.case = TRUE)]
first_4_chars <- lapply(selected_strings, substr, start = 1, stop = 4)
print(first_4_chars)


