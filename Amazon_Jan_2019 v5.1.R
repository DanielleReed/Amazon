# 1/27/2019: Dani Reed and CJ Arayata
# Monell Chemical Senses Center
# Amazon Food Reviews R-Script


# Set up: Load libraries and data ####
# setwd("C:/Users/cj/Desktop/Dani Amazon Project")

# Pacman is a useful package manager that will install packages on the fly
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, readr, tidyr, plyr, dplyr)

# Load raw review data
Reviews <- read_csv("Reviews.csv")

# Clean up: all review text to lower case
Reviews$Text <- tolower(Reviews$Text)

# Number of unique reviewers: 256,059
length(unique(Reviews$UserId))

# Number of unique products: 74,258
length(unique(Reviews$ProductId))


# Table 1: Taste vs. price, per review ####

string <- c("taste")
Reviews$taste_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_taste_percent <- table(Reviews$taste_keyword)
percent_taste <- calculate_taste_percent[[2]]/(calculate_taste_percent[[1]] + calculate_taste_percent[[2]])

string <- c("price")
Reviews$price_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_price_percent <- as.matrix(table(Reviews$price_keyword))
percent_price_percent <- calculate_price_percent[[2]]/(calculate_price_percent[[1]] + calculate_price_percent[[2]])


# Table 2: Taste Quality Word Counts ####

# Sweet
string <- c("sweet")
Reviews$sweet_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_sweet_percent <- as.matrix(table(Reviews$sweet_keyword))
percent_sweet_percent <- calculate_sweet_percent[[2]]/(calculate_sweet_percent[[1]] + calculate_sweet_percent[[2]])

# Bitter
string <- c("bitter")
Reviews$bitter_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_bitter_percent <- as.matrix(table(Reviews$bitter_keyword))
percent_bitter_percent <- calculate_bitter_percent[[2]]/(calculate_bitter_percent[[1]] + calculate_bitter_percent[[2]])

# Sour
string <- c("sour")
Reviews$sour_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_sour_percent <- as.matrix(table(Reviews$sour_keyword))
percent_sour_percent <- calculate_sour_percent[[2]]/(calculate_sour_percent[[1]] + calculate_sour_percent[[2]])

# Salty
string <- c("salty")
Reviews$salty_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_salty_percent <- as.matrix(table(Reviews$salty_keyword))
percent_salty_percent <- calculate_salty_percent[[2]]/(calculate_salty_percent[[1]] + calculate_salty_percent[[2]])

# Umami/savory
string <- c("umami")
Reviews$umami_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_umami_percent <- as.matrix(table(Reviews$umami_keyword))
percent_umami_percent <- calculate_umami_percent[[2]]/(calculate_umami_percent[[1]] + calculate_umami_percent[[2]])

string <- c("savory")
Reviews$savory_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_savory_percent <- as.matrix(table(Reviews$savory_keyword))
percent_savory_percent <- calculate_savory_percent[[2]]/(calculate_savory_percent[[1]] + calculate_savory_percent[[2]])


# Table 3: Phrase counts for sweet taste ####

string <- c("too sweet")
Reviews$too_sweet_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_too_sweet_percent <- as.matrix(table(Reviews$too_sweet_keyword))
percent_too_sweet_percent <- calculate_too_sweet_percent[[2]]/(calculate_too_sweet_percent[[1]] + calculate_too_sweet_percent[[2]])

string <- c("not sweet enough")
Reviews$not_sweet_keyword <- (1:nrow(Reviews) %in% c(sapply(string, grep, Reviews$Text, fixed = TRUE)))+0
calculate_not_sweet_percent <- as.matrix(table(Reviews$not_sweet_keyword))
percent_not_sweet_percent <- calculate_not_sweet_percent[[2]]/(calculate_not_sweet_percent[[1]] + calculate_not_sweet_percent[[2]])


# Table 4: Polarizing Products (Part 1) ####

# First, trim down all reviews
Reviews50 <- Reviews %>% group_by(ProductId) %>% filter(n() >50)

# Number of products, redux
length(unique(Reviews50$ProductId)) # 1,799

# Get the standard deviation in the star rating
sd <- aggregate(Reviews50$Score, by=list(Reviews50$ProductId), FUN = sd)

# Arrange by sd; larger sd is more polarizing
sd <- arrange(sd, desc(x))

names(sd) <- c("ASIN", "std.dev")


# Table 4: Polarizing Products (Part 2) ####
# Map product codes (Amazon ID "ASIN") to actual product name

# Vector of top 100 items (most polarizing)
first.100 <- sd$ASIN[1:100]

# Initialize blank matrix
product.codebook <- matrix(nrow = 100, ncol = 2)

# Go to product page, and get the title from the website
for (i in 1:100){
        
        # read a url page
        try({
        url <- paste0('https://www.amazon.com/dp/', first.100[i])
        webpage <- read_html(url)
        
        # get the title (after cleaning a bit)
        title <- html_nodes(webpage,'#title') %>% 
                html_text()
        title <- gsub("\n", "", title)
        title <- gsub(" ", "", title)

        # write it to our blank matrix that matches the product key
        product.codebook[i] <- first.100[i]
        product.codebook[i, 2] <- title
        })
}


# Polish a bit and write
polished.product.codebook <- as.data.frame(product.codebook)
names(polished.product.codebook) <- c("ASIN", "Description")

write.csv(polished.product.codebook, "polished.codebook.results.csv", row.names = F)

# Cut to complete cases
polished.product.codebook <- polished.product.codebook[complete.cases(polished.product.codebook), ]

# Merge back in the polarizing data (standard deviation)
products <- merge(polished.product.codebook, sd, all.x = T) %>% 
        arrange(desc(std.dev))

# Tie back to all reviews
product.reviews <- merge(Reviews50, products, by.x = "ProductId", by.y = "ASIN", all.y = T)

# Create manual list of the ASINs I want (products in Table 4)
## The top 11 we picked are the human-edible items (i.e not petfood)
target.products <- c("B001M08YZA", "B004TJF3BE", "B00507A02Q",
"B002OMV09W", "B000HDKZKU", "B000EM9E2Y",
"B000F6SNPS", "B000CRIBCA", "B000AQJRWG",
"B002CENRLG", "B002EDEMLY")

# These are all the reviews for the Top 11 products: For manual coding
target.reviews <- product.reviews[product.reviews$ProductId %in% target.products, ]
write.csv(target.reviews, "target_reviews_for_reading.csv", row.names = F)

# Summary table for the Top 11 products
summary.reviews <- ddply(target.reviews, .(ProductId, Description), summarise,
                         review.count = length(UserId),
                         mean.rating = mean(Score),
                         std.dev = mean(std.dev),
                         taste.count = sum(taste_keyword),
                         bitter.count = sum(bitter_keyword),
                         sweet.count = sum(sweet_keyword),
                         sour.count = sum(sour_keyword),
                         salty.count = sum(salty_keyword)) %>% 
        arrange(desc(std.dev))