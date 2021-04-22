### Plots: number of addresses with at least 10 and 20 observations, by life span

## Packages
library(data.table)
library(ggplot2)

# Reading data
dt = readRDS("data/raw/wallets_from_blockchain.rds")
# satoshi to btc
dt[, value := as.numeric(value)/(10^8)]
# labels
labels = data.table(read.csv("data/vivian/label_addresses.csv"))
names(labels)[c(1,2)] = c("address", "label")
dt = merge(dt, unique(labels[, .(address, label)]), by = "address", all.x = TRUE)

# New categories:
dict = data.table(read.csv("data/new labels/new_labels.csv"))
# Grouping especifically P2P with service
#dict[new_category == "P2P Lending", new_category := "Service"]
# Removing another blockchain
#dict[new_category == "Another blockchain", new_category := NA]
# Changing Mining label 
#dict[new_category == "Mining Pool/Cloud Mining", new_category := "Mining"]
# merging
#setnames(dt, "category", "old_category")
#dt = merge(dt, dict[, .(label, new_category)], by = "label", all.x = TRUE)
dict[, new_wallet_explorer := wallet_explorer]
dict[wallet_explorer == 'Old/historic', new_wallet_explorer := NA] # comment
dict[new_category == 'Darknet Marketplace', new_wallet_explorer := new_category] # comment
dt = merge(dt, dict[, .(label, new_wallet_explorer)], by = "label", all.x = TRUE) # comment
setnames(dt, "new_wallet_explorer", "category") # comment
#setnames(dt, "new_category", "category")
dt = dt[!is.na(category)]
dt[, category := as.character(category)]

# Converting time stamps to date
dt[, date := as.POSIXct(timestamp, origin = '1970-01-01', tz = 'UCT')]

# first dates
setorder(dt, address, date)
dt[, first_date := first(date), by = address]

# Expanding: (note: this uses a lot of memory and is slow. maybe using a loop is better.)
life_span = expand.grid(list("address" = unique(dt$address), "life_span" = seq(500,5000,by=500)))
dt = merge(dt, life_span, by = "address", allow.cartesian = TRUE)
dt = dt[date <= first_date + as.difftime(life_span, units = "hours")]
# creating new count variable
dt[, count := .N, by = c("address", "life_span")]
dt = unique(dt[, .(address, label, category, count, life_span)])

sub10 = dt[count >= 10]
sub10[, count := .N, by = c("category", "life_span")]
sub10 = unique(sub10[, .(category,life_span,count)])

plt10 = ggplot(sub10, aes(x = life_span, y = count, col = category)) +
  ggtitle("# Addresses min 10 obs.") +
  xlab("Life span") + 
  ylab("N addresses") +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~category, ncol = 3, scales = "free_y")
ggsave('graphics/life span/min_10obs.png', plt10, width = 7, height = 6)

sub20 = dt[count >= 20]
sub20[, count := .N, by = c("category", "life_span")]
sub20 = unique(sub20[, .(category,life_span,count)])

plt20 = ggplot(sub20, aes(x = life_span, y = count, col = category)) +
  ggtitle("# Addresses min 20 obs.") +
  xlab("Life span") + 
  ylab("N addresses") +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~category, ncol = 3, scales = "free_y")
ggsave('graphics/life span/min_20obs.png', plt20, width = 7, height = 6)



