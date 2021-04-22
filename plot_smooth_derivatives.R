####### Plotting Smooth Derivatives ########

# Packages
library(data.table)
library(ggplot2)

## Function to plot smooth curves ##
plot_smooth_derivatives = function(wallets, addr_sample, nobs, free_scale = FALSE){
    for (category in names(addr_sample)){
        addr_cat = addr_sample[[category]]
        dt = wallets[address %in% addr_cat]
        credit_plt = ggplot(data = dt[increment_type == "credit"]) +
            ggtitle(paste(category), "- Credit") +
            xlab("Time") + 
            ylab("Deriv.") + 
            geom_line(aes(x = ts_normal, y = deriv_increments_sm), color = "#00BFC4") 
        if(free_scale == TRUE){
            credit_plt = credit_plt + facet_wrap(~address, nrow = 4, scales = "free_y")
        }else{
            credit_plt = credit_plt + facet_wrap(~address, nrow = 4)
        }
        debt_plt = ggplot(data = dt[increment_type == "debt"]) +
            ggtitle(paste(category), "- Debt") +
            xlab("Time") + 
            ylab("Deriv.") + 
            geom_line(aes(x = ts_normal, y = deriv_increments_sm), color = "#F8766B") 
        if(free_scale == TRUE){
            debt_plt = debt_plt + facet_wrap(~address, nrow = 4, scales = "free_y")
            file_plt = paste0("graphics/derivatives/free scale/smooth_derivatives_", gsub(" ", "_", tolower(category)))
            wd = 11
        }else{
            debt_plt = debt_plt + facet_wrap(~address, nrow = 4)
            file_plt = paste0("graphics/derivatives/fixed scale/smooth_derivatives_", gsub(" ", "_", tolower(category)))
            wd = 10
        }
        ggsave(paste0(file_plt, "_", nobs, "obs_credit.png"), credit_plt, width = wd, height = 11)
        ggsave(paste0(file_plt, "_", nobs, "obs_debt.png"), debt_plt, width = wd, height = 11)
    }
}

## 10 obs ##
wallets = data.table(readRDS("data/treated/wallets_log_smoothed_balanced_10obs.rds"))
# sample to plot
addr_sample = readRDS("data/treated/balanced samples/plt_sample_addr_10obs.rds")
plot_smooth_derivatives(wallets, addr_sample, 10)
plot_smooth_derivatives(wallets, addr_sample, 10, free_scale = TRUE)

## 20 obs ##
wallets = data.table(readRDS("data/treated/wallets_log_smoothed_balanced_20obs.rds"))
# sample to plot
addr_sample = readRDS("data/treated/balanced samples/plt_sample_addr_20obs.rds")
plot_smooth_derivatives(wallets, addr_sample, 20)
plot_smooth_derivatives(wallets, addr_sample, 20, free_scale = TRUE)
