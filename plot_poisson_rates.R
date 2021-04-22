####### Plotting Poisson Rates ########

# Packages
library(data.table)
library(ggplot2)

## Function to plot poisson_rates ##
plot_poisson_rates = function(nobs, free_scale=FALSE){
    wallets = readRDS(paste0("data/treated/rates/poisson_rates_", nobs, "obs.rds"))
    addr_sample = readRDS(paste0("data/treated/balanced samples/plt_sample_addr_", nobs, "obs.rds"))
    for (category in names(addr_sample)){
        addr_cat = addr_sample[[category]]
        dt = wallets[address %in% addr_cat]
        credit_plt = ggplot(data = dt[increment_type == "credit"]) +
            ggtitle(paste(category, "- Credit")) +
            xlab("Time") + 
            ylab("Rate") + 
            geom_line(aes(x = ts_normal, y = lambda), color = "#00BFC4") 
        if(free_scale == TRUE){
            credit_plt = credit_plt + facet_wrap(~address, nrow = 4, scales = "free_y")
        }else{
            credit_plt = credit_plt + facet_wrap(~address, nrow = 4)
        }
        debt_plt = ggplot(data = dt[increment_type == "debt"]) +
            ggtitle(paste(category, "- Debt")) +
            xlab("Time") + 
            ylab("Rate") + 
            geom_line(aes(x = ts_normal, y = lambda), color = "#F8766B") 
        if(free_scale == TRUE){
            debt_plt = debt_plt + facet_wrap(~address, nrow = 4, scales = "free_y")
            file_plt = paste0("graphics/poisson rates/free scale/poisson_rates_", gsub(" ", "_", tolower(category)))
            wd = 11
        }else{
            debt_plt = debt_plt + facet_wrap(~address, nrow = 4)
            file_plt = paste0("graphics/poisson rates/fixed scale/poisson_rates_", gsub(" ", "_", tolower(category)))
            wd = 10
        }
        ggsave(paste0(file_plt, "_", nobs, "obs_credit.png"), credit_plt, width = wd, height = 11)
        ggsave(paste0(file_plt, "_", nobs, "obs_debt.png"), debt_plt, width = wd, height = 11)
    }
}

## 10 obs ##
plot_poisson_rates(10, free_scale = FALSE)
plot_poisson_rates(10, free_scale = TRUE)

## 20 obs ##
plot_poisson_rates(20, free_scale = FALSE)
plot_poisson_rates(20, free_scale = TRUE)
