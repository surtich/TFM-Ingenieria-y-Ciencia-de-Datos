# load package
library("bmmb")

# load data
data(exp_data_all)
# see first 6 rows
head(exp_data_all)

tmp_tab <- table(exp_data_all$C, exp_data_all$L, exp_data_all$R)
tmp_tab
tmp_tab[, , 1]
tmp_tab[, , 2]
