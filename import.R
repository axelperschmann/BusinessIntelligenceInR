filename_data = "data/1017_01_M_08_E_20090331/monthly_bba_aa_20090331.csv"

# read csv file
mydata = read.csv(filename_data, sep = ";")

test = subset(mydata, ISIN=="DE0005087506")