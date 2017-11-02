#Stacking multiple columns into a categorical/value pair of columns.
#Author: Blake Madden

require(dplyr)
require(tidyr)

#construct a dataset where you have a column showing the week
#of sales, followed by a column for each baker and his/her
#respective sales from that week.
sales.data <- as_tibble(list(
  date = c('10/01/2017', '10/08/2017', '10/15/2017', '10/22/2017', '10/29/2017'),
  Blake = c(320, 310, 310, 290, 220),
  Mizzie = c(330, 350, 360, 365, 340),
  Gabi = c(340, 380, 410, 420, 450)
))

#our sales data will appear like this:
# date        Blake    Mizzie    Gabi
# 10/01/2017  320      330       340
# 10/08/2017  310      350       340
# ...

#'gather' will combine the columns that we specify into
#single categorical (factor) column with the respective
#values matched up in another column.
stacked.sales.data <- gather(sales.data,
                             #this will be the new categorical column
                             key='Baker',
                             #where the values from the combined
                             #variables go to
                             value='Sales',
                             #the list of columns to stack.
                             #in this case, all columns *except* date.
                             -date)

#now it will appear like this:
# date        Baker   Sales
# 10/01/2017  Blake   320
# 10/08/2017  Blake   310
# ...
# 10/01/2017  Mizzie  330
# 10/08/2017  Mizzie  350
# ...
# 10/01/2017  Gabi    340
# 10/08/2017  Gabi    380

#Uncomment this if using RStudio to see the
#original data and output:
#View(sales.data)
#View(stacked.sales.data)