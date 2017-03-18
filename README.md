# eBay
Probability that an iPad will sell on eBay ( either new or used) given information of product data like description, biddable, price, startprice, condition, cellular, carrier, color, storage,  productline 

The dependent variable in this problem is the variable sold, 
which labels if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). 

The independent variables consist of 9 pieces of product data available at the time the iPad listing is posted, 
and a unique identifier:
  
description = The text description of the product provided by the seller.

biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).

startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).

condition = The condition of the product (new, used, etc.)

cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).

carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.

color = The color of the iPad.

storage = The iPad's storage capacity (in gigabytes).

productline = The name of the product being sold.
 
Source: kaggle.com
