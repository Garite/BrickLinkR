# BrickLinkR
R Shiny web application to assist with BrickLink inventory management.

This app is developmental and at any given time is unlikely to work without error given I have no involvement in the development of the BrickLink API or the BrickLink website in general.

In its current form it can only run in the browser from the account of a specific BrinkLink user using their unique API credentials.
My credentials (obviously) are excluded along with other R code required for the app to run.

## Price Guide

See ttem sales and stock for any part, set, or minifig in a given condition and color with graphs, tables and statistics.
Optionally, upload shopping cart items or a file of your own items to view them in the market context.
Below are some screen captures.

Price by quantity

[![priceguide1](https://github.com/leonawicz/BrickLinkR/raw/master/images/bricklinkr_PQ1.png)](#BrickLinkR)

Price by quantity continued

[![priceguide2](https://github.com/leonawicz/BrickLinkR/raw/master/images/bricklinkr_PQ2.png)](#BrickLinkR)

CDF of quantity as a function of price and comparison with price of items in cart.

[![priceguide3](https://github.com/leonawicz/BrickLinkR/raw/master/images/bricklinkr_PQ3.png)](#BrickLinkR)

Probabilities based on the CDF.

[![priceguide4](https://github.com/leonawicz/BrickLinkR/raw/master/images/bricklinkr_PQ4.png)](#BrickLinkR)

## Add a MYCOST field

Add a MYCOST field prior to uploading your items to BrickLink.
This is for files which do not contain a mycost field but can overwrite an existing field as well.
It is useful for setting costs prior to uploading.
The new file containing the MYCOST field displays in the browser and can be highlighted, copied and pasted to the BrickLink mass upload form.
The MYCOST field can be set as a simple constant fraction of the item prices or, more commonly, based on a total cost of all items which is allocated proportionally based on items' values.

[![mycost](https://github.com/leonawicz/BrickLinkR/raw/master/images/bricklinkr_addMCfield.png)](#BrickLinkR)
