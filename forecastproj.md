U.S. Gasoline Production
========================================================
author: Team VP
date: November 30th, 2018
autosize: TRUE
width: 1920
height: 1080


<style>
  .col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
  }
  .col3 {
    columns: 3 100px;
    -webkit-columns: 3 100px;
    -moz-columns: 3 100px;
  }
</style>



The Data
========================================================
U.S. Energy Information Administration  
Weekly U.S Product Supplied of Finished Motor Gasoline  

- Time Series - Weekly data points from 2/8/1991 to current
- Dependent Variable - Average barrels per day (thousands)
- Created dummy variables for each season to use as exogenous variables<br/>




<div>
<iframe src="C:/Users/ejvpaba/Desktop/Python/Forecasting/oil.html" style="position:absolute;height:65%;width:75%"></iframe>
</div>


Data Subsets
========================================================
For forecasting purposes, we will be looking at the last 260 weeks (~5 years)

- Training set (.tr) - Week 1 through week 206 (~3 years)

- Validation set (.val) - Remaining 52 weeks (final ~1 year)


<div>
<iframe src="C:/Users/ejvpaba/Desktop/Python/Forecasting/oil_seg.html" style="position:absolute;height:65%;width:85%"></iframe>
</div>

Data Variations and Frequencies
========================================================
<br/>
<br/>
<br/>
 Created three variations of data:  
<br/>
<br/>


<div class="col3">
oil_round:<br/>
frequency = 52<br/>
start oil_round.tr = (2013, 46)<br/>
end oil_round.val = (2018, 45)<br/>

oil_freq:<br/>
frequency = 52.179 $(365.25/7)$<br/>  
start oil_round.tr = 2013.8740589<br/>
end oil_round.val = 2018.8377823<br/>  

oil_freq:<br/>
frequency = 52.179 $(365.25/7)$, 12, 4<br/>
start oil_round.tr = 2013.8740589<br/>
end oil_round.val = 2018.8377823<br/>
</div>

Seasons
========================================================

Adding seasons to the combination of the training and validation data:



```
Error in parse(text = x, srcfile = src) : <text>:24:34: unexpected ','
23: 
24: autoplot(seasonsts[,"oil_round"]),
                                     ^
```
