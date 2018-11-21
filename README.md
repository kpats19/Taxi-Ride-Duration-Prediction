# Taxi-Ride-Duration-Prediction

Requirements :- Java version 1.8.0, R libraries : lubridate, geosphere, h2o, Metrics 

We performed tests on 2 data: Baseline where latitude and longitude were rounded to 
its ceiling value, and other with latitude and longitude transformed by PCA (Principle Componenet Analysis)

code.R is the code containing processing on both PCA transformed and Baseline Data.
==> From Line 1 to 171 is the code runnning for PCA Transformed data
==> Line 172 onwards is the code for Baseline data. 

test_1.csv and train_1.csv are datasets downloaded from Kaggle.com
Output.csv file contains output of all algorithms (3 for PCA data and 3 for 
Baseline data) and Actual trip Duration as well.

The code will take lot of time to run, as it runs and plots RMSLE values for 
various hyper parameters.


  **********************************************************************
*********** The code may sometime break beacause of memory issues ***********
  ********************************************************************** 

In such case, do the following:
Open Task Manager (Ctrl + Alt + Del)
1. Search for "Java(TM) Platform SE Binary" and it may hold a lot of memory.
2. Kill it
3. Reinitiallise the h2o server from line 97 (for PCA transformed data) or 
   line 245 (for Baseline data) and rerun the loop onwards.
