# CTAMACE Prediction Tool
`CTAMACE` is a web application which can be used to predict major cardiovascular 
events (MACE) two years following 
coronary multidetector computed tomography (MDCT) using combined anatomical coronary findings in MDCT and clinical features

**CTA:** Computed Tomography Angiography  
**MACE:** Major Adverse Cardiovascular Events

## Background
We published a related article with title of  "Comparison of 
Conventional vs. Machine Lerning scoring for 
prediction of Major Cardiovascular Event
 Using Coronary Multidetector Computed Tomography" in `Frontiers in Cardiovascular Medicine`.  We trained seven machine learning (ML) models on patients MDCT dataset to predict MACE at two years following of MDCT. In contrary to previous studies, it utilized many anatomical and clinical features for prediction.  
 All ML models had better or at least the same prediction in comparison to conventional scoring systems.  
 Both linear and non-linear algorithms were utilized to detect diverse type of relations between observations.  
 ML models were as follows:  
 1. Random Forest
 2. Extreme Gradient Boosting (xgboost)
 3. Gradient Boosting Method (GBM)
 4. Generalized Linear Model as Logistic Regression with Ridge Penalty
 5. Feed Forward Neural Network
 2. Stacked Ensembled Generalized Linear Model (Logistic Regression)
 3. Stacked Ensembled Naive-Bayes


### The link to online `CTAMACE`:
**[CTAMACE Prediction Tool](https://behnam-hedayat.shinyapps.io/ctamace)**

## Instructions

### Preparing your dataset
Because preprocessing and training steps were conducted on our dataset, to use the saved models, you should rename your dataset variable to those similar to our study dataset.  
The acceptable variables names are provided in `Prediction Tool` tab under `Variables Name` box.It is a collapsed box which you can extend by clicking on `+` on the right side of box header.

### Uploading dataset
After renaming variables, you can upload your dataset to the api in `Upload File` box. Acceptable file formats are .rds, .csv, .sav and .xlsx. 
If you do not upload a dataset, by default, a new test test with know target variable has been provided to the app to be used for prediction.

### Selecting models and prediction

In `Upload File` section, you can select among seven models we trained in our study. After selecting a model, push the `Predict...` button to initiates prediction on the dataset. It would take a little time to complete prediction process.

### Dataset with known target variable
If your dataset has a column named `Total_MACE` as target variable, the app would peforms prediction and then assesses its prediction performance by different performance measures.


### Dataset with unknown target variable
If your dataset does not have a column names `Total_MACE` as target variable, the app would just performs prediction, then a table of prediction of observations is provided. It would be applicable for prediction of patients at the time of MDCT.

