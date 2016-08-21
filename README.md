# Project Description
Getting and Cleaning Data Course Projectменьше 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:</br>

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:</br>

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.</br>

Merges the training and the test sets to create one data set.</br>
Extracts only the measurements on the mean and standard deviation for each measurement.</br>
Uses descriptive activity names to name the activities in the data set</br>
Appropriately labels the data set with descriptive variable names.</br>
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#How to use
<b>1)</b> Download <b>"run_analysis.r"</b> file from this repo into your local machine</br>
<b>2)</b> Download and unzip https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip file.</br>
<b>3)</b> Place <b>"run_analysis.r"</b> file into unzipped "UCI HAR Dataset" folder</br>
<b>4)</b> Run R studio and load <b>"run_analysis.r"</b> file using command 'source("run_analysis.R")'. Once it loaded, script will run automatically. As the result <b>"tidydata.txt"</b> file will be created in "UCI HAR Dataset" folder
