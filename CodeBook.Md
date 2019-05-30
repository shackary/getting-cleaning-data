# Codebook  
## Getting and Cleaning Data - Course Project  

### Data  
  
This project makes use of the 
[UCI Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
  
The file `tidy_data.csv` consists of 180 observations, one for each of six
activities undertaken by 30 subjects. The first two columns identify the subject
ID and the activity, and the remaining 66 describe a number of measurements
taken from smartphone accelerometer and gyroscope data.  
  
Measurements are normalized and bounded within [-1,1].  
  
### Variables  
  
#### Subject  
  
The `subject` variable is the anonymous ID number for the subjects in the study.
Range is from 1-30.  
  
#### Activity  
  
The `activity` variable codes for each of the six actitvies measured and has 
6 levels:

* `sitting` = Sitting
* `standing` = Standing
* `laying` = Lying down
* `walking` = Walking
* `walkingDownstairs` = Walking down stairs
* `walkingUpstairs` = Walking up stairs

#### Measurements  
  
The remaining variables provide summaries (as **means**) of the aggregate gyro
and accelerometer activity from each subject during each activity. The variable
names consist of up to 6 segments:

* `t` or `f` = Indicates the (t)ime or (f)requency domain
* `Body` or `Gravity` = Indicates whether the value represents the body or
gravity component of the measurement
* `Accelerometer` or `Gyroscope` = Indicates whether the value represents an 
accelerometer or gyrocsope measurement
* `Jerk` = Optional: indicates that the value represents a jerk signal 
measurement
* `Mag` = Optional: indicates that the value represents magnitude of the
3-dimensional signal
* `Mean` or `Std` = Indicates whether the value represents a mean or a standard
deviation *(note: all the data presented here are given as means across
multiple tests)*
* `X` or `Y` or `Z` = Present for all non-magnitude (`Mag`) measurements: indicates
which axis the measurement was taken on

For example, `tBodyGyroscopeMeanX` represents the mean of body
component of the gyroscope data on the X-axis, as represented on the time 
domain.
  
For more information about how the upstream data were collected and processed, 
visit [the researchers' website](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)  
  
### Transformations  
  
In order to produce the tidy data set, the following transformations were
performed:

1. Combined the test and trial data, including subject and activity levels, into
a single data set
2. Extracted only the values for the means and standard deviations of each 
measurement
3. Coded the activies and variable names to be human-readable
4. Computed the mean of of each variable for each subject and each activity
5. Wrote the results to `tidy_data.csv`
  
The script that performed these transformations is available as `run_analysis.R`
in this repo. The script is annotated with more detail about how each step of 
the transformation was performed.
 