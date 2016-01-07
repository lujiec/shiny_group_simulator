# shiny_group_simulator
This is a simulation app showing the relationship between group distribution and AUC performance, in order to demonstrate the fact that the mixing proportion of trajectories groups has impact on the temporal pattern of performance of predictive model as measured by AUC. 

### Input data file 
- beta.csv: this file contains the coefficiency of polynomial functions used as mean function in simulation

### Input parameters
* GP parameters
  * GP sigma - global sigma (overall noise level of the data)
  * GP L - length parameter in kernal function (smaller-> jumpy, larger-> smooth)
* Group Distribution
* Percentage of Positive
* Show/hide plots

### output plots:
* Simulated trajectory groups
* group by labels
* AUC performance

