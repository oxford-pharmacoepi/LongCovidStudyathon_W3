# Long Covid and PASC Studyathon - Study package
                                                                                      
## Package overview
This package contains all the required code to run for the study on Long Covid
and PASC taking place in Oxford, April 2023.

## Package extraction
You can clone this repository in your console like so, from cmd:
 ``` cmd
git clone git@github.com:oxford-pharmacoepi/Studyathon_LCPASC.git
```
or directly download it as a .zip file into your computer, from the menu above.

## How to use it
You should only need to use the file `CodetoRun.R` in the main folder. 
First fill all the `...` with the information from your database, and change
the required variables (dates, names and database information availability)
to suit your particular needs. 

When you run that script, the whole code will start running. It will first
instantiate the cohorts and then calculate everything for objectives 1, 2 and 3.
If you re-run the code and the cohorts are already instantiated, you can set
`instantiateCohorts = FALSE` so as to not repeat long calculations. Same with
any other part of the study, like the creation of the study cohorts, 
`getStudyCohorts = FALSE`, the incidence calculations, `doIncidencePrevalence = FALSE`,
etc.

## Results
All the results will be zipped into a folder called `[your database name]_Results`. 
Some of them will have to be inspected and tuned afterwards, during the
Studyathon, for instance the LCA clustering. 

## Trajectories
For the trajectories part, note that it might not work if your database
engine is not SQLite or Postgresql. If so, be aware that the package has not been
tested against those types of database engines, so you might have to set it as `FALSE`.

## Plots
The package does offer some visualisations, which you can check in your final
.zip file, but these are mainly orientative, to check it has worked well. 
Further plots will be done after receipt of raw outputs.