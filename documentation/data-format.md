#### Data format
Use the following guidelines to prepare you data for modelling using `msm` and `ELECT` packages.  

- Longitudinal in long format (one record per observation); see below
- Interval-censored transition times are OK
- Data with exact time of transition is also OK
- Data with a mix of interval censoring, right censoring, and exact times are OK
- Up to five-state models are OK 
- One of the state is the dead state (else you cannot really talk about life expectancy)
- State are denoted by integers 1,2,3..., with the dead state being the highest integer

Here are two example of the require data format (with x a covariate)

```
  id state   age   x
   3     1   65     19
   3     1   67     19
   3     1   68     19
   3     1   74     19
   3     2   75     19
   3     3   77.3   19
```
and 
```
   id state  age    x
    4     1     0       1
    4     2     1       1
    4     1    1.7     1
    4     1    3.7     1
    4     1    5.7     1
    4     1    7.7     1
    4    -2    9.7     1
```
The -2 in the last example denotes a right-censored state. For the fitting of models it is essential that consecutive records for one individual do not contain the same age. Age can be in years, but also in months. Please review the materials on the ELECT web site: [http://www.ucl.ac.uk/~ucakadl/indexELECT.html](http://www.ucl.ac.uk/~ucakadl/indexELECT.html) to prepare for and gain further understanding of the multistate model and data requirements. 
