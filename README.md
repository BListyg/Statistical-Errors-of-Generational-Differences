# Statistical-Errors-of-Generational-Differences

There's copious methodological research that demonstrates the negative effects categorizing a continuous variable can have on power, information loss, error, etc. The purpose behind this simulation was to help me get an understanding of what happens when age is categorized into the three generations (Millenial, GenX, and Baby Boomers). 

To do this, I created 3 functions that do the following:

* Generate X and Y variables with user-defined R^2 and sample size
  
  * Note: For the purpose of this simulation I made age and the outcome variable (what I call "Entitlement", negatively related such that as age increases Entitlement decreases. Further, I added a vector of N~(0,1) values to the Y variable before it was rescaled to add in measurement error. I will probably take this out later as I am still fine-tuning this.
  
* Rescale the Y variable to be placed on a typical 5-point Likert scale

* Rescale the X variables to be placed within a 22 to 75 range (typical age range of people within the workforce).

* Categorizes the age variable into the "typical" generations used in various research

* Computes 3 linear models regressing Entitlement on:
  * Continuous age
  * Binned generations
  * Randomly assigned generations
  
* Returns the RMSE and MAE of the three models

* the a,b,c,d,e,f and plots are the simulations
  
These three functions differ in that they sample from different types of workplaces (this is my IO background showing): a workplace where age is normally distributed amongst employees (I envisioned a Wal-mart when I made it), a workplace where age is skewed toward older adults, a workplace where age is skewed towards younger adults.

While this is still a work-in-progress (as are tons of other things on my Github), I think the differences between the RMSE for the three conditions are interesting and counterintuitive to what I would've expected. As the R^2 between age and entitlement increases, the two values diverge (this was expected), but they stay relatively even until ~0.5 - ~0.6, going against what I initially thought. Again, this is still a work in progress and I am going to keep chipping away at this as I am learning a lot in the process and find this interesting.
