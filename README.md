# FRS
Functional code for race scoring

This code computes ranking for athletes who compete in stair races. 

Races are scored thuswise:

If a race is a 100 point race, the winner of each gender get 100 points. 
The next person gets 100*(5/6), the next gets 100*(5/7) etc. Esentially, 
the winer gets 100*(5/5), and the denominator increases for each step down 
the finishing order  

Internally, until printing, the code keeps the scores as rationals ( fractions )
until the results are printed, so as to avoid rounding errors from repeating fractions 
having inexact conversions to flating-point.  

Then we find all the scores for a given athlete in the past year, sort them from 
highest to lowest points scored, and sum the best five races. This is the 
athlete's score.  Then we simply sort by score to rank athletes. 

We also have many sub-scores to be computed.  We separate the males and females 
first, then beak into age groups, then filter out foreigners.  

