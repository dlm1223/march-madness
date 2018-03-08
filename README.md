# March Madness Optimization

<br />

This is the full code used for the [Shiny march madness app](https://bracketmath.shinyapps.io/ncaa/). You can run the code yourself to customize more things such as increasing the number of simulations, increasing the bracket pool size, changing the projection model, and more. To run, the order of files is: 1. "simulate" 2. "ownership" 3. "optimize brackets" in that order. Below I explain the methodology. <br /> <br />

### 1. Projection and Simulation

The algorithm starts by projecting all the tournament matchups and then simulating the tournament:

``` r
head(probs, 20)
```

    ##                      R1     R2     R3     R4     R5     R6
    ## 1 Gonzaga        0.9806 0.8524 0.6378 0.4946 0.3310 0.2254
    ## 1 Villanova      0.9810 0.7342 0.5352 0.3962 0.2186 0.1250
    ## 1 North Carolina 0.9886 0.8214 0.6274 0.3792 0.2278 0.1190
    ## 1 Kansas         0.9754 0.7650 0.5116 0.3468 0.1910 0.0940
    ## 2 Kentucky       0.9650 0.6608 0.4714 0.2668 0.1454 0.0678
    ## 2 Louisville     0.9522 0.6632 0.4348 0.2178 0.1124 0.0490
    ## 4 West Virginia  0.8896 0.6620 0.2540 0.1610 0.0892 0.0452
    ## 2 Duke           0.9588 0.6992 0.4740 0.2062 0.0850 0.0418
    ## 2 Arizona        0.9498 0.5706 0.3494 0.1086 0.0484 0.0230
    ## 3 Ucla           0.9630 0.6478 0.2232 0.0946 0.0464 0.0204
    ## 3 Oregon         0.9260 0.6580 0.2700 0.1024 0.0472 0.0198
    ## 4 Purdue         0.8068 0.4992 0.2196 0.1152 0.0504 0.0188
    ## 10 Wichita State 0.7528 0.2742 0.1628 0.0798 0.0412 0.0186
    ## 4 Florida        0.8342 0.5282 0.1818 0.0978 0.0392 0.0186
    ## 5 Virginia       0.7638 0.3842 0.1574 0.0952 0.0376 0.0156
    ## 7 St Marys       0.6622 0.3186 0.2136 0.0812 0.0314 0.0124
    ## 3 Florida State  0.8700 0.5936 0.2734 0.0796 0.0306 0.0120
    ## 4 Butler         0.8416 0.5612 0.1992 0.0760 0.0296 0.0116
    ## 5 Iowa State     0.7298 0.3806 0.1666 0.0864 0.0330 0.0112
    ## 3 Baylor         0.8724 0.4982 0.2062 0.0634 0.0230 0.0088

Above are the probabilities of teams reaching each round for 2017, 5000 simulations <br /> <br />

### 2. Bracket-Pool Simulation

Then, using [ESPN Pick Percentages](http://games.espn.com/tournament-challenge-bracket/2017/en/whopickedwhom), you can simulate a pool of brackets.

``` r
head(ownership, 20)
```

    ##                      R1     R2     R3     R4     R5     R6
    ## 1 North Carolina 0.9834 0.9490 0.8342 0.4366 0.2742 0.1442
    ## 1 Villanova      0.9890 0.9162 0.7964 0.4300 0.2618 0.1322
    ## 2 Duke           0.9882 0.9390 0.7716 0.4112 0.2572 0.1166
    ## 1 Kansas         0.9856 0.9144 0.7836 0.5244 0.2166 0.1076
    ## 1 Gonzaga        0.9834 0.9134 0.6588 0.3740 0.1640 0.0866
    ## 3 Ucla           0.9776 0.8614 0.4660 0.2550 0.1620 0.0860
    ## 2 Kentucky       0.9762 0.8450 0.4394 0.2206 0.1392 0.0730
    ## 2 Arizona        0.9828 0.9002 0.7260 0.3874 0.1508 0.0674
    ## 2 Louisville     0.9746 0.7128 0.4724 0.1726 0.0540 0.0282
    ## 3 Oregon         0.9600 0.7596 0.3014 0.1078 0.0334 0.0184
    ## 4 West Virginia  0.9190 0.4636 0.1282 0.0574 0.0216 0.0112
    ## 7 Michigan       0.7766 0.2364 0.1404 0.0524 0.0190 0.0102
    ## 3 Baylor         0.9182 0.5308 0.1106 0.0354 0.0176 0.0086
    ## 5 Notre Dame     0.8668 0.4660 0.1446 0.0548 0.0172 0.0072
    ## 5 Virginia       0.8228 0.4166 0.0610 0.0264 0.0144 0.0068
    ## 3 Florida State  0.8702 0.5906 0.1352 0.0508 0.0138 0.0068
    ## 4 Florida        0.8744 0.4714 0.0690 0.0270 0.0150 0.0060
    ## 4 Purdue         0.8774 0.4652 0.0660 0.0372 0.0144 0.0058
    ## 4 Butler         0.9308 0.6336 0.0846 0.0228 0.0122 0.0052
    ## 8 Wisconsin      0.6848 0.0568 0.0420 0.0182 0.0112 0.0052

Above are the ownership percentages by round for the pool of brackets, 5000 brackets. You can start to see that certain teams are overvalued in the pool of brackets compared to their projections (Duke, UNC), while others seem to be undervalued in the pool relative to their projection (Gonzaga)

### 3. Optimization

Finally, you can apply your scoring to get finishes for each bracket compared to the others across the simulations, along with mean-points scored for a given bracket across the simulations. With this, you can find different solutions, Ex: 1 Bracket to maximize P(90th percentile). 3 brackets to maximize P(97th), 1 bracket to maximize points, etc. Below is the bracket for 2017 which maximized P(90th percentile).

``` r
bracket<-brackets[which.max(brackets$Prob90), 1:63]
plotBracket(bracket)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

Although this bracket was not successfull (North Carolina won in 2017), the system is still useful in that it gives you the optimal bracket, given your projections and your scoring system. By changing the projections and running over multiple years, it also allows for some interesting analyses like how many upsets you should typically put in your bracket.
