March Madness Optimization
-----------------------------

This is the full code used for the [Shiny march madness
app](https://bracketmath.shinyapps.io/ncaa/). You can run the code
yourself to customize more things such as increasing the number of
simulations, increasing the bracket pool size, changing the projection
model, and more. To run, the order of files is: 1-simulate-tournament,
2-simulate-brackets, 3-calculate payouts, 4-optimize-brackets in that
order. The data for the shiny app is in the Shiny folder but it is just
less customizable. Below I explain the methodology. <br /> <br />

### 1. Projection and Simulation

The algorithm starts by projecting all the tournament matchups and then
simulating the tournament:

    head(probs, 20)

    ##                     R1    R2    R3    R4    R5    R6
    ## 1 Villanova      0.984 0.861 0.660 0.464 0.290 0.184
    ## 1 Virginia       0.969 0.798 0.621 0.425 0.286 0.158
    ## 2 Duke           0.976 0.824 0.499 0.350 0.190 0.120
    ## 2 Purdue         0.961 0.711 0.501 0.229 0.127 0.071
    ## 2 Cincinnati     0.903 0.700 0.472 0.241 0.148 0.065
    ## 1 Kansas         0.912 0.695 0.511 0.243 0.103 0.059
    ## 3 Michigan State 0.901 0.705 0.355 0.225 0.117 0.057
    ## 2 North Carolina 0.975 0.762 0.467 0.250 0.129 0.052
    ## 4 Gonzaga        0.895 0.646 0.387 0.221 0.107 0.044
    ## 1 Xavier         0.975 0.674 0.349 0.192 0.080 0.038
    ## 3 Texas Tech     0.877 0.552 0.231 0.101 0.041 0.018
    ## 5 Ohio State     0.752 0.296 0.147 0.070 0.028 0.018
    ## 3 Michigan       0.820 0.492 0.246 0.124 0.051 0.017
    ## 3 Tennessee      0.861 0.556 0.280 0.109 0.040 0.015
    ## 5 West Virginia  0.868 0.468 0.155 0.065 0.031 0.013
    ## 4 Arizona        0.821 0.480 0.135 0.060 0.030 0.011
    ## 6 Houston        0.662 0.343 0.153 0.054 0.017 0.007
    ## 5 Kentucky       0.685 0.369 0.120 0.055 0.023 0.006
    ## 4 Wichita State  0.877 0.497 0.121 0.054 0.019 0.005
    ## 4 Auburn         0.809 0.459 0.169 0.049 0.013 0.005

Above are the probabilities of teams reaching each round for 2018, 1000
simulations <br /> <br />

### 2. Bracket-Pool Simulation

Then, using [ESPN Pick
Percentages](http://games.espn.com/tournament-challenge-bracket/2018/en/whopickedwhom),
you can simulate a pool of brackets.

    head(ownership, 20)

    ##            Team_Full    R1    R2    R3    R4    R5    R6
    ## 59        1 Virginia 0.986 0.938 0.645 0.532 0.334 0.172
    ## 58       1 Villanova 0.992 0.946 0.817 0.624 0.301 0.156
    ## 29  3 Michigan State 0.975 0.892 0.476 0.330 0.199 0.093
    ## 14            2 Duke 0.979 0.880 0.472 0.297 0.167 0.093
    ## 21          1 Kansas 0.976 0.909 0.759 0.279 0.152 0.081
    ## 36  2 North Carolina 0.986 0.899 0.561 0.347 0.164 0.078
    ## 28        3 Michigan 0.967 0.794 0.331 0.202 0.085 0.044
    ## 2          4 Arizona 0.939 0.598 0.220 0.170 0.095 0.043
    ## 64          1 Xavier 0.979 0.859 0.477 0.187 0.080 0.033
    ## 9       2 Cincinnati 0.976 0.842 0.549 0.111 0.059 0.030
    ## 18         4 Gonzaga 0.961 0.727 0.359 0.156 0.053 0.028
    ## 41          2 Purdue 0.979 0.848 0.602 0.160 0.053 0.022
    ## 23        5 Kentucky 0.830 0.339 0.096 0.078 0.038 0.014
    ## 51       3 Tennessee 0.945 0.681 0.267 0.037 0.018 0.011
    ## 55      3 Texas Tech 0.919 0.562 0.183 0.047 0.018 0.010
    ## 62   4 Wichita State 0.904 0.455 0.063 0.042 0.018 0.008
    ## 61   5 West Virginia 0.864 0.469 0.073 0.050 0.015 0.008
    ## 37      5 Ohio State 0.789 0.220 0.082 0.026 0.013 0.005
    ## 15         6 Florida 0.803 0.349 0.100 0.021 0.008 0.005
    ## 57 13 Unc Greensboro 0.039 0.019 0.009 0.006 0.005 0.005

Above are the ownership percentages by round for the pool of brackets,
1000 brackets. You can start to see that certain teams are overvalued in
the pool of brackets compared to their projections (ex: Mich St), while others seem to
be undervalued in the pool relative to their projection (ex: Villanova).

### 3. Optimization

Finally, you can apply your scoring to get finishes for each bracket in
the pool compared to eachother across the simulations. For example, the 2nd bracket generated in the pool of brackets will have different finishes for each simulation, and so by summarizing its finishes I can get its P(90th percentile) or P(95th percentile). Then you can set up an
optimization to return the optimal bracket(s) for any number of
specifications, Ex: Return 1 Bracket to maximize P(90th percentile). 3
brackets to maximize P(97th), 1 bracket to maximize points, etc. I can
also test out other brackets against the pool of 1000 brackets in order
to get alternative brackets which may do well but weren’t included in
the pool.

Below is the bracket which maximized P(90th percentile) for 2018:

    brackets$Prob90<-apply(brackets[, grepl("Percentile", colnames(brackets)) & !grepl("Actual", colnames(brackets))], 1, function(x) sum(x>=.90)/sims)
    max(brackets$Prob90) #projected P(90th percentile)

    ## [1] 0.306

    plotBracket(brackets[which.max(brackets$Prob90), 1:63], text.size = .8)

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    brackets[which.max(brackets$Prob90), c("Percentile.Actual", "Score.Actual")]

    ##     Percentile.Actual Score.Actual
    ## 261             0.925         1070

Using this system allows you optimize the brackets you enter for march
madness. In addition, it allows you to change the scoring system, pool
sizes, number of brackets entered, and projection system. By testing out
the projected finish based on different strategies like point
maximization, or point maximization in R1-2 only, or other ideas, you
can get an idea of how you should balance expected point maximization
with being contrarian.
