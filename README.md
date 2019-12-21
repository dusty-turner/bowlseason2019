Instructions
============

(Version: 4.1.3)

**This app is a work in progress.  Please check back often to see improvements with the functionality.**

Here are the instructions:

Using the Google form embeded to the right, make your selections.
----------------------------------------------------------------

-   ‘0’ means you have 100% confidence that team ‘A’ will win.
-   ‘100’ means you have 100% confidence that team ‘B’ will win.
-   ‘50’ means that you have no idea who will win.
-   You may update your selections. Selections made after the a game has
    started will not be counted.

Email and Pin
-------

-   Provide an email and pin in order to see your current selections in the 'Selections' tab.
-   As games are played, you will be able to see other people's selections.

Scoring
-------

-   Scores will be made using the Brier score. Essentially the more
    confident you are in a correct pick, the better you will score will
    be. However, don’t be overconfident and wrong, be cause you will be
    penalized.

-   The lower the score the better.

-   Here is the formula.

Sum<sub>all games(t)</sub>[f<sub>t</sub>-o<sub>t</sub>]^2


Where

*S* = total score

*n* = number of games

*t* = specific game

*f*<sub>*t*</sub> = probability that you assigned the home team winning
of game t

*o*<sub>*t*</sub> = 0 if home team lost, 1 if home team won

Notes:

**Lower score is better**

You are essentially penalized the squared difference between the outcome
of the home team (0/1) and the probability you assigned of the home team
winning.
