---
title: Example data
layout: home
nav_order: 3
parent: R package TextSEM
---

# Example data

For illustration, we use a set of student evaluation of teaching data. The data were scraped from an online website conforming to its site requirement, containing 38,240 teaching evaluations on 1,000 instructors.

For each evaluation, we have information on the overall numerical rating of the teaching of the instructor, how difficult the class was, whether the student took the class for credit or not, grade the student received, etc. The data also contain short textual comments about the instructor's teaching, as well as a list of tabs describing the course. Part of the data are shown below:

```
'data.frame' : 38240 obs. of  13 variables:
 $ id        : int  1 2 3 4 5 6 7 8 9 10 ...
 $ profid    : int  1 1 1 1 1 1 1 1 1 1 ...
 $ rating    : num  5 5 4 3 1 5 5 2 3 3 ...
 $ difficulty: int  3 4 5 5 5 5 5 4 5 5 ...
 $ credit    : int  1 1 1 1 1 1 1 1 1 1 ...
 $ grade     : int  5 4 5 7 3 NA 6 7 7 8 ...
 $ book      : int  0 0 0 0 0 1 1 1 1 1 ...
 $ take      : int  1 1 1 0 0 0 1 0 NA NA ...
 $ attendance: int  1 1 0 1 1 1 1 1 1 0 ...
 $ tags      : chr  "respected;accessible outside class;skip 
                    class? you won't pass ." "accessible outside 
                    class;lots of homework;respected" "tough 
                    grader;lots of homework;accessible outside 
                    class" "tough grader;so many papers;lots of 
                    homework" ...
 $ comments  : chr  "best professor i've had in college . only 
                    thing i dont like is the writing assignments" 
                    "Professor has been the best math professor 
                    I've had at thus far . He assigns a heavy 
                    amount of homework but "| __truncated__ "He 
                    was a great professor . he does give a lot 
                    of homework but he will work with you if you 
                    don't clearly unders"| __truncated__ 
                    "Professor is an incredibly respected teacher, 
                    however his class is extremely difficult . I 
                    believe he just ass"| __truncated__ ...
 $ date      : chr  "04/17/2018" "02/13/2018" "01/07/2018" 
                    "12/11/2017" ...
 $ gender    : num  1 1 1 1 1 1 1 1 1 1 ...
```

The data are included with the R package and can be accessed using

```r
data(prof1000)
```
