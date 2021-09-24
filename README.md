# Le-Monde-puzzle-1133


    If ADULE-ELUDE=POINT, was is the largest possible value of POINT? With the convention that all letters correspond to different digits and no digit can start with 0. Same question when ADULE+ELUDE=POINT.

The run of a brute force R search return 65934 as the solution (codegolf welcomed!)

    dify<-function(aluda,point) 
      (sum(aluda*10^(4:0))-sum(rev(aluda)*10^(4:0)))
    num2dig<-function(dif) (dif%/%10^(0:4))%%10
    sl=NULL
    for (t in 1:1e6){
      adule=sample(0:9,5)
      while((dify(aluda)<=0)||(!prod(adule[c(1,5)])))
         adule=sample(0:9,5)
    point=rev(num2dig(dify(adule)))
    if ((!sum(duplicated(point)))&(prod(point%in%(0:9)[-adule-1])))
      sl=rbind(sl,c(adule,point))}
    sl=as.matrix(distinct(as.data.frame(sl),.keep_all = TRUE))

where distinct is a dplyr R function.

> 94581-18549
[1] 76032

The code can be easily turned into solving the second question

> 31782+28713
[1] 60495


