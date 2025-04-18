# import data from MS Access
ImportMsAccessDb <- 
  function(
    .beginIDDate,  ## YYYY-MM-DD 
    .endIDDate,  ## YYYY-MM-DD
    .path="S:/MSJM/NEW_DATABASE/DoubleEntry_Database_be.accdb" ## default backend since 2011
  ){
    require(tidyverse) 
    require(RODBC)
    require(hms)
    beginIDDate <- 
      format(date(.beginIDDate), format="%m/%d/%Y")
    endIDDate <- 
      format(date(.endIDDate), format="%m/%d/%Y")
    Database <- 
      odbcConnectAccess2007(access.file=.path)
    Sample_SampleID_TrapEffort <- 
      sqlQuery(
        channel=Database, 
        query=paste0(
          "SELECT Sample.GearID, Sample.LunarPhase, Sample.MethodCode, Sample.Q_SID, Sample.QaDone, Sample.RiverDepth, Sample.SampleDate, Sample.SampleID, Sample.SampleRowID, Sample.SampleTime, Sample.StationCode, Sample.Turbidity, Sample.Updates, Sample.UserName, Sample.Velocity, Sample.WaterTemperature, Sample.WeatherCode, SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID, TrapEffort.Acre_feet, TrapEffort.Area_sq_ft, TrapEffort.Cone, TrapEffort.Counter, TrapEffort.DebrisTubs, TrapEffort.DebrisType, TrapEffort.DepthAdjust, TrapEffort.Diel, TrapEffort.FishProperly, TrapEffort.FlowEndMeter, TrapEffort.FlowSetTime, TrapEffort.FlowStartMeter, TrapEffort.GearConditionCode, TrapEffort.Habitat, TrapEffort.PumpFlow, TrapEffort.RiverFlows, TrapEffort.SampleRowID, TrapEffort.SampleWeight, TrapEffort.SpatialCode, TrapEffort.StartCounter, TrapEffort.Thalweg, TrapEffort.TrapComments, TrapEffort.TrapRowID, TrapEffort.TrapSampleType, TrapEffort.TrapStartDate, TrapEffort.TrapStartTime, TrapEffort.volume
          FROM (Sample INNER JOIN SampleID ON Sample.SampleID = SampleID.SampleID) INNER JOIN TrapEffort ON Sample.SampleRowID = TrapEffort.SampleRowID
          GROUP BY Sample.GearID, Sample.LunarPhase, Sample.MethodCode, Sample.Q_SID, Sample.QaDone, Sample.RiverDepth, Sample.SampleDate, Sample.SampleID, Sample.SampleRowID, Sample.SampleTime, Sample.StationCode, Sample.Turbidity, Sample.Updates, Sample.UserName, Sample.Velocity, Sample.WaterTemperature, Sample.WeatherCode, SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID, TrapEffort.Acre_feet, TrapEffort.Area_sq_ft, TrapEffort.Cone, TrapEffort.Counter, TrapEffort.DebrisTubs, TrapEffort.DebrisType, TrapEffort.DepthAdjust, TrapEffort.Diel, TrapEffort.FishProperly, TrapEffort.FlowEndMeter, TrapEffort.FlowSetTime, TrapEffort.FlowStartMeter, TrapEffort.GearConditionCode, TrapEffort.Habitat, TrapEffort.PumpFlow, TrapEffort.RiverFlows, TrapEffort.SampleRowID, TrapEffort.SampleWeight, TrapEffort.SpatialCode, TrapEffort.StartCounter, TrapEffort.Thalweg, TrapEffort.TrapComments, TrapEffort.TrapRowID, TrapEffort.TrapSampleType, TrapEffort.TrapStartDate, TrapEffort.TrapStartTime, TrapEffort.volume
          HAVING (((SampleID.IDDate)>=#", beginIDDate, "# And (SampleID.IDDate)<=#", endIDDate, "#))
          ORDER BY Sample.SampleDate, Sample.SampleID;"), 
        rows_at_time=1) |>
      select(-SampleID.1, -SampleRowID.1) |>
      as_tibble()
    Catch_Sample_SampleID_TrapEffort <- 
      sqlQuery(
        channel=Database, 
        query=paste0(
          "SELECT Catch.CatchRowID, Catch.CatchSampNum, Catch.Count, Catch.CountRef, Catch.Dead, Catch.ForkLength, Catch.LifeStage, Catch.MarkCode, Catch.Maturation, Catch.OrganismCode, Catch.race, Catch.SampleRowID, Catch.Weight, Sample.GearID, Sample.LunarPhase, Sample.MethodCode, Sample.Q_SID, Sample.QaDone, Sample.RiverDepth, Sample.SampleDate, Sample.SampleID, Sample.SampleRowID, Sample.SampleTime, Sample.StationCode, Sample.Turbidity, Sample.Updates, Sample.UserName, Sample.Velocity, Sample.WaterTemperature, Sample.WeatherCode, SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID, TrapEffort.Acre_feet, TrapEffort.Area_sq_ft, TrapEffort.Cone, TrapEffort.Counter, TrapEffort.DebrisTubs, TrapEffort.DebrisType, TrapEffort.DepthAdjust, TrapEffort.Diel, TrapEffort.FishProperly, TrapEffort.FlowEndMeter, TrapEffort.FlowSetTime, TrapEffort.FlowStartMeter, TrapEffort.GearConditionCode, TrapEffort.Habitat, TrapEffort.PumpFlow, TrapEffort.RiverFlows, TrapEffort.SampleRowID, TrapEffort.SampleWeight, TrapEffort.SpatialCode, TrapEffort.StartCounter, TrapEffort.Thalweg, TrapEffort.TrapComments, TrapEffort.TrapRowID, TrapEffort.TrapSampleType, TrapEffort.TrapStartDate, TrapEffort.TrapStartTime, TrapEffort.volume
          FROM ((Catch INNER JOIN Sample ON Catch.SampleRowID = Sample.SampleRowID) INNER JOIN SampleID ON Sample.SampleID = SampleID.SampleID) INNER JOIN TrapEffort ON Sample.SampleRowID = TrapEffort.SampleRowID
          GROUP BY Catch.CatchRowID, Catch.CatchSampNum, Catch.Count, Catch.CountRef, Catch.Dead, Catch.ForkLength, Catch.LifeStage, Catch.MarkCode, Catch.Maturation, Catch.OrganismCode, Catch.race, Catch.SampleRowID, Catch.Weight, Sample.GearID, Sample.LunarPhase, Sample.MethodCode, Sample.Q_SID, Sample.QaDone, Sample.RiverDepth, Sample.SampleDate, Sample.SampleID, Sample.SampleRowID, Sample.SampleTime, Sample.StationCode, Sample.Turbidity, Sample.Updates, Sample.UserName, Sample.Velocity, Sample.WaterTemperature, Sample.WeatherCode, SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID, TrapEffort.Acre_feet, TrapEffort.Area_sq_ft, TrapEffort.Cone, TrapEffort.Counter, TrapEffort.DebrisTubs, TrapEffort.DebrisType, TrapEffort.DepthAdjust, TrapEffort.Diel, TrapEffort.FishProperly, TrapEffort.FlowEndMeter, TrapEffort.FlowSetTime, TrapEffort.FlowStartMeter, TrapEffort.GearConditionCode, TrapEffort.Habitat, TrapEffort.PumpFlow, TrapEffort.RiverFlows, TrapEffort.SampleRowID, TrapEffort.SampleWeight, TrapEffort.SpatialCode, TrapEffort.StartCounter, TrapEffort.Thalweg, TrapEffort.TrapComments, TrapEffort.TrapRowID, TrapEffort.TrapSampleType, TrapEffort.TrapStartDate, TrapEffort.TrapStartTime, TrapEffort.volume
          HAVING (((SampleID.IDDate)>=#", beginIDDate, "# And (SampleID.IDDate)<=#", endIDDate, "#))
          ORDER BY Sample.SampleDate, Sample.SampleTime;"), 
        rows_at_time=1) |>
      select(-SampleID.1, -SampleRowID.1, -SampleRowID.2) |>
      as_tibble()
    dat <- 
      full_join(
        x=Catch_Sample_SampleID_TrapEffort, 
        y=Sample_SampleID_TrapEffort, 
        by=join_by(
          SampleRowID, GearID, LunarPhase, MethodCode, Q_SID, QaDone, RiverDepth, SampleDate, SampleID, SampleTime,
          StationCode, Turbidity, Updates, UserName, Velocity, WaterTemperature, WeatherCode, IDDate, IDMonth, IDWeek, IDYear, Acre_feet,
          Area_sq_ft, Cone, Counter, DebrisTubs, DebrisType, DepthAdjust, Diel, FishProperly, FlowEndMeter, FlowSetTime, FlowStartMeter,
          GearConditionCode, Habitat, PumpFlow, RiverFlows, SampleWeight, SpatialCode, StartCounter, Thalweg, TrapComments, TrapRowID,
          TrapSampleType, TrapStartDate, TrapStartTime, volume)) |> 
      mutate(
        race=case_match(race, c("W", "w")~"W", c("S", "s")~"S", "F"~"F", "L"~"L", "n/p"~"n/p"),
        SampleDateTime=SampleTime, 
        SampleDate=date(SampleDate), 
        SampleTime=format(SampleTime, format="%H:%M:%S") |> as_hms(), 
        IDDate=date(IDDate),
        TrapStartDateTime=TrapStartTime,
        TrapStartDate=date(TrapStartDate),
        TrapStartTime=format(TrapStartTime, format="%H:%M:%S") |> as_hms()) |> 
      arrange(SampleDateTime, OrganismCode, MarkCode, ForkLength)
    date(dat$SampleDateTime) <- 
      dat$SampleDate
    date(dat$TrapStartDateTime) <- 
      dat$TrapStartDate
    dat <- 
      mutate(dat, SampleMinutes=as.double.difftime(SampleDateTime-TrapStartDateTime)*60)
    SampleID <- 
      sqlQuery(
        channel=Database, 
        query=paste0(
          "SELECT SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID
          FROM SampleID
          GROUP BY SampleID.IDDate, SampleID.IDMonth, SampleID.IDWeek, SampleID.IDYear, SampleID.SampleID
          HAVING (((SampleID.IDDate)>=#", beginIDDate, "# And (SampleID.IDDate)<=#", endIDDate, "#))
          ORDER BY SampleID.IDDate;"), 
        rows_at_time=1) |>
      as_tibble() |>
      mutate(IDDate=date(IDDate), IDYear=as.double(IDYear)) |> 
      arrange(IDDate)
    odbcClose(channel=Database)
    return(list(df=dat, SampleID=SampleID))}


# estimate daily passage by IDYear & IDWeek(s) --OR-- by beginning & ending IDDate
# if setting .yIntercept=FALSE use same setting in WeeklyPhatCI(.yIntercept=FALSE)
estimatePdglm <- 
  function(
    .df, 
    .IDYear=NULL,  ## YYYY
    .IDWeek=NULL,  ## 1:52
    .beginIDDate=NULL,  ## YYYY-MM-DD
    .endIDDate=NULL,  ## YYYY-MM-DD
    .unmarked=TRUE,
    .GlmData,
    .bootModel=c("na", "nonparametric", "parametric"),
    .return=c("Pd", "Qdi", "both")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .bootModel <- 
      match.arg(.bootModel)
    .return <- 
      match.arg(.return)
    if(is.null(c(.beginIDDate, .endIDDate))){
      df0 <- 
        filter(.df, IDYear==.IDYear, IDWeek%in%.IDWeek)}
    if(is.null(c(.IDYear, .IDWeek))){
      df0 <- 
        filter(.df, IDDate%in%seq(as.Date(.beginIDDate), as.Date(.endIDDate), by="days"))}
    if(.bootModel=="na"){
      Data <- 
        split(.GlmData, .GlmData$StationCode)
      Models <- 
        map(Data, \(df) glm(formula=E~PercQdi, data=df, family=binomial, weights=nRelease))}
    if(.bootModel=="nonparametric"){
      bootData <- 
        split(.GlmData, .GlmData$StationCode) |> map(\(df) slice_sample(df, prop=1, replace=TRUE))
      Models <- 
        map(bootData, \(df) glm(formula=E~PercQdi, data=df, family=binomial, weights=nRelease))}
    if(.bootModel=="parametric"){
      Data <- 
        split(.GlmData, .GlmData$StationCode)
      Fits <- 
        map(Data, \(df) glm(formula=E~PercQdi, data=df, family=binomial, weights=nRelease))
      bootData <- 
        map(Fits, simulate) |> map(rename, E=sim_1) |> map2(map(Data, select, -E), bind_cols)
      Models <- 
        map2(Fits, bootData, \(fit, df) update(object=fit, data=df))}
    df1 <- 
      bind_rows(
        filter(df0, OrganismCode!="RBT"|is.na(OrganismCode)), 
        filter(df0, OrganismCode=="RBT", ForkLength<=200), 
        filter(df0, OrganismCode=="RBT", ForkLength>200) |> 
          mutate(CountRef=1)) |>
      filter(CountRef==0|is.na(CountRef))
    if(isTRUE(.unmarked)){
      trg <-
        left_join(
          x=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength!=0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(raceSubCount=sum(Count)) |> 
            mutate(SubCount=sum(raceSubCount), raceProp=raceSubCount/SubCount) |> 
            ungroup(), 
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength==0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(PlusCount=sum(Count)) |> 
            ungroup() |> 
            select(-race), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode)) |>
        replace_na(list(PlusCount=0)) |> 
        mutate(AddCount=raceProp*PlusCount) |>
        right_join(
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a"), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode, race)) |>
        filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p")}
    if(isFALSE(.unmarked)){
      trg <-
        left_join(
          x=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped"), ForkLength!=0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(raceSubCount=sum(Count)) |> 
            mutate(SubCount=sum(raceSubCount), raceProp=raceSubCount/SubCount) |> 
            ungroup(), 
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped"), ForkLength==0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(PlusCount=sum(Count)) |> 
            ungroup() |> 
            select(-race), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode)) |>
        replace_na(list(PlusCount=0)) |> 
        mutate(AddCount=raceProp*PlusCount) |>
        right_join(
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped")), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode, race)) |>
        filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p", ForkLength>0)}
    .Qdi <-
      group_by(df0, IDDate, SampleTime, StationCode, Qd=Q_SID) |> 
      summarize(SampleDateTime=unique(SampleDateTime), Xdi=sum(unique(Acre_feet), na.rm=TRUE)) |> 
      ungroup() |>
      mutate(
        PercQdi=(Xdi/Qd)*100,
        PercQd=PercQdi)
    Qdi <-
      split(.Qdi, .Qdi$StationCode) |>
      map(column_to_rownames, var="SampleDateTime")
    lrm <- setdiff(names(Models), names(Qdi))
    Models[lrm] <- NULL
    Tdi <-
      map2(Models, Qdi, predict.glm, type="response") |>
      map(as.data.frame, nm="Tdi") |> 
      map(rownames_to_column, var="SampleDateTime") |> 
      bind_rows(.id="StationCode") |>
      as_tibble()
    Td <- 
      left_join(
        x=map(Qdi, rownames_to_column, var="SampleDateTime") |> 
          bind_rows() |> 
          as_tibble(), 
        y=Tdi, 
        by=join_by(SampleDateTime, StationCode)) |> 
      mutate(SampleDateTime=as_datetime(SampleDateTime, tz="US/Pacific")) |> 
      group_by(IDDate) |> 
      summarise(
        PercQd=sum(PercQdi), 
        "Td"=sum(Tdi))
    Pd <- 
      full_join(
        x=group_by(trg, IDDate, SampleTime, StationCode, OrganismCode, race) |> 
          summarise(
            raceSubCount=sum(Count), 
            AddCount=unique(AddCount), 
            TotalCount=raceSubCount+AddCount, 
            SampleWeight=unique(SampleWeight), 
            Cdi=TotalCount/SampleWeight) |> 
          group_by(IDDate, OrganismCode, race) |> 
          summarize(Cd=sum(Cdi)) |> 
          ungroup(),
        y=Td,
        by=join_by(IDDate)) |>
      mutate(Pd=Cd/Td) |>
      complete(IDDate, OrganismCode, race, fill=list(Cd=0, Pd=0)) |>
      filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p") |>
      select(-PercQd, -Td) |>
      full_join(y=Td, by=join_by(IDDate)) |>
      left_join(
        y=group_by(trg, IDDate, OrganismCode, race) |> 
          summarize(
            minFL=min(ForkLength), 
            medFL=median(ForkLength),
            maxFL=max(ForkLength), 
            meanFL=mean(ForkLength), 
            sdFL=sd(ForkLength),
            cvFL=sdFL/meanFL) |> 
          ungroup(), 
        by=join_by(IDDate, OrganismCode, race))
    if(.return=="Pd"){
      return(Pd=Pd)}
    if(.return=="Qdi"){
      return(Qdi=as_tibble(select(bind_rows(Qdi), -PercQd)))}
    if(.return=="both"){
      return(list(Qdi=as_tibble(select(bind_rows(Qdi), -PercQd)), Pd=Pd))}}


estimatePd <- 
  function(
    .df, 
    .IDYear=NULL,  ## YYYY
    .IDWeek=NULL,  ## 1:52
    .beginIDDate=NULL,  ## YYYY-MM-DD
    .endIDDate=NULL,  ## YYYY-MM-DD
    .unmarked=TRUE,
    .MSTModel=c("Td32", "Td39", "Td41", "Td42EF", "Td42GH", "Td44", "Td76", "Td79", "Td84", "Td142"),
    .yIntercept=TRUE, ## TRUE (default) fits model w/ y-Intercept; FALSE forces line thru origin
    .bootModel=c("na", "nonparametric", "semiparametric", "parametric")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .MSTModel <- 
      match.arg(.MSTModel)
    .bootModel <- 
      match.arg(.bootModel)
    if(is.null(c(.beginIDDate, .endIDDate))){
      df0 <- 
        filter(.df, IDYear==.IDYear, IDWeek%in%.IDWeek)}
    if(is.null(c(.IDYear, .IDWeek))){
      df0 <- 
        filter(.df, IDDate%in%seq(as.Date(.beginIDDate), as.Date(.endIDDate), by="days"))}
    if(isTRUE(.yIntercept)){
      Model <- 
        lm(Efficiency~PercQd, filter(Reg, MSTModel==.MSTModel))
      if(.bootModel=="na"){
        Model <- Model}
      if(.bootModel=="nonparametric"){
        Model <- 
          lm(Efficiency~PercQd, slice_sample(Model$model, prop=1, replace=TRUE))}
      if(.bootModel=="parametric"){
        bootData <- 
          simulate(Model) |> 
          rename(Efficiency=sim_1) |> 
          bind_cols(select(Model$model, -Efficiency))
        Model <- 
          update(object=Model, data=bootData)}
      if(.bootModel=="semiparametric"){
        Model <- 
          lm(
            Efficiency~PercQd, 
            tibble(
              Efficiency=Model$fitted.values+sample(Model$residuals, replace=TRUE), 
              PercQd=Model$model$PercQd))}}
    if(isFALSE(.yIntercept)){
      Model <- 
        lm(Efficiency~PercQd-1, filter(Reg, MSTModel==.MSTModel))
      if(.bootModel=="na"){
        Model <- Model}
      if(.bootModel=="nonparametric"){
        Model <- 
          lm(Efficiency~PercQd-1, slice_sample(Model$model, prop=1, replace=TRUE))}
      if(.bootModel=="parametric"){
        bootData <- 
          simulate(Model) |> 
          rename(Efficiency=sim_1) |> 
          bind_cols(select(Model$model, -Efficiency))
        Model <- 
          update(object=Model, data=bootData)}
      if(.bootModel=="semiparametric"){
        Model <- 
          lm(
            Efficiency~PercQd-1, 
            tibble(
              Efficiency=Model$fitted.values+sample(Model$residuals, replace=TRUE), 
              PercQd=Model$model$PercQd))}}
    df1 <- 
      bind_rows(
        filter(df0, OrganismCode!="RBT"|is.na(OrganismCode)), 
        filter(df0, OrganismCode=="RBT", ForkLength<=200), 
        filter(df0, OrganismCode=="RBT", ForkLength>200) |> 
          mutate(CountRef=1)) |>
      filter(CountRef==0|is.na(CountRef))
    if(isTRUE(.unmarked)){
      trg <-
        left_join(
          x=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength!=0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(raceSubCount=sum(Count)) |> 
            mutate(SubCount=sum(raceSubCount), raceProp=raceSubCount/SubCount) |> 
            ungroup(), 
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength==0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(PlusCount=sum(Count)) |> 
            ungroup() |> 
            select(-race), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode)) |>
        replace_na(list(PlusCount=0)) |> 
        mutate(AddCount=raceProp*PlusCount) |>
        right_join(
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a"), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode, race)) |>
        filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p")}
    if(isFALSE(.unmarked)){
      trg <-
        left_join(
          x=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped"), ForkLength!=0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(raceSubCount=sum(Count)) |> 
            mutate(SubCount=sum(raceSubCount), raceProp=raceSubCount/SubCount) |> 
            ungroup(), 
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped"), ForkLength==0) |> 
            group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
            summarize(PlusCount=sum(Count)) |> 
            ungroup() |> 
            select(-race), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode)) |>
        replace_na(list(PlusCount=0)) |> 
        mutate(AddCount=raceProp*PlusCount) |>
        right_join(
          y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode%in%c("Ad_pelvic", "Adclipped")), 
          by=join_by(IDDate, StationCode, SampleTime, OrganismCode, race)) |>
        filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p", ForkLength>0)}
    Td <- 
      group_by(df0, IDDate, SampleTime, StationCode, Qd=Q_SID) |> 
      summarize(Xdi=sum(unique(Acre_feet), na.rm=TRUE)) |> 
      group_by(IDDate) |> 
      summarize(Xd=sum(Xdi), Qd=unique(Qd)) |> 
      mutate(PercQd=(Xd/Qd)*100) |> 
      ungroup() |>
      mutate(Td=map_dbl(PercQd, \(PercQd) predict.lm(object=Model, newdata=tibble(`PercQd`=PercQd))))
    Pd <- 
      left_join(
        x=Td, 
        y=group_by(trg, IDDate, SampleTime, StationCode, OrganismCode, race) |> 
          summarize(
            raceSubCount=sum(Count), 
            AddCount=unique(AddCount), 
            TotalCount=raceSubCount+AddCount, 
            SampleWeight=unique(SampleWeight), 
            Cdi=TotalCount/SampleWeight) |> 
          group_by(IDDate, OrganismCode, race) |> 
          summarize(Cd=sum(Cdi)) |> 
          ungroup(), 
        by=join_by(IDDate)) |> 
      mutate(Pd=Cd/Td) |>
      complete(IDDate, OrganismCode, race, fill=list(Cd=0, Pd=0)) |>
      filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p") |>
      select(-Xd, -Qd, -PercQd, -Td) |>
      full_join(y=select(Td, -Xd, -Qd), by=join_by(IDDate)) |>
      left_join(
        y=group_by(trg, IDDate, OrganismCode, race) |> 
          summarize(
            minFL=min(ForkLength), 
            medFL=median(ForkLength),
            maxFL=max(ForkLength), 
            meanFL=mean(ForkLength), 
            sdFL=sd(ForkLength),
            cvFL=sdFL/meanFL) |> 
          ungroup(), 
        by=join_by(IDDate, OrganismCode, race))
    return(Pd=Pd)}


estimatePropCdFrySmolt <- 
  function(
    .df, 
    .IDYear=NULL,  ## YYYY
    .IDWeek=NULL,  ## 1:52
    .beginIDDate=NULL,  ## YYYY-MM-DD
    .endIDDate=NULL  ## YYYY-MM-DD
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    if (is.null(c(.beginIDDate, .endIDDate))){
      df1 <- 
        filter(.df, IDYear==.IDYear, IDWeek%in%.IDWeek)}
    if (is.null(c(.IDYear, .IDWeek))){
      df1 <- 
        filter(.df, IDDate%in%seq(as.Date(.beginIDDate), as.Date(.endIDDate), by="days"))}
    df1 <- 
      bind_rows(
        filter(df1, OrganismCode!="RBT"|is.na(OrganismCode)), 
        filter(df1, OrganismCode=="RBT", ForkLength<=200), 
        filter(df1, OrganismCode=="RBT", ForkLength>200) |> 
          mutate(CountRef=1)) |>
      filter(CountRef==0|is.na(CountRef))
    Cd <-
      left_join(
        x=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength!=0) |> 
          group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
          summarize(raceSubCount=sum(Count)) |> 
          mutate(SubCount=sum(raceSubCount), raceProp=raceSubCount/SubCount) |> 
          ungroup(), 
        y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a", ForkLength==0) |> 
          group_by(IDDate, StationCode, SampleTime, OrganismCode, race) |> 
          summarize(PlusCount=sum(Count)) |> 
          ungroup() |> 
          select(-race), 
        by=join_by(IDDate, StationCode, SampleTime, OrganismCode)) |>
      replace_na(list(PlusCount=0)) |> 
      mutate(AddCount=raceProp*PlusCount) |>
      right_join(
        y=filter(df1, OrganismCode%in%c("CHN", "RBT"), MarkCode=="n/a"), 
        by=join_by(IDDate, StationCode, SampleTime, OrganismCode, race)) |>
      filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p") |>
      mutate(Juvenile=cut(ForkLength, breaks=c(0, 45, 200), label=c("Fry", "Smolt"))) |>
      group_by(IDDate, SampleTime, StationCode, OrganismCode, race, Juvenile) |>
      summarise(
        raceSubCount=sum(Count), 
        AddCount=unique(AddCount), 
        TotalCount=raceSubCount+AddCount, 
        SampleWeight=unique(SampleWeight), 
        Cdi=TotalCount/SampleWeight) |> 
      group_by(IDDate, OrganismCode, race, Juvenile) |> 
      summarize(Cd=sum(Cdi)) |> 
      ungroup() |> 
      pivot_wider(names_from=Juvenile, values_from=Cd, names_prefix="Cd") |>
      replace_na(list(CdFry=0, CdSmolt=0)) |>
      mutate(PropCdFry=CdFry/(CdFry+CdSmolt), PropCdSmolt=CdSmolt/(CdFry+CdSmolt)) |>
      select(-CdSmolt, -CdFry)
    return(Cd=Cd)}


WeeklyPhat <- 
  function(
    .Pd, 
    .SampleID,
    .propFrySmolt,
    .IDYear,  ## YYYY
    .IDWeek,  ## 1:52
    .OrganismCode=c("CHN", "RBT"), 
    .race=c("W", "S", "F", "L", "n/p")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .OrganismCode <- 
      match.arg(.OrganismCode)
    .race <- 
      match.arg(.race)
    PhatDays <- 
      filter(.SampleID, IDYear==.IDYear, IDWeek%in%.IDWeek) |> pull(IDDate)
    df0 <- 
      left_join(x=.Pd, y=.propFrySmolt, by=join_by(IDDate, OrganismCode, race))
    df1 <- 
      filter(df0, IDDate%in%PhatDays, OrganismCode==.OrganismCode, race==.race) |> mutate(PdFry=Pd*PropCdFry, PdSmolt=Pd*PropCdSmolt)
    NN <- 
      length(PhatDays)
    nn <- 
      nrow(df1)
    Phat <- 
      (NN/nn)*c(sum(df1$Pd, na.rm=TRUE), sum(df1$PdFry, na.rm=TRUE), sum(df1$PdSmolt, na.rm=TRUE)) |> replace_na(replace=0)
    names(Phat) <- 
      c("Phat", "FryPhat", "SmoltPhat")
    return(round(Phat))}


# calculate weekly Phat and CI (assuming the Student t distribution)
# missing days are imputed with weekly mean Pd
# if setting .yIntercept=FALSE it's assumed the same setting was used upstream i.e., estimatePd(..., .yIntercept=FALSE)
WeeklyPhatCI <- 
  function(
    .Pd, 
    .SampleID,
    .IDYear,  ## YYYY
    .IDWeek,  ## 1:52
    .MSTModel=c("Td32", "Td39", "Td41", "Td42EF", "Td42GH", "Td44", "Td76", "Td79", "Td84", "Td142"),
    .yIntercept=TRUE, ## TRUE (default) fits model w/ y-Intercept; FALSE forces line thru origin
    .OrganismCode=c("CHN", "RBT"), 
    .race=c("W", "S", "F", "L", "n/p"), 
    .percent=95  ## default 95% CI
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .OrganismCode <- 
      match.arg(.OrganismCode)
    .race <- 
      match.arg(.race)
    .MSTModel <- 
      match.arg(.MSTModel)
    if(isTRUE(.yIntercept)){
      Model <- 
        lm(Efficiency~PercQd, filter(Reg, MSTModel==.MSTModel))
      varTd <- 
        sigma(Model)^2
      varA <- 
        vcov(Model)[1,1]
      varB <- 
        vcov(Model)[2,2]
      covAB <- 
        vcov(Model)[1,2]}
    if(isFALSE(.yIntercept)){
      Model <- 
        lm(Efficiency~PercQd-1, filter(Reg, MSTModel==.MSTModel))
      varTd <- 
        sigma(Model)^2
      varA <- 
        0
      varB <- 
        vcov(Model)
      covAB <- 
        0}
    PhatDays <- 
      filter(.SampleID, IDYear==.IDYear, IDWeek%in%.IDWeek) |> pull(IDDate)
    df1 <- 
      filter(.Pd, IDDate%in%PhatDays, OrganismCode==.OrganismCode, race==.race)
    NN <- 
      length(PhatDays)
    nn <- 
      length(unique(df1$IDDate))
    df2 <- 
      mutate(df1, varPd=(Pd*(1-Td))/Td+varTd*((Pd*(1-Td)+Pd^2*Td))/Td^3)
    Phat <- 
      (NN/nn)*sum(df2$Pd)
    tmat <- 
      left_join(
        x=tibble(IDDate=PhatDays), 
        y=select(df2, IDDate, PropQd=PercQd, Pd, Td) |> 
          mutate(PropQd=PropQd/100), 
        by=join_by(IDDate)) |> 
      select(-IDDate) |> 
      t()
    df3 <- 
      left_join(
        x=data.frame(IDDate=PhatDays), 
        y=select(df2, IDDate, PropQd=PercQd, Pd, Td, varPd) |> 
          mutate(PropQd=PropQd/100), 
        by=join_by(IDDate)) |> 
      select(-IDDate)
    ij <- 
      lapply(seq(NN-1), function(x) x:(NN-1))
    suppressWarnings({
      covPiPj <- 
        lapply(
          X=ij, 
          FUN=function(.col){
            cbind(
              covTiTj=varA+tmat[1,.col[1]]*covAB+tmat[1,.col+1]*covAB+tmat[1,.col[1]]*tmat[1,.col+1]*varB, 
              PiPj=tmat[2,.col[1]]*tmat[2,.col+1], 
              TiTj=tmat[3,.col[1]]*tmat[3,.col+1])})})
    covPiPj <- 
      do.call(rbind, covPiPj)
    covPiPj <- 
      data.frame(covPiPj=covPiPj[,'covTiTj']*covPiPj[,'PiPj']/covPiPj[,'TiTj'])
    sum_covPiPj <- 
      sum(covPiPj[,'covPiPj'], na.rm=TRUE)
    sum_varPd <- 
      sum(df3$varPd, na.rm=TRUE)
    s2Pd <- 
      var(df2$Pd)
    varPhat <- 
      ((1-nn/NN)*NN^2/nn*s2Pd)+((NN/nn)*(sum_varPd+2*sum_covPiPj))
    talpha <- 
      (100-.percent)*0.01
    tt <- 
      qt(p=(1-talpha/2), df=nn-1)
    bounds <- 
      tt*varPhat^0.5
    Lower <- 
      Phat-bounds
    Upper <- 
      Phat+bounds
    Phatd <- 
      left_join(
        x=filter(.SampleID, IDDate%in%PhatDays) |> 
          select(-SampleID), 
        y=select(df1, IDDate, OrganismCode, race, Pd, minFL, maxFL) |> 
          mutate(Pd=round(Pd)), 
        by=join_by(IDDate)) |> 
      replace_na(list(OrganismCode=.OrganismCode, race=.race))
    LowerCI <- 
      paste0((100-.percent)/2, "%")
    UpperCI <- 
      paste0(100-((100-.percent)/2), "%")
    PhatCI <- 
      c(Lower, Phat, Upper) |> round()
    names(PhatCI) <- 
      c(LowerCI, "Phat", UpperCI)
    return(PhatCI)}


# bootstrap estimate of weekly Phat
# missing days imputed using weekly mean bootstrapped Pd
bootWeeklyPhatglm <- 
  function(
    .df, 
    .SampleID,
    .propFrySmolt,
    .IDYear,  ## YYYY
    .IDWeek,  ## 1:52
    .OrganismCode=c("CHN", "RBT"), 
    .race=c("W", "S", "F", "L", "n/p"),
    .GlmData,
    .bootModel=c("na", "nonparametric", "parametric")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .OrganismCode <- 
      match.arg(.OrganismCode)
    .race <- 
      match.arg(.race)
    .bootModel <- 
      match.arg(.bootModel)
    df0 <- 
      estimatePdglm(.df=.df, .IDYear=.IDYear, .IDWeek=.IDWeek, .GlmData=.GlmData, .bootModel=.bootModel, .return="Pd")
    df1 <- 
      left_join(x=df0, y=.propFrySmolt, by=join_by(IDDate, OrganismCode, race))
    PhatDays <- 
      filter(.SampleID, IDYear==.IDYear, IDWeek%in%.IDWeek) |> pull(IDDate)
    df2 <- 
      filter(df1, OrganismCode==.OrganismCode, race==.race) |> mutate(PdFry=Pd*PropCdFry, PdSmolt=Pd*PropCdSmolt)
    NN <- 
      length(PhatDays)
    nn <- 
      nrow(df2)
    df3 <- 
      slice_sample(df2, prop=1, replace=TRUE)
    Phat <- 
      (NN/nn)*c(sum(df3$Pd, na.rm=TRUE), sum(df3$PdFry, na.rm=TRUE), sum(df3$PdSmolt, na.rm=TRUE)) |> replace_na(replace=0)
    names(Phat) <- 
      c("Phat*", "FryPhat*", "SmoltPhat*")
    return(round(Phat))}


bootWeeklyPhat <- 
  function(
    .df, 
    .SampleID,
    .propFrySmolt,
    .IDYear,  ## YYYY
    .IDWeek,  ## 1:52
    .MSTModel=c("Td32", "Td39", "Td41", "Td42EF", "Td42GH", "Td44", "Td76", "Td79", "Td84", "Td142"),
    .yIntercept=TRUE, ## TRUE (default) fits model w/ y-Intercept; FALSE forces line thru origin
    .OrganismCode=c("CHN", "RBT"), 
    .race=c("W", "S", "F", "L", "n/p"),
    .bootModel=c("na", "nonparametric", "semiparametric", "parametric")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .OrganismCode <- 
      match.arg(.OrganismCode)
    .race <- 
      match.arg(.race)
    .MSTModel <- 
      match.arg(.MSTModel)
    .bootModel <- 
      match.arg(.bootModel)
    if(isTRUE(.yIntercept)){
      df0 <- 
        estimatePd(.df=.df, .IDYear=.IDYear, .IDWeek=.IDWeek, .MSTModel=.MSTModel, .yIntercept=TRUE, .bootModel=.bootModel)}
    if(isFALSE(.yIntercept)){
      df0 <- 
        estimatePd(.df=.df, .IDYear=.IDYear, .IDWeek=.IDWeek, .MSTModel=.MSTModel, .yIntercept=FALSE, .bootModel=.bootModel)}
    PhatDays <- 
      filter(.SampleID, IDYear==.IDYear, IDWeek%in%.IDWeek) |> pull(IDDate)
    df1 <- 
      left_join(x=df0, y=.propFrySmolt, by=join_by(IDDate, OrganismCode, race))
    df2 <- 
      filter(df1, OrganismCode==.OrganismCode, race==.race) |> mutate(PdFry=Pd*PropCdFry, PdSmolt=Pd*PropCdSmolt)
    NN <- 
      length(PhatDays)
    nn <- 
      nrow(df2)
    df3 <- 
      slice_sample(df2, prop=1, replace=TRUE)
    Phat <- 
      (NN/nn)*c(sum(df3$Pd, na.rm=TRUE), sum(df3$PdFry, na.rm=TRUE), sum(df3$PdSmolt, na.rm=TRUE)) |> replace_na(replace=0)
    names(Phat) <- 
      c("Phat*", "FryPhat*", "SmoltPhat*")
    return(round(Phat))}

# function to scrape flow data from CDEC and calculate peak daily discharge
DischargeCFS <- 
  function(
    .beginIDDate,  ## YYYY-MM-DD 
    .endIDDate  ## YYYY-MM-DD
  ){
    require(rvest) 
    require(tidyverse)
    TimeZoneStart <- 
      as_datetime(paste(.beginIDDate, "07:00:00"), tz="US/Pacific") |> 
      format(format="%Z")
    TimeZoneEnd <- 
      as_datetime(paste(.endIDDate, "07:00:00"), tz="US/Pacific") |> 
      format(format="%Z")
    if(TimeZoneStart=="PDT"){HourStart <- "08"}
    if(TimeZoneStart=="PST"){HourStart <- "07"}
    if(TimeZoneEnd=="PDT"){HourEnd <- "08"}
    if(TimeZoneEnd=="PST"){HourEnd <- "07"}
    Start <- date(.beginIDDate) - 1
    End <- date(.endIDDate)
    FlowsURL <- 
      paste0(
        "https://cdec.water.ca.gov/dynamicapp/selectQuery?Stations=BND&SensorNums=20&dur_code=H&Start=", 
        Start, 
        "T", 
        HourStart, 
        "%3A00&End=", 
        End, 
        "T",
        HourEnd,
        "%22") 
    FlowsHTML <- read_html(FlowsURL)
    FlowsTable <- FlowsHTML |> html_nodes("table") %>% .[[2]] 
    FlowsTibble <- 
      FlowsTable |> 
      html_table() |> 
      select(SampleDateTime=1, FlowCFS=2) |> 
      filter(FlowCFS!="--") |>
      mutate(
        SampleDateTime=mdy_hm(SampleDateTime, tz="US/Pacific"), 
        SampleDate=date(SampleDateTime), 
        SampleTime=hour(SampleDateTime), 
        IDDate=if_else(SampleTime%in%8:23, true=SampleDate+1, false=SampleDate),
        FlowCFS=str_remove_all(string=FlowCFS, pattern=",") |> as.numeric()) 
    FlowCFS <- 
      group_by(FlowsTibble, IDDate) |> 
      summarise(
        AvgDischargeCFS=mean(FlowCFS, na.rm=TRUE) |> round(), 
        PeakDischargeCFS=max(FlowCFS, na.rm=TRUE) |> round())
    return(FlowCFS)}

# function to scrape flow data from CDEC and calculate mean discharge for 1st day of a trap efficiency trial
ReleaseRecaptureCFS <- 
  function(
    .ReleaseDate,  ## "YYYY-MM-DD" 
    .ReleaseTime,  ## "HH:MM:SS"
  ){
    require(rvest) 
    require(tidyverse) 
    require(hms)
    Start <- date(.ReleaseDate)
    End <- Start + 1
    TimeZoneStart <- 
      as_datetime(paste(Start, .ReleaseTime), tz="US/Pacific") |> 
      format(format="%Z")
    TimeZoneEnd <- 
      as_datetime(paste(End, "07:00:00"), tz="US/Pacific") |> 
      format(format="%Z")
    if(TimeZoneStart=="PDT"){HourStart <- as_hms(.ReleaseTime) |> hour()}
    if(TimeZoneStart=="PST"){HourStart <- as_hms(.ReleaseTime) |> hour() - 1}
    if(TimeZoneEnd=="PDT"){HourEnd <- "08"}
    if(TimeZoneEnd=="PST"){HourEnd <- "07"}
    FlowsURL <- 
      paste0(
        "https://cdec.water.ca.gov/dynamicapp/selectQuery?Stations=BND&SensorNums=20&dur_code=H&Start=", 
        Start, 
        "T", 
        HourStart, 
        "%3A00&End=", 
        End, 
        "T",
        HourEnd,
        "%22") 
    FlowsHTML <- read_html(FlowsURL)
    FlowsTable <- FlowsHTML |> html_nodes("table") %>% .[[2]] 
    FlowsTibble <- 
      FlowsTable |> 
      html_table() |> 
      select(SampleDateTime=1, FlowCFS=2) |> 
      filter(FlowCFS!="--") |>
      mutate(
        SampleDateTime=mdy_hm(SampleDateTime, tz="US/Pacific"), 
        SampleDate=date(SampleDateTime), 
        SampleTime=hour(SampleDateTime), 
        IDDate=if_else(SampleTime%in%8:23, true=SampleDate+1, false=SampleDate),
        FlowCFS=str_remove_all(string=FlowCFS, pattern=",") |> as.numeric()) 
    FlowCFS <- 
      group_by(FlowsTibble, IDDate) |> 
      summarise(AvgDischargeCFS=mean(FlowCFS, na.rm=TRUE) |> round()) |> 
      mutate(ReleaseDate=date(.ReleaseDate), ReleaseTime=as_hms(.ReleaseTime)) |>
      relocate(ReleaseDate, ReleaseTime, .after=IDDate)
    return(FlowCFS)}
