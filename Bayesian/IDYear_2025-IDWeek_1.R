library(tidyverse)
library(brms)

source(file="RBDDfun.R")

accdb <- 
  readRDS(file="queries/IDYear_2025-IDWeek_1-accdb.RDS")

# save standalone data and SampleID tables
dat <- 
  accdb$df

dat <- 
  mutate(dat, SampleDateTime__StationCode=paste0(SampleDateTime, "__", StationCode)) # need to add this to ImportMsAccessDb()

SampleID <- 
  accdb$SampleID

# import trap efficiency models
Td44glmm188 <- 
  read_csv("Td44glm188.csv") |> 
  mutate(ReleaseDate=mdy(ReleaseDate), StationCode=factor(StationCode), nRecapture=round(nRelease*E))


REpriors <- 
  c(prior(prior=normal(1, 1), class=b), 
    prior(prior=lkj(2), class=cor), 
    prior(prior=normal(-5, 1), class=Intercept), 
    prior(prior=exponential(0.02), class=phi, lb=0), 
    prior(prior=exponential(2), class=sd, lb=0))


# to change to sum coding
options(contrasts=c('contr.sum','contr.sum'))

t44n188re <-
  brm(
    nRecapture | trials(nRelease) ~ 1 + PercQdi + (1 + PercQdi | StationCode),
    prior=REpriors,
    data=Td44glmm188,
    family=beta_binomial,
    cores=4,
    control=list(adapt_delta=0.99),
    save_pars=save_pars(all=TRUE))

plot(t44n188re)

t44n188re <- 
  add_criterion(t44n188re, criterion="loo", moment_match=TRUE)



# check to make sure there aren't any non-standard RST deployments e.g., Gate 5 West & Gate 5 East
table(dat$StationCode, useNA="ifany") # Bayesian glm allows non-standard RST deployments. The predictions will be on the grand mean with uncertainty drawn from group means


BayesianPd1glm <- 
  function(
    .df, 
    .IDYear=NULL,  ## YYYY
    .IDWeek=NULL,  ## 1:52
    .beginIDDate=NULL,  ## YYYY-MM-DD
    .endIDDate=NULL,  ## YYYY-MM-DD
    .unmarked=TRUE,
    .brmsfit,
    .nRelease,
    .allow_new_levels=FALSE, # allow non-standard RST deployments e.g., Gate 5 West
    .return=c("Pd", "PdDraws", "Qdi", "everything")
  ){
    require(tidyverse)
    options(dplyr.summarise.inform=FALSE)
    .return <- 
      match.arg(.return)
    if(is.null(c(.beginIDDate, .endIDDate))){
      df0 <- 
        filter(.df, IDYear==.IDYear, IDWeek%in%.IDWeek)}
    if(is.null(c(.IDYear, .IDWeek))){
      df0 <- 
        filter(.df, IDDate%in%seq(as.Date(.beginIDDate), as.Date(.endIDDate), by="days"))}
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
      summarize(SampleDateTime__StationCode=unique(SampleDateTime__StationCode), Xdi=sum(unique(Acre_feet), na.rm=TRUE)) |> 
      ungroup() |>
      mutate(
        PercQdi=(Xdi/Qd)*100,
        PercQd=PercQdi)
    Qdi <-
      split(.Qdi, .Qdi$StationCode) |>
      map(mutate, nRelease=.nRelease) |>
      bind_rows(.id="StationCode")
    mat_ <-
      posterior_predict(object=.brmsfit, newdata=Qdi, re_formula=NULL, allow_new_levels=.allow_new_levels)/.nRelease
    colnames(mat_) <- 
      Qdi$SampleDateTime__StationCode
    Td <- 
      left_join(
        x=Qdi, 
        y=as_tibble(mat_, .name_repair="minimal") |> 
          rownames_to_column(var="DrawIndexPosteriorPredict") |>
          pivot_longer(cols=starts_with(match="2"), names_to="SampleDateTime__StationCode", values_to="Tdi"), 
        by=join_by(SampleDateTime__StationCode)) |> 
      mutate(
        SampleDateTime=str_split_i(string=SampleDateTime__StationCode, pattern="__", i=1) |> as_datetime(tz="US/Pacific"), 
        DrawIndexPosteriorPredict=as.integer(DrawIndexPosteriorPredict)) |> 
      group_by(IDDate, DrawIndexPosteriorPredict) |>
      summarise(Td=sum(Tdi), PercQd=sum(PercQdi)) |>
      ungroup() 
    PdDraws <- 
      full_join(
        x=group_by(trg, IDDate, SampleTime, StationCode, OrganismCode, race) |> 
          # summarise(
          reframe(
            raceSubCount=sum(Count), 
            AddCount=unique(AddCount), 
            TotalCount=raceSubCount+AddCount, 
            SampleWeight=unique(SampleWeight), 
            Cdi=TotalCount/SampleWeight) |> 
          group_by(IDDate, OrganismCode, race) |> 
          summarize(Cd=sum(Cdi)) |> 
          ungroup(),
        y=Td,
        by=join_by(IDDate), 
        relationship="many-to-many") |>
      mutate(Pd=Cd/Td) |>
      complete(IDDate, OrganismCode, race, fill=list(Cd=0, Pd=0)) |>
      filter(OrganismCode=="CHN"&race!="n/p" | OrganismCode=="RBT"&race=="n/p") |>
      full_join(y=Td, by=join_by(IDDate, DrawIndexPosteriorPredict, Td, PercQd))
    Pd <- 
      group_by(PdDraws, IDDate, OrganismCode, race, Cd, PercQd) |>
      summarise(
        Td_med=median(Td, na.rm=TRUE),
        Td_L95=quantile(Td, probs=0.025, na.rm=TRUE),
        Td_U95=quantile(Td, probs=0.975, na.rm=TRUE),
        Pd_med=median(Pd, na.rm=TRUE),
        Pd_L95=quantile(Pd, probs=0.025, na.rm=TRUE),
        Pd_U95=quantile(Pd, probs=0.975, na.rm=TRUE)) |>
      ungroup() |>
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
    if(.return=="PdDraws"){
      return(PdDraws)}
    if(.return=="Pd"){
      return(Pd)}
    if(.return=="Qdi"){
      return(as_tibble(select(bind_rows(Qdi), -PercQd, -nRelease)))}
    if(.return=="everything"){
      return(list(Qdi=as_tibble(select(bind_rows(Qdi), -PercQd, -nRelease)), PdDraws=PdDraws, Pd=Pd))}}

Pd <- 
  BayesianPd1glm(
    .df=dat, 
    .beginIDDate="2025-01-01", 
    .endIDDate="2025-01-07", 
    .brmsfit=t44n188re, 
    .nRelease=1000, 
    .return="everything")

Pd$Pd

write_csv(Pd$Pd, file="Bayesian/IDYear_2025-IDWeek_1-Pd.csv")

write_csv(Pd$Qdi, file="Bayesian/IDYear_2025-IDWeek_1-PercQdi.csv")

write_csv(Pd$PdDraws, file="Bayesian/IDYear_2025-IDWeek_1-PdDraws.csv")
