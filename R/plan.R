# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  # Covariates
  calcvars = haven::read_sas(file_in("S:/Regards/analysis/Freeze/20180401/calcvars.sas7bdat")) %>% 
    mutate(id_num = Id_num %>% as.character(),
           inhomedate = as.Date(InHomeDate, origin = "1960-01-01"),
           last_fudate = as.Date(last_fudate, origin = "1960-01-01"),
           region = REGION,
           nonHDL = Cholest-Hdl) %>% 
    select(id_num, Stroke_SR, CAD_SR_ECG,
           inhomedate, last_fudate,    # information needed to calculate time to event
           Age, Race, Gender, Smoke#, SBP, DBP, BMI, Ldl, Hdl, Crp,
           # Income_4cat, ED_Cat, Diab_SRMed_glu, region,
           # Diabetes_Meds_SR_insulin, Diabetes_Meds_SR_pills, Cholest, nonHDL
    ) %>% 
    mutate_if(is.character, factor, exclude=""),
  
  # Retrieve Stroke & CHD
  stroke_dat = haven::read_sas(file_in("S:/Regards/analysis/Freeze/20180401/stroke.sas7bdat")),
  
  chd_dat = haven::read_sas(file_in("S:/Regards/analysis/MI dataset/archive/events_analytic_20170403.sas7bdat")),
  
  
  strk = stroke_dat %>% group_by(id_num) %>%
    summarize(stroke_date = min(stroke_date), strk = 1),
  
  chd = chd_dat %>% filter(chd14==1) %>% group_by(id_num) %>%
    summarize(chd_date = min(chd14dt), chd = 1),
  
  analytic_dat = calcvars %>% filter(!(Stroke_SR=="Y" | CAD_SR_ECG=="Y")) %>% 
    left_join(strk, by = "id_num") %>% 
    left_join(chd, by = "id_num") %>% 
    mutate(
      strk = ifelse(is.na(strk), 0, strk),
      chd = ifelse(is.na(chd), 0, chd),
      Smoke_current = Smoke=="Current",
    ) %>% 
    rowwise() %>%
    mutate(
      type=case_when(
        strk==1 & chd==0 ~ 1,
        strk==0 & chd==1 ~ 2,
        strk==1 & chd==1 &  stroke_date < chd_date ~ 1,
        strk==1 & chd==1 &  stroke_date >= chd_date ~ 2,
        TRUE ~ 0
      ),
      event_time = {
        if(type==1) stroke_date-inhomedate
        else if(type==2) chd_date-inhomedate
        else last_fudate-inhomedate
      }
    ) %>%
    ungroup(),
  
  cs_strk_dat = analytic_dat %>% mutate(type = ifelse(type==1, TRUE, FALSE)),
  cs_chd_dat = analytic_dat %>% mutate(type = ifelse(type==2, TRUE, FALSE)),
  
  strk_cs_mdl = coxph(Surv(event_time, type)~ Smoke_current + Age + Race, data=cs_strk_dat),
  # ggcoxadjustedcurves(strk_cs_mdl, data=cs_strk_dat)
  chd_cs_mdl = coxph(Surv(event_time, type)~ Smoke_current + Age + Race, data=cs_chd_dat),
  
  cs_joint_dat = analytic_dat %>% create_joint_dat(),
  # CHD as the reference level
  # Intr is the additional for strk risk
  joint_cs_mdl = coxph(Surv(event_time, type)~ Smoke_current + intr_smoke + Age + intr_age +Race + 
                          intr_Race+ strata(strt), data=cs_joint_dat)
  
)
