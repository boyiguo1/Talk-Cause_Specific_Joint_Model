# Custom functions are an important part of a drake workflow.
# This is where you write them.
# Details: https://books.ropensci.org/drake/plans.html#functions

create_joint_dat <- function(dat){
  censored_dat <- dat %>% filter(type==0) %>% 
    mutate(type=FALSE, strt=0)
  type1_dat <- dat %>% filter(type==1) %>%
    mutate(strt = 0, type=TRUE)
  type2_dat <- dat %>% filter(type==2) %>%
    mutate(strt = 1, type=TRUE)
  
  
  type1_dat_dup <- dat %>% filter(type==1) %>% mutate(strt=1, type=FALSE)
  type2_dat_dup <- dat %>% filter(type==2) %>% mutate(strt=0, type=FALSE)
  censored_dat_dup <- censored_dat %>% 
    mutate(strt=1)
  
  
  ret <- rbind(
    censored_dat,
    type1_dat,
    type2_dat,
    type1_dat_dup,
    type2_dat_dup,
    censored_dat_dup
  ) %>% 
    arrange(id_num) %>% 
    rowwise() %>% 
    mutate(
      intr_smoke = {
        if(strt==1) FALSE
        else Smoke_current
      },
       intr_age= {
         if(strt==1) 0
         else Age
       },
      intr_Race = {
        if(strt==1) "B"
        else Race
      }
    ) %>% 
    ungroup()
  
  return(ret)
  
}