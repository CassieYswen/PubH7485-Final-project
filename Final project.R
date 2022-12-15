+## create table one and calculate unadjusted ATE
  #install.packages("labelled")
  library(ggplot2)
library(dplyr)
library(forcats)
library(knitr)
library(forcats)
library(labelled)
library(tableone)
library(MASS)
library(gtsummary)
library(rms)
library("MatchIt")
#library(mice)
household<-read.csv("/Users/wenys/Downloads/PubH7485/atus_00002.csv", header=TRUE, 
                    na.strings = c(99,996,
                                   997,998,999, 
                                   9995,9999,99999.99 ))

household1 <- subset(household, select = -c(YEAR, CASEID,PERNUM,LINENO,WT20,HH_CHILD,KIDUND18))

household1<-household1%>%
  filter(!is.na(COVIDTELEW))

household1$HOUSETYPE<-as.factor(household1$HOUSETYPE)
household1$RACE<-as.factor(household1$RACE)

household1<-household1%>%
  mutate(ACT_HHACT = ifelse(is.na(ACT_HHACT), 99,ACT_HHACT))%>%
  mutate(METRO =factor(METRO),
         METRO =fct_collapse(METRO, "metropolitan"=c("1","2","3"),
                             "nonmetropolitan"="4",
                             "NA"="5"),
         FAMINCOME=factor(FAMINCOME),
         FAMINCOME=fct_recode(FAMINCOME,"Less than $5,000"="1",
                              "$5,000 to $7,499"="2",
                              "$7,500 to $9,999"="3",
                              "$10,000 to $12,499"="4",
                              "$12,500 to $14,999"="5",
                              "$15,000 to $19,999"="6",
                              "$20,000 to $24,999"="7",
                              "$25,000 to $29,999"="8",
                              "$30,000 to $34,999"="9",
                              "$35,000 to $39,999"="10",
                              "$40,000 to $49,999"="11",
                              "$50,000 to $59,999"="12",
                              "$60,000 to $74,999"="13",
                              "$75,000 to $99,999"="14",
                              "$100,000 to $149,999"="15",
                              "$150,000 and over"="16"),
         HHTENURE=factor(HHTENURE),
         HHTENURE=fct_recode(HHTENURE,"owned"="1",
                             "rent"="2",
                             "occupied"="3"
         ),
         HOUSETYPE=factor(HOUSETYPE),
         HOUSETYPE=fct_recode(HOUSETYPE,"House, apartment, flat"="1",
                              "Housing unit in nontransient hotel, motel, etc."="2",
                              "Housing unit permanent in transient hotel, motel"="3",
                              "Housing unit in rooming house"="4",
                              "Mobile home or trailer with no permanent room added"="5",
                              "Mobile home or trailer with 1 or more rooms added"="6",
                              "Housing unit not specified above"="7",
                              #"Quarters not housing unit in rooming boarding house"="8",
                              #"Unit not permanent, in transient hotel or motel"="9",
                              #"Unoccupied tent site or trailer site"="10",
                              #"Student quarters in college dorm"="11",
                              "Other unit not specified above"="12"),
         SEX=factor(SEX),
         SEX=fct_recode(SEX, "male"="1",
                        "female"="2"),
         RACE=factor(RACE),
         RACE=fct_recode(RACE,"Black only"="110" ,"White only"="100", "Hawaiian Pacific Islander only"="132", 
                         "Asian only"="131",
                         " White-Black"="200", 
                         "Black-American Indian"="210", 
                         " American Indian, Alaskan Native"="120", 
                         "Asian-Hawaiian"="230", 
                         "White-American Indian"="201", 
                         "White-Asian"="202", 
                         "White-Black-American Indian"="300", 
                         "Black-Asian"="211", 
                         "American Indian-Asian"="220" ,
                         "White-Hawaiian"="203" ,
                         "Black-Hawaiian"="212" ,
                         "White-Asian-Hawaiian"="320",
                         "White-American Indian-Asian"="310"),
         MARST=factor(MARST),
         MARST=fct_recode(MARST,"Married - spouse present"="1",
                          "Married - spouse absent"="2",
                          "Widowed"="3",
                          "Divorced"="4",
                          "Separated"="5",
                          "Never married"="6"),
         EDUCYRS=factor(EDUCYRS),
         EDUCYRS=fct_recode(EDUCYRS,"Master's degree"="316",
                            "Bachelor's degree"="217",
                            "Twelfth grade"="112",
                            "College--two years"="214",
                            "College--three years"="215",
                            "Professional degree"="320",
                            "Eleventh grade"="111",
                            "First through fourth grade"="102",
                            "Doctoral degree"="321",
                            " Ninth grade"="109",
                            "College--one year"="213",
                            "College--four years"="216",
                            "Tenth grade"="110",
                            "Fifth through sixth grade"="105",
                            "Less than first grade"="101",
                            "Seventh through eighth grade"="107"),
         MULTJOBS=factor(MULTJOBS),
         MULTJOBS=fct_recode(MULTJOBS,"Yes"="1",
                             "No"="0"),
         FULLPART=factor(FULLPART),
         FULLPART=fct_recode(FULLPART, "full time"="1",
                             "part time"="2"),
         SPOUSEPRES=factor(SPOUSEPRES),
         SPOUSEPRES=fct_recode(SPOUSEPRES,"Spouse present"="1",
                               "Unmarried partner present"="2",
                               "No spouse or unmarried partner present"="3"),
         KIDUND1=factor(KIDUND1),
         KIDUND1=fct_recode(KIDUND1, "No"="0",
                            "Yes"="1"),
         COVIDTELEW=factor(COVIDTELEW),
         COVIDTELEW=fct_recode(COVIDTELEW,"No"="1",
                               "Yes"="2")
  )
household1 <- set_variable_labels(household1, METRO = "Metropolitan/central city ", FAMINCOME  = "Family income",
                                  HH_SIZE  = "Number of people in household ", HHTENURE  = "Living quarters owned, rented, or occupied without rent",AGE="Age",SEX="Sex",RACE="Race",
                                  HOUSETYPE  = "Type of housing unit", HH_NUMKIDS = "Number of children under 18 in household",
                                  HH_NUMADULTS  = "Number of adults in household ", MARST  = "Marital status",
                                  EDUCYRS  = "Years of education ", MULTJOBS  = "Has more than one job ", FULLPART = "Full time/part time employment status ",
                                  UHRSWORKT = "Hours usually worked per week", EARNWEEK  = "Weekly earnings",
                                  SPOUSEPRES  = "Spouse or unmarried partner in household ", KIDUND1  = "Own child under 1 in household ",
                                  COVIDTELEW = "Worked remotely for pay due to COVID-19 pandemic ", ACT_HHACT = "Household activities")





#for (i in 1:24){
#cat(i, sum(is.na(household1[ ,i])), "\n")
#}

#summary(household1[13])
#summary(household1[14])
#summary(household1[15])
#summary(household1[16])

vars = setdiff(colnames(household1), c("COVIDTELEW", "MULTJOBS", #so treatment and the outcomes aren't shown in the SMD plot
                                       "ACT_HHACT"))
t1 <- CreateTableOne(vars = vars,data = household1, , strata = "COVIDTELEW", test = FALSE)
colnames <- c("Level", "No", "Yes", "SMD")

invisible(capture.output(table <- print(t1, showAllLevels = TRUE, smd = TRUE, varLabels = TRUE) ))
kable(table, col.names = colnames, caption = "Table One for All Study Covariates")





household1$UHRSWORKT<-as.numeric(household1$UHRSWORKT)
#household1$MULTJOBS_imputed<-ifelse(is.na(household1$MULTJOBS), "0", household1$MULTJOBS)
#household1$FULLPART_imputed<-ifelse(is.na(household1$FULLPART), "1", household1$FULLPART)
#household1$UHRSWORKT_imputed<-ifelse(is.na(household1$UHRSWORKT), "40.00", household1$UHRSWORKT)
#household1$EARNWEEK_imputed<-ifelse(is.na(household1$EARNWEEK ), "1015.00 ", household1$EARNWEEK)
household1= complete(mice(household1, m = 1, maxit = 10, seed = 777, printFlag = FALSE))
#household1<-subset(household1,select=-c(MULTJOBS,FULLPART,UHRSWORKT,EARNWEEK))
#househole activity
hha_t <- household1$ACT_HHACT[household1$COVIDTELEW == "Yes"] 
hha_c <- household1$ACT_HHACT[household1$COVIDTELEW == "No"] 
et_hha <- mean(hha_t)
ec_hha <- mean(hha_c)
# multiple jobs
household1$MULTJOBS<-as.numeric(household1$MULTJOBS)
xjobs_t <- household1$MULTJOBS[household1$COVIDTELEW == "Yes"] 
xjobs_c <- household1$MULTJOBS[household1$COVIDTELEW == "No"] 
et_xjobs <- mean(xjobs_t)
ec_xjobs <- mean(xjobs_c)
# Unadjusted ATE for household activity&multiple jobs
print(et_hha - ec_hha, digits = 3)
print(et_xjobs - ec_xjobs, digits = 3)
num_t <- table(household1$COVIDTELEW)[2]
num_c <- table(household1$COVIDTELEW)[1]
# Standard Error for household activity&multiple jobs
se_hha <- sqrt(var(hha_t)/num_t + var(hha_c)/num_c)
se_xjobs <- sqrt(var(xjobs_t)/num_t + var(xjobs_c)/num_c)
print(se_hha, digits = 3)
print(se_xjobs, digits = 3)

# Confidence Interval for household activity
conf_int_hha <- abs(et_hha - ec_hha) + c(-1, 1)*qnorm(0.975)*se_hha
conf_int_xjobs <- abs(et_xjobs - ec_xjobs) + c(-1, 1)*qnorm(0.975)*se_xjobs
print(conf_int_hha, digits = 3)
print(conf_int_xjobs, digits = 3)

#Function to get the bootstrap SE
get_boot_ci <- function(x, method = "normal", level) {
  if (method == "normal") {
    m <- mean(x)
    se <- sd(x)
    mult <- qnorm((1 + level)/2)
    ci <- m + c(-1, 1) * mult * se
  } else if (method == "quantile" | method == "percentile") {
    ci <- unname(quantile(x, c((1 - level)/2, (1 + level) / 2)))
  } else {
    error("Invalid confidence interval method")
  }
  return(ci)
}
#Function that estimates the ATE and Bootstrap SE using propensity score stratification
get_ate_pss <- function(formula, data, level = 0.95, response, B = 100) {
  ps_mod <- glm(formula = formula, data = data, family = "binomial") #this is the propensity score model
  ps <- predict(ps_mod, type = "response")
  ps_quintile <- cut(ps, breaks = c(0, quantile(ps, p = c(0.2, 0.4, 0.6, 0.8)), 1), labels = 1:5)
  n <- nrow(data)
  nj <- table(ps_quintile)
  te_quintile <- with(data, tapply(get(response)[A == 1], ps_quintile[A == 1], mean) - #with(data,get(response)) is a cool way to get column of values for the                                                                                          variable you want
                        tapply(get(response)[A == 0], ps_quintile[A == 0], mean)
  )
  ate_est <- sum(te_quintile * nj/n)
  
  # bootstrap for SE and CI
  ate_boot <- replicate(B, expr = {
    samp <- slice_sample(.data = data, prop = 1, replace = TRUE)
    ps_mod_boot <- glm(formula = formula, data = samp, family = "binomial")
    ps_boot <- predict(ps_mod_boot, type = "response")
    ps_quintile_boot <- cut(
      ps_boot, breaks = c(0, quantile(ps, p = c(0.2, 0.4, 0.6, 0.8)), 1), labels = 1:5)
    n <- nrow(samp)
    nj <- table(ps_quintile_boot)
    te_quintile <- with(samp, tapply(get(response)[A == 1], ps_quintile[A == 1], mean) -
                          tapply(get(response)[A == 0], ps_quintile[A == 0], mean)
    )
    sum(te_quintile * nj/n)
  })
  
  ci <- get_boot_ci(ate_boot, level = level, method = "normal")
  ci_print <- paste0("(", paste(style_number(unname(ci), digits = 2), collapse = ", "), ")")
  se <- sd(ate_boot)
  
  return(list(est = ate_est, se = se, ci = ci, ci_print = ci_print, 
              ps = ps))
}

#Function that estimates the ATE and Bootstrap SE using propensity score IPW2
get_ate_ipw<- function(formula, data, level = 0.95, response, B = 100) {
  ps_mod <- glm(formula = formula, data = data, family = "binomial")
  ps <- predict(ps_mod, type = "response")
  w1 <- data$A/ps
  w0 <- (1 - data$A)/(1 - ps)
  w <- with(data, w1 * A + w0 * (1 - A))
  ate_est <- with(data, weighted.mean(get(response), w1) - 
                    weighted.mean(get(response), w0))
  
  # bootstrap for SE and CI
  ate_boot <- replicate(B, expr = {
    samp <- slice_sample(.data = data, prop = 1, replace = TRUE)
    ps_mod_boot <- glm(formula = formula, data = samp, family = "binomial")
    ps_boot <- predict(ps_mod_boot, type = "response")
    w1_boot <- samp$A/ps_boot
    w0_boot <- (1 - samp$A)/(1 - ps_boot)
    with(samp, weighted.mean(get(response), w1_boot) - 
           weighted.mean(get(response), w0_boot))
  })
  
  ci <- get_boot_ci(ate_boot, level = level, method = "normal")
  ci_print <- paste0("(", paste(style_number(unname(ci), digits = 2), collapse = ", "), ")")
  se <- sd(ate_boot)
  
  return(list(est = ate_est, se = se, ci = ci, ci_print = ci_print,
              ps = ps, w1 = w1, w0 = w0, w = w))
}

get_ate_reg_binary <- function(formula, data, level = 0.95, B = 100) {
  reg_mod <- glm(formula = formula, data = data, family = "binomial")
  data_trt <- data_ctr <-data
  data_trt$A = 1
  data_ctr$A = 0
  pred1 <- predict(reg_mod, newdata = data_trt, type = "response")
  pred0 <- predict(reg_mod, newdata = data_ctr, type = "response")
  ate_est <- mean(pred1 - pred0)
  
  # bootstrap for SE and CI
  ate_boot <- replicate(B, expr = {
    samp <- slice_sample(.data = data, prop = 1, replace = TRUE)
    reg_mod <- glm(formula = formula, data = samp, family = "binomial")
    samp_trt <- samp_ctr <-samp
    samp_trt$A = 1
    samp_ctr$A = 0
    samp_pred1 <- predict(reg_mod, newdata = samp_trt, type = "response")
    samp_pred0 <- predict(reg_mod, newdata = samp_ctr, type = "response")
    mean(samp_pred1 - samp_pred0)
  })
  
  ci <- get_boot_ci(ate_boot, level = level, method = "normal")
  ci_print <- paste0("(", paste(style_number(unname(ci), digits = 2), collapse = ", "), ")")
  se <- sd(ate_boot)
  
  return(list(est = ate_est, se = se, ci = ci, ci_print = ci_print))
}

get_ate_reg_continuous <- function(formula, data, level = 0.95, B = 100) {
  reg_mod <- lm(formula = formula, data = data)
  
  data_trt <- data_ctr <-data
  data_trt$A = 1
  data_ctr$A = 0
  pred1 <- predict(reg_mod, newdata = data_trt, type = "response")
  pred0 <- predict(reg_mod, newdata = data_ctr, type = "response")
  ate_est <- mean(pred1 - pred0)
  
  # bootstrap for SE and CI
  ate_boot <- replicate(B, expr = {
    samp <- slice_sample(.data = data, prop = 1, replace = TRUE)
    reg_mod <- lm(formula = formula, data = samp)
    
    samp_trt <- samp_ctr <-samp
    samp_trt$A = 1
    samp_ctr$A = 0
    samp_pred1 <- predict(reg_mod, newdata = samp_trt, type = "response")
    samp_pred0 <- predict(reg_mod, newdata = samp_ctr, type = "response")
    mean(samp_pred1 - samp_pred0)
  })
  
  ci <- get_boot_ci(ate_boot, level = level, method = "normal")
  ci_print <- paste0("(", paste(style_number(unname(ci), digits = 2), collapse = ", "), ")")
  se <- sd(ate_boot)
  
  return(list(est = ate_est, se = se, ci = ci, ci_print = ci_print))
}

#turning telehealth into "A"
household1$A = ifelse(household1$COVIDTELEW == "Yes",1,0)
#household1<-subset(household1,select = -c(MULTJOBS,FULLPART,UHRSWORKT,EARNWEEK,COVIDTELEW))
# Add squared terms to dataset directly
#--------------------------------------------
#household1<-subset(household1,select=-c(MULTJOBS,FULLPART,UHRSWORKT,EARNWEEK))
household1_sq <-household1%>%
  select(-COVIDTELEW,-EDUCYRS)%>%
  mutate(
    across(where(is.numeric) & !c(ACT_HHACT,A), function(x) x^2, .names = "{.col}_sq")
  )
#find the best fitted propensity score model 
ps_mod = stepAIC(glm(A ~ . - ACT_HHACT+ HH_NUMADULTS:HOUSETYPE+FULLPART:EARNWEEK+MULTJOBS:EARNWEEK+UHRSWORKT:EARNWEEK+AGE:EARNWEEK,data=household1_sq),
                 direction="both",trace=FALSE)
#find the best fitted reg model for household activity
act_mod=stepAIC(lm(ACT_HHACT ~ A*(. ),data=household1_sq),
                direction="both",trace=FALSE)
#fine the best fitted reg model for multiple job
xjob_mod=stepAIC(glm(MULTJOBS ~ . ,data=household1_sq),
                 direction="both",trace=FALSE)
## propensity score stratification & ipw2 & regression adjustment results of household activities
set.seed(070876681)
ate_househole_ipw <- get_ate_ipw(ps_mod$formula, data = household1_sq,response="ACT_HHACT")
ate_househole_pss <- get_ate_pss(ps_mod$formula, data = household1_sq,response="ACT_HHACT")
ate_househole_reg <- get_ate_reg_continuous(lm(ACT_HHACT ~ A + METRO + HH_SIZE + HHTENURE + HH_NUMADULTS + 
                                                 AGE + SEX + RACE + MULTJOBS + FULLPART + UHRSWORKT + EARNWEEK + 
                                                 HH_SIZE_sq + HH_NUMKIDS_sq + HH_NUMADULTS_sq + AGE_sq + UHRSWORKT_sq + 
                                                 EARNWEEK_sq + A:METRO + A:HH_NUMADULTS + A:FULLPART + A:UHRSWORKT + 
                                                 A:EARNWEEK + A:HH_SIZE_sq + A:HH_NUMKIDS_sq + A:HH_NUMADULTS_sq + 
                                                 A:EARNWEEK_sq, data = household1_sq), data = household1_sq)
## propensity score stratification & ipw2 & regression adjustment results of multiple jobs
ate_xjobs_ipw <- get_ate_ipw(ps_mod$formula, data = household1_sq,response="MULTJOBS")
ate_xjobs_pss <- get_ate_pss(ps_mod$formula, data = household1_sq,response="MULTJOBS")
ate_xjobs_reg <- get_ate_reg_continuous(xjob_mod$formula, data = household1_sq)



## Creating the SMD plot

library(survey)
library(tidyverse)
vars = setdiff(colnames(household1), c("COVIDTELEW", "MULTJOBS", #so treatment and the outcomes aren't shown in the SMD plot
                                       "ACT_HHACT"))

#We only want to assess balance between the important covariates - particularly those used in the models  



#Balanced resulting from using the final propensity score model
svy_d <- household1 %>%
  svydesign(ids = ~ 1, data = ., weights = ~ ate_househole_ipw$w)
tabWeighted_d <- svyCreateTableOne(vars = vars, strata = "COVIDTELEW",
                                   data = svy_d, test = FALSE)


dataPlot <- data.frame(variable  = rownames(ExtractSmd(t1)),
                       Unweighted = as.numeric(ExtractSmd(t1)),
                       `Weighted ` = as.numeric(ExtractSmd(tabWeighted_d))
)

## Create long-format data for ggplot2
dataPlotMelt <- reshape2::melt(data = dataPlot,
                               id.vars       = c("variable"),
                               variable.name = "Method",
                               value.name    = "SMD")

## Order variable names by magnitude of SMD
varNames <- as.character(dataPlot$variable)[order(dataPlot$Unweighted)]

## Order factor levels in the same order
dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                levels = varNames)

## Plot using ggplot2
ggplot(data = dataPlotMelt,
       mapping = aes(x = variable, y = SMD, group = Method, color = Method)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  labs(x = "") +
  scale_x_discrete(labels = lapply(
    list(
      EDUCYRS = "Eduration Years",
      EARNWEEK = "Weekly Earning",
      FAMINCOME = "Family Income",
      FULLPART = "Full/Part Time Job",
      METRO = "Metro area",
      RACE = "Race",
      MARST = "Marital Status",
      UHRSWORKT = "Hours spent on work",
      SPOUSEPRES = "Spouse present",
      HOUSETYPE = "Housetype",
      SEX = "Sex",
      HHTENURE = "House ownership",
      AGE = "Age",
      
      HH_NUMADULTS = "Number of adults",
      KIDUND1 = "Kids under 1 year old",
      HH_NUMKIDS= "Number of children under 18",
      HH_SIZE="Number of people  "
    ),
    str_wrap, width = 30)
  ) +
  scale_color_discrete(
    labels = list(Unweighted = "Unweighted",
                  
                  Weighted = "Final PS Model")
  ) +
  coord_flip() +
  theme_bw() + 
  theme(legend.key = element_blank(), legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = 2))

#create a mediator
mediator<-household1$HH_SIZE
household_m<-subset(household1,select=-HH_SIZE)
household_m$mediator<-mediator

### perform variable selection via stepwise selection
med_mod=stepAIC(lm(mediator ~ COVIDTELEW*(. -EDUCYRS),data=household_m),
                direction="both",trace=FALSE)

########################## Estimate CDE and NIE for househole activities #################### ####################################################################################



m = mean(household_m$mediator)



mod.outcome<-lm(ACT_HHACT ~ HH_NUMKIDS + HH_NUMADULTS + AGE + SEX + 
                  MARST+COVIDTELEW+COVIDTELEW:mediator+mediator, data = household_m)

mod.mediator<-lm( mediator ~ HH_NUMKIDS + HH_NUMADULTS + AGE + SEX + 
                    MARST+COVIDTELEW, data = household_m)

#CDE(m) and NIE of birth weight model
CDE_household <- mean(mod.outcome$coefficients[6] + 
                        mod.outcome$coefficients[7]*m)
NIE_household <- mean(mod.outcome$coefficients[13]*mod.mediator$coefficients[11] + 
                        mod.outcome$coefficients[7]*mod.mediator$coefficients[11])

print("CDE_household")
print(CDE_household, digits = 3)
print("NIE_household")
print(NIE_household, digits = 3 )


n <- nrow(household_m)
CDE.boot.act <- NULL
NIE.boot.act <- NULL

B <- 200
set.seed(237653)
for(t in 1:B) {
  jobs.boot <- household_m[sample(1:n, n, replace = TRUE), ]
  outcome.boot <- lm(ACT_HHACT ~ HH_NUMKIDS + HH_NUMADULTS + AGE + SEX + 
                       MARST+COVIDTELEW+COVIDTELEW:mediator+mediator, data=jobs.boot)
  mediator.boot <- lm(mediator ~ HH_NUMKIDS + HH_NUMADULTS + AGE + SEX + 
                        MARST+COVIDTELEW, data=jobs.boot)
  
  CDE.boot.act <- c(CDE.boot.act,mean(outcome.boot$coefficients[6] + 
                                        outcome.boot$coefficients[7]*median(jobs.boot$mediator)))
  NIE.boot.act  <- c(NIE.boot.act,mean(outcome.boot$coefficients[13]*mediator.boot$coefficients[11] + 
                                         outcome.boot$coefficients[7]*mediator.boot$coefficients[11]))
}

#calculate SEs
cde_act_sd<-sd(CDE.boot.act)
print(cde_act_sd, digits = 3)
CI_cde_act <- CDE_household + c(-1, 1)*qnorm(0.975)*cde_act_sd
print(CI_cde_act, digits = 3)
nie_act_sd<-sd(NIE.boot.act)
print(nie_act_sd, digits = 3)
CI_nie_act <- NIE_household + c(-1, 1)*qnorm(0.975)*nie_act_sd
print(CI_nie_act, digits = 3)


#################### Classification Learning - Optimal Decision List ############### ####################################################################################
library(randomForest)

### fit random forest to estimate the conditional means
set.seed(1997)
data_trt <- household1[which(household1$COVIDTELEW == "Yes"), ]
data_ctr <- household1[which(household1$COVIDTELEW == "No"), ]

### household activity random forest and conditional ATE (Z); then recursive partitioning
act_treat <- randomForest(ACT_HHACT ~ . - ACT_HHACT - EDUCYRS, 
                          data = data_trt, ntree = 500)
act_ctr <- randomForest(ACT_HHACT ~ . - ACT_HHACT - EDUCYRS, 
                        data = data_ctr, ntree = 500)
pred1 <- c(predict(act_treat), predict(act_treat, newdata = data_ctr))
pred0 <- c(predict(act_ctr, newdata = data_trt), predict(act_ctr))
Z_act <- pred1 - pred0
Z_act_label = ifelse(Z_act > 0, 1, 0)
Z_act_weight = abs(Z_act)
fit_act = rpart(Z_act_label ~ . - ACT_HHACT - EDUCYRS, data = rbind(data_trt,data_ctr), weights = Z_act_weight)
pfit_act <- prune(fit_act, cp=fit_act$cptable[which.min(fit_act$cptable[,"xerror"]),"CP"])
rpart.plot(pfit_act)
```