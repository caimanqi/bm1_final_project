#SLR for each variable
SLR_is30dayreadmit <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$is30dayreadmit)
#confint(SLR_is30dayreadmit)
#coef(summary(SLR_is30dayreadmit))
row_is30dayreadmit <- cbind(coef(summary(SLR_is30dayreadmit))[, c(1:2, 4)], confint(SLR_is30dayreadmit))[-1, ]


SLR_cindex <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$cindex)
row_cindex <- cbind(coef(summary(SLR_cindex))[, c(1:2, 4)], confint(SLR_cindex))[-1, ]


SLR_evisit <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$evisit)
row_evisit <- cbind(coef(summary(SLR_evisit))[, c(1:2, 4)], confint(SLR_evisit))[-1, ]


SLR_ageyear <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$ageyear)
row_ageyear <- cbind(coef(summary(SLR_ageyear))[, c(1:2, 4)], confint(SLR_ageyear))[-1, ]


SLR_gender <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$gender)
row_gender <- cbind(coef(summary(SLR_gender))[, c(1:2, 4)], confint(SLR_gender))[-1, ]


SLR_maritalstatus <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$maritalstatus)
row_maritalstatus <- cbind(coef(summary(SLR_maritalstatus))[, c(1:2, 4)], confint(SLR_maritalstatus))[-1, ]

SLR_insurancetype <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$insurancetype)
row_insurancetype <- cbind(coef(summary(SLR_insurancetype))[, c(1:2, 4)], confint(SLR_insurancetype))[-1, ]

SLR_bpsystolic <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$bpsystolic)
row_bpsystolic <- cbind(coef(summary(SLR_bpsystolic))[, c(1:2, 4)], confint(SLR_bpsystolic))[-1, ]


SLR_o2sat <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$o2sat)
row_o2sat <- cbind(coef(summary(SLR_o2sat))[, c(1:2, 4)], confint(SLR_o2sat))[-1, ]


SLR_heartrate <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$heartrate)
row_heartrate <- cbind(coef(summary(SLR_heartrate))[, c(1:2, 4)], confint(SLR_heartrate))[-1, ]


SLR_respirationrate <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$respirationrate)
row_respirationrate <- cbind(coef(summary(SLR_respirationrate))[, c(1:2, 4)], confint(SLR_respirationrate))[-1, ]


SLR_bpdiastolic <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$bpdiastolic)
row_bpdiastolic <- cbind(coef(summary(SLR_bpdiastolic))[, c(1:2, 4)], confint(SLR_bpdiastolic))[-1, ]

SLR_res <- rbind(row_is30dayreadmit, row_cindex, row_evisit, row_ageyear, row_gender, row_maritalstatus, row_insurancetype, row_bpsystolic, row_o2sat, row_heartrate, row_respirationrate, row_bpdiastolic)


#MLR
MLR_ghprojrct <- lm(ghproject_tidy$losdays2 ~ ghproject_tidy$is30dayreadmit + ghproject_tidy$cindex + ghproject_tidy$evisit + ghproject_tidy$ageyear + ghproject_tidy$gender +ghproject_tidy$maritalstatus +ghproject_tidy$insurancetype + ghproject_tidy$bpsystolic + ghproject_tidy$o2sat + ghproject_tidy$heartrate + ghproject_tidy$respirationrate + ghproject_tidy$bpdiastolic)
#summary(MLR_ghprojrct)
MLR_coef <- coef(summary(MLR_ghprojrct))[,c(1:2,4)]
MLR_conf<- confint(MLR_ghprojrct)
MLR_res <- cbind(MLR_coef, MLR_conf)[-1,]

cbind(SLR_res, MLR_res)
