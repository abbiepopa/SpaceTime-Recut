iq <- read.csv('~/Documents/amc_apc_tdj/data/raw/all_IQ.csv')

amc <- read.csv('~/Documents/amc_apc_tdj/data/raw/amc_completers2016-02-19.csv')
apc <- read.csv('~/Documents/amc_apc_tdj/data/raw/apc_completers_2016-02-19.csv')
tdja <- read.csv('~/Documents/amc_apc_tdj/data/raw/tdja_completers_2016-03-04.csv')
tdjv <- read.csv('~/Documents/amc_apc_tdj/data/raw/tdjv_completers_2016-02-19.csv')



#cube amc, square apc, nothing tdjs

iq_amc <- merge(iq, amc)
iq_amc$final_ratio_cube <- iq_amc$final_ratio^3

iq_amc_22q_mask <- which(iq_amc$DX == '22q')
iq_amc_TD_mask <- which(iq_amc$DX == 'td')
iq_amc_sca_mask <- grep('xx', iq_amc$DX)


cor.test(iq_amc[iq_amc_22q_mask, 'FSIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_22q_mask, 'FSIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_TD_mask, 'FSIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_TD_mask, 'FSIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_sca_mask, 'FSIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_sca_mask, 'FSIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_22q_mask, 'VIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_22q_mask, 'VIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_TD_mask, 'VIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_TD_mask, 'VIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_sca_mask, 'VIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_sca_mask, 'VIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio_cube'])


cor.test(iq_amc[iq_amc_22q_mask, 'PIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_22q_mask, 'PIQ'], iq_amc[iq_amc_22q_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_TD_mask, 'PIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_TD_mask, 'PIQ'], iq_amc[iq_amc_TD_mask, 'final_ratio_cube'])

cor.test(iq_amc[iq_amc_sca_mask, 'PIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio'])
cor.test(iq_amc[iq_amc_sca_mask, 'PIQ'], iq_amc[iq_amc_sca_mask, 'final_ratio_cube'])



iq_apc <- merge(iq, apc)
iq_apc$final_ratio_sqr <- iq_apc$final_ratio^2

iq_apc_22q_mask <- which(iq_apc$DX == '22q')
iq_apc_TD_mask <- which(iq_apc$DX == 'td')
iq_apc_sca_mask <- grep('xx', iq_apc$DX)


cor.test(iq_apc[iq_apc_22q_mask, 'FSIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_22q_mask, 'FSIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_TD_mask, 'FSIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_TD_mask, 'FSIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_sca_mask, 'FSIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_sca_mask, 'FSIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_22q_mask, 'VIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_22q_mask, 'VIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_TD_mask, 'VIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_TD_mask, 'VIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_sca_mask, 'VIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_sca_mask, 'VIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio_sqr'])


cor.test(iq_apc[iq_apc_22q_mask, 'PIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_22q_mask, 'PIQ'], iq_apc[iq_apc_22q_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_TD_mask, 'PIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_TD_mask, 'PIQ'], iq_apc[iq_apc_TD_mask, 'final_ratio_sqr'])

cor.test(iq_apc[iq_apc_sca_mask, 'PIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio'])
cor.test(iq_apc[iq_apc_sca_mask, 'PIQ'], iq_apc[iq_apc_sca_mask, 'final_ratio_sqr'])

###tdja

iq_tdja <- merge(iq, tdja)

iq_tdja_22q_mask <- which(iq_tdja$DX == '22q')
iq_tdja_TD_mask <- which(iq_tdja$DX == 'td')
iq_tdja_sca_mask <- grep('xx', iq_tdja$DX)


cor.test(iq_tdja[iq_tdja_22q_mask, 'FSIQ'], iq_tdja[iq_tdja_22q_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_TD_mask, 'FSIQ'], iq_tdja[iq_tdja_TD_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_sca_mask, 'FSIQ'], iq_tdja[iq_tdja_sca_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_22q_mask, 'VIQ'], iq_tdja[iq_tdja_22q_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_TD_mask, 'VIQ'], iq_tdja[iq_tdja_TD_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_sca_mask, 'VIQ'], iq_tdja[iq_tdja_sca_mask, 'final_ratio'])



cor.test(iq_tdja[iq_tdja_22q_mask, 'PIQ'], iq_tdja[iq_tdja_22q_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_TD_mask, 'PIQ'], iq_tdja[iq_tdja_TD_mask, 'final_ratio'])


cor.test(iq_tdja[iq_tdja_sca_mask, 'PIQ'], iq_tdja[iq_tdja_sca_mask, 'final_ratio'])

###tdjv

iq_tdjv <- merge(iq, tdjv)

iq_tdjv_22q_mask <- which(iq_tdjv$DX == '22q')
iq_tdjv_TD_mask <- which(iq_tdjv$DX == 'td')
iq_tdjv_sca_mask <- grep('xx', iq_tdjv$DX)


cor.test(iq_tdjv[iq_tdjv_22q_mask, 'FSIQ'], iq_tdjv[iq_tdjv_22q_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_TD_mask, 'FSIQ'], iq_tdjv[iq_tdjv_TD_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_sca_mask, 'FSIQ'], iq_tdjv[iq_tdjv_sca_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_22q_mask, 'VIQ'], iq_tdjv[iq_tdjv_22q_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_TD_mask, 'VIQ'], iq_tdjv[iq_tdjv_TD_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_sca_mask, 'VIQ'], iq_tdjv[iq_tdjv_sca_mask, 'final_ratio'])



cor.test(iq_tdjv[iq_tdjv_22q_mask, 'PIQ'], iq_tdjv[iq_tdjv_22q_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_TD_mask, 'PIQ'], iq_tdjv[iq_tdjv_TD_mask, 'final_ratio'])


cor.test(iq_tdjv[iq_tdjv_sca_mask, 'PIQ'], iq_tdjv[iq_tdjv_sca_mask, 'final_ratio'])