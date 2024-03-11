destring fiscalyear , replace
rename banks bank_name
egen bank = group(bank_name )
xtset bank fiscalyear

egen bank_group = group(bankgroup)

 import excel "C:\Users\somya\OneDrive\Documents\cg_sp_merged.xlsx", sheet("Sheet1") firstrow
 save "C:\Users\somya\OneDrive\Documents\cg_sp_merged.dta"
 
destring stc - aqrdummy, ignore("NA" "ER") replace

winsor2 roa, cuts(1 99)
winsor2 gnparatio , cuts(1 99)
winsor2 crar, cuts(0 99)
gen log_assets = log(totalassets )

***Board PCA***
/*-- Bartlett's sphericity and KMO for Board PCA appropriateness--*/
factortest boardsize boardmeetings noofindependentdirectorsinboard noofnonexecutivedirectorsinboard noofwomendirectorsinboard 
estat kmo

pca boardsize boardmeetings noofindependentdirectorsinboard noofnonexecutivedirectorsinboard noofwomendirectorsinboard
predict p1_boardf p2_boardf
gen board_pca1f = (p1_boardf + p2_boardf)/2
egen min_board1f = min(board_pca1f)
egen max_board1f = max(board_pca1f)
gen norm_board_pca1f = (board_pca1f - min_board1f) /(max_board1f - min_board1f )
twoway (line norm_board_pca1f fiscalyear, sort), by (bank_name)
twoway (line board_pca1f fiscalyear, sort), by (bank_name)

*NRC PCA*
pca nomremcommitteesize nomremcommitteemeetings noofindependentdirectorsinnomrem
predict p1_nrc1f
egen min_nrc1f = min(p1_nrc1f)
egen max_nrc1f = max(p1_nrc1f)
gen norm_nrc_pca1f = (p1_nrc1f - min_nrc1f) /(max_nrc1f - min_nrc1f)
twoway (line norm_nrc_pca1f fiscalyear, sort), by (bank_name)

*RMC PCA*
pca rmcsize rmcmeetings noofindependentdirectorsinrmc
predict p1_rmc1f
rename p1_rmc1f pca_rmc1f
egen min_rmc1f = min(pca_rmc1f)
egen max_rmc1f = max(pca_rmc1f)
gen norm_rmc_pca1f = (pca_rmc1f - min_rmc1f) /(max_rmc1f - min_rmc1f)
twoway (line norm_rmc_pca1f fiscalyear, sort), by (bank_name)

*audit PCA*
pca auditcommitteesize auditmeetings noofindependentdirectorsinauditc
predict p11f p21f
gen audit_pca1f = (p11f + p21f)/2
egen min_a1f = min(audit_pca1f)
egen max_a1f = max(audit_pca1f)
gen norm_audit_pca1f = (audit_pca1f - min_a1f) /(max_a1f - min_a1f)

*overall index*
pca boardsize - noofindependentdirectorsinrmc stc co 
predict p1_o1f p2_o1f p3_o1f p4_o1f p5_o1f
gen pca1f = (p1_o1f + p2_o1f + p3_o1f + p4_o1f + p5_o1f) /5
egen min_o1f = min(pca1f)
egen max_o1f = max(pca1f)
gen norm_pca1f = (pca1f - min_o1f) /(max_o1f - min_o1f)
twoway (line norm_pca1f fiscalyear, sort), by (bank_name)

/* Bankgroup Dummy variable*/

generate bgdummy = 1  /*PSB*/
replace bgdummy = 0 if bankgroup == "PVB"
 
*---- BANKGROUP-WISE OVERALL AND COMPONENT-WISE PCA----*
/* Bankgroup-wise yearly normalised overall PCA*/  
bysort fiscalyear bgdummy: egen cond_normpca2f = mean(norm_pca1f)
twoway (line cond_normpca2f fiscalyear, sort), by (bankgroup)

/* Bankgroup-wise yearly normalised Audit Committee PCA*/  
bysort fiscalyear bgdummy: egen cond_auditnormpca2f = mean(norm_audit_pca1f)
twoway (line cond_auditnormpca2f fiscalyear, sort), by (bankgroup)

/* Bankgroup-wise yearly normalised RMC Committee PCA*/  
bysort fiscalyear bgdummy: egen cond_rmcnormpca2f = mean(norm_rmc_pca1f)
twoway (line cond_rmcnormpca2f fiscalyear, sort), by (bankgroup)

/* Bankgroup-wise yearly normalised NRC Committee PCA*/  
bysort fiscalyear bgdummy: egen cond_nrcnormpca2f = mean(norm_nrc_pca1f)
twoway (line cond_nrcnormpca2f fiscalyear, sort), by (bankgroup)

/* Bankgroup-wise yearly normalised Banks' Board PCA*/  
bysort fiscalyear bgdummy: egen cond_boardnormpca2f = mean(norm_board_pca1f)
twoway (line cond_boardnormpca2f fiscalyear, sort), by (bankgroup)


*---- ALL BANKS OVERALL AND COMPONENT-WISE PCA----* 

/* All Banks yearly normalised overall PCA*/
bysort fiscalyear: egen cond_normpca3f = mean(norm_pca1f)
twoway (line cond_normpca3f fiscalyear, sort)

/* All Banks yearly normalised Audit Committee PCA*/
bysort fiscalyear: egen cond_auditpca3f = mean(norm_audit_pca1f)
twoway (line cond_auditpca3f fiscalyear, sort)

/* All Banks yearly normalised RMC Committee PCA*/
bysort fiscalyear: egen cond_rmcpca3f = mean(norm_rmc_pca1f)
twoway (line cond_rmcpca3f fiscalyear, sort)

/* All Banks yearly normalised NRC PCA*/
bysort fiscalyear: egen cond_nrcpca3f = mean(norm_nrc_pca1)
twoway (line cond_nrcpca3f fiscalyear, sort)

/* All Banks yearly normalised Board PCA*/
bysort fiscalyear: egen cond_boardpca3f = mean(norm_board_pca1f)
twoway (line cond_boardpca3f fiscalyear, sort)


***Regressions for the overall panel***
**reghdfe nim boardsize  gnparatio_w log_assets , absorb( bank)
**reghdfe nim boardsize noofindependentdirectorsinboard gnparatio_w log_assets , absorb( bank)
***reghdfe crar_w boardmeetings  gnparatio_w log_assets , absorb( bank) vce(robust)***


reghdfe nim norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets , absorb(bank) vce(robust)

reghdfe nim norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets , absorb(bank) vce(robust) 
reghdfe nim norm_board_pca norm_rmc_pca norm_audit_pca log_assets i.bank_group  , noabsorb vce(robust)
reghdfe daratio norm_board_pca norm_rmc_pca norm_audit_pca log_assets , absorb(bank) vce(robust)
reghdfe daratio norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets i.bank_group , noabsorb vce(robust)
reghdfe daratio norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets , absorb(bank) vce(robust)
reghdfe gnparatio_w norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets , absorb(bank) vce(robust)
reghdfe gnparatio_w norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets , noabsorb vce(robust)

reghdfe nim norm_board_pca norm_rmc_pca log_assets i.bank  , noabsorb vce(robust)


/* BANKGROUP-WISE REGRESSIONS*/
reghdfe nim norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets if bgdummy==1, absorb(bank) vce(robust) /*PSBs*/
reghdfe nim norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets if bgdummy==0, absorb(bank) vce(robust) /*PVBs*/


bysort bgdummy: xtreg nim norm_board_pca norm_rmc_pca norm_nrc_pca norm_audit_pca log_assets, fe robust

 

*REGHDFE*
**ROA**
*1. entity and time fixed effects
reghdfe roa norm_rmc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe roa norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe roa norm_nrc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe roa norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe roa norm_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

reghdfe roa norm_rmc_pca1f norm_audit_pca1f norm_nrc_pca1f norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe roa norm_rmc_pca1f norm_audit_pca1f norm_nrc_pca1f norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe roa norm_rmc_pca1f log_assets cdratio  , absorb(fiscalyear) vce(robust) 
reghdfe roa norm_audit_pca1f log_assets cdratio  , absorb(fiscalyear) vce(robust) 
reghdfe roa norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe roa norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe roa norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe roa norm_rmc_pca1f norm_audit_pca1f norm_nrc_pca1f norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 


reghdfe nim norm_board_pca1n norm_rmc_pca1n norm_nrc_pca1n norm_audit_pca1n log_assets cdratio , absorb(bank fiscalyear) vce(robust)

**NIM**
*1. entity and time fixed effects

reghdfe nim norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe nim norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe nim norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe nim norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe nim norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe nim norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only

reghdfe nim norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe nim norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe nim norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe nim norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe nim norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe nim norm_rmc_pca1f norm_audit_pca1f norm_nrc_pca1f norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 

**DA_RATIO**
*1. entity and time fixed effects
reghdfe daratio norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe daratio norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe daratio norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe daratio norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe daratio norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)

reghdfe daratio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe daratio norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe daratio norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe daratio norm_nrc_pca1f log_assets cdratio , absorb (fiscalyear) vce(robust) 
reghdfe daratio norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe daratio norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe daratio norm_rmc_pca1f norm_audit_pca1f norm_nrc_pca1f norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 


**ROE**
*1. entity and time fixed effects
reghdfe roe norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe roe norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe roe norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe roe norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe roe norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)
reghdfe roe norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe roe norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe roe norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe roe norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe roe norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe roe norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust)
reghdfe roe norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 


**zscore**
*1. entity and time fixed effects
reghdfe zscore norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe zscore norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe zscore norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe zscore norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe zscore norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)
reghdfe zscore norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe zscore norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe zscore norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe zscore norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe zscore norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe zscore norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust)
reghdfe zscore norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 

 
**gnpa**
*1. entity and time fixed effects
reghdfe gnparatio norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe gnparatio norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe gnparatio norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe gnparatio norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe gnparatio norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)
reghdfe gnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe gnparatio norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe gnparatio norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe gnparatio norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe gnparatio norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe gnparatio norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust)
reghdfe gnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 



**nnpa**
*1. entity and time fixed effects
reghdfe nnparatio norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe nnparatio norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe nnparatio norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe nnparatio norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe nnparatio norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)
reghdfe nnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe nnparatio norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe nnparatio norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe nnparatio norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe nnparatio norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe nnparatio norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust)
reghdfe nnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb( fiscalyear) vce(robust) 


**pcr**
*1. entity and time fixed effects
reghdfe pcr norm_rmc_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe pcr norm_audit_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 
reghdfe pcr norm_nrc_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe pcr norm_board_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust) 
reghdfe pcr norm_pca1f log_assets cdratio , absorb(bank fiscalyear) vce(robust)
reghdfe pcr norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(bank fiscalyear) vce(robust) 

 *2. time fixed effects only
reghdfe pcr norm_rmc_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe pcr norm_audit_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 
reghdfe pcr norm_nrc_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe pcr norm_board_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust) 
reghdfe pcr norm_pca1f log_assets cdratio , absorb(fiscalyear) vce(robust)
reghdfe pcr norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, absorb(fiscalyear) vce(robust) 





*GMM*
xtabond2 nim L.nim norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.nim, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 nim L.nim norm_rmc_pca1f log_assets cdratio , gmmstyle(L.nim, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep 
xtabond2 nim L.nim norm_audit_pca1f log_assets cdratio, gmmstyle(L.nim, collapse) ivstyle( norm_audit_pca1f cdratio i.fiscalyear log_assets) twostep
xtabond2 nim L.nim norm_nrc_pca1f log_assets cdratio, gmmstyle(L.nim, collapse) ivstyle( norm_nrc_pca1f cdratio i.fiscalyear log_assets) twostep
xtabond2 nim L.nim norm_board_pca1f log_assets cdratio, gmmstyle(L.nim, collapse) ivstyle( norm_board_pca1f cdratio i.fiscalyear log_assets) twostep
xtabond2 nim L.nim norm_pca1f log_assets cdratio, gmmstyle(L.nim, collapse) ivstyle( norm_pca1f cdratio i.fiscalyear log_assets) twostep

xtabond2 roa L.roa norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.roa, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 roa L.roa norm_rmc_pca1f log_assets cdratio , gmmstyle(L.roa, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 roa L.roa norm_audit_pca1f log_assets cdratio, gmmstyle(L.roa, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 roa L.roa norm_nrc_pca1f log_assets cdratio , gmmstyle(L.roa, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 roa L.roa norm_board_pca1f log_assets cdratio, gmmstyle(L.roa, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 roa L.roa norm_pca1f log_assets cdratio, gmmstyle(L.roa, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust



xtabond2 roe L.roe norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.roe, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 roe L.roe norm_rmc_pca1f log_assets cdratio , gmmstyle(L.roe, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 roe L.roe norm_audit_pca1f log_assets cdratio, gmmstyle(L.roe, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 roe L.roe norm_nrc_pca1f log_assets cdratio , gmmstyle(L.roe, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 roe L.roe norm_board_pca1f log_assets cdratio, gmmstyle(L.roe, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 roe L.roe norm_pca1f log_assets cdratio, gmmstyle(L.roe, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust

xtabond2 daratio L.daratio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.daratio, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 daratio L.daratio norm_rmc_pca1f log_assets cdratio , gmmstyle(L.daratio, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 daratio L.daratio norm_audit_pca1f log_assets cdratio, gmmstyle(L.daratio, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 daratio L.daratio norm_nrc_pca1f log_assets cdratio , gmmstyle(L.daratio, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 daratio L.daratio norm_board_pca1f log_assets cdratio, gmmstyle(L.daratio, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 daratio L.daratio norm_pca1f log_assets cdratio, gmmstyle(L.daratio, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust


xtabond2 gnparatio L.gnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.gnparatio, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 gnparatio L.gnparatio norm_rmc_pca1f log_assets cdratio , gmmstyle(L.gnparatio, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 gnparatio L.gnparatio norm_audit_pca1f log_assets cdratio, gmmstyle(L.gnparatio, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 gnparatio L.gnparatio norm_nrc_pca1f log_assets cdratio , gmmstyle(L.gnparatio, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 gnparatio L.gnparatio norm_board_pca1f log_assets cdratio, gmmstyle(L.gnparatio, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 gnparatio L.gnparatio norm_pca1f log_assets cdratio, gmmstyle(L.gnparatio, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust


xtabond2 nnparatio L.nnparatio norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.nnparatio, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 nnparatio L.nnparatio norm_rmc_pca1f log_assets cdratio , gmmstyle(L.nnparatio, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 nnparatio L.nnparatio norm_audit_pca1f log_assets cdratio, gmmstyle(L.nnparatio, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 nnparatio L.nnparatio norm_nrc_pca1f log_assets cdratio , gmmstyle(L.nnparatio, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 nnparatio L.nnparatio norm_board_pca1f log_assets cdratio, gmmstyle(L.nnparatio, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 nnparatio L.nnparatio norm_pca1f log_assets cdratio, gmmstyle(L.nnparatio, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust

xtabond2 zscore L.zscore norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.zscore, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 zscore L.zscore norm_rmc_pca1f log_assets cdratio , gmmstyle(L.zscore, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 zscore L.zscore norm_audit_pca1f log_assets cdratio, gmmstyle(L.zscore, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 zscore L.zscore norm_nrc_pca1f log_assets cdratio , gmmstyle(L.zscore, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 zscore L.zscore norm_board_pca1f log_assets cdratio, gmmstyle(L.zscore, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 zscore L.zscore norm_pca1f log_assets cdratio, gmmstyle(L.zscore, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust

xtabond2 pcr L.pcr norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f log_assets cdratio, gmmstyle(L.pcr, collapse) ivstyle( norm_rmc_pca1f norm_nrc_pca1f norm_audit_pca1f norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 pcr L.pcr norm_rmc_pca1f log_assets cdratio , gmmstyle(L.pcr, collapse) ivstyle( norm_rmc_pca1f cdratio log_assets) twostep robust
xtabond2 pcr L.pcr norm_audit_pca1f log_assets cdratio, gmmstyle(L.pcr, collapse) ivstyle( norm_audit_pca1f cdratio  log_assets) twostep robust
xtabond2 pcr L.pcr norm_nrc_pca1f log_assets cdratio , gmmstyle(L.pcr, collapse) ivstyle( norm_nrc_pca1f cdratio log_assets) twostep robust
xtabond2 pcr L.pcr norm_board_pca1f log_assets cdratio, gmmstyle(L.pcr, collapse) ivstyle( norm_board_pca1f cdratio log_assets) twostep robust
xtabond2 pcr L.pcr norm_pca1f log_assets cdratio, gmmstyle(L.pcr, collapse) ivstyle( norm_pca1f cdratio log_assets) twostep robust
