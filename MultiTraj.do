/* REFERENCE MATERIAL
	Multi-trajectories paper: https://www.andrew.cmu.edu/user/bjones/pdf/multtraj.pdf
	Example code: https://andrewpwheeler.wordpress.com/2016/10/06/group-based-trajectory-models-in-stata-some-graphs-and-fit-statistics/
*/

// Import Data - we converted from SPSS in R
	use "Z:\7-Data\M&B\LSAC dataset\Study_2\Study_2\Multi_Trajecotry_Analysis_Domain_Specific_Movement_Behaviours.dta", clear

// Generate timepoint variables
	gen t0 = 10
	gen t1 = 12
	gen t2 = 14

// Copied from Jo's code
***I used code from Andrew wheeler website - code to obtain other fit statistics for deciding best model to use - ***
	program summary_table_procTraj
		preserve
		*now lets look at the average posterior probability
		gen Mp = 0
		foreach i of varlist _traj_ProbG* {
			replace Mp = `i' if `i' > Mp 
		}
		sort _traj_Group
		*and the odds of correct classification
		by _traj_Group: gen countG = _N
		by _traj_Group: egen groupAPP = mean(Mp)
		by _traj_Group: gen counter = _n
		gen n = groupAPP/(1 - groupAPP)
		gen p = countG/ _N
		gen d = p/(1-p)
		gen occ = n/d
		*Estimated proportion for each group
		scalar c = 0
		gen TotProb = 0
		foreach i of varlist _traj_ProbG* {
		   scalar c = c + 1
		   quietly summarize `i'
		   replace TotProb = r(sum)/ _N if _traj_Group == c 
		}
		gen d_pp = TotProb/(1 - TotProb)
		gen occ_pp = n/d_pp
		*This displays the group number [_traj_~p], 
		*the count per group (based on the max post prob), [countG]
		*the average posterior probability for each group, [groupAPP]
		*the odds of correct classification (based on the max post prob group assignment), [occ] 
		*the odds of correct classification (based on the weighted post. prob), [occ_pp]
		*and the observed probability of groups versus the probability [p]
		*based on the posterior probabilities [TotProb]
		list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
		restore
	end
/*
FAILED TRY WITH SCALING	
// See if a basic two group solution can be fit. Does not use domains.
	log using "~/Traj/log", text
		// MODEL
		traj, multgroups(2) /// 
		var1(LPA_at_10 LPA_at_12 LPA_at_14) indep1(t0-t2) order1(2 2) model1(cnorm) min1(0) max1(930) ///
		var2(MVPA_at_10 MVPA_at_12 MVPA_at_14) indep2(t0-t2) order2(2 2) model2(cnorm) min2(0) max2(860) ///
		var3(SB_at_10 SB_at_12 SB_at_14) indep3(t0-t2) order3(2 2) model3(cnorm) min3(0) max3(1250) ///
		var4(sleep_at_10 sleep_at_12 sleep_at_14) indep4(t0-t2) order4(2 2) model4(cnorm) min4(0) max4(1140) 
		// PLOT
		multtrajplot, xtitle(Age) ytitle1(LPA) ytitle2(MVPA) ytitle3(SB) ytitle3(Sleep) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		//graph save Graph "~/Downloads/Traj/Boys_TWOGROUPS.gph"
	log close
*/

// See if a basic two group solution can be fit. Does not use domains.
	log using "~/Traj/log", text
		// MODEL
		traj, multgroups(2) /// 
		var1(zLPA_at_10 zLPA_at_12 zLPA_at_14) indep1(t0-t2) order1(2 2) model1(cnorm) min1(-1.5) max1(11) ///
		var2(zMVPA_at_10 zMVPA_at_12 zMVPA_at_14) indep2(t0-t2) order2(2 2) model2(cnorm) min2(-1.5) max2(7.5) ///
		var3(zSB_at_10 zSB_at_12 zSB_at_14) indep3(t0-t2) order3(2 2) model3(cnorm) min3(-5) max3(4) ///
		var4(zsleep_at_10 zsleep_at_12 zsleep_at_14) indep4(t0-t2) order4(2 2) model4(cnorm) min4(-6.5) max4(7) 
		// PLOT
		multtrajplot, xtitle(Age) ytitle1(LPA) ytitle2(MVPA) ytitle3(SB) ytitle4(Sleep) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		//graph save Graph "~/Downloads/Traj/Boys_TWOGROUPS.gph"
	log close
	
	

// BELOW WE TESTED DIFFERENT NUMBERS OF GROUPS, BY SEX, TO SEE WHICH FIT BEST

	log using "~/Traj/log", text
	// BOYS
	// TWO GROUPS
		//MODEL
		traj if SEX==0, multgroups(2) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2) model3(logit)
		//PLOT
		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/Boys_TWOGROUPS.gph"
		//SUMMARY
		summary_table_procTraj
		
	// THREE GROUPS
		traj if SEX==0, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/Boys_THREGROUPS.gph"
		summary_table_procTraj
		
	// FOUR GROUPS
		traj if SEX==0, multgroups(4) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/Boys_FOURGROUPS.gph"
		summary_table_procTraj
		
	// FIVE GROUPS
		traj if SEX==0, multgroups(5) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/Boys_FIVEGROUPS.gph"
		summary_table_procTraj
		
	// SIX GROUPS
		traj if SEX==0, multgroups(6) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/Boys_SIXGROUPS.gph"
		summary_table_procTraj

	// GIRLS
	// TWO GROUPS
		traj if SEX==1, multgroups(2) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/GIRLS_TWOGROUPS.gph"
		summary_table_procTraj
		
	// THREE GROUPS
		traj if SEX==1, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/GIRLS_THREGROUPS.gph"
		summary_table_procTraj
		
	// FOUR GROUPS
		traj if SEX==1, multgroups(4) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/GIRLS_FOURGROUPS.gph"
		summary_table_procTraj
		
	// FIVE GROUPS
		traj if SEX==1, multgroups(5) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/GIRLS_FIVEGROUPS.gph"
		summary_table_procTraj
		
	// SIX GROUPS
		traj if SEX==1, multgroups(6) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/GIRLS_SIXGROUPS.gph"
		summary_table_procTraj
			
	// ALL
	// TWO GROUPS
		traj , multgroups(2) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/ALL_TWOGROUPS.gph"
		summary_table_procTraj
		
	// THREE GROUPS
		traj , multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/ALL_THREGROUPS.gph"
		summary_table_procTraj
		
	// FOUR GROUPS
		traj , multgroups(4) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/ALL_FOURGROUPS.gph"
		summary_table_procTraj
		
	// FIVE GROUPS
		traj , multgroups(5) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/ALL_FIVEGROUPS.gph"
		summary_table_procTraj
		
	// SIX GROUPS
		traj , multgroups(6) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(2 2 2 2 2 2) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 2 2 2 2 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 2 2 2 2 2) model3(logit)

		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		graph save Graph "~/Downloads/Traj/ALL_SIXGROUPS.gph"
		summary_table_procTraj
	log close

// THESE SEEMED TO BE THE BEST MODELS FOR EACH SEX. WE TESTED DIFFERENT POLYNOMIALS AND ARRVIED AT THE BELOW.
	// BOYS THREE LEVEL - VARYING
		traj if SEX==0, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(1 1 1) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 3 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 3 3) model3(logit)
		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)


	//GIRLS THREE LEVEL - VARYING
		traj if SEX==1, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(1 2 1) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 3 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 3 3) model3(logit)
		multtrajplot, xtitle(Age) ytitle1(Sleep) ytitle2(TV) ytitle3(Sports) ylabel1(0(1)3) ylabel2(0(1)3) ylabel3(0 1)
		
// TO TEST FOR EFFECT ON AN OUTCOME, ADD TO MODEL WITH outcome(**VARIABLE**) omodel(**DISTRIBUTION**)
	//EXAMPLES
	// BOYS - BODY FAT
		traj if SEX==0, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(1 1 1) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 3 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 3 3) model3(logit) outcome(A20DX_TFAT) omodel(normal)
		
	// GIRLS - MENTAL HEALTH	
		traj if SEX==1, multgroups(3) var1(Sleep_Y5 Sleep_Y8 Sleep_Y10 Sleep_Y13 Sleep_Y16) indep1(t0-t4) order1(1 2 1) model1(cnorm) min1(0) max1(12) ///
		var2(TV_Y5 TV_Y8 TV_Y10 TV_Y13 TV_Y16) indep2(t0-t4) order2(2 3 2) model2(cnorm) min2(0) max2(5) ///
		var3(orgsport_y5 orgsport_y8 orgsport_y10 orgsport_y13 orgsport_y16 ) indep3(t0-t4) order3(2 3 3) model3(logit) outcome(A20_MCS) omodel(normal)
