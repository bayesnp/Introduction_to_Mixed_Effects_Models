ODS HTML CLOSE;
ODS HTML;
/* CLOSE keyword stops SAS from writing any more output to the existing "Results Viewer" window. 
* The second ODS opens a new "Results Viewer" window when further output is created 
* (and simultaneously closes the old "Results Viewer" window). */

DM "clear output";    * clears the output window ;
DM "clear log";       * clears the log window;
options nodate nonumber pageno=1 pagesize=75 linesize=132 ORIENTATION=LANDSCAPE;

libname ahles 'H:\Liy_Analy\instr\stats15.fall\sas' ; 

/* 
data dartrci.try;
  set WORK.cogrci70;
  keep id group treat3 fu_per age educ_neuro e4_allele smoking2 d_f5_proc_speed b_f5_proc_speed ;
run;

data ahles.psyoncol;
  set dartrci.try;
run;
*/

proc format ;
value tx    1='chemo' 
            2='chemo no' 
            3='control' ;
value ptnf  0='control' 
            1='patient' ;
value posttm 1 = 'posttx'  
             2 = '1yr' 
			 3 = '2yr' ;
value imprgp 1='canImpr'
             2='canNotImpr'
			 3='control' ;
run; 

data ahles.psyoncol ;
  set ahles.psyoncol ;
  txgrp = . ;
  if treat3 = 'Chemo'   then txgrp = 1 ;
  if treat3 = 'Local'   then txgrp = 2 ;
  if treat3 = 'Control' then txgrp = 3 ;
  format txgrp tx. ;

  ptn = . ;
  if treat3 = 'Chemo'   then ptn = 1 ;
  if treat3 = 'Local'   then ptn = 1 ;
  if treat3 = 'Control' then ptn = 0 ;
  format ptn ptnf. ;

  ptime = . ;         /* posttx followup time */
  if fu_per = 'PT' then ptime = 1 ;
  if fu_per = '1Y' then ptime = 2 ;
  if fu_per = '2Y' then ptime = 3 ;
  format ptime posttm. ;

RUN;

title 'd_f5_proc_speed: pooling over time' ;
title2 'Observed means pooling over time for Fig1, not adjusted';
PROC MEANS DATA = ahles.psyoncol n mean median std lclm uclm MAXDEC=3 PRINT ;
VAR d_f5_proc_speed ;
CLASS treat3 e4_allele smoking2 ;
RUN; 

title 'txgrp*e4_allele*smoking2' ;
title2 'LEMEANS provide model-predicted means and CIs';
PROC MIXED method=REML data=ahles.psyoncol covtest;
    CLASS ID txgrp ptime e4_allele smoking2 ;
	MODEL d_f5_proc_speed = b_f5_proc_speed age educ_neuro txgrp e4_allele smoking2 ptime txgrp*e4_allele 
                  e4_allele*smoking2 smoking2*txgrp e4_allele*smoking2*txgrp / SOLUTION CL DDFM=KENWARDROGER ;
	RANDOM INTERCEPT / SUBJECT = ID TYPE = UN ; 
	/* REPEATED / TYPE=CS SUBJECT = ID RCORR ;  */
ESTIMATE 'e4_allele*smoking2' e4_allele*smoking2
.25 -.25
-.25 .25
;
/* NOTE: txgrp*e4_allele*smoking2 in ESTIMATE must have 3-way contrasts or you get "Non-est" error */
/* SAS is unlike R, first term txgrp is on the outmost layer of the 2 by 2 by 3 table 
/*                    chemo                   chemo no            control        
/*              E4Absent E4Present    E4Absent E4Present    E4Absent E4Present
/*              -------- ---------    -------- ---------    -------- ---------
/* EverSmoked    1       -1            0        0           -1        1
/* NeverSmoked  -1        1	           0        0            1       -1
/*              --------------------------------------------------------------
/*
/*                    chemo                   chemo no            control        
/*              E4Absent E4Present    E4Absent E4Present    E4Absent E4Present
/*              -------- ---------    -------- ---------    -------- ---------
/* EverSmoked   0.084     0.138           0        0         0.232     0.258
/* NeverSmoked  0.202    -0.245           0        0         0.196     0.306
/*              --------------------------------------------------------------
/* (0.084 - 0.202) - (0.138 - (-0.245)) =  -0.501,
/* (0.232 - 0.196) - (0.258 -   0.306)  =   0.084; thus -0.501 - 0.084 = -0.585
/* 'Ever smoked' is significantly worse for E4Absent than E4Present in chemo patients as compared with Controls
/* effect of smoking on E4 status in chemo patienets the same as that in control?
/* bewlow they are stacked vertically.                      */
ESTIMATE 'overall E4 effect?' e4_allele
 1   -1
/ CL; 
ESTIMATE 'chemo_v_ctrl: E4*Smoking' txgrp*e4_allele*smoking2
 1   -1
-1    1
 0    0
 0    0
-1    1
 1   -1
 / CL
/* / DIVISOR = 2.8284 */
; 
ESTIMATE 'chemo.no_v_ctrl: E4*Smoking' txgrp*e4_allele*smoking2
 0    0
 0    0
 1   -1
-1    1
-1    1
 1   -1
 / CL
;
ESTIMATE 'chemo_v_chemo.no: E4*Smoking' txgrp*e4_allele*smoking2
 1   -1
-1    1
-1    1
 1   -1
 0    0
 0    0
 / CL
;
ESTIMATE 'smoking protects E4+ in chemo?'
/*                    chemo                   chemo no            control        
/*              E4Absent E4Present    E4Absent E4Present    E4Absent E4Present
/*              -------- ---------    -------- ---------    -------- ---------
/* EverSmoked    1       -1            0        0            0        0
/* NeverSmoked  -1        1	           0        0            0        0
/*              --------------------------------------------------------------
/* 'Non-est' problem if only the contrast above.  Need to work out the CONTRASTS over
   txgrp, which is 0 because 1 -1, -1, 1 cancels out.  Also contrast over E4 is zero,
   contrast over smoking2 is zero, txgrp*e4_allele is zero; txgrp*smoking2 is zero;
   e4_allele*smoking2 is also zero; what remains are the e4_allele*smoking 2, collapsed
   over txgrp; and the txgrp*e4_allele*smoking2 3-way interaction.
*/
e4_allele*smoking2
 1 -1
-1  1
txgrp*e4_allele*smoking2
 1   -1
-1    1
 0    0
 0    0
 0    0
 0    0
/ CL ;
ESTIMATE 'smoking protects E4+ in chemo.no?'
e4_allele*smoking2
 1 -1
-1  1
txgrp*e4_allele*smoking2
 0    0
 0    0
 1   -1
-1    1
 0    0
 0    0
/ CL ;
ESTIMATE 'smoking protects E4+ in control?'
e4_allele*smoking2
 1 -1
-1  1
txgrp*e4_allele*smoking2
 0    0
 0    0
 0    0
 0    0
 1   -1
-1    1
/ CL ;

LSMEANS e4_allele*smoking2*txgrp / CL ;
/* store work.mixedout; */
run;

title 'Patients vs Controls' ;
title2 'Observed means pooling over time';
PROC MEANS DATA = ahles.psyoncol n mean median std lclm uclm MAXDEC=3 PRINT ;
VAR d_f5_proc_speed ;
CLASS ptn e4_allele smoking2 ;
RUN; 

title 'Patients vs Controls' ;
title2 'BLUP on protective effect of smoking on E4+ individuals cf controls';
PROC MIXED method=REML data=ahles.psyoncol covtest;
    CLASS ID ptn ptime e4_allele smoking2 ;
	MODEL d_f5_proc_speed = b_f5_proc_speed age educ_neuro b_f5_proc_speed ptn e4_allele smoking2 ptime ptn*e4_allele 
                  e4_allele*smoking2 smoking2*ptn e4_allele*smoking2*ptn / SOLUTION CL DDFM=KENWARDROGER ;
	RANDOM INTERCEPT / SUBJECT = ID TYPE = UN ; 
/* check output "class information" to align the correct contrast coefs */
ESTIMATE 'worse in E4+ than E4-?' e4_allele
 -1   1
/ CL; 
ESTIMATE 'E4+ worse for ptn cf ctrl?' e4_allele*ptn
/*
*              control    patn  
*              -------- ---------
* Absent         1         -1       
* Present       -1          1	     
*              ------------------
*/
  1  -1
 -1   1
/ CL; 
ESTIMATE 'Never smoking bad, more so for E4Present?' e4_allele*smoking2
/*
*              E4Absent E4Present  
*              -------- ---------
* ever smoke         1       -1       
* never smoke       -1        1	     
*              ------------------
*/
 1    -1
 -1    1
/ CL; 
ESTIMATE 'E4*Smoking greater in Ptns than Ctrl?' ptn*e4_allele*smoking2
/*                    control               patn        
*              E4Absent E4Present     E4Absent E4Present
*              -------- ---------     -------- ---------
* EverSmoked    -1         1              1       -1
* NeverSmoked    1        -1	         -1        1
*              -----------------------------------------
*/
-1    1
 1   -1
 1   -1
-1    1
 / CL
; 
ESTIMATE 'smoking protects E4+ in controls?'
/*                    controls          patn
/*              E4Absent E4Present    E4Absent E4Present
/*              -------- ---------    -------- ---------
/* EverSmoked    1       -1            0        0
/* NeverSmoked  -1        1	           0        0
/*              ----------------------------------------
/* 'Non-est' problem if only the contrast above.  Need to work out the CONTRASTS over
   patn, which is 1 -1, -1, 1.  Contrast over E4 is zero because (1 -1 0 0) sums to zero.
   Contrast over smoking2 is zero, patn*e4_allele is zero; patn*smoking2 is zero;
   e4_allele*smoking2 is also zero; what remains are the e4_allele*smoking2 for controls, 
   collapsed over patn; and the patn*e4_allele*smoking2 3-way interaction.
*/
e4_allele*smoking2
 1 -1
-1  1
ptn*e4_allele*smoking2
 1   -1
-1    1
 0    0
 0    0
/ CL ;
ESTIMATE 'smoking protects E4+ in patients?'
e4_allele*smoking2
 1 -1
-1  1
ptn*e4_allele*smoking2
 0    0
 0    0
 1   -1
-1    1
/ CL ;
LSMEANS e4_allele*smoking2*ptn / CL ;
run;

title ;
RUN;
