proc lifetest data=katrina;
	/* time is whatever your time/tenure variable is */
	time hour*reason(0);
run;

proc lifetest data=katrina;
	/* time is whatever your time/tenure variable is */
	time hour*reason(1);
run;

proc lifetest data=katrina;
	/* time is whatever your time/tenure variable is */
	time hour*reason(2);
run;

proc lifetest data=katrina;
	/* time is whatever your time/tenure variable is */
	time hour*reason(3);
run;

proc lifetest data=katrina;
	/* time is whatever your time/tenure variable is */
	time hour*reason(4);
run;

proc means data=katrina median mean;
	var hour;
	by reason;
run;

/*Survival curves*/
proc lifetest data=katrina plots=s(cl) notable;
	time hour*survive(1);
	/* strata statement is whatever variable you want to
		produce separate curves for. here, we'll
		compare those who had prior work experience
		to those who didn't */
	strata reason / diff=all;
run;


/*Hazard Plots*/
data katrina1;
	set katrina;
	/* if censored, make week=53 instead of 52 */
	if survive=1 then hour=49;
run;
proc lifetest data=katrina1 method=life plots=h(cl) width=1;
	time hour*survive(1);
	strata reason;
run;




