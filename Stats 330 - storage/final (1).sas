*FINAL PROJECT;

filename shortcut "C:\Users\ward32\Downloads\Final_Data\*.txt";


data final;
	infile shortcut dlm="@";
	length ID $ 5 Course $ 10;
	input ID $ Date Course $ Credit Grade $;
	if substr(Grade,1,1) = "A" then 
		GPAgrade=4.0;
	else if substr(Grade,1,1) = "B" then 
		GPAgrade=3.0;
	*....;
	if substr(Grade,2,1) = "+" then 
		GPAgrade=GPAgrade + .4;
	*....;
run;

*Alternateve approach to numeric GPA;

proc format;
	invalue lettertnum
	"A" = 4.0
	"A-" = 3.7
	other=0
	;
run;

data final;
	infile shortcut dlm="@";
	length ID $ 5 Course $ 10;
	input ID $ Date Course $ Credit Grade $;
	GPAgrade=input(Grade, lettertnum.);

run;

proc print data=final (obs=200);
run;

proc contents data=final;
run;
