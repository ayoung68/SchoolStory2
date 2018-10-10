*C:\Users\ward32\Downloads\Form A1.csv;

data student (keep=ID i StudentAns) answers (keep=ID i CorrectAns);
	infile "C:\Users\ward32\Downloads\Form A1.csv" dlm="," dsd missover;
	input ID $ blank (q1-q150) ($);
	array k{150} $ q1-q150;
	if ID = "AAAAKEY" then 
		do i = 1 to 150;
			CorrectAns=k{i};
			output answers;
		end;
	else 
		do i = 1 to 150;
			StudentAns = k{i};
			output student;
		end;
run; 

proc sql;
	create table test as
	select student.ID,
		sum(case when StudentAns=CorrectAns then 1 else 0 end) as totalscore,
		round(calculated totalscore/150, .01) as percscore
	from answers, student
	where student.i=answers.i
	group by student.ID
	;
quit;

proc print data=test (obs=200);
run;


*grades by domain (read in domains);
proc sql;
	create table test as
	select student.ID, domainnumber,
		sum(case when StudentAns=CorrectAns then 1 else 0 end) as totalscore,
		round(calculated totalscore/150, .01) as percscore
	from answers, student, domains
	where student.i=answers.i=domains.questionnumber
	group by student.ID, domains.domainnumber
	;
quit;


*make it a macro;


%macro Nathan(form, key, tablename);

data student (keep=ID i StudentAns) answers (keep=ID i CorrectAns);
	infile "C:\Users\ward32\Downloads\Form &form.1.csv" dlm="," dsd missover;
	input ID $ blank (q1-q150) ($);
	array k{150} $ q1-q150;
	if ID = "&key" then 
		do i = 1 to 150;
			CorrectAns=k{i};
			output answers;
		end;
	else 
		do i = 1 to 150;
			StudentAns = k{i};
			output student;
		end;
run; 

proc sql;
	create table &tablename as
	select student.ID,
		sum(case when StudentAns=CorrectAns then 1 else 0 end) as totalscore,
		round(calculated totalscore/150, .01) as percscore
	from answers, student
	where student.i=answers.i
	group by student.ID
	;
quit;

%mend;

%Nathan(form=A, key=AAAAKEY , tablename=tableA);
%Nathan(form=B, key=BBBBKEY, tablename=tableB);
*continue with forms C and D;

proc print data=tableA;
run;

*combine into one table;

proc sql;
	create table AllForms as
	select *
	from tableA 
	union 
	select * 
	from tableB
	;
quit;

*alternatively;

data AllFormsDataStep;
	set tableA tableB;
run;

proc sql;
	select * 
	from AllForms
	;
quit;
