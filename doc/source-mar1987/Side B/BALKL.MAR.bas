1 ON ERROR GOTO 20000
10 REM BALKL.MAR=UITDRUKKEN BALANS KLANTEN versie 240687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
60 OPTION RUN
70 DIM m1$(30),btkrt(30),btld(30),oms$(30),m2$(30),nrr$(50)
80 BUFFERS 20
90 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
130 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
140 FIELD 1,28 AS rec.nummer$:GOSUB 660:CLOSE 1
145 IF bl=72 THEN lnbl=50 ELSE lnbl=44
150 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
160 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
170 IF FIND$(drv$+"vkfaktr.rnd")=""THEN RUN"menu.mar"ELSE OPEN"K",3,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
180 FIELD 3,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datl$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
190 PRINT cls$:PRINT FNat$(0,0,"UITDRUKKEN BALANS KLANTEN")
200 PRINT FNat$(1,0,STRING$(80,172))
210 PRINT FNat$(12,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(12,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
220 PRINT FNat$(14,12,"MIN KLANTNUMMER : ......");eol$;hel$;:PRINT FNat$(14,30,"");:INPUT"",mini$:PRINT zcht$;:IF LEN(mini$)<>6 THEN 220
230 PRINT FNat$(15,12,"MAX KLANTNUMMER : ......");eol$;hel$;:PRINT FNat$(15,30,"");:INPUT"",maxi$:PRINT zcht$;:IF LEN(maxi$)<>6 THEN 230
240 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
250 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 250
260 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
270 PRINT FNat$(19,5,"PRINTER EN PAPIER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
280 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 280
290 IF at$="M"THEN 650
300 LPRINT CHR$(27);"C";CHR$(72);
310 LPRINT CHR$(27);"N";CHR$(1);
320 tp$="BALANS KLANTEN"
330 pteller=pteller+1
340 LPRINT CHR$(14);tp$;" ";firm$:LPRINT TAB(120);"PAGINA : ";USING"##";pteller:LPRINT TAB(116);"DATUM : ";d$:LPRINT
350 z$=STRING$(132,"-")
360 LPRINT:LPRINT z$:LPRINT TAB(1);"NUMMER";TAB(8);"NAAM KLANT";TAB(33);"FAKT.";TAB(40);"DATUM";TAB(49);"TOTAAL";TAB(59);"REEDS";TAB(69);"BETALING";TAB(79);"NOG TE";TAB(89);"VERVALDAG";TAB(99);"CUMUL"
370 LPRINT TAB(33);"NR.";TAB(49);"BEDRAG";TAB(59);"ONTVANGEN";TAB(69);"STUK";TAB(79);"ONTVANGEN"
380 LPRINT z$:LPRINT
390 IF p<>0 THEN RETURN
400 kontrole=SEEKRANK(3,2,0)
410 IF mini$=""THEN 420 ELSE kontrole=SEEKKEY(3,2,0,mini$)
420 tot=0:ttot=0:nt=0:tnt=0:trb=0
430 a$=FETCHKEY$(3)
440 IF a$>maxi$THEN 640
450 p=1
460 kontrole=SEEKKEY(3,2,0,a$):IF kontrole<>0 THEN 640 ELSE GET 3
470 kontrole=SEEKKEY(1,2,0,rec.nrk$):IF kontrole<>0 THEN STOP ELSE GET 1
480 t=t+1:LPRINT:LPRINT TAB(1);a$;TAB(8);MID$(rec.naam$,1,20);
490 tot=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.nttb$)+VAL(rec.ttb$)+VAL(rec.medektr$)+VAL(rec.taks$)+VAL(rec.reserve$)
500 IF MID$(rec.boek$,1,2)="CV"THEN tot=-tot
510 ttot=ttot+tot:nt=tot-VAL(rec.rb$):tnt=tnt+nt:trb=trb+VAL(rec.rb$):vtot=vtot+tot-VAL(rec.rb$)
520 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
530 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
540 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
550 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
560 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
570 IF tot=VAL(rec.rb$)THEN dat2$=""
580 LPRINT TAB(33);rec.boek$;TAB(40);rec.dat$;TAB(49);USING"#########";tot;:LPRINT TAB(59);USING"#########";VAL(rec.rb$);:LPRINT TAB(69);rec.bdok$;TAB(79);USING"#########";tot-VAL(rec.rb$);:LPRINT TAB(89);dat2$;:LPRINT TAB(99);USING"#########";tnt
590 t=t+1
600 IF t>lnbl THEN t=0 ELSE 620
610 LPRINT CHR$(12);:GOSUB 330
620 kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 420 ELSE GET 3
630 GOTO 490
640 LPRINT z$:LPRINT"N O G   T E   O N T V A N G E N :";TAB(99);USING"#########";vtot:LPRINT CHR$(12);
650 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
660 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
670 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 690 ELSE 680
680 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
690 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M