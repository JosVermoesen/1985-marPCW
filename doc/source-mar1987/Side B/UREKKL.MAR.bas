1 ON ERROR GOTO 20000
10 REM UREKKL.MAR=UITDRUKKEN REKENINGUITTREKSEL KLANTEN DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 DIM m1$(30),btkrt(30),btld(30),oms$(30),m2$(30),nrr$(50)
90 BUFFERS 20
100 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
110 OPEN"R",1,drive$+"99.rnd",30
120 FIELD 1,6 AS rec.nummer$
130 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
140 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$:GET 1,48:std$=rec.m$
150 FIELD 1,28 AS rec.nummer$:GOSUB 600:CLOSE 1
160 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
170 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
180 IF FIND$(drv$+"vkfaktr.rnd")="" THEN RUN "menu.mar" ELSE OPEN"K",3,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
190 FIELD 3,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datl$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
200 PRINT cls$:PRINT FNat$(0,0,"UITDRUKKEN REKENINGUITTREKSEL KLANT")
210 PRINT FNat$(1,0,STRING$(80,172))
220 PRINT FNat$(12,10,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(12,26,"");:INPUT"",dat$:IF dat$=""THEN dat$=d$
230 PRINT zcht$;:PRINT FNat$(16,5,"PRINTER EN PAPIER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
240 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 240
250 IF at$="M"THEN 590
260 PRINT FNat$(18,5,"NUMMER KLANT : 400......    ");hel$;:PRINT FNat$(18,23,"");:INPUT"",klant$:PRINT zcht$;:IF klant$=""THEN 590
270 kontrole=SEEKKEY(1,2,0,klant$):IF kontrole<>0 THEN 260 ELSE GET 1
280 PRINT hel$;:PRINT FNat$(20,20,rec.naam$);zcht$;+" OK (J/N)?";:
290 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 290
300 IF a$="N"THEN 260
310 LPRINT CHR$(27);"C";CHR$(72);
320 LPRINT CHR$(27);"N";CHR$(1);
330 LPRINT CHR$(18);
340 tp$="* REKENINGUITTREKSEL *"
350 LPRINT:LPRINT CHR$(14);TAB(18);tp$:LPRINT:LPRINT firm$:LPRINT strt$:LPRINT std$:LPRINT:LPRINT
360 LPRINT TAB(50);"DATUM : ";d$:LPRINT:LPRINT:LPRINT:LPRINT:LPRINT:LPRINT:LPRINT TAB(50);rec.naam$:LPRINT TAB(50);rec.straat$:LPRINT TAB(50);rec.postkode$+MID$(rec.stad$,1,20)
370 LPRINT:LPRINT:LPRINT:z$=STRING$(80,"-")
380 LPRINT:LPRINT z$:LPRINT TAB(1);"FAKT.";TAB(9);"DATUM";TAB(18);"TOTAAL";TAB(28);"REEDS";TAB(38);"BETALING";TAB(48);"NOG TE";TAB(58);"VERVALDAG";TAB(68);"CUMUL"
390 LPRINT TAB(1);"NR.";TAB(18);"BEDRAG";TAB(28);"ONTVANGEN";TAB(38);"STUK";TAB(48);"ONTVANGEN"
400 LPRINT z$:LPRINT
410 kontrole=SEEKRANK(3,2,0)
420 tot=0:ttot=0:nt=0:tnt=0:trb=0
430 kontrole=SEEKKEY(3,2,0,klant$):IF kontrole<>0 THEN 570 ELSE GET 3
440 kontrole=SEEKKEY(1,2,0,rec.nrk$):IF kontrole<>0 THEN STOP ELSE GET 1
450 tot=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.nttb$)+VAL(rec.ttb$)+VAL(rec.medektr$)+VAL(rec.taks$)+VAL(rec.reserve$)
460 IF MID$(rec.boek$,1,2)="CV"THEN tot=-tot
470 ttot=ttot+tot:nt=tot-VAL(rec.rb$):tnt=tnt+nt:trb=trb+VAL(rec.rb$):vtot=vtot+tot-VAL(rec.rb$)
480 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
490 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
500 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
510 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
520 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
530 IF tot=VAL(rec.rb$)THEN dat2$=""
540 LPRINT TAB(1);rec.boek$;TAB(9);rec.dat$;TAB(18);USING"#########";tot;:LPRINT TAB(28);USING"#########";VAL(rec.rb$);:LPRINT TAB(38);rec.bdok$;TAB(48);USING"#########";tot-VAL(rec.rb$);:LPRINT TAB(58);dat2$;:LPRINT TAB(68);USING"#########";tnt
550 kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 570 ELSE GET 3
560 GOTO 450
570 LPRINT z$:LPRINT"T O T A A L";TAB(68);USING"#########";vtot:LPRINT CHR$(12);
580 vtot=0:GOTO 260
590 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
600 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
610 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 630 ELSE 620
620 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
630 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT