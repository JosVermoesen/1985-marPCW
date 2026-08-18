1 ON ERROR GOTO 20000
10 REM OVFAKT.MAR=OPVRAGEN FAKTUREN PER KLANT
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 DIM m1$(30),btkrt(30),btld(30),oms$(30),m2$(30),nrr$(50)
80 BUFFERS 20
90 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
130 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
140 FIELD 1,28 AS rec.nummer$:GOSUB 520:CLOSE 1
150 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
160 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
170 IF FIND$(drv$+"vkfaktr.rnd")=""THEN RUN"menu.mar"ELSE OPEN"K",3,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
180 FIELD 3,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datl$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
190 PRINT cls$:PRINT FNat$(0,0,"OPVRAGEN FAKTUREN/CREDITNOTA'S KLANTEN"):PRINT STRING$(80,CHR$(172))
200 PRINT FNat$(3,0,"NR KLANT : 400")
210 PRINT FNat$(7,0,"");:PRINT TAB(1);"FAKT.";TAB(8);"DATUM";TAB(19);"TOTAAL";TAB(28);"REEDS";TAB(37);"BETALING";TAB(46);"NOG TE";TAB(54);"VERVALDAG"
220 PRINT TAB(1);"NR.";TAB(19);"BEDRAG";TAB(28);"BETAALD";TAB(37);"STUK";TAB(46);"BETALEN":PRINT STRING$(80,"-"):PRINT FNat$(22,0,STRING$(80,172))
230 tot=0:ttot=0:nt=0:tnt=0:trb=0
240 PRINT FNat$(3,14,"......   ");hel$;:PRINT FNat$(3,14,"");:INPUT"",klant$:PRINT zcht$;:IF klant$=""THEN 460
250 kontrole=SEEKKEY(1,2,0,klant$):IF kontrole<>0 THEN 240 ELSE GET 1
260 PRINT hel$;:PRINT FNat$(3,33,rec.naam$):PRINT FNat$(4,33,rec.straat$):PRINT FNat$(5,33,rec.stad$):PRINT FNat$(6,33,"TEL.NR.:"+rec.telefoon$);zcht$;:PRINT" OK (J/N)?";
270 antw$="JN":q$=UPPER$(INKEY$):IF INSTR(antw$,q$)=0 OR q$=""THEN 270
280 IF q$="N"THEN 200 ELSE PRINT zcht$;:PRINT FNat$(6,53,eol$)
290 a$=FETCHKEY$(3)
300 p=1
310 kontrole=SEEKKEY(3,2,0,klant$):IF kontrole<>0 THEN 440 ELSE GET 3
320 tot=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.nttb$)+VAL(rec.ttb$)+VAL(rec.medektr$)+VAL(rec.taks$)+VAL(rec.reserve$)
330 IF MID$(rec.boek$,1,2)="CV"THEN tot=-tot
340 ttot=ttot+tot:nt=tot-VAL(rec.rb$):tnt=tnt+nt:trb=trb+VAL(rec.rb$)
350 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
360 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
370 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
380 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
390 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
400 dl=dl+1:PRINT hel$;:PRINT FNat$(dl+9,0,"");TAB(1);rec.boek$;TAB(8);rec.dat$;TAB(17);USING"########";tot;:PRINT TAB(27);USING"########";VAL(rec.rb$);:PRINT TAB(36);rec.bdok$;TAB(45);USING"########";tnt;:PRINT TAB(54);dat2$
410 IF dl>10 THEN GOSUB 470
420 PRINT zcht$;:kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 440 ELSE GET 3
430 GOTO 320
440 dl=0:PRINT FNat$(23,0,"DRUK <RETURN> VOOR VERVOLG");eol$;
450 a$=INKEY$:IF a$=""THEN 450 ELSE IF ASC(a$)=13 THEN 190 ELSE 450
460 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
470 REM window opkuisen
480 PRINT zcht$;:PRINT FNat$(23,0,"DRUK <RETURN> VOOR HET VOLGENDE SCHERM FAKTUREN");eol$
490 a$=INKEY$:IF a$=""THEN 490 ELSE IF ASC(a$)=13 THEN 500 ELSE 490
500 FOR kls=11 TO 21:PRINT FNat$(kls,0,eol$):NEXT kls
510 PRINT hel$;:dl=0:RETURN
520 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
530 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 550 ELSE 540
540 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
550 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN