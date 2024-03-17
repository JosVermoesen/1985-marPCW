1 ON ERROR GOTO 20000
10 REM OAFAKT.MAR=OPVRAGEN FAKTUREN PER LEVERANCIER
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM option run
80 DIM m1$(30),btkrt(30),btld(30),oms$(30),m2$(30),nrr$(50)
90 BUFFERS 20
100 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
110 OPEN"R",1,drive$+"99.rnd",30
120 FIELD 1,6 AS rec.nummer$
130 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
140 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
150 FIELD 1,28 AS rec.nummer$:GOSUB 530:CLOSE 1
160 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
170 FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
180 IF FIND$(draa$+"afactuur.rnd")=""THEN RUN"menu.mar"ELSE OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2
190 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.medektr$,9 AS rec.invest$
200 PRINT cls$:PRINT FNat$(0,0,"OPVRAGEN FAKTUREN/CREDITNOTA'S LEVERANCIERS"):PRINT STRING$(80,CHR$(172))
210 PRINT FNat$(3,0,"NR LEVERANCIER : 440")
220 PRINT FNat$(7,0,"");:PRINT TAB(1);"FAKT.";TAB(8);"DATUM";TAB(19);"TOTAAL";TAB(28);"REEDS";TAB(37);"BETALING";TAB(46);"NOG TE";TAB(54);"VERVALDAG";TAB(64);"OMSCHRIJVING"
230 PRINT TAB(1);"NR.";TAB(19);"BEDRAG";TAB(28);"BETAALD";TAB(37);"STUK";TAB(46);"BETALEN":PRINT STRING$(80,"-"):PRINT FNat$(22,0,STRING$(80,172))
240 tot=0:ttot=0:nt=0:tnt=0:trb=0
250 PRINT FNat$(3,20,"......   ");hel$;:PRINT FNat$(3,20,"");:INPUT"",lev$:PRINT zcht$;:IF lev$=""THEN 470
260 kontrole=SEEKKEY(1,2,0,lev$):IF kontrole<>0 THEN 250 ELSE GET 1
270 PRINT hel$;:PRINT FNat$(3,33,rec.naam$):PRINT FNat$(4,33,rec.straat$):PRINT FNat$(5,33,rec.stad$):PRINT FNat$(6,33,"TEL.NR.:"+rec.telefoon$);zcht$;:PRINT" OK (J/N)?";
280 antw$="JN":q$=UPPER$(INKEY$):IF INSTR(antw$,q$)=0 OR q$=""THEN 280
290 IF q$="N"THEN 210 ELSE PRINT zcht$;:PRINT FNat$(6,53,eol$)
300 a$=FETCHKEY$(3)
310 p=1
320 kontrole=SEEKKEY(3,2,0,lev$):IF kontrole<>0 THEN 450 ELSE GET 3
330 tot=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+VAL(rec.invvhf$)+VAL(rec.sptaks$)-VAL(rec.medektr$)
340 IF MID$(rec.boek$,1,2)="CA"THEN tot=-tot
350 dl=dl+1:ttot=ttot+tot:nt=tot-VAL(rec.rbet$):tnt=tnt+nt:trb=trb+VAL(rec.rbet$)
360 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
370 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
380 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
390 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
400 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
410 PRINT hel$;:PRINT FNat$(dl+9,0,"");TAB(1);rec.boek$;TAB(8);rec.dat$;TAB(17);USING"########";tot;:PRINT TAB(27);USING"########";VAL(rec.rbet$);:PRINT TAB(36);rec.bdok$;TAB(45);USING"########";tnt;:PRINT TAB(54);dat2$;TAB(64);MID$(rec.oms$,1,17)
420 IF dl>10 THEN GOSUB 480
430 PRINT zcht$;:kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 450 ELSE GET 3
440 GOTO 330
450 dl=0:PRINT FNat$(23,0,"DRUK <RETURN> VOOR VERVOLG");eol$;
460 a$=INKEY$:IF a$=""THEN 460 ELSE IF ASC(a$)=13 THEN 200 ELSE 460
470 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
480 REM window opkuisen
490 PRINT zcht$;:PRINT FNat$(23,0,"DRUK <RETURN> VOOR HET VOLGENDE SCHERM FAKTUREN");eol$
500 a$=INKEY$:IF a$=""THEN 500 ELSE IF ASC(a$)=13 THEN 510 ELSE 500
510 FOR kls=11 TO 21:PRINT FNat$(kls,0,eol$):NEXT kls
520 PRINT hel$;:dl=0:RETURN
530 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
540 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 560 ELSE 550
550 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
560 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" T