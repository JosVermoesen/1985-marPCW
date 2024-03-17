1 ON ERROR GOTO 20000
10 REM VFAKTIN.MAR=MANUEEL INVOEREN VERKOOPFAKTUREN versie 040487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
80 OPEN "r",1,drive$+"99.rnd",30:FIELD 1,28 AS rec.nummer$:GOSUB 610:CLOSE 1
90 PRINT cls$:PRINT FNat$(12,5,"INVOER FAKTUUR/CREDITNOTA (F/C) ?");
100 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$="" THEN 100
110 IF m$="F" THEN r=11 ELSE r=13
120 BUFFERS 20
130 recleng1=162:OPEN "K",1,drk$+"klanten.rnd",drk$+"klanten.key",2,recleng1
140 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
150 IF r=11 THEN t$="VERKOOPFAKTUREN MANUEEL INBRENGEN" ELSE t$="CREDITNOTA'S OP VERKOOPFAKTUREN MANUEEL INBRENGEN"
160 tkvk=0:bt6=0:bt19=0:bt25=0:bt33=0:bt0=0:nttb=0:tb6=0:tb19=0:tb25=0:tb33=0:tb0=0:ttb=0:tekst=0:ttaksbtw=0:temp=0
170 REM
180 PRINT cls$:PRINT FNat$(0,0,t$):PRINT STRING$(79,172):PRINT FNat$(0,69,"NR : ");:INPUT"",anr$:IF LEN(anr$)>4 THEN 180 ELSE anr=VAL(anr$)
190 PRINT FNat$(3,0,"NUMMER KLANT :"):PRINT FNat$(22,0,STRING$(79,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR EINDE");
200 PRINT FNat$(3,15,"......");eol$:PRINT FNat$(3,15,"");:INPUT "",nrk$:IF nrk$="" THEN 430:IF LEN(nrk$)<>6 THEN 200
210 kontrole=SEEKKEY(1,2,0,nrk$):IF kontrole<>0 THEN 200 ELSE GET 1
220 PRINT FNat$(3,25,rec.naam$):PRINT FNat$(4,25,rec.stad$);"OK (J/N) ? ";
230 naam$=rec.naam$
240 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$="" THEN 240
250 IF invoer$="N" THEN 200
260 t=0
270 PRINT FNat$(7,15,"1. JAAR (JJ) : ");:INPUT "",d9$:IF t=1 THEN RETURN
280 PRINT FNat$(8,15,"2. DATUM (DD/MM/JJ) : ");:INPUT "",d$:IF t=1 THEN RETURN
290 PRINT FNat$(9,15,"3. VERVALDAG (DD/MM/JJ) : ");:INPUT "",dat1$:IF t=1 THEN RETURN
300 PRINT FNat$(10,15,"4. MEDEKONTRAKTANT (J/N)? : ");:INPUT "",mktr$:mktr$=UPPER$(mktr$):IF INSTR("JN",mktr$)=0 THEN 300 ELSE IF t=1 THEN RETURN
310 PRINT FNat$(11,15,"5. HANDELSGOED 6 % : ");:INPUT "",tb6$:tb6=VAL(tb6$):IF t=1 THEN RETURN
320 PRINT FNat$(12,15,"6. HANDELSGOED 17 % : ");:INPUT "",tb19$:tb19=VAL(tb19$):IF t=1 THEN RETURN
330 PRINT FNat$(13,15,"7. HANDELSGOED 19 % : ");:INPUT "",tb25$:tb25=VAL(tb25$):IF t=1 THEN RETURN
340 PRINT FNat$(14,15,"8. HANDELSGOED 25 % : ");:INPUT "",tb33$:tb33=VAL(tb33$):IF t=1 THEN RETURN
350 PRINT FNat$(15,15,"9. HANDELSGOED 0 % : ");:INPUT "",nttb$:nttb=VAL(nttb$):IF t=1 THEN RETURN
360 PRINT FNat$(16,15,"10. BEDRAG BTW : ");:INPUT "",tbtw$:tbtw=VAL(tbtw$):IF t=1 THEN RETURN
370 PRINT FNat$(17,15,"11. SPECIALE TAKS : ");:INPUT "",ttaksbtw$:ttaksbtw=VAL(ttaksbtw$):IF t=1 THEN RETURN
380 t=1:ttb=tb6+tb19+tb25+tb33
390 PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"DRUK <E> VOOR WEGSCHRIJVEN - <A> ANNULEREN - OF EEN NUMMER");
400 INPUT "",p$:IF UPPER$(p$)="E" THEN 440 ELSE IF UPPER$(p$)="A" THEN 180 ELSE IF VAL(p$)<0 OR VAL(p$)>11 THEN 390
410 ON VAL(p$) GOSUB 270,280,290,300,310,320,330,340,350,360,370
420 t=1:ttb=tb6+tb19+tb25+tb33:GOTO 390
430 CLOSE 1:CLOSE 2:CLOSE 3:RUN "menu.mar"
440 CLOSE 1
450 IF r=11 THEN s$="VF" ELSE s$="CV"
460 sleutel$=s$+STR$(anr)
470 IF FIND$(drv$+"vkfaktr.rnd")="" THEN GOSUB 580
480 OPEN "K",1,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
490 FIELD 1,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datum$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
500 LSET rec.nrk$=nrk$:LSET rec.dat$=d$:LSET rec.datum$=dat1$
510 IF UPPER$(mktr$)="N" THEN 530
520 tb6=0:tb19=0:tb25=0:tb33=0:tbtw=0:ttaksbtw=0
530 LSET rec.bt6$=STR$(tb6):LSET rec.bt19$=STR$(tb19):LSET rec.bt25$=STR$(tb25):LSET rec.bt33$=STR$(tb33):LSET rec.nttb$=STR$(nttb):LSET rec.rb$="":LSET rec.reserve$="":LSET rec.taks$=STR$(ttaksbtw)
540 LSET rec.ttb$=STR$(tbtw-ttaksbtw):temp=ttb:ttb=0
550 IF UPPER$(mktr$)="J" THEN LSET rec.medektr$=STR$(temp) ELSE LSET rec.medektr$=""
560 LSET rec.bdok$="":LSET rec.boek$=s$+MID$(sleutel$,4,4):kontrole=ADDREC(1,2,0,nrk$):a=FETCHREC(1):kontrole=ADDKEY(1,2,1,d9$+sleutel$,a)
570 CLOSE 1:GOTO 130
580 CREATE 1,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
590 CLOSE 1
600 RETURN
610 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
620 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 640 ELSE 630
630 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
640 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1: