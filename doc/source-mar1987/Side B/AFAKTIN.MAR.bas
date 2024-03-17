1 ON ERROR GOTO 20000
10 REM AFAKTIN.MAR=INVOEREN MANUEEL AANKOOPFAKTUREN DD versie 040487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
80 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN "r",1,drive$+"99.rnd",30:FIELD 1, 28 AS rec.nummer$:GOSUB 630:CLOSE 1
100 PRINT cls$:PRINT FNat$(12,5,"INVOER FAKTUUR/CREDITNOTA (F/C) ?");
110 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$="" THEN 110
120 IF m$="F" THEN r=1 ELSE r=3
130 BUFFERS 40
140 OPEN "K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
150 FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
160 IF FIND$(dra$+"afactuur.rnd")="" THEN GOSUB 580 ELSE OPEN "K",3,dra$+"afactuur.rnd",dra$+"afactuur.key",2
170 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
180 IF r=1 THEN t$="AANKOOPFAKTUREN MANUEEL INBRENGEN" ELSE t$="CREDITNOTA'S OP AANKOOPFAKTUREN MANUEEL INBRENGEN"
190 invest=0:tellerlijn=0:PRINT cls$:PRINT FNat$(0,0,t$):PRINT STRING$(79,172):PRINT FNat$(0,69,"NR : ");:INPUT "",anr$:IF LEN(anr$)>4 THEN 190 ELSE anr=VAL(anr$)
200 PRINT FNat$(3,0,"NR LEVERANCIER :"):PRINT FNat$(22,0,STRING$(79,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR EINDE");eol$;
210 PRINT FNat$(3,17,"......");eol$:PRINT FNat$(3,17,"");:INPUT "",nrl$:IF nrl$="" THEN 560 ELSE IF LEN(nrl$)<>6 THEN 210
220 kontrole=SEEKKEY(1,2,0,nrl$):IF kontrole<>0 THEN 210 ELSE GET 1
230 naam$=rec.naam$
240 PRINT FNat$(3,27,rec.naam$):PRINT FNat$(4,27,rec.stad$);"OK (J/N) ? ";
250 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$="" THEN 250
260 IF invoer$="N" THEN 210
270 PRINT FNat$(8,17,"1. DATUM FAKTUUR       :"):PRINT SPACE$(17);"2. OMSCHRIJVING        :":PRINT SPACE$(17);"3. BEDRAG BTW          :":PRINT SPACE$(17);"4. BTW NIET AFTREKBAAR :":PRINT SPACE$(17);"5. SPECIALE TAKS       :"
280 tellerlijn=tellerlijn-1
290 PRINT SPACE$(17);"6. BTW MEDEKONTRAKTANT :":PRINT SPACE$(17);"7. BTW INVOER VERL.HEF.:":PRINT SPACE$(17);"   TOTAAL GOED/DIENST  :":PRINT SPACE$(17);"   TOTAAL INVESTERING  :":PRINT SPACE$(17);"   TOTAAL FAKTUUR      :"
300 t=0
310 PRINT FNat$(8,42,"../../.."):PRINT FNat$(8,42,"");:INPUT "",dat$:IF LEN(dat$)<>8 THEN 310
320 IF T=1 THEN 420
330 PRINT FNat$(9,42,"");:INPUT "",oms$:IF t=1 THEN 390
340 PRINT FNat$(10,42,"");:INPUT "",btw$:btw=VAL(btw$):IF t=1 THEN 390
350 PRINT FNat$(11,42,"");:INPUT "",nbtw$:nbtw=VAL(nbtw$):IF t=1 THEN 390
360 PRINT FNat$(12,42,"");:INPUT "",sptaks$:sptaks=VAL(sptaks$):IF t=1 THEN 390
370 PRINT FNat$(13,42,"");:INPUT "",btwmktr$:btwmktr=VAL(btwmktr$):IF t=1 THEN 390
380 PRINT FNat$(14,42,"");:INPUT "",btwinvhf$:btwinvhf=VAL(btwinvhf$):IF t=1 THEN 390
390 PRINT FNat$(15,42,"");:INPUT "",hg$:hg=VAL(hg$):IF t=1 THEN 390
400 PRINT FNat$(16,42,"");:INPUT "",invest$:invest=VAL(invest$):IF t=1 THEN 390
410 PRINT FNat$(17,42,"");hg+btw+nbtw+sptaks-btwmktr+btwinvhf
420 t=1
430 PRINT FNat$(23,0,"GEEF NUMMER OM TE WIJZIGEN - <E> VOOR EINDE - <A> VOOR ANNULEREN");
440 antw$="123456789EA":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$="" THEN 440
450 IF UPPER$(invoer$)="E" THEN 470 ELSE IF UPPER$(invoer$)="A" THEN 190
460 ON VAL(invoer$) GOTO 310,330,340,350,360,370,380
470 PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"WEGSCHRIJVEN NAAR SCHIJF...");
480 IF r=3 THEN s$="CA" ELSE s$="AF"
490 LSET rec.nrl$=nrl$:LSET rec.dat$=dat$:LSET rec.oms$=oms$:LSET rec.hg$=STR$(hg):LSET rec.btw$=STR$(btw):LSET rec.nbtw$=STR$(nbtw):LSET rec.rbet$="":LSET rec.bdok$="":LSET rec.invvhf$=STR$(btwinvhf):LSET rec.sptaks$=STR$(sptaks)
500 LSET rec.invest$=STR$(invest):LSET rec.btwmedektr$=STR$(btwmktr)
510 IF r=1 THEN s$="AF" ELSE s$="CA"
520 sleutel$=nrl$:sleutel2$=s$+STR$(anr):LSET rec.kode$=s$+MID$(sleutel2$,4,4)
530 d9$=MID$(dat$,7,2)+MID$(dat$,4,2)+MID$(dat$,1,2)
540 kontrole=ADDREC(3,2,0,sleutel$):a=FETCHREC(3):kontrole=ADDKEY(3,2,1,MID$(d9$,1,2)+sleutel2$,a)
550 GOTO 190
560 CLOSE 1:CLOSE 2:CLOSE 3
570 RUN "menu.mar"
580 BUFFERS 10:CREATE 3,dra$+"afactuur.rnd",dra$+"afactuur.key",2,132
590 CLOSE 3
600 OPEN "K",3,dra$+"afactuur.rnd",dra$+"afactuur.key",2
610 RETURN
620 CLOSE 1
630 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
640 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 660 ELSE 650
650 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
660 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE 