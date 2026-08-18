1 ON ERROR GOTO 20000
10 REM AFAKT.MAR=INVOEREN AANKOOPFAKTUREN DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 PRINT cls$:PRINT FNat$(12,10,"INVOER FAKTUUR/CREDITNOTA (F/C) ?");
80 DIM nrr$(20),hggoed(20)
90 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$=""THEN 90
100 IF m$="F"THEN r=1 ELSE r=3
110 BUFFERS 40
120 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,r:anr=VAL(rec.nummer$):GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,30:taks$=rec.nummer$:GET 1,7:mktr$=rec.nummer$:GET 1,8:invhf$=rec.nummer$:GET 1,37:boekj$=rec.nummer$
160 GET 1,16:bta$=rec.nummer$:GET 1,17:nbt$=rec.nummer$:GET 1,10:arl$=rec.nummer$:GET 1,19:btc$=rec.nummer$:GET 1,26:secret$=rec.nummer$:FIELD 1,28 AS rec.nummer$:GET 1,46:firm$=rec.nummer$:GOSUB 1740:CLOSE 1
170 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
180 lt=INSTR(firm$,"  "):b$=MID$(firm$,1,2)+MID$(firm$,lt-4,4)
190 FOR t=1 TO 6
200 c$=c$+MID$(STR$(ASC(MID$(b$,t,1))),2,1)
210 NEXT t
220 c$=MID$(STR$(VAL(c$)*610225!),2,6)
230 IF c$=secret$THEN 250 ELSE PRINT"ONJUIST SLEUTELNUMMER INGEBRACHT..."
240 RUN"menu.mar"
250 FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
260 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2:IF FIND$(draa$+"afactuur.rnd")=""THEN GOSUB 1610 ELSE OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2
270 FIELD 2,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
280 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
290 anr=anr+1:IF r=1 THEN t$="AANKOOPFAKTUREN"ELSE t$="CREDITNOTA'S OP AANKOOPFAKTUREN"
300 invest=0:tellerlijn=0:PRINT cls$:PRINT hel$;:PRINT FNat$(0,0,t$):PRINT zcht$;:PRINT FNat$(0,72,"NR:");hel$;USING"####";anr;:PRINT zcht$;:PRINT STRING$(80,CHR$(172))
310 PRINT:PRINT"NR LEVERANCIER : 440":PRINT:PRINT:PRINT STRING$(80,"-");"NUMMER NAAM REKENING                                BEDRAG   INVEST/KOST (I/K)";STRING$(80,"-")
320 PRINT FNat$(12,0,STRING$(80,CHR$(172)))
330 PRINT zcht$;:PRINT FNat$(14,0,"1. DATUM FAKTUUR       :"):PRINT"2. OMSCHRIJVING        :":PRINT"3. BEDRAG BTW          :":PRINT"4. BTW NIET AFTREKBAAR :":PRINT"5. SPECIALE TAKS       :"
340 PRINT"6. BTW MEDEKTR. (J/N)  :":PRINT"7. BTW INVOER VERL.HEF.:":PRINT FNat$(18,40,"   TOTAAL GOED/DIENST  :"):PRINT FNat$(19,40,"   TOTAAL INVESTERING  :"):PRINT FNat$(20,40,"   TOTAAL FAKTUUR      :"):PRINT FNat$(22,0,STRING$(80,CHR$(172)))
350 PRINT FNat$(3,20,"......   "):PRINT FNat$(3,20,"");hel$;:INPUT"",nrl$:PRINT zcht$;:IF nrl$="" THEN 1520
360 kontrole=SEEKKEY(1,2,0,nrl$):IF kontrole<>0 THEN 350 ELSE GET 1
370 naam$=rec.naam$
380 PRINT hel$;:PRINT FNat$(3,28,rec.naam$):PRINT FNat$(4,28,rec.stad$);zcht$;" OK (J/N) ?";
390 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$=""THEN 390
400 IF invoer$="N"THEN 350 ELSE PRINT FNat$(4,59,eol$)
410 tellerlijn=0:hg=0:tellertje=1:dl=0
420 WHILE tellertje<>0
430 tellerlijn=tellerlijn+1:dl=dl+1
440 PRINT FNat$(dl+8,0,"......   "):PRINT hel$;:PRINT FNat$(dl+8,0,"");:INPUT"",nrr$(tellerlijn):IF nrr$(tellerlijn)=""THEN tellertje=0 ELSE IF LEN(nrr$(tellerlijn))<>6 THEN 440
450 IF tellertje=0 THEN 580
460 kontrole=SEEKKEY(2,2,0,nrr$(tellerlijn)):IF kontrole<>0 THEN 440 ELSE GET 2
470 PRINT hel$;:PRINT FNat$(dl+8,7,rec.om$);zcht$;" OK (J/N) ?";
480 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$=""THEN 480
490 IF invoer$="N"THEN 440
500 PRINT FNat$(dl+8,38,eol$):PRINT FNat$(dl+8,52,"");hel$;:INPUT"",getal:IF getal=0 THEN 580 ELSE hggoed(tellerlijn)=getal
510 PRINT FNat$(dl+8,75,"");
520 antw$="IK":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 520
530 PRINT FNat$(dl+8,75,at$):PRINT zcht$;
540 IF at$="I"THEN invest=invest+getal
550 hg=hg+hggoed(tellerlijn)
560 IF dl=3 THEN dl=0 ELSE 580
570 FOR kls=1 TO 3:PRINT FNat$(8+kls,0,eol$):NEXT kls
580 WEND
590 IF tellertje=0 AND tellerlijn=1 THEN 350
600 tellerlijn=tellerlijn-1
610 t=0
620 PRINT FNat$(14,25,d$):PRINT FNat$(14,25,"");:INPUT"",dat$:IF dat$=""THEN dat$=d$ELSE IF LEN(dat$)<>8 THEN 620
630 IF T=1 THEN 770
640 PRINT FNat$(15,25,"");:INPUT"",oms$:IF t=1 THEN 740
650 PRINT FNat$(16,25,"");:INPUT"",btw$:btw=VAL(btw$):IF t=1 THEN 740
660 PRINT FNat$(17,25,"");:INPUT"",nbtw$:nbtw=VAL(nbtw$):IF t=1 THEN 740
670 PRINT FNat$(18,25,"");:INPUT"",sptaks$:sptaks=VAL(sptaks$):IF t=1 THEN 740
680 PRINT FNat$(19,25,"");
690 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$="" THEN 690
700 IF invoer$="J" THEN btwmktr=btw ELSE btwmktr=0
710 PRINT FNat$(19,25,invoer$);
720 IF t=1 THEN 740
730 PRINT FNat$(20,25,"");:INPUT"",btwinvhf$:btwinvhf=VAL(btwinvhf$):IF t=1 THEN 740
740 PRINT FNat$(18,65,"");USING"#########";hg
750 PRINT FNat$(19,65,"");USING"#########";invest
760 PRINT FNat$(20,65,"");USING"#########";hg+btw+nbtw+sptaks-btwmktr+btwinvhf
770 t=1
780 PRINT zcht$;:PRINT FNat$(23,0,"GEEF NUMMER OM TE WIJZIGEN - <E> VOOR EINDE - <A> VOOR ANNULEREN");eol$;
790 antw$="1234567EA":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$=""THEN 790
800 IF invoer$="E"THEN 820 ELSE IF invoer$="A"THEN 300
810 ON VAL(invoer$)GOTO 620,640,650,660,670,680,730
820 PRINT FNat$(23,0,"WEGSCHRIJVEN NAAR SCHIJF...");eol$;
830 REM wegschrijven individ. leveranciersrek
840 jaaromzet=VAL(rec.jaaromzet$):saldo=VAL(rec.saldo$):IF r=3 THEN 860
850 saldo=saldo+btw+nbtw+hg+sptaks-btwmktr+btwinvhf:jaaromzet=jaaromzet+btw+nbtw+hg+sptaks-btwmktr+btwinvhf:GOTO 870
860 saldo=saldo+((btw+nbtw+hg+sptaks-btwmktr+btwinvhf)*(-1)):jaaromzet=jaaromzet+((btw+nbtw+hg+sptaks-btwmktr+btwinvhf)*(-1))
870 LSET rec.saldo$=STR$(saldo):LSET rec.jaaromzet$=STR$(jaaromzet):PUT 1
880 REM wegschrijven collective leveranciers
890 kontrole=SEEKKEY(2,2,0,arl$):IF kontrole<>0 THEN 1720 ELSE GET 2
900 ms=VAL(rec.maandsaldo$):IF r=3 THEN 920
910 ms=ms+((btw+nbtw+hg+sptaks-btwmktr+btwinvhf)*(-1)):GOTO 930
920 ms=ms+btw+nbtw+hg+sptaks-btwmktr+btwinvhf
930 LSET rec.maandsaldo$=STR$(ms):PUT 2
940 REM wegschrijven kosten.invest. rekening
950 FOR teller=1 TO tellerlijn:kontrole=SEEKKEY(2,2,0,nrr$(teller)):IF kontrole<>0 THEN 1720 ELSE GET 2
960 ms=VAL(rec.maandsaldo$):IF r=3 THEN 980
970 ms=ms+hggoed(teller):GOTO 990
980 ms=ms+(hggoed(teller)*(-1))
990 LSET rec.maandsaldo$=STR$(ms):PUT 2:NEXT teller
1000 REM wegschrijven btw rekening
1010 IF r=1 THEN btw$=bta$ELSE btw$=btc$
1020 kontrole=SEEKKEY(2,2,0,btw$):IF kontrole<>0 THEN 1720 ELSE GET 2
1030 ms=VAL(rec.maandsaldo$):IF r=3 THEN 1050
1040 ms=ms+btw+sptaks:GOTO 1060
1050 ms=ms-btw-sptaks
1060 LSET rec.maandsaldo$=STR$(ms):PUT 2
1070 kontrole=SEEKKEY(2,2,0,invhf$):IF kontrole<>0 THEN 1720 ELSE GET 2
1080 ms=VAL(rec.maandsaldo$):IF r=1 THEN ms=ms+btwinvhf ELSE ms=ms-btwinvhf
1090 LSET rec.maandsaldo$=STR$(ms):PUT 2
1100 REM wegschrijven niet aftrekbare btw
1110 kontrole=SEEKKEY(2,2,0,nbt$):IF kontrole<>0 THEN 1720 ELSE GET 2
1120 ms=VAL(rec.maandsaldo$):IF r=3 THEN 1140
1130 ms=ms+nbtw:GOTO 1150
1140 ms=ms+(nbtw*(-1))
1150 LSET rec.maandsaldo$=STR$(ms):PUT 2
1160 kontrole=SEEKKEY(2,2,0,mktr$):IF kontrole<>0 THEN 1720 ELSE GET 2
1170 ms=VAL(rec.maandsaldo$):IF r=1 THEN ms=ms-btwmktr ELSE ms=ms+btwmktr
1180 LSET rec.maandsaldo$=STR$(ms):PUT 2
1190 REM wegschrijven naar fakturatiebestand
1200 IF r=3 THEN s$="CA"ELSE s$="AF"
1210 LSET rec.nrl$=nrl$:LSET rec.nrr$=nrr$(1):LSET rec.dat$=dat$:LSET rec.oms$=oms$:LSET rec.hg$=STR$(hg):LSET rec.btw$=STR$(btw):LSET rec.nbtw$=STR$(nbtw):LSET rec.rbet$="":LSET rec.bdok$="":LSET rec.invvhf$=STR$(btwinvhf):LSET rec.sptaks$=STR$(sptaks)
1220 LSET rec.invest$=STR$(invest):LSET rec.btwmedektr$=STR$(btwmktr)
1230 IF r=1 THEN s$="AF"ELSE s$="CA"
1240 sleutel$=nrl$:sleutel2$=s$+STR$(anr):LSET rec.kode$=s$+MID$(sleutel2$,4,4)
1250 d9$=MID$(dat$,7,2)+MID$(dat$,4,2)+MID$(dat$,1,2)
1260 kontrole=ADDREC(3,2,0,sleutel$):a=FETCHREC(3):kontrole=ADDKEY(3,2,1,MID$(d9$,1,2)+sleutel2$,a)
1270 IF MID$(boekj$,1,2)="ON"THEN GOSUB 1650
1280 CLOSE 1:IF FIND$(drj$+"journaal.rnd")=""THEN GOSUB 1560 ELSE OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2
1290 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
1300 n=0:nr$=STR$(n):kontrole=SEEKKEY(1,2,0,nr$):GET 1:n=VAL(rec.datum$)
1310 FOR teller=1 TO tellerlijn:IF hggoed(teller)=0 THEN 1340
1320 IF r=3 THEN ms=-hggoed(teller)ELSE ms=hggoed(teller)
1330 LSET rec.num$=nrr$(teller):LSET rec.datum$=dat$:LSET rec.omschr$=s$+MID$(STR$(anr),2,LEN(STR$(anr)))+" "+naam$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,nrr$(teller),y)
1340 NEXT teller
1350 IF btw=0 AND sptaks=0 THEN 1380
1360 IF r=3 THEN ms=-btw-sptaks ELSE ms=btw+sptaks
1370 LSET rec.num$=btw$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,btw$,y)
1380 IF btwmktr=0 THEN 1410
1390 IF r=3 THEN ms=btwmktr ELSE ms=-btwmktr
1400 LSET rec.num$=mktr$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,mktr$,y)
1410 IF nbtw=0 THEN 1440
1420 IF r=3 THEN ms=-nbtw ELSE ms=nbtw
1430 LSET rec.num$=nbt$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,nbt$,y)
1440 IF r=3 THEN ms=hg+btw+nbtw+sptaks-btwmktr+btwinvhf ELSE ms=-(hg+btw+nbtw+sptaks-btwmktr+btwinvhf)
1450 LSET rec.num$=arl$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,arl$,y)
1460 IF btwinvhf=0 THEN 1490
1470 IF r=3 THEN ms=-btwinvhf ELSE ms=btwinvhf
1480 LSET rec.num$=invhf$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,invhf$,y)
1490 nr=0:nr$=STR$(nr):kontrole=SEEKKEY(1,2,0,nr$):GET 1:LSET rec.datum$=STR$(n):PUT 1
1500 CLOSE 1:OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2:FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$
1510 GOTO 290
1520 CLOSE 1:OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$
1530 LSET rec.nummer$=STR$(anr-1):PUT 1,r
1540 CLOSE 1:CLOSE 2:CLOSE 3
1550 RUN"menu.mar"
1560 BUFFERS 10:CREATE 1,drj$+"journaal.rnd",drj$+"journaal.key",2,50
1570 CLOSE 1
1580 OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2:FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
1590 nr=0:nr$=STR$(nr):LSET rec.num$="":LSET rec.datum$=STR$(nr):LSET rec.omschr$="":LSET rec.bedrg$=STR$(0):kontrole=ADDREC(1,2,0,nr$):PUT 1
1600 RETURN
1610 BUFFERS 10:CREATE 3,draa$+"afactuur.rnd",draa$+"afactuur.key",2,132
1620 CLOSE 3
1630 recleng=132:OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2,recleng
1640 RETURN
1650 CLOSE 1
1660 IF FIND$(draa$+"afactuur.dp1")=""THEN CREATE 1,draa$+"afactuur.dp1",draa$+"afactuur.kp1",2 ELSE OPEN"K",1,draa$+"afactuur.dp1",draa$+"afactuur.kp1",2
1670 FIELD 1,4 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
1680 LSET rec.nrl$=nrl$:LSET rec.nrr$=nrr$(1):LSET rec.dat$=dat$:LSET rec.oms$=oms$:LSET rec.hg$=STR$(hg):LSET rec.btw$=STR$(btw):LSET rec.nbtw$=STR$(nbtw):LSET rec.rbet$="":LSET rec.bdok$="":LSET rec.invvhf$=STR$(btwinvhf):LSET rec.sptaks$=STR$(sptaks)
1690 LSET rec.kode$=s$+MID$(sleutel2$,4,4):LSET rec.invest$=STR$(invest):LSET rec.btwmedektr$=STR$(btwmktr)
1700 kontrole=ADDREC(1,2,0,sleutel$):a=FETCHREC(1):kontrole=ADDKEY(1,2,1,MID$(d9$,1,2)+sleutel2$,a)
1710 RETURN
1720 PRINT FNat$(23,0,"U HEBT GEGEVENS IN BESTANDEN ONBEDACHT GEWIJZIGD !!!");eol$
1730 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
1740 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1750 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1770 ELSE 1760
1760 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1770 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME N