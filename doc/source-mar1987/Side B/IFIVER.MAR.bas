1 ON ERROR GOTO 20000
10 REM IFIVER.MAR=INVOEREN FINANCIELE VERRICHTINGEN DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 DIM m1$(50),btkrt(50),btld(50),oms$(50),m2$(50),nrr$(50)
90 BUFFERS 20
100 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
110 OPEN"R",1,drive$+"99.rnd",30
120 FIELD 1,6 AS rec.nummer$
130 GET 1,1:afnr=VAL(rec.nummer$):GET 1,11:vfnr=VAL(rec.nummer$):GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,27:bkbtk$=rec.nummer$:GET 1,28:tgbtk$=rec.nummer$
140 GET 1,31:tk$=rec.nummer$:GET 1,32:tp$=rec.nummer$:GET 1,33:tb1$=rec.nummer$:GET 1,34:tb2$=rec.nummer$:GET 1,35:tb3$=rec.nummer$:GET 1,41:nk$=rec.nummer$:GET 1,42:np$=rec.nummer$:GET 1,38:tv$=rec.nummer$:GET 1,39:nv$=rec.nummer$
150 GET 1,43:nb1$=rec.nummer$:GET 1,44:nb2$=rec.nummer$:GET 1,45:nb3$=rec.nummer$:FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$:GET 1,26:secret$=rec.nummer$:FIELD 1,28 AS rec.nummer$:GOSUB 1690
160 lt=INSTR(firm$,"  "):b$=MID$(firm$,1,2)+MID$(firm$,lt-4,4)
170 FOR t=1 TO 6
180 c$=c$+MID$(STR$(ASC(MID$(b$,t,1))),2,1)
190 NEXT t
200 c$=MID$(STR$(VAL(c$)*610225!),2,6)
210 IF c$=secret$THEN 230 ELSE PRINT"ONJUIST SLEUTELNUMMER INGEBRACHT..."
220 RUN"menu.mar"
230 FIELD 1,6 AS rec.nummer$
240 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2
250 FIELD 2,30 AS rec.oms$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
260 IF VAL(MID$(tk$,3,5))<>0 THEN 280
270 kontrole=SEEKKEY(2,2,0,nk$):GET 2:tk$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tk$:PUT 1,31
280 IF VAL(MID$(tp$,3,5))<>0 THEN 300
290 kontrole=SEEKKEY(2,2,0,np$):GET 2:tp$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tp$:PUT 1,32
300 IF VAL(MID$(tb1$,3,5))<>0 THEN 320
310 kontrole=SEEKKEY(2,2,0,nb1$):GET 2:tb1$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tb1$:PUT 1,33
320 IF VAL(MID$(tb2$,3,5))<>0 THEN 340
330 kontrole=SEEKKEY(2,2,0,nb2$):GET 2:tb2$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tb2$:PUT 1,34
340 IF VAL(MID$(tb3$,3,5))<>0 THEN 360
350 kontrole=SEEKKEY(2,2,0,nb3$):GET 2:tb3$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tb3$:PUT 1,35
360 IF VAL(MID$(tv$,3,5))<>0 THEN 380
370 kontrole=SEEKKEY(2,2,0,nv$):GET 2:tv$=MID$(rec.oms$,1,2)+STR$(0):LSET rec.nummer$=tv$:PUT 1,38
380 CLOSE 1
390 t$="INVOEREN FINANCIELE VERRICHTINGEN"
400 PRINT cls$:PRINT FNat$(0,0,t$):PRINT FNat$(1,0,STRING$(80,172))
410 PRINT FNat$(3,0,"NR FIN. REK. :"):PRINT FNat$(22,0,STRING$(80,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR EINDE");eol$
420 PRINT FNat$(5,0,"NR BAL. REK. :")
430 PRINT FNat$(7,0,"DATUM     NR. KLANT/LEVERANC.   TE BETALEN  BETAALD  DOKUMENT"):PRINT STRING$(80,"-")
440 PRINT FNat$(16,0,STRING$(80,172));
450 PRINT FNat$(18,3,"ONTV./UITG.(O/U) :  "):PRINT FNat$(19,0,"   FAKTUUR (J/N)    :  "):PRINT FNat$(20,0,"1. FAKTUUR NR       : ....  JAAR : ..  "):PRINT FNat$(18,39,"2. BET. KORTING :         "):PRINT FNat$(19,39,"3. BEDRAG       :")
460 PRINT FNat$(20,39,"4. OMSCHRIJVING :")
470 PRINT FNat$(3,15,"......   ");hel$;:PRINT FNat$(3,15,"");:INPUT"",nrf$:PRINT zcht$;:IF LEN(nrf$)=0 THEN 1630 ELSE IF LEN(nrf$)<>6 THEN 470
480 kontrole=SEEKKEY(2,0,0,nrf$):IF kontrole<>0 THEN 470 ELSE GET 2
490 PRINT hel$;:PRINT FNat$(3,30,rec.oms$);zcht$+" OK (J/N)";
500 antw$="JN":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 500
510 IF p$="N"THEN 470 ELSE PRINT FNat$(3,61,eol$)
520 IF nrf$=nk$THEN dok$=tk$ELSE IF nrf$=np$THEN dok$=tp$ELSE IF nrf$=nb1$THEN dok$=tb1$ELSE IF nrf$=nb2$THEN dok$=tb2$ELSE IF nrf$=nb3$THEN dok$=tb3$ELSE dok$=tv$
530 IF nrf$=nk$THEN record=31 ELSE IF nrf$=np$THEN record=32 ELSE IF nrf$=nb1$THEN record=33 ELSE IF nrf$=nb2$THEN record=34 ELSE IF nrf$=nb3$THEN record=35 ELSE record=38
540 saldo=VAL(rec.jaarsaldo$)+VAL(rec.maandsaldo$):dok$=MID$(dok$,1,2)+MID$(STR$(VAL(MID$(dok$,3,3))+1),2,3):PRINT FNat$(0,40,"SALDO : ")hel$;USING"#########";saldo:PRINT FNat$(0,72,dok$);zcht$;
550 bestand$=drf$+MID$(dok$,1,2)+".rnd":bestand2$=drf$+MID$(dok$,1,2)+".key":IF FIND$(bestand$)=""THEN GOSUB 1640 ELSE OPEN"K",1,bestand$,bestand2$,2
560 FIELD 1,1 AS rec.m1$,1 AS rec.m2$,6 AS rec.nrr$,9 AS rec.btld$,20 AS rec.omsch$,9 AS rec.btkrt$,5 AS rec.dok$,8 AS rec.d$
570 nr$=STR$(0)
580 kontrole=SEEKKEY(1,2,0,nr$):IF kontrole<>0 THEN STOP ELSE GET 1
590 n=VAL(rec.nrr$)
600 faktaf=0:d=d+1
610 CLOSE 3:PRINT FNat$(5,15,"......   ");hel$:PRINT FNat$(5,15,"");:INPUT"",nrr$(d):PRINT zcht$;:IF nrr$(d)=""AND d=1 THEN 1630 ELSE IF nrr$(d)=""THEN 1220 ELSE IF LEN(nrr$(d))<>6 THEN 610
620 kontrole=SEEKKEY(2,2,0,nrr$(d)):IF kontrole<>0 THEN 610 ELSE GET 2
630 PRINT hel$;:PRINT FNat$(5,30,rec.oms$);zcht$+" OK (J/N) ?";
640 antw$="JN":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 640
650 IF p$="N"THEN 610 ELSE PRINT FNat$(5,61,eol$);
660 REM
670 dl=dl+1
680 REM
690 REM
700 REM
710 FOR schv=18 TO 20:PRINT FNat$(schv,22,STRING$(16," ")):PRINT FNat$(schv,57,STRING$(22," ")):NEXT schv
720 PRINT FNat$(18,22,"       "):PRINT hel$;:PRINT FNat$(18,22,"");:INPUT"",m1$(d):PRINT zcht$;:m1$(d)=UPPER$(m1$(d)):antw$="OU":IF m1$(d)=""THEN 730 ELSE IF INSTR(antw$,m1$(d))=0 OR LEN(m1$(d))<>1 THEN 720
730 PRINT FNat$(19,22,"       "):PRINT hel$;:PRINT FNat$(19,22,"");:INPUT"",m2$(d):PRINT zcht$;:m2$(d)=UPPER$(m2$(d)):antw$="JN":IF m2$(d)=""THEN 740 ELSE IF INSTR(antw$,m2$(d))=0 OR LEN(m2$(d))<>1 THEN 730
740 IF m1$(d)=""THEN 610 ELSE IF m2$(d)=""THEN 690
750 IF m2$(d)="N"THEN faktaf=1 ELSE 770
760 lt=0:GOTO 960
770 IF m1$(d)="O"AND MID$(nrr$(d),1,3)="400"THEN OPEN"K",3,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2 ELSE IF UPPER$(m1$(d))="U"AND MID$(nrr$(d),1,3)="440"THEN OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2 ELSE 720
780 IF m1$(d)="O"THEN 790 ELSE 800
790 FIELD 3,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datl$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
800 IF m1$(d)="U"THEN 810 ELSE 820
810 FIELD 3,6 AS rec.nrk$,6 AS REC.NR$,8 AS rec.dat$,20 AS rec.o$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
820 PRINT zcht$;:PRINT FNat$(20,22,"....  JAAR : ..  ");hel$;:PRINT FNat$(20,22,"");:INPUT"",far$:far=VAL(far$):IF far=0 THEN 610 ELSE PRINT FNat$(20,35,"");:INPUT"",s2$
830 IF m1$(d)="O"THEN s$="VF"ELSE s$="AF"
840 far$=s2$+s$+STR$(far)
850 kontrole=SEEKKEY(3,2,1,far$):IF kontrole<>0 THEN 820 ELSE GET 3
860 xtra1=VAL(rec.medektr$)+VAL(rec.taks$)+VAL(rec.reserve$):xtra2=VAL(rec.invvhf$)+VAL(rec.sptaks$)-VAL(rec.btwmedektr$)
870 IF m1$(d)="O"THEN ptb=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.nttb$)+VAL(rec.ttb$)+xtra1 ELSE ptb=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+xtra2
880 IF dl=8 THEN dl=1 ELSE 900
890 FOR kls=1 TO 7:PRINT FNat$(kls+8,0,eol$):NEXT kls
900 PRINT hel$;:PRINT FNat$(8+dl,0,rec.dat$)+"  "+rec.nrk$+"                   ";USING"#########";ptb;:PRINT FNat$(8+dl,46,rec.rb$)+" "+rec.bdok$+zcht$+" OK (J/N)?";
910 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 910
920 IF a$="N"THEN PRINT FNat$(dl+8,0,eol$);:GOTO 820 ELSE PRINT FNat$(dl+8,64,eol$)
930 nrk$=rec.nrk$
940 lt=0
950 PRINT FNat$(18,57,"");hel$;:INPUT"",btkrt$:btkrt(d)=VAL(btkrt$):PRINT FNat$(18,57,eol$);USING"########";btkrt(d):IF lt=1 THEN 1030
960 PRINT hel$;:btld(d)=ptb-VAL(rec.rb$)-btkrt(d):PRINT FNat$(19,57,"");USING"########";btld(d);:PRINT zcht$;" OK (J/N)?";
970 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 970
980 IF a$="J"THEN PRINT FNat$(19,66,eol$):GOTO 1000 ELSE 990
990 PRINT FNat$(19,57,eol$);hel$;:INPUT"",btld(d)
1000 IF lt=1 THEN 1030
1010 PRINT zcht$;:PRINT FNat$(20,57,"....................   ");hel$;:PRINT FNat$(20,57,"");:INPUT"",oms$(d):IF LEN(oms$(d))>20 THEN 1010
1020 lt=1
1030 PRINT zcht$;:PRINT FNat$(23,0,"DRUK <2><3><4> OM TE WIJZIGEN - <V> VOOR VERVOLG");eol$;
1040 antw$="V234":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 1040
1050 IF p$="V"THEN 1070
1060 ON VAL(p$)-1 GOTO 950,960,1010
1070 IF faktaf=1 THEN 1170 ELSE rb=VAL(rec.rb$)+btkrt(d)+btld(d)
1080 LSET rec.rb$=STR$(rb):LSET rec.bdok$=dok$
1090 PUT 3:CLOSE 3
1100 IF UPPER$(m1$(d))="O"AND MID$(nrr$(d),1,3)="400"THEN OPEN"K",3,drk$+"klanten.rnd",drk$+"klanten.key",2 ELSE OPEN"K",3,drl$+"leveranc.rnd",drl$+"leveranc.key",2 ELSE 720
1110 IF UPPER$(m1$(d))="O"AND MID$(nrr$(d),1,3)="400"THEN 1120 ELSE 1130
1120 FIELD 3,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
1130 IF UPPER$(m1$(d))="U"AND MID$(nrr$(d),1,3)="440"THEN FIELD 3,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
1140 kontrole=SEEKKEY(3,2,0,nrk$):IF kontrole<>0 THEN STOP ELSE GET 3
1150 sld=VAL(rec.saldo$)-btld(d)-btkrt(d)
1160 LSET rec.saldo$=STR$(sld):PUT 3:CLOSE 3
1170 IF m1$(d)="O"OR m1$(d)="o"THEN saldo=saldo+btld(d)ELSE saldo=saldo-btld(d)
1180 IF m1$(d)="O"OR m1$(d)="o"THEN tsaldo=tsaldo+btld(d)ELSE tsaldo=tsaldo-btld(d)
1190 IF m1$(d)="O"OR m1$(d)="o"THEN btld(d)=-btld(d)-btkrt(d)ELSE btld(d)=btld(d)+btkrt(d)
1200 IF m1$(d)="U"OR m1$(d)="u"THEN btkrt(d)=-btkrt(d)
1210 PRINT FNat$(0,40,"SALDO : ");hel$;USING"#########";saldo:PRINT zcht$;:GOTO 600
1220 D=D-1
1230 FOR t=1 TO d
1240 n=n+1:LSET rec.m1$=m1$(t):LSET rec.m2$=m2$(t):LSET rec.nrr$=nrr$(t):LSET rec.btld$=STR$(btld(t)):LSET rec.omsch$=oms$(t):LSET rec.btkrt$=STR$(btkrt(t)):LSET rec.dok$=dok$:LSET rec.d$=d$
1250 nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$)
1260 NEXT t
1270 nr$=STR$(0):kontrole=SEEKKEY(1,2,0,nr$):GET 1:LSET rec.nrr$=STR$(n)
1280 PUT 1:CLOSE 1
1290 IF FIND$(drj$+"journaal.rnd")=""THEN GOSUB 1580 ELSE OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2
1300 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
1310 nr$=STR$(0):kontrol=SEEKKEY(1,2,0,nr$):GET 1:n=VAL(rec.datum$)
1320 FOR t=1 TO d
1330 LSET rec.num$=nrr$(t):LSET rec.datum$=d$:LSET rec.omschr$=oms$(t):LSET rec.bedrg$=STR$(btld(t)):n=n+1:nr$=STR$(n)
1340 kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,nrr$(t),y)
1350 IF m1$(t)="O"OR m1$(t)="o"THEN rek$=tgbtk$ELSE rek$=bkbtk$
1360 IF btkrt(t)=0 THEN 1400
1370 LSET rec.num$=rek$:LSET rec.bedrg$=STR$(btkrt(t))
1380 n=n+1:nr$=STR$(n)
1390 kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rek$,y)
1400 NEXT t
1410 n=n+1:nr$=STR$(n):LSET rec.num$=nrf$:LSET rec.omschr$=dok$:LSET rec.bedrg$=STR$(tsaldo)
1420 kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,nrf$,y)
1430 nr$=STR$(0):kontrole=SEEKKEY(1,2,0,nr$):GET 1:LSET rec.datum$=STR$(n):PUT 1
1440 CLOSE 1
1450 FOR t=1 TO d
1460 kontrole=SEEKKEY(2,2,0,nrr$(t)):IF kontrole<>0 THEN STOP ELSE GET 2
1470 ms=VAL(rec.maandsaldo$):ms=ms+btld(t)
1480 LSET rec.maandsaldo$=STR$(ms):PUT 2
1490 IF m1$(t)="O"OR m1$(t)="o"THEN rek$=tgbtk$ELSE rek$=bkbtk$
1500 IF btkrt(t)=0 THEN 1530
1510 kontrole=SEEKKEY(2,2,0,rek$):IF kontrole<>0 THEN STOP ELSE GET 2
1520 ms=VAL(rec.maandsaldo$):ms=ms+btkrt(t):LSET rec.maandsaldo$=STR$(ms):PUT 2
1530 NEXT t
1540 kontrole=SEEKKEY(2,2,0,nrf$):IF kontrole<>0 THEN STOP ELSE GET 2
1550 ms=VAL(rec.maandsaldo$):ms=ms+tsaldo:LSET rec.maandsaldo$=STR$(ms):PUT 2:CLOSE 2
1560 OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$:LSET rec.nummer$=dok$:PUT 1,record:CLOSE 1:CLOSE 2:CLOSE 3
1570 CLEAR:RUN
1580 BUFFERS 20:CREATE 1,drj$+"journaal.rnd",drj$+"journaal.key",2,50
1590 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
1600 nr=0:nr$=STR$(nr):LSET rec.datum$=nr$:LSET rec.omschr$="":LSET rec.bedrg$="":LSET rec.num$=""
1610 kontrole=ADDREC(1,2,0,nr$):PUT 1
1620 RETURN
1630 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
1640 BUFFERS 20:CREATE 1,bestand$,bestand2$,2,128
1650 FIELD 1,1 AS rec.m1$,1 AS rec.m2$,6 AS rec.nrr$,9 AS rec.btld$,20 AS rec.omsch$,9 AS rec.btkrt$,5 AS rec.dok$,8 AS rec.d$:n=0:nr$=STR$(n)
1660 LSET rec.nrr$=nr$:LSET rec.m1$="":LSET rec.m2$="":LSET rec.btld$="":LSET rec.omsch$="":LSET rec.btkrt$="":LSET rec.dok$="":LSET rec.d$=""
1670 kontrole=ADDREC(1,2,0,nr$):PUT 1
1680 RETURN
1690 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1700 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1720 ELSE 1710
1710 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1720 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
2