1 ON ERROR GOTO 20000
5 CHAIN MERGE"vfakt.txt",20,ALL
10 REM VFAKT.MAR=INVOEREN VERKOOPFAKTUREN DD versie 130787
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
66 IF anr<>0 THEN 370
90 OPTION RUN
100 DIM btkode(60),om$(60),kp(60),ant(60),btw(60),nr$(60),tekst$(5),taks(60)
110 PRINT cls$:PRINT FNat$(12,12,"INVOER FAKTUUR/CREDITNOTA (F/C) ?");
120 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$=""THEN 120
130 IF m$="F"THEN r=11 ELSE r=13
140 BUFFERS 20
145 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
150 OPEN"R",1,drive$+"99.rnd",30
160 FIELD 1,6 AS rec.nr$
170 GET 1,r:anr=VAL(rec.nr$):GET 1,20:d9$=rec.nr$:d$=MID$(rec.nr$,5,2)+"/"+MID$(rec.nr$,3,2)+"/"+MID$(rec.nr$,1,2):GET 1,18:btv$=rec.nr$:GET 1,19:btc$=rec.nr$:GET 1,9:ark$=rec.nr$:GET 1,37:boekj$=rec.nr$
180 GET 1,21:rv6$=rec.nr$:GET 1,22:rv19$=rec.nr$:GET 1,23:rv25$=rec.nr$:GET 1,24:rv33$=rec.nr$:GET 1,25:rv0$=rec.nr$:GET 1,26:secret$=rec.nr$
190 GET 1,29:vkpmktr$=rec.nr$:GET 1,30:taksen$=rec.nr$:GET 1,40:creditnr$=rec.nr$:GET 1,36:rv00$=rec.nr$
200 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
210 GET 1,48:stt$=rec.m$:GET 1,49:tel$=rec.m$:GET 1,50:hr$=rec.m$:GET 1,51:btwnr$=rec.m$:GET 1,52:banknr$=rec.m$:FIELD 1,28 AS rec.nummer$:GOSUB 3150:CLOSE 1
215 IF bl=72 THEN lnbl=30 ELSE lnbl=24
220 lt=INSTR(firm$,"  "):b$=MID$(firm$,1,2)+MID$(firm$,lt-4,4)
230 FOR t=1 TO 6
240 c$=c$+MID$(STR$(ASC(MID$(b$,t,1))),2,1)
250 NEXT t
260 c$=MID$(STR$(VAL(c$)*610225!),2,6)
270 IF c$=secret$THEN 310
280 IF anr<6 THEN 310
290 PRINT"ONJUIST SLEUTELNUMMER INGEBRACHT..."
300 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
310 recleng1=162:OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2,recleng1
320 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
330 rlen=4
340 OPEN"R",3,drb$+"btw.rnd",rlen:FIELD 3,2 AS rec.ct$:GET 3,1:pct(1)=VAL(rec.ct$):GET 3,2:pct(2)=VAL(rec.ct$):GET 3,3:pct(3)=VAL(rec.ct$):GET 3,4:pct(4)=VAL(rec.ct$):GET 3,5:pct(5)=VAL(rec.ct$):GET 3,6:pct(6)=VAL(rec.ct$):CLOSE 3
350 OPEN"K",2,dra$+"artikels.rnd",dra$+"artikels.key",2:REM OPEN"O",3,"B:vfaktuur.rnd"
360 FIELD 2,25 AS rec.om$,9 AS rec.kostprijs$,2 AS rec.btw$,9 AS rec.jaaromzet2$,9 AS rec.maandomzet2$,9 AS rec.stock$,9 AS rec.minstock$
370 anr=anr+1:IF r=11 THEN t$="VERKOOPFAKTUREN"ELSE t$="CREDITNOTA'S OP VERKOOPFAKTUREN"
380 tb00=0:nnttb=0:bt00=0:tkvk=0:bt6=0:bt19=0:bt25=0:bt33=0:bt0=0:nttb=0:tb6=0:tb19=0:tb25=0:tb33=0:tb0=0:ttb=0:tekst=0:ttaksbtw=0:temp=0
390 FOR pr=1 TO 60:kp(pr)=0:btkode(pr)=0:ant(pr)=0:taks(pr)=0:NEXT pr
400 PRINT cls$:PRINT FNat$(0,0,"");hel$;t$:PRINT FNat$(0,72,"");zcht$;"NR:";hel$;USING"#####";anr:PRINT zcht$;:PRINT FNat$(1,0,STRING$(80,172))
410 PRINT FNat$(3,0,"NR KLANT : 400"):PRINT FNat$(22,0,STRING$(80,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR MENU")
420 PRINT FNat$(6,0,"NR ARTIKEL    OMSCHRIJVING                  PRIJS/STUK      AANTAL      TOTAAL"):PRINT STRING$(80,"-")
430 PRINT FNat$(16,0,STRING$(80,172))
440 PRINT FNat$(18,0,"1. ARTIKEL NUMMER  :"):PRINT FNat$(19,0,"2. BTW KODE (0-6)  :"):PRINT FNat$(20,0,"3. KORTING %-AGE   :"):PRINT FNat$(18,39,"4. MEDEKONTRAKTANT :      (J/N)")
450 PRINT FNat$(19,39,"6. VERVALDAG       :")
460 PRINT FNat$(3,14,"......   ");hel$;:PRINT FNat$(3,14,"");:INPUT"",nrk$:PRINT zcht$;:IF nrk$=""THEN 1160
470 kontrole=SEEKKEY(1,2,0,nrk$):IF kontrole<>0 THEN 460 ELSE GET 1
480 PRINT hel$;:PRINT FNat$(3,23,rec.naam$):PRINT FNat$(4,23,rec.stad$);zcht$;" OK (J/N) ? ";
490 naam$=rec.naam$
500 antw$="JN":invoer$=UPPER$(INKEY$):IF INSTR(antw$,invoer$)=0 OR invoer$=""THEN 500
510 IF invoer$="N"THEN 460 ELSE PRINT FNat$(4,54,eol$)
520 t=1:dl=1
530 PRINT FNat$(18,21,"..........   ");hel$;:PRINT FNat$(18,21,"");:INPUT"",oms$
540 IF tekst>0 THEN 550 ELSE IF UPPER$(oms$)="OMS"THEN 560 ELSE IF UPPER$(oms$)="TEKST"THEN 3020 ELSE 730
550 IF UPPER$(oms$)="TEKST"THEN 3020 ELSE 850
560 nr$(t)="OMSCHRIJV.":PRINT FNat$(7+dl,0,"");USING"##";t;:PRINT" ";nr$(t);:PRINT FNat$(7+dl,14,"");:INPUT"",om$(t):PRINT FNat$(7+dl,44,"");:INPUT"",kp(t):PRINT FNat$(7+dl,44,"");USING"#######.##";kp(t)
570 PRINT FNat$(7+dl,60,"");:INPUT"",ant(t):PRINT FNat$(7+dl,69,"");USING"#########";ant(t)*kp(t)
580 PRINT zcht$;:PRINT FNat$(19,21,"    ")
590 PRINT FNat$(19,21,"");hel$;:INPUT"",btw$:btkode(t)=VAL(btw$):IF INSTR("0123456",btw$)=0 OR LEN(btw$)<>1 THEN 580
600 IF btkode(t)=0 THEN ber$="UITVOER (J/N)?"ELSE ber$=STR$(pct(btkode(t)))+"% (J/N)?"
610 PRINT zcht$;:PRINT FNat$(19,24,ber$);:PRINT hel$;
620 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 620
630 IF a$="N"THEN PRINT FNat$(19,24,STRING$(14," ")):GOTO 580 ELSE PRINT FNat$(19,24,STRING$(14," "))
640 IF btkode(t)=5 THEN taks(t)=pct(5)-pct(4)
650 IF btkode(t)=5 THEN btw(t)=pct(4)ELSE btw(t)=pct(btkode(t))
660 IF rfl=1 THEN RETURN
670 GOTO 720
680 IF r<>11 THEN 700
690 mvk=mvk+ant(t):jvk=jvk+ant(t):st=st-ant(t):GOTO 710
700 mvk=mvk-ant(t):jvk=jvk-ant(t):st=st+ant(t)
710 LSET rec.maandomzet2$=STR$(mvk):LSET rec.jaaromzet2$=STR$(jvk):LSET rec.stock$=STR$(st):PUT 2
720 GOTO 850
730 nr$(t)=oms$:kontrole=SEEKKEY(2,2,0,nr$(t)):IF kontrole<>0 THEN 530 ELSE GET 2
740 om$(t)=rec.om$:kp(t)=VAL(rec.kostprijs$):btkode(t)=VAL(rec.btw$):mvk=VAL(rec.maandomzet2$):jvk=VAL(rec.jaaromzet2$):st=VAL(rec.stock$)
750 PRINT hel$;:PRINT FNat$(7+dl,0,"");USING"##";t;:PRINT" ";nr$(t);:PRINT FNat$(7+dl,14,om$(t));zcht$;" OK (J/N)?";
760 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 760
770 IF a$="N"THEN PRINT FNat$(dl+7,0,eol$):GOTO 530 ELSE PRINT FNat$(dl+7,40,eol$)
780 PRINT hel$;:PRINT FNat$(7+dl,44,"");USING"#######.##";kp(t);:PRINT zcht$;:PRINT" OK?";
790 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 790
800 IF at$="J"THEN PRINT FNat$(7+dl,54,eol$):GOTO 820
810 PRINT hel$;:PRINT FNat$(7+dl,44,eol$);:INPUT"",kp(t):GOTO 780
820 PRINT hel$;:PRINT FNat$(7+dl,60,"");:INPUT"",ant(t):PRINT FNat$(dl+7,69,"");USING"#########";ant(t)*kp(t)
830 PRINT FNat$(19,21,"");btkode(t):rfl=1:GOSUB 600
840 GOTO 680
850 rfl=0:PRINT zcht$;:PRINT FNat$(23,0,"<V>ERVOLG - <D>IREKT DRUKKEN - <A>NNULEREN");eol$;
860 antw$="LVDA":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 860
870 IF a$="A"THEN 880 ELSE IF a$="L"THEN 860 ELSE IF a$="V"AND t>50 THEN 860 ELSE IF a$="D"THEN 1000 ELSE IF a$="V"AND t<50 THEN 965
880 FOR pr=1 TO t-tekst:IF nr$(pr)="OMSCHRIJV."THEN 960
890 kontrole=SEEKKEY(2,2,0,nr$(pr)):GET 2
900 mvk=VAL(rec.maandomzet2$):jvk=VAL(rec.jaaromzet2$):st=VAL(rec.stock$)
910 IF r<>11 THEN 930
920 mvk=mvk-ant(pr):jvk=jvk-ant(pr):st=st+ant(pr):GOTO 940
930 mvk=mvk+ant(pr):jvk=jvk+ant(pr):st=st-ant(pr)
940 LSET rec.maandomzet2$=STR$(mvk):LSET rec.jaaromzet2$=STR$(jvk):LSET rec.stock$=STR$(st)
950 PUT 2
960 NEXT pr:GOTO 380
965 IF tekst>4 THEN 850
970 t=t+1:dl=dl+1:IF dl=9 THEN dl=1 ELSE 530
980 FOR kls=1 TO 8:PRINT FNat$(kls+7,0,eol$):NEXT kls
990 GOTO 530
1000 PRINT hel$;:PRINT FNat$(20,21,"");:INPUT"",krt$:krt=VAL(krt$):IF krt>100 THEN 1000
1010 PRINT FNat$(18,60,"");:INPUT"",mktr$:mktr$=UPPER$(mktr$):IF INSTR("JN",mktr$)=0 OR mktr$=""THEN 1010
1020 dat$=d$
1030 dd$=STR$((VAL(MID$(d$,1,2))+VAL(rec.vervaldag$))MOD 30)
1040 mm$=STR$(VAL(MID$(d$,4,2))+INT((VAL(MID$(d$,1,2))+VAL(rec.vervaldag$))/30))
1050 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
1060 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
1070 dat2$=dd$+"/"+mm$+"/"+MID$(d$,7,2)
1080 PRINT FNat$(19,60,dat2$):PRINT FNat$(19,60,"");:INPUT"",dat1$:IF dat1$=""THEN dat1$=dat2$ELSE IF LEN(dat1$)<>8 THEN 1020
1090 IF a$="D"THEN 1170
1100 IF r=11 THEN s$="VF"ELSE s$="CV"
1110 REM anr$=s$+STR$(anr):WRITE #3,anr$,d$,rec.naam$,rec.straat$,rec.postkode$,rec.stad$,rec.btwnr$,t
1120 REM FOR pr=1 TO 1:WRITE #3,nr$(pr),om$(pr),kp(pr),ant(pr),btw(pr):NEXT pr
1130 REM WRITE #3,krt,prt,nrk$,dat1$
1140 d$=dat$
1150 GOTO 370
1160 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
1170 PRINT zcht$;:PRINT FNat$(23,0,"PRINTER KLAAR - PAPIER TOP OF FORM ? DRUK <RETURN> VERVOLG");eol$;
1180 a$=INKEY$:IF a$=""THEN 1180 ELSE IF ASC(a$)=13 THEN 1190 ELSE 1180
1190 IF r=11 THEN tp$="*** FAKTUUR ***"ELSE tp$="** CREDITNOTA **"
1740 PRINT FNat$(23,0,"FAKTUUR/CREDITNOTA HERDRUKKEN <J/N> - OF ANNULEREN <A>");eol$;
1750 antw$="JNA":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 1750
1760 IF a$="A"THEN 880 ELSE IF a$="N"THEN 1780
1770 ttaksbtw=0:tkvk=0:nttb=0:ttb=0:tb6=0:tb19=0:tb25=0:tb33=0:tb0=0:tbtw=0:vv=0:bt6=0:bt19=0:bt25=0:bt33=0:bt0=0:tb00=0:bt00=0:nnttb=0:GOTO 1200
1780 CHAIN"vfakt2.mar",,ALL
3000 REM
3010 REM
3020 REM
3030 tekst=tekst+1
3040 PRINT FNat$(dl+7,5,"");:LINE INPUT"",tekst$(tekst)
3050 GOTO 850
3150 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
3160 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 3180 ELSE 3170
3170 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
3180 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020