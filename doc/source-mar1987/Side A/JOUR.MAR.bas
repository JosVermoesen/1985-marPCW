1 ON ERROR GOTO 20000
10 REM JOUR.MAR=INVOEREN JOURNAALPOSTEN DD versie 040487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 DIM bedrag(30),nrr$(30)
80 BUFFERS 20
90 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,6:telboek=VAL(rec.nummer$):GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,26:secret$=rec.nummer$
130 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
140 GET 1,48:stt$=rec.m$:GET 1,49:tel$=rec.m$:GET 1,50:hr$=rec.m$:GET 1,51:btwnr$=rec.m$:GET 1,52:banknr$=rec.m$:FIELD 1,28 AS rec.nummer$:GOSUB 680:CLOSE 1
150 lt=INSTR(firm$,"  "):b$=MID$(firm$,1,2)+MID$(firm$,lt-4,4)
160 FOR t=1 TO 6
170 c$=c$+MID$(STR$(ASC(MID$(b$,t,1))),2,1)
180 NEXT t
190 c$=MID$(STR$(VAL(c$)*610225!),2,6)
200 IF c$=secret$THEN 220 ELSE PRINT"ONJUIST SLEUTELNUMMER INGEBRACHT..."
210 RUN"menu
220 IF FIND$(drj$+"journaal.rnd")=""THEN GOSUB 620 ELSE OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2
230 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
240 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2
250 FIELD 2,30 AS rec.oms$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
260 t$="I N V O E R E N    J O U R N A A L P O S T E N"
270 PRINT cls$:PRINT FNat$(0,0,t$):PRINT STRING$(80,CHR$(172))
280 PRINT:PRINT FNat$(3,0,"OMSCHRIJVING :"):PRINT FNat$(22,0,STRING$(80,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR EINDE")
290 PRINT FNat$(5,0,STRING$(80,"-")):PRINT FNat$(6,0,"NUMMER NAAM                                   DEBET    CREDIT     SALDO"):PRINT FNat$(7,0,STRING$(80,"-"))
300 t=1:dl=1:saldo=0
310 PRINT FNat$(16,0,STRING$(80,CHR$(172)));
320 PRINT FNat$(18,0,"1. REKENING NUMMER :"):PRINT FNat$(19,0,"2. DEBET OF CREDIT :"):PRINT FNat$(20,0,"3. BEDRAG          :")
330 PRINT FNat$(3,15,eol$):PRINT FNat$(3,15,"....................   ");hel$;:PRINT FNat$(3,15,"");:INPUT"",oms$:PRINT zcht$;:IF LEN(oms$)=0 THEN 670 ELSE IF LEN(oms$)>20 THEN 330
340 PRINT FNat$(18,21,eol$):PRINT FNat$(18,21,"......   ");eol$;hel$;:PRINT FNat$(18,21,"");:INPUT"",rek$
350 nrr$(t)=rek$:kontrole=SEEKKEY(2,0,0,nrr$(t)):IF kontrole<>0 THEN 340 ELSE GET 2
360 PRINT hel$;:PRINT FNat$(7+dl,0,"");nrr$(t)" "rec.oms$;zcht$;" OK (J/N)?";
370 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 370
380 IF a$="N"THEN PRINT FNat$(7+dl,0,eol$):GOTO 340 ELSE PRINT FNat$(7+dl,38,eol$):GOTO 390
390 PRINT FNat$(19,21,eol$):PRINT FNat$(19,21,"");hel$;:INPUT"",dc$:PRINT zcht$;:dc$=UPPER$(dc$):antw$="DC":IF INSTR(antw$,dc$)=0 OR LEN(dc$)<>1 THEN 390
400 PRINT FNat$(20,21,eol$):PRINT FNat$(20,21,"");hel$;:INPUT"",bedrag(t):IF dc$="C"THEN bedrag(t)=-bedrag(t)
410 saldo=saldo+bedrag(t):IF bedrag(t)<0 THEN c=ABS(bedrag(t))ELSE d=bedrag(t)
420 PRINT hel$;:PRINT FNat$(dl+7,42,"");USING"#########";d:PRINT FNat$(dl+7,52,"");USING"#########";c:PRINT FNat$(dl+7,62,"");USING"#########";saldo:PRINT zcht$;:d=0:c=0
430 PRINT FNat$(23,0,"<V>ERVOLG - <W>EGSCHRIJVEN - <A>NNULEREN");eol$;
440 antw$="VWA":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 440
450 IF a$="A"THEN 270 ELSE IF a$="W"THEN 490 ELSE IF a$="V"THEN 460
460 t=t+1:dl=dl+1:IF dl=8 THEN dl=1 ELSE 340
470 FOR kls=1 TO 8:PRINT FNat$(kls+7,0,eol$):NEXT kls
480 GOTO 340
490 IF saldo<>0 THEN PRINT hel$;:PRINT CHR$(7);:PRINT FNat$(23,0,"DEBET <> CREDIT !!!!");eol$;ELSE 510
500 FOR wacht=1 TO 5000:NEXT wacht:GOTO 430
510 REM vervolg
520 FOR pr=1 TO t
530 kontrole=SEEKKEY(2,0,0,nrr$(pr)):IF kontrole<>0 THEN STOP ELSE GET 2
540 ms=VAL(rec.maandsaldo$):ms=ms+bedrag(pr)
550 LSET rec.maandsaldo$=STR$(ms):PUT 2:NEXT pr
560 n=0:nr$=STR$(n):kontrole=SEEKKEY(1,0,0,nr$):GET 1:n=VAL(rec.datum$)
570 LSET rec.datum$=d$:LSET rec.omschr$=oms$
580 FOR pr=1 TO t:LSET rec.bedrg$=STR$(bedrag(pr)):LSET rec.num$=nrr$(pr):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):PUT 1:y=FETCHREC(1):kontrole=ADDKEY(1,0,1,nrr$(pr),y)
590 NEXT pr
600 nr=0:nr$=STR$(nr):kontrole=SEEKKEY(1,0,0,nr$):GET 1:LSET rec.datum$=STR$(n):PUT 1
610 GOTO 260
620 CREATE 1,drj$+"journaal.rnd",drj$+"journaal.key",2,50
630 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
640 nr=0:nr$=STR$(nr):LSET rec.datum$=nr$:LSET rec.omschr$="":LSET rec.bedrg$="":LSET rec.num$=""
650 kontrole=ADDREC(1,2,0,nr$):PUT 1
660 RETURN
670 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
680 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
690 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 710 ELSE 700
700 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
710 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOS