1 ON ERROR GOTO 20000
10 REM ITFD.MAR=INVOEREN TELLERS FINANCIELE VERRICHTINGEN DD
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
130 GET 1,1:afnr=VAL(rec.nummer$):GET 1,11:vfnr=VAL(rec.nummer$):GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,27:bkbtk$=rec.nummer$:GET 1,28:tgbtk$=rec.nummer$
140 GET 1,31:tk$=rec.nummer$:GET 1,32:tp$=rec.nummer$:GET 1,33:tb1$=rec.nummer$:GET 1,34:tb2$=rec.nummer$:GET 1,35:tb3$=rec.nummer$:GET 1,41:nk$=rec.nummer$:GET 1,42:np$=rec.nummer$:GET 1,38:tv$=rec.nummer$
150 GET 1,43:nb1$=rec.nummer$:GET 1,44:nb2$=rec.nummer$:GET 1,45:nb3$=rec.nummer$:FIELD 1,28 AS rec.nummer$:GOSUB 450:FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$:GET 1,39:nv$=rec.nummer$
160 FIELD 1,6 AS rec.nummer$
170 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2
180 FIELD 2,30 AS rec.oms$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
190 kontrole=SEEKKEY(2,2,0,nk$):GET 2:tk$=MID$(rec.oms$,1,2)+MID$(tk$,3,3):LSET rec.nummer$=tk$:PUT 1,31
200 kontrole=SEEKKEY(2,2,0,np$):GET 2:tp$=MID$(rec.oms$,1,2)+MID$(tp$,3,3):LSET rec.nummer$=tp$:PUT 1,32
210 kontrole=SEEKKEY(2,2,0,nb1$):GET 2:tb1$=MID$(rec.oms$,1,2)+MID$(tb1$,3,3):LSET rec.nummer$=tb1$:PUT 1,33
220 kontrole=SEEKKEY(2,2,0,nb2$):GET 2:tb2$=MID$(rec.oms$,1,2)+MID$(tb2$,3,3):LSET rec.nummer$=tb2$:PUT 1,34
230 kontrole=SEEKKEY(2,2,0,nb3$):GET 2:tb3$=MID$(rec.oms$,1,2)+MID$(tb3$,3,3):LSET rec.nummer$=tb3$:PUT 1,35
240 kontrole=SEEKKEY(2,2,0,nv$):GET 2:tv$=MID$(rec.oms$,1,2)+MID$(tv$,3,3):LSET rec.nummer$=tv$:PUT 1,38
250 t$="INBRENGEN TELLERS FINANCIELE VERRICHTINGEN"
260 PRINT cls$:PRINT FNat$(0,0,t$):PRINT FNat$(1,0,STRING$(80,172))
270 PRINT FNat$(3,0,"NR FINANCIELE REKENING :"):PRINT FNat$(22,0,STRING$(80,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR EINDE");
280 PRINT FNat$(3,25,"......   ");hel$;:PRINT FNat$(3,25,"");:INPUT"",nrf$:PRINT zcht$;:IF LEN(nrf$)=0 THEN 440 ELSE IF LEN(nrf$)<>6 THEN 280
290 kontrole=SEEKKEY(2,0,0,nrf$):IF kontrole<>0 THEN 280 ELSE GET 2
300 PRINT hel$;:PRINT FNat$(3,35,rec.oms$);zcht$+" OK (J/N)";
310 antw$="JN":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 310
320 IF p$="N"THEN 280
330 IF nrf$=nk$THEN dok$=tk$ELSE IF nrf$=np$THEN dok$=tp$ELSE IF nrf$=nb1$THEN dok$=tb1$ELSE IF nrf$=nb2$THEN dok$=tb2$ELSE IF nrf$=nb3$THEN dok$=tb3$ELSE dok$=tv$
340 IF nrf$=nk$THEN record=31 ELSE IF nrf$=np$THEN record=32 ELSE IF nrf$=nb1$THEN record=33 ELSE IF nrf$=nb2$THEN record=34 ELSE IF nrf$=nb3$THEN record=35 ELSE record=38
350 saldo=VAL(rec.jaarsaldo$)+VAL(rec.maandsaldo$):PRINT FNat$(0,45,"SALDO : ");hel$;eol$;USING"#########";saldo:PRINT FNat$(0,72,dok$)
360 PRINT zcht$;:PRINT FNat$(12,5,"NUMMER VAN HET LAATST VERWERKTE DOKUMENT : ");hel$;:INPUT"",nummer:IF nummer>999 OR nummer<0 THEN 360
370 dok$=MID$(dok$,1,2)+MID$(STR$(nummer),2,3)
380 PRINT zcht$;:PRINT FNat$(0,45,"SALDO : ");hel$;eol$;USING"#########";saldo;:PRINT FNat$(0,72,dok$);
390 PRINT zcht$;:PRINT FNat$(23,0,"DRUK <A> VOOR ANNULEREN - <W> VOOR WEGSCHRIJVEN");eol$
400 antw$="AW":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 400
410 IF p$="A"THEN 440
420 LSET rec.nummer$=dok$
430 PUT 1,record
440 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
450 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
460 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 480 ELSE 470
470 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
480 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF