1 ON ERROR GOTO 20000
10 REM ITA.MAR=INITIALISATIE TELLERS ARTIKELEN DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 BUFFERS 6
90 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"r",1,drive$+"99.rnd",30
110 FIELD 1,28 AS rec.nummer$
120 GOSUB 290:CLOSE 1
130 OPEN"K",1,dra$+"artikels.rnd",dra$+"artikels.key",2
140 FIELD 1,25 AS rec.om$,9 AS rec.kostprijs$,2 AS rec.btw$,9 AS rec.jaaromzet$,9 AS rec.maandomzet$,9 AS rec.stock$,9 AS rec.minstock$
150 PRINT cls$
160 PRINT FNat$(0,0,"MAANDOMZET ARTIKELS OP NUL PLAATSEN")
170 PRINT STRING$(80,CHR$(172))
180 PRINT FNat$(3,5,"MAANDOMZET ARTIKELS UITVEGEN ? (J/N) ?");
190 antw$="JN":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 190
200 IF p$="N"THEN 280
210 PRINT hel$;:kontrole=SEEKRANK(1,0,0)
220 a$=FETCHKEY$(1):kontrole=SEEKKEY(1,0,0,a$):IF kontrole=103 THEN 280 ELSE IF kontrole<>0 THEN STOP ELSE GET 1
230 LSET rec.maandomzet$=STR$(0):PUT 1
240 t=t+1
250 PRINT FNat$(12,5,a$);"   ";t
260 kontrole=SEEKNEXT(1,0):IF kontrole<>101 THEN 280
270 GOTO 220
280 PRINT zcht$;:CLOSE 1:CLOSE 2:RUN"menu.mar"
290 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
300 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 320 ELSE 310
310 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
320 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:C