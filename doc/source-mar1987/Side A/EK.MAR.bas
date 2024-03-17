1 ON ERROR GOTO 20000
10 REM EK.MAR=UITDRUKKEN ETIKETTEN KLANTEN versie 050487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 BUFFERS 40
90 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 470:CLOSE 1
130 IF FIND$(drk$+"klanten.rnd")=""THEN PRINT"KLANTENBESTAND BESTAAT NOG NIET !!!"ELSE 150
140 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
150 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
160 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
170 tp$="UITDRUKKEN ETIKETTEN KLANTEN"
180 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
190 PRINT FNat$(10,12,"GESORTEERD <N>UMMERISCH - <A>LFABETISCH - <P>OSTKODE");
200 antw$="NAP":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 200
210 IF a$="N"THEN kode=0 ELSE IF a$="A"THEN kode=1 ELSE IF a$="P"THEN kode=2
220 PRINT FNat$(14,12,"MIN SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(14,32,"");:INPUT "",mini$:PRINT zcht$;:IF LEN(mini$)>6 THEN 220
230 PRINT FNat$(15,12,"MAX SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(15,32,"");:INPUT "",maxi$:PRINT zcht$;:IF LEN(maxi$)>6 THEN 230
240 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
250 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 250
260 IF at$="J" THEN LPRINT CHR$(15); ELSE LPRINT CHR$(18);
270 kontrole=SEEKRANK(1,2,kode)
280 IF mini$="" THEN 290 ELSE kontrole=SEEKKEY(1,2,kode,mini$)
290 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU : ");
300 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 300
310 IF at$="M"THEN 460
320 LPRINT CHR$(27);"C";CHR$(72);
330 LPRINT CHR$(27);"N";CHR$(0);
340 a$=FETCHKEY$(1)
350 IF a$>maxi$ THEN 400
360 kontrole=SEEKKEY(1,2,kode,a$):IF kontrole<>0 THEN 380 ELSE GET 1
370 LPRINT:LPRINT TAB(22);rec.numkl$:LPRINT:LPRINT TAB(2);rec.naam$:LPRINT TAB(2);rec.straat$:LPRINT:LPRINT TAB(2);rec.postkode$+MID$(rec.stad$,1,25):LPRINT:LPRINT
380 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 400
390 IF kontrole=0 THEN 370 ELSE 340
400 REM
410 REM
420 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
430 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 430
440 CLOSE 1:CLOSE 2
450 IF at$="H"THEN 80
460 RUN"menu.mar"
470 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
480 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 500 ELSE 490
490 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
500 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF