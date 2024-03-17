1 ON ERROR GOTO 20000
10 REM VSOFT.MAR=STARTPROGRAMMA (c)VSOFT versie 030887
15 LPRINT CHR$(27);"c";
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hi$=CHR$(27)+CHR$(112):lo$=CHR$(27)+CHR$(113)
70 OPTION FILES "m":IF FIND$("dd.vsf")="" THEN pcvers$="H" ELSE pcvers$="D"
80 IF pcvers$="D" THEN 390
90 PRINT cls$
100 sub$="c:/mardata/":a$=FINDDIR$(sub$+"x01/")
110 IF LEN(a$)=0 THEN GOTO 180 ELSE 120
120 i=0:bst$(0)=" ":WHILE bst$(i)<>"":i=i+1:bst$(i)=FIND$(sub$+"*.vsf",i):WEND
130 IF i=1 THEN 140 ELSE 180
140 PRINT "er werd nog geen label geplakt op boekhouding : X01"
150 PRINT "label (max. 30 tekens) : ";:INPUT "",label$
160 OPEN "O",1,sub$+"x01.vsf",2:WRITE #1,label$:CLOSE 1
170 GOTO 120
180 PRINT cls$:PRINT FNat$(0,5,"BEDRIJF SELECTEREN OP HARD DISK"):PRINT STRING$(79,CHR$(172)):PRINT FNat$(22,0,STRING$(79,CHR$(172)))
190 FOR t=1 TO i-1
200 OPEN "i",1,sub$+bst$(t),2:INPUT #1,label$(t):CLOSE 1
210 z$=MID$(bst$(t),1,3)+" "+label$(t)+" "+STRING$(30-LEN(label$(t)),"."):PRINT FNat$(t+4,5,z$);" ";hi$;STR$(t);lo$;
220 NEXT t
230 PRINT FNat$(23,0,"MAAK UW KEUZE OF <N>IEUW BEDRIJF ? ");lo$;
240 antw$="12345678N":antwoord$=INKEY$:IF INSTR(antw$,UPPER$(antwoord$))=0 OR antwoord$="" THEN 240
250 IF UPPER$(antwoord$)="N" THEN 310
260 IF VAL(antwoord$)>i-1 THEN 230
270 antwoord=VAL(antwoord$):drive$=sub$+MID$(bst$(antwoord),1,3)+"/"
280 PRINT lo$;
290 OPEN "O",1,"version.vsf",2:WRITE #1,drive$:CLOSE 1
300 GOTO 400
310 IF i=0 THEN i=1
320 PRINT FNat$(23,0,eol$):best$="x0"+MID$(STR$(i),2,1):PRINT lo$;:PRINT FNat$(23,5,best$);" : LABEL : ";hi$;:INPUT "",label$(t)
330 PRINT lo$;:antwoord=t:label$(t)=MID$(label$(t),1,30)
340 drive$=sub$+best$+"/"
350 OPEN "O",1,"version.vsf",2:WRITE #1,drive$:CLOSE 1
360 OPEN "O",1,sub$+best$+".vsf",2:WRITE #1,label$(t):CLOSE 1
370 CHDIR("/"):CHDIR("mardata"):MKDIR(best$):CHDIR("/boekhoud")
380 GOTO 400
390 OPEN "O",1,"version.vsf",2:WRITE #1,"a:":CLOSE 1:drive$="a:"
400 REM samenkomst na opstart dd en hd
410 cls$=CHR$(27)+"E":DEF FNat$(k,l)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)
420 REM flash$=CHR$(27)+"[5m":flashoff$=CHR$(27)+"[0m"
430 REM eol$=CHR$(27)+"[K"
440 REM
450 REM hi$=CHR$(27)+"[1m":lo$=CHR$(27)+"[0m"
460 OPTION FILES"m":REM OPTION RUN
470 PRINT curoff$:PRINT CLS$:GOSUB 1040
480 PRINT FNat$(6,12)CHR$(150);STRING$(67,154);CHR$(156)
490 IF LEN(drive$)=2 THEN 500 ELSE 520
500 PRINT FNat$(6,13)CHR$(149);" VERWIJDER DE STARTDISKETTE EN BRENG DE               IN DE DRIVES ";CHR$(149)
510 PRINT FNat$(47,13);hi$;"DATADISKETTES";lo$:GOTO 530
520 PRINT FNat$(6,13)CHR$(149);"     BOEKHOUDING VOOR BEDRIJF : ";hi$;label$(antwoord);lo$;STRING$(30-LEN(label$(antwoord))," ");"     ";CHR$(149)
530 PRINT FNat$(6,14)CHR$(149);:PRINT FNat$(29,14)"Druk < > voor VERVOLG";:PRINT FNat$(74,14)CHR$(149)
540 PRINT FNat$(35,14);hi$;"V";lo$
550 PRINT FNat$(6,15)CHR$(147);STRING$(67,154);CHR$(153):PRINT FNat$(35,14);
560 WHILE UPPER$(INKEY$)<>"V":WEND
570 IF FIND$(drive$+"99.RND")=""THEN PRINT FNat$(10,18);"DE INGEBRACHTE DISKETTES BEVATTEN";hi$;" GEEN";lo$;" INITIALISATIE-BESTAND":PRINT FNat$(19,20)"INITIALISATIEPROCEDURE WORDT OPGESTART S VOOR START E VOOR EINDE";curon$;:ELSE 600
580 antw$="SE":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 580
590 IF at$="S"THEN 1160 ELSE IF at$="E"THEN PRINT cls$;:SYSTEM: REM  curoff$;:PRINT FNat$(10,18);eol$:PRINT FNat$(19,20);eol$:GOTO 560
600 OPEN"R",1,drive$+"99.RND",30:FIELD 1,6 AS bj$:GET 1,5:CLOSE 1
610 IF LEN(drive$)>2 THEN 630
620 PRINT FNat$(47,13)"DATADISKETTES"
630 PRINT FNat$(35,14)"V"
640 PRINT FNat$(26,17)CHR$(150);STRING$(26,154);CHR$(156)
650 PRINT FNat$(26,18)CHR$(149);"  DATUM  :";:PRINT FNat$(53,18)CHR$(149)
660 PRINT FNat$(26,19)CHR$(147);STRING$(26,154);CHR$(153)
670 z=39:PRINT FNat$(z,18);lo$;".. / .. / ..":PRINT FNat$(z,18);hi$;curon$;
680 d$=""
690 FOR x=1 TO 3
700 FOR y=1 TO 2
710 a$=INKEY$:IF a$=""OR a$>"9"OR a$<"0"THEN 710 ELSE z=z+1:d$=d$+a$:PRINT a$;:NEXT y
720 z=z+3:PRINT FNat$(z,18);:NEXT x
730 dag$=MID$(d$,1,2):maand$=MID$(d$,3,2):jaar$=MID$(d$,5,2)
740 IF VAL(dag$)<1 OR VAL(dag$)>31 THEN 670 ELSE IF VAL(maand$)<1 OR VAL(maand$)>12 THEN 670 ELSE IF VAL(jaar$)<>VAL(bj$)THEN 670
750 PRINT lo$:PRINT curoff$
760 PRINT FNat$(26,21)CHR$(150);STRING$(26,154);CHR$(156)
770 PRINT FNat$(26,22)CHR$(149);"  SLEUTELNUMMER : ";:PRINT FNat$(53,22)CHR$(149)
780 PRINT FNat$(26,23)CHR$(147);STRING$(26,154);CHR$(153)
790 beurt%=0
800 z=45:sleutel$="":c$="":firm$=""
810 PRINT FNat$(z,22);lo$;"......"
820 FOR x=1 TO 6
830 PRINT FNat$(z,22);curon$;hi$;
840 a$=INKEY$:IF a$=""OR a$<"0"OR a$>"9"THEN 840 ELSE z=z+1:sleutel$=sleutel$+a$:PRINT a$;:NEXT x
850 beurt%=beurt%+1:PRINT curoff$
860 OPEN"R",1,drive$+"99.RND",30
870 FIELD 1,28 AS firm$:GET 1,46
880 firm$=firm$+STRING$(28-LEN(firm$),"  ")
890 lt=INSTR(firm$,"  "):b$=MID$(firm$,1,2)+MID$(firm$,lt-4,4)
900 FOR t=1 TO 6
910 c$=c$+MID$(STR$(ASC(MID$(b$,t,1))),2,1)
920 NEXT t
930 c$=MID$(STR$(VAL(c$)*610225!),2,6):PRINT c$
940 IF beurt%=3 AND c$<>sleutel$THEN CLOSE 1:PRINT CLS$;flash$:GOSUB 1010:SYSTEM
950 IF c$<>sleutel$THEN CLOSE 1:GOTO 800 ELSE PRINT lo$
960 FIELD 1,6 AS rec.nummer$
970 LSET rec.nummer$=jaar$+maand$+dag$:PUT 1,20
980 LSET rec.nummer$=sleutel$:PUT 1,26
990 CLOSE 1
1000 RUN"MENU.MAR"
1010 PRINT FNAT$(4,10)"OFWEL HEBT U STEEDS EEN VERKEERDE SLEUTELNUMMER INGEBRACHT EN DAN VRA-          GEN WIJ U VRIENDELIJK ZO SNEL MOGELIJK MET ONS KONTAKT OP TE NEMEN..."
1020 PRINT FNat$(4,14)"OFWEL SCHENDT U HET AUTEURSRECHT OP DIT PROGRAMMA ... EN DAN LAAT U UW BOEK-    HOUDING BEST AFHANDELEN DOOR EEN ERKEND BOEKHOUDER ..."
1030 RETURN
1040 PRINT FNat$(13,1);CHR$(150);STRING$(52,154);CHR$(156)
1050 b$=CHR$(27)+CHR$(112)+" "+CHR$(27)+CHR$(113):PRINT SPACE$(13)+CHR$(149)+"       "+b$+b$+"  "+b$+b$+"   "+b$+b$+b$+b$+"    "+b$+b$+b$+"   "+b$+b$+b$+b$+b$+b$+b$+"  "+b$+b$+b$+b$+b$+b$+"       "+CHR$(149)
1060 PRINT FNat$(0,2);SPACE$(13)+CHR$(149)+"       "+b$+b$+"  "+b$+b$+"  "+b$+b$+"  "+b$+b$+"  "+b$+b$+" "+b$+b$+"   "+b$+b$+"   "+b$+"  "+b$+" "+b$+b$+" "+b$+"       "+CHR$(149)
1070 PRINT FNat$(0,3);SPACE$(13)+CHR$(149)+"       "+b$+b$+"  "+b$+b$+"  "+b$+b$+"     "+b$+b$+"   "+b$+b$+"  "+b$+b$+" "+b$+"      "+b$+b$+"         "+CHR$(149)
1080 PRINT FNat$(0,4);SPACE$(13)+CHR$(149)+"       "+b$+b$+"  "+b$+b$+"   "+b$+b$+b$+b$+"  "+b$+b$+"   "+b$+b$+"  "+b$+b$+b$+b$+"      "+b$+b$+"         "+CHR$(149)
1090 PRINT FNat$(0,5);SPACE$(13)+CHR$(149)+"       "+b$+b$+"  "+b$+b$+"      "+b$+b$+" "+b$+b$+"   "+b$+b$+"  "+b$+b$+" "+b$+"      "+b$+b$+"         "+CHR$(149)
1100 PRINT FNat$(0,6);SPACE$(13)+CHR$(149)+"        "+b$+b$+b$+b$+"   "+b$+b$+"  "+b$+b$+"  "+b$+b$+" "+b$+b$+"   "+b$+b$+"        "+b$+b$+"         "+CHR$(149)
1110 PRINT FNat$(0,7);SPACE$(13)+CHR$(149)+"         "+b$+b$+"     "+b$+b$+b$+b$+"    "+b$+b$+b$+"   "+b$+b$+b$+b$+"      "+b$+b$+b$+b$+"        "+CHR$(149)
1120 PRINT FNat$(13,9)CHR$(149);:PRINT FNat$(21,9);CHR$(150);STRING$(36,154);CHR$(156);:PRINT FNat$(66,9)CHR$(149)
1130 PRINT FNat$(13,10)CHR$(147);STRING$(7,154);CHR$(157);hi$;"  M.A.R.  BELGISCH  BOEKHOUDPAKKET  ";lo$;CHR$(151);STRING$(7,154);CHR$(153)
1140 PRINT FNat$(21,11);CHR$(147);STRING$(36,154);CHR$(153)
1150 RETURN
1160 titel$="I N I T .   H O O F D R E K E N I N G E N   &   T E L L E R S":GOSUB 1930
1170 IF LEN(drive$)>2 THEN 1220
1180 PRINT curoff$:drk$="a:":drl$="a:":drr$="a:":dra$="b:":drb$="a:":draa$="b:":drv$="b:":drj$="b:":drf$="b:":drt$="a:"
1190 REM
1200 REM
1210 REM
1220 BUFFERS 6
1230 IF LEN(drive$)=2 THEN CREATE 2,drr$+"rekening.rnd",drr$+"rekening.key",2,60 ELSE CREATE 2,drive$+"rekening.rnd",drive$+"rekening.key",2,60
1240 IF LEN(drive$)=2 THEN OPEN"R",1,drt$+"99.rnd",30 ELSE OPEN "R",1,drive$+"99.rnd",30
1250 FIELD 1,6 AS rec.nummer$
1260 FIELD 2,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
1270 nummer$="0":LSET rec.nummer$=nummer$
1280 FOR t=1 TO 6:PUT 1,t:NEXT t
1290 FOR t=7 TO 10:GOSUB 1650:NEXT t
1300 LSET rec.nummer$=nummer$:FOR t=11 TO 15:PUT 1,t:NEXT t
1310 FOR t=16 TO 19:GOSUB 1650:NEXT t
1320 FOR t=21 TO 25:GOSUB 1650:NEXT t
1330 LSET rec.nummer$="":PUT 1,26:LSET rec.nummer$=nummer$
1340 FOR t=27 TO 30:GOSUB 1650:NEXT t
1350 LSET rec.nummer$=nummer$:FOR t=31 TO 38:PUT 1,t:NEXT t
1360 t=36:GOSUB 1650
1370 FOR t=39 TO 40:GOSUB 1650:NEXT t
1380 FOR t=41 TO 45:GOSUB 1650:NEXT t
1390 fl=0
1400 FIELD 1,6 AS rec.nummer$:PRINT FNat$(20,6)"0. STARTBOEKJAAR : ";eol$;hi$;curon$;:INPUT"19",bj$:PRINT curoff$;lo$;:LSET rec.nummer$=bj$:PUT 1,5:IF LEN(bj$)<>2 THEN 1400 ELSE IF VAL(bj$)<86 OR VAL(bj$)>90 THEN 1400
1410 IF fl=1 THEN 1570
1420 FIELD 1,28 AS rec.nummer$:PRINT FNat$(20,8)"1. NAAM BEDRIJF : ";eol$;hi$;curon$;:INPUT"",n$:LSET rec.nummer$=n$:PUT 1,46:PRINT lo$;curoff$
1430 IF fl=1 THEN 1570
1440 PRINT FNat$(20,10)"2. STRAAT & NUMMER : ";eol$;hi$;curon$;:INPUT"",s$:LSET rec.nummer$=s$:PUT 1,47:PRINT lo$;curoff$
1450 IF fl=1 THEN 1570
1460 PRINT FNat$(20,12)"3. POSTCODE & WOONPLAATS : ";eol$;hi$;curon$;:INPUT"",st$:LSET rec.nummer$=st$:PUT 1,48:PRINT lo$;curoff$
1470 IF fl=1 THEN 1570
1480 PRINT FNat$(20,14)"4. TELEFOONNUMMER : ";eol$;hi$;curon$;:INPUT"",t$:LSET rec.nummer$=t$:PUT 1,49:PRINT lo$;curoff$
1490 IF fl=1 THEN 1570
1500 PRINT FNat$(20,16)"5. H.R. NR. : ";eol$;hi$;curon$;:INPUT"",hr$:LSET rec.nummer$="HR NR: "+hr$:PUT 1,50:PRINT lo$;curoff$
1510 IF fl=1 THEN 1570
1520 PRINT FNat$(20,18)"6. BTW. NR. : ";eol$;hi$;curon$;:INPUT"",bt$:LSET rec.nummer$="BTW NR: "+bt$:PUT 1,51:PRINT lo$;curoff$
1530 IF fl=1 THEN 1570
1540 PRINT FNat$(20,20)"7. BANKINSTELLING & NUMMER : ";eol$;hi$;curon$;:INPUT"",bank$:LSET rec.nummer$=bank$:PUT 1,52:PRINT lo$;curoff$
1550 fl=1
1560 PRINT FNat$(13,23)">>> DRUK NR. OM TE WIJZIGEN - <E> VOOR EINDE <<<"
1570 antw$="01234567E":p$=INKEY$:p$=UPPER$(p$):IF INSTR(antw$,p$)=0 OR p$=""THEN 1570
1580 IF p$="E"THEN 1600
1590 ON VAL(p$)+1 GOTO 1400,1420,1440,1460,1480,1500,1520,1540
1600 IF LEN(drive$)=2 THEN 1610 ELSE 1630
1610 LSET rec.nummer$=drk$:PUT 1,61:LSET rec.nummer$=drl$:PUT 1,62:LSET rec.nummer$=drr$:PUT 1,63:LSET rec.nummer$=dra$:PUT 1,64:LSET rec.nummer$=drb$:PUT 1,65:LSET rec.nummer$=draa$:PUT 1,66
1620 LSET rec.nummer$=drv$:PUT 1,67:LSET rec.nummer$=drf$:PUT 1,68:LSET rec.nummer$=drj$:PUT 1,69:LSET rec.nummer$=drt$:PUT 1,70:LSET rec.nummer$="72":PUT 1,72:GOTO 1640
1630 LSET rec.nummer$=drive$:FOR t=61 TO 70:PUT 1,t:NEXT t
1640 GOSUB 1820:CLOSE 2:CLOSE 1:PRINT cls$:GOSUB 1040:GOTO 640
1650 READ a$,b$:IF a$="==="THEN RETURN
1660 antw$=b$
1670 LSET rec.om$=a$:LSET rec.nummer$=antw$:LSET rec.jaarsaldo$=nummer$:rec.maandsaldo$=nummer$:LSET rec.telhistor$=STR$(0)
1680 kontrole=ADDREC(2,2,0,antw$):PUT 2
1690 PUT 1,t
1700 RETURN
1710 DATA "BTW MEDEKONTRAKTANT VAK 14","498000","BTW INVOER VERLEGG.HEFF.VAK 13","498300"
1720 DATA "KLANTEN","400000","LEVERANCIERS","440000"
1730 DATA "BTW OP AANKOPEN VAK 21","498500","NIET AFTREKBARE BTW","665000"
1740 DATA "BTW OP VERKOPEN VAK 11","498100","HERZIENINGEN VAK 31 en 32","498600"
1750 DATA "VERKOPEN 6 % VAK 01","700000","VERKOPEN 17 % VAK 02","701000"
1760 DATA "VERKOPEN 19 % VAK 03","702000","VERKOPEN 25 EN 33 % VAK 04","703000"
1770 DATA "VERKOPEN UITVOER VAK 05","710000","BEKOMEN BETALINGSKORTINGEN","757000"
1780 DATA "TOEGESTANE BETALINGSKORTINGEN","655000","VERKOPEN MEDEKONTRAKTANT VAK 06","704000"
1790 DATA "TAKSEN VAK 12","498200","VERKOPEN KODE 6 0 %","711000","TRANSFERTEN","582000","CREDITNOTA'S VERKOPEN VAK 09","709000","KAS","571000"
1800 DATA "POST","561000","BANK 1","551000","BANK 2","552000"
1810 DATA "BANK 3","553000","===","==="
1820 BUFFERS 20
1830 CLOSE 2:IF LEN(drive$)=2 THEN CREATE 2,drk$+"klanten.rnd",drk$+"klanten.key",2,162 ELSE CREATE 2,drive$+"klanten.rnd",drive$+"klanten.key",2,162
1840 CLOSE 2
1850 IF LEN(drive$)=2 THEN CREATE 2,drl$+"leveranc.rnd",drl$+"leveranc.key",2,152 ELSE CREATE 2,drive$+"leveranc.rnd",drive$+"leveranc.key",2,152
1860 CLOSE 2
1870 IF LEN(drive$)=2 THEN CREATE 2,dra$+"artikels.rnd",dra$+"artikels.key",2,74 ELSE CREATE 2,drive$+"artikels.rnd",drive$+"artikels.key",2,74
1880 CLOSE 2
1890 recleng=4:IF LEN(drive$)=2 THEN OPEN"R",2,drb$+"btw.rnd",recleng ELSE OPEN "R",2,drive$+"btw.rnd",recleng
1900 FIELD 2,2 AS rbtw$
1910 LSET rbtw$=" 6":PUT 2,1:LSET rbtw$="17":PUT 2,2:LSET rbtw$="19":PUT 2,3:LSET rbtw$="25":PUT 2,4:LSET rbtw$="33":PUT 2,5:LSET rbtw$=" 0":PUT 2,6:CLOSE 2
1920 RETURN
1930 ttl=LEN(titel$):PRINT CLS$;curoff$:PRINT FNat$((73-ttl)/2,0)CHR$(134)+STRING$((ttl+4),138)+CHR$(140)
1940 PRINT FNat$((73-ttl)/2,1)CHR$(133)+SPACE$(2)+hi$+titel$+lo$+SPACE$(2)+CHR$(133)
1950 PRINT FNat$((73-ttl)/2,2)CHR$(131)++STRING$((ttl+4),138)+CHR$(137)
1960 RETURN
1970 a$=CHR$(149)
20000 PRINT FNat$(0,23)eol$+"foutkode :";ERR
20010 PRINT FNat$(30,23)"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?";eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN R