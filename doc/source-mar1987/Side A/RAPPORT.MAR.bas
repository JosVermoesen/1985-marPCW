10 ON ERROR GOTO 2170
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,pcdrive$:CLOSE 1
80 z1$=SPACE$(20):z2$=SPACE$(14)
90 y=20
100 DIM lngt(y),srt$(y),srtr$(y),lngtsrtvld(y),nm$(y),rank$(y),subt$(y),sort(y),mini$(y),maxi$(y),av(y),veld(y,y),pct(y,y),subtot$(y,y),inhoud$(y,y),ff$(y),zoekstring$(y,y)
110 PRINT cls$:PRINT zcht$;:PRINT FNat$(0,13,"VSOFT RAPPORTGENERATOR  versie 1.0  (c) 1987"):PRINT STRING$(79,CHR$(172)):PRINT FNat$(5,15,"NIEUWE BESTANDSDEFINITIES INBRENGEN... ");hel$;"1";zcht$;
120 PRINT FNat$(7,15,"BESTANDSDEFINITIES AFDRUKKEN ......... ");hel$;"2";zcht$;
130 PRINT FNat$(9,15,"RAPPORT OPMAKEN ...................... ");hel$;"3";zcht$;
140 PRINT FNat$(11,15,"TERUG NAAR MENU ...................... ");hel$;"0";zcht$;
150 PRINT FNat$(13,15,"                            UW KEUZE : ");hel$;"0";zcht$;
160 PRINT FNat$(13,54,"");
170 antw$="0123":keuze$=INKEY$:IF INSTR(antw$,keuze$)=0 OR keuze$="" THEN 170
180 keuze=VAL(keuze$):IF keuze=0 THEN RUN "menu.mar"
190 ON keuze GOSUB 210,900,1950
200 GOTO 110
210 REM bestout.bas=routine bestandsdefinitie
220 GOSUB 1610
230 PRINT FNat$(3,15,eol$);hel$;:INPUT "",nbstnd$:IF nbstnd$="" THEN PRINT zcht$;:RETURN
240 IF LEN(nbstnd$)>8 THEN 230
250 PRINT FNat$(4,15,eol$);pcdrive$:PRINT FNat$(4,15,"");:INPUT "",drive$:IF drive$="" THEN drive$=pcdrive$
260 IF FIND$(drive$+nbstnd$+".rnd")="" THEN 230
270 PRINT FNat$(5,15,eol$);:INPUT "",trl
280 PRINT FNat$(6,15,eol$);:INPUT "",a
290 FOR t=1 TO a
300 PRINT FNat$(9,15,eol$);USING "##";t
310 PRINT FNat$(12,33,eol$);lngt(t):PRINT FNat$(12,33,"");:INPUT "",beta$:IF beta$="" THEN 320 ELSE lngt(t)=VAL(beta$)
320 PRINT FNat$(12,33,eol$);lngt(t)
330 PRINT FNat$(13,33,eol$);nm$(t):PRINT FNat$(13,33,"");:INPUT "",beta$:IF beta$="" THEN 340 ELSE nm$(t)=beta$
340 PRINT FNat$(13,33,eol$);nm$(t)
350 PRINT FNat$(14,33,eol$);srt$(t):PRINT FNat$(14,33,"");:INPUT "",beta$:IF beta$="" THEN 360 ELSE srt$(t)=MID$(beta$,1,1)
360 antw$="AN":IF INSTR(antw$,UPPER$(srt$(t)))=0 THEN 350
370 PRINT FNat$(14,33,eol$);srt$(t)
380 PRINT FNat$(15,33,eol$);srtr$(t):PRINT FNat$(15,33,"");:INPUT "",beta$:IF beta$="" THEN 390 ELSE srtr$(t)=MID$(beta$,1,1)
390 antw$="JN":IF INSTR(antw$,UPPER$(srtr$(t)))=0 THEN 380
400 PRINT FNat$(15,33,eol$);srtr$(t)
410 IF UPPER$(srtr$(t))="J" THEN 420 ELSE 470
420 PRINT FNat$(16,33,eol$);lngtsrtvld(t):PRINT FNat$(16,33,"");:INPUT "",beta$:IF beta$="" THEN 430 ELSE lngtsrtvld(t)=VAL(beta$)
430 IF lngtsrtvld(t)>lngt(t) THEN 420
440 PRINT FNat$(16,33,eol$);lngtsrtvld(t)
450 PRINT FNat$(17,33,eol$);rank$(t):PRINT FNat$(17,33,"");:INPUT "",beta$:IF beta$="" THEN 460 ELSE rank$(t)=beta$
460 antw$="01234567":IF INSTR(antw$,rank$(t))=0 OR LEN(rank$(t))<>1 THEN 450 ELSE GOTO 480
470 lngtsrtvld(t)=0
480 NEXT t
490 PRINT zcht$;:PRINT FNat$(23,0,"<W>EGSCHRIJVEN - <K>ONTROLEREN - <A>NNULEREN : DRUK DE JUISTE TOETS");
500 antw$="WKA":pkey$=INKEY$:IF INSTR(antw$,UPPER$(pkey$))=0 OR pkey$="" THEN 500
510 PRINT hel$;:IF UPPER$(pkey$)="K" THEN 290 ELSE IF UPPER$(pkey$)="A" THEN PRINT zcht$;:CLEAR:RUN ELSE PRINT zcht$;
520 IF FIND$(pcdrive$+"999.rnd")="" THEN OPEN "r",1,pcdrive$+"999.rnd",30:FIELD 1,8 AS rc.nbstnd$:LSET rc.nbstnd$=STR$(1):PUT 1,1 ELSE OPEN "r",1,pcdrive$+"999.rnd",30
530 FIELD 1,8 AS rc.nbstnd$
540 GET 1,1:nr=VAL(rc.nbstnd$)+1:LSET rc.nbstnd$=STR$(nr):PUT 1,1:LSET rc.nbstnd$=nbstnd$:PUT 1,nr:CLOSE 1
550 OPEN "o",1,pcdrive$+nbstnd$+".fil",2
560 WRITE #1,trl,drive$,a:FOR t=1 TO a:WRITE #1,nm$(t),lngt(t),srt$(t),srtr$(t),lngtsrtvld(t),rank$(t)
570 NEXT t:CLOSE 1
580 RETURN
590 REM bestin.bas=routine bestandsdefinitie inladen
600 IF nbstnd$="" THEN PRINT FNat$(23,0,"eerst een bestand selecteren a.u.b.");:RETURN ELSE OPEN "i",1,pcdrive$+nbstnd$+".fil",2
610 INPUT #1,trl,drive$,a:FOR t=1 TO a:INPUT #1,nm$(t),lngt(t),srt$(t),srtr$(t),lngtsrtvld(t),rank$(t)
620 NEXT t:CLOSE 1
630 RETURN
640 REM bestkeuz.bas=routine bestand selecteren
650 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(79,CHR$(172))):PRINT FNat$(22,0,STRING$(79,CHR$(172))):PRINT FNat$(23,0,"GEEF HET NR. VAN HET BESTAND EN DRUK <RETURN>");
660 IF FIND$(pcdrive$+"999.rnd")="" THEN PRINT "nog geen bestandsdefinitie voor handen":RETURN ELSE OPEN "r",1,pcdrive$+"999.rnd",30
670 FIELD 1,8 AS rc.nbstnd$
680 GET 1,1:FOR t=2 TO VAL(rc.nbstnd$):GET 1,t:PRINT FNat$(t+1,0,"");t-1;". ";rc.nbstnd$:NEXT t
690 PRINT FNat$(t+2,0,eol$);"maak uw keuze : ";hel$;:INPUT "",keuze$:PRINT zcht$;:keuze=VAL(keuze$):IF keuze>t-1 THEN 690 ELSE IF keuze$="" THEN CLEAR:RUN
700 GET 1,keuze+1:nbstnd$=rc.nbstnd$
710 CLOSE 1
720 RETURN
730 REM subroutine openen bestanden
740 IF a=0 THEN RETURN
750 BUFFERS 20
760 OPEN "k",1,drive$+nbstnd$+".rnd",drive$+nbstnd$+".key",2,trl
770 lf=0:FOR t=1 TO a:lf=lf+lngt(t):NEXT t
780 FIELD 1,lf AS a$
790 RETURN
800 REM subroutine velden uit record distileren
810 zk=1
820 FOR t=1 TO a
830 field$(t)=MID$(a$,zk,lngt(t))
840 NEXT t
850 RETURN
860 REM subroutine titel
870 PRINT FNat$(9,17,eol$);hel$;titel$:PRINT FNat$(9,17,"");:INPUT "",beta$:PRINT zcht$;:IF beta$="" AND titel$="" THEN RUN ELSE IF beta$="" THEN 880 ELSE titel$=beta$
880 REM
890 RETURN
900 tp$="AFDRUKKEN BESTANDDEFINITIE":GOSUB 640
910 GOSUB 590
920 REM bestanddefinitie afdrukken op scherm
930 INPUT "naar scherm of printer ?",antw$
940 z$=STRING$(80,"-")
950 IF UPPER$(antw$)="P" THEN 1020
960 PRINT cls$:PRINT "bestandsdefinitie":PRINT:PRINT "naam ";drive$+nbstnd$+".fil";" met max. recordlengte van ";trl;" bytes."
970 PRINT:PRINT "omschrijving","type veld","lengte veld","srtrveld/rank","lengte srtrveld"
980 PRINT z$
990 FOR t=1 TO a:PRINT nm$(t);TAB(24);srt$(t),lngt(t),srtr$(t);"/";rank$(t),lngtsrtvld(t):NEXT t:PRINT zcht$;
1000 PRINT:INPUT "druk enter voor vervolg",antw$
1010 GOTO 1070
1020 LPRINT "bestandsdefinitie":LPRINT:LPRINT "naam ";drive$+nbstnd$+".fil";" met max. recordlengte van ";trl;" bytes."
1030 LPRINT:LPRINT "omschrijving","type veld","lengte veld","srtrveld/rank","lengte srtrveld"
1040 LPRINT z$
1050 FOR t=1 TO a:LPRINT nm$(t);TAB(24);srt$(t),lngt(t),srtr$(t);"/";rank$(t),lngtsrtvld(t):NEXT t
1060 LPRINT CHR$(12);
1070 RETURN
1080 REM subroutine rapport lay-out opbouwen
1090 PRINT FNat$(10,17,eol$);hel$;USING "##";ind
1100 PRINT FNat$(12,17,eol$);subt$(ind):PRINT FNat$(12,17,"");:INPUT "",beta$:IF beta$="" THEN 1110 ELSE subt$(ind)=beta$
1110 PRINT FNat$(12,17,eol$);subt$(ind)
1120 PRINT FNat$(13,17,z1$):PRINT FNat$(13,17,"");sort(ind);:PRINT FNat$(13,17,"");:INPUT "",beta$:IF beta$="" THEN  1130 ELSE sort(ind)=VAL(beta$)
1130 IF sort(ind)>7 OR sort(ind)<0 THEN 1120
1140 PRINT FNat$(13,17,z1$):PRINT FNat$(13,17,"");sort(ind)
1150 PRINT FNat$(14,17,z1$):PRINT FNat$(14,17,mini$(ind));:PRINT FNat$(14,17,"");:INPUT "",beta$:IF beta$="" THEN 1160 ELSE mini$(ind)=beta$
1160 PRINT FNat$(14,17,z1$):PRINT FNat$(14,17,mini$(ind))
1170 PRINT FNat$(13,65,eol$);av(ind):PRINT FNat$(13,65,"");:INPUT "",beta$:IF beta$="" THEN 1180 ELSE av(ind)=VAL(beta$)
1180 IF av(ind)>a THEN 1170
1190 PRINT FNat$(13,65,eol$);av(ind)
1200 PRINT FNat$(14,65,eol$);maxi$(ind);:PRINT FNat$(14,65,"");:INPUT "",beta$:IF beta$="" THEN 1210 ELSE maxi$(ind)=beta$
1210 PRINT FNat$(14,65,eol$);maxi$(ind)
1220 PRINT FNat$(15,65,eol$);ff$(ind):PRINT FNat$(15,65,"");:INPUT "",beta$:IF beta$="" THEN 1230 ELSE ff$(ind)=beta$
1230 antw$="JN":IF INSTR(antw$,UPPER$(ff$(ind)))=0 OR LEN(ff$(ind))<>1 THEN 1220
1240 PRINT FNat$(15,65,eol$);ff$(ind)
1250 FOR t=1 TO av(ind)
1260 PRINT FNat$(16,17,eol$);USING "##";t
1270 PRINT FNat$(18,22,z2$):PRINT FNat$(18,22,"");veld(ind,t);:PRINT FNat$(18,22,"");:INPUT "",beta$:IF beta$="" THEN 1280 ELSE veld(ind,t)=VAL(beta$)
1280 IF veld(ind,t)>a OR veld(ind,t)<1 THEN 1270 ELSE GOSUB 2140
1290 PRINT FNat$(18,22,z2$):PRINT FNat$(18,22,"");veld(ind,t)
1300 PRINT zcht$;:PRINT FNat$(18,25,"OK (J/N) ? ");
1310 antw$="JN":beta$=INKEY$:IF INSTR(antw$,UPPER$(beta$))=0 OR beta$="" THEN 1310
1320 PRINT hel$;:IF UPPER$(beta$)="N" THEN 1270 ELSE PRINT FNat$(18,25,"          ")
1330 IF UPPER$(srt$(veld(ind,t)))="N" THEN PRINT FNat$(18,65,eol$);"NUMMERISCH":GOTO 1340 ELSE PRINT FNat$(18,65,eol$);"ALFANUMM.":GOTO 1380
1340 PRINT FNat$(19,65,eol$);pct(ind,t):PRINT FNat$(19,65,"");:INPUT "",beta$:IF beta$="" THEN 1350 ELSE pct(ind,t)=VAL(beta$)
1350 PRINT FNat$(19,65,eol$);pct(ind,t)
1360 PRINT FNat$(20,65,eol$);subtot$(ind,t):PRINT FNat$(20,65,"");:INPUT "",beta$:IF beta$="" THEN 1370 ELSE subtot$(ind,t)=beta$ ':GOTO 1140
1370 antw$="JN":IF INSTR(antw$,UPPER$(subtot$(ind,t)))=0 OR LEN(subtot$(ind,t))<>1 THEN 1360 ELSE 1440
1380 PRINT FNat$(19,22,z2$):PRINT FNat$(19,22,inhoud$(ind,t)):PRINT FNat$(19,22,"");:INPUT "",beta$:IF beta$="" THEN 1390 ELSE inhoud$(ind,t)=beta$
1390 antw$="JN":IF INSTR(antw$,UPPER$(inhoud$(ind,t)))=0 OR LEN(inhoud$(ind,t))<>1 THEN 1380
1400 PRINT FNat$(19,22,z2$):PRINT FNat$(19,22,inhoud$(ind,t))
1410 IF UPPER$(inhoud$(ind,t))="N" THEN 1440
1420 PRINT FNat$(20,22,z2$):PRINT FNat$(20,22,zoekstring$(ind,t)):PRINT FNat$(20,22,"");:INPUT "",beta$:IF beta$="" THEN 1430 ELSE zoekstring$(ind,t)=beta$
1430 PRINT FNat$(20,22,z2$):PRINT FNat$(20,22,zoekstring$(ind,t))
1440 NEXT t:PRINT zcht$;:PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"DRUK <N>OG SUBROUTINES - <E>INDE");
1450 antw$="NE":beta$=INKEY$:IF INSTR(antw$,UPPER$(beta$))=0 OR beta$="" THEN 1450 
1460 IF UPPER$(beta$)="N" THEN switch=1:RETURN ELSE switch=0:RETURN
1470 REM subroutine wegschrijven rapportdefinitie
1480 PRINT zcht$;:PRINT FNat$(0,39,"RAPPORT BESTANDSNAAM : ........");eol$;hel$:PRINT FNat$(0,62,"");:INPUT "",rapnaam$:IF LEN(rapnaam$)>8 OR rapnaam$="" THEN 1480
1490 file$=pcdrive$+rapnaam$+".rap":IF FIND$(file$)<>"" THEN PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"RAPPORT ONDER DEZE NAAM BESTAAT REEDS !";:GOTO 1470 ELSE 1500
1500 OPEN "o",1,file$,2
1510 WRITE #1,rapport,titel$,ind
1520 FOR t=1 TO ind
1530 WRITE #1,subt$(t),sort(t),mini$(t),maxi$(t),av(t),ff$(t)
1540 FOR tt=1 TO av(t)
1550 WRITE #1,veld(t,tt),pct(t,tt),subtot$(t,tt),inhoud$(t,tt),zoekstring$(t,tt)
1560 NEXT tt
1570 NEXT t
1580 WRITE #1,totlengte,condens$,blad,sprong
1590 CLOSE 1
1600 RETURN
1610 PRINT cls$
1620 PRINT FNat$(0,0,"BESTANDSDEFINITIE")
1630 PRINT FNat$(1,0,STRING$(79,CHR$(172)))
1640 PRINT FNat$(3,0,"NAAM BESTAND :")
1650 PRINT FNat$(4,0,"DRIVE        :")
1660 PRINT FNat$(5,0,"RECORDLENGTE :")
1670 PRINT FNat$(6,0,"AANT. VELDEN :")
1680 PRINT FNat$(8,0,STRING$(79,"-")):PRINT FNat$(9,0,"INFO VELD NR :"):PRINT FNat$(10,0,STRING$(79,"-"))
1690 PRINT FNat$(12,0,"LENGTE VELD                    :")
1700 PRINT FNat$(13,0,"OMSCHRIJVING                   :")
1710 PRINT FNat$(14,0,"ALFANUMERISCH/NUMERISCH        :")
1720 PRINT FNat$(15,0,"SORTEERSLEUTEL                 :")
1730 PRINT FNat$(16,0,"LENGTE VELD ALS SORTEERSLEUTEL :")
1740 PRINT FNat$(17,0,"NR VAN DE INDEXRIJ (0-7)       :")
1750 PRINT FNat$(22,0,STRING$(79,CHR$(172)))
1760 RETURN
1770 PRINT FNat$(3,0,"RAPPORT MET BESTANDDEFINITIE : ");hel$;drive$+nbstnd$+".FIL":PRINT zcht$;
1780 PRINT FNat$(5,0,"OMSCHRIJVING     TYPE VELD    LENGTE VELD   SRTRVELD/RANK       LENGTE SRTRVELD")
1790 PRINT FNat$(6,0,STRING$(79,"-")):PRINT FNat$(8,0,STRING$(79,"-"))
1800 PRINT FNat$(9,0,"TITEL RAPPORT  : ")
1810 PRINT FNat$(10,0,"SUBROUTINE     :")
1820 PRINT FNat$(12,0,"SUBTITEL       :")
1830 PRINT FNat$(13,0,"SORTEERRIJ     :"):PRINT FNat$(13,39,"AANTAL VELDEN           :")
1840 PRINT FNat$(14,0,"MIN SLEUTEL    :")
1850 PRINT FNat$(14,39,"MAX SLEUTEL             :")
1860 PRINT  FNat$(16,0,"HUIDIG VELD NR :"):PRINT FNat$(15,39,"FORMFEED NA ROUTINE ?   :")
1870 PRINT FNat$(18,0,"NR VELD IN BESTAND  : ")
1880 PRINT FNat$(18,39,"VELD TYPE IS            :")
1890 PRINT FNat$(19,0,"VOORWAARDLIJK (J/N) :")
1900 PRINT FNat$(19,39,"GEEF %-AGE (100=TOTAAL) :")
1910 PRINT FNat$(20,0,"TE ZOEKEN STRING    :")
1920 PRINT FNat$(20,39,"SUBTOTAAL (J/N)         :")
1930 PRINT FNat$(22,0,STRING$(79,CHR$(172)));
1940 RETURN
1950 REM rapport opmaken
1960 tp$="RAPPORTDEFINITIE":GOSUB 640
1970 rapport=keuze
1980 GOSUB 590
1990 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT STRING$(79,CHR$(172))
2000 GOSUB 1770
2010 GOSUB 860:switch=1
2020 ind=0:WHILE switch<>0:ind=ind+1:GOSUB 1080:WEND
2030 totlengte=0:FOR t=1 TO av(ind):totlengte=totlengte+lngt(veld(ind,t)):NEXT t:PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"TOTALE LENGTE PER REGEL IS :");hel$;totlengte;:FOR t=1 TO 5000:NEXT t
2040 PRINT zcht$;:PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"GECONDENSEERD AFDRUKKEN (J/N) ");
2050 antw$="JN":condens$=INKEY$:IF INSTR(antw$,UPPER$(condens$))=0 OR condens$="" THEN 2050
2060 PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"GEEF LENGTE VAN BLAD IN LIJNEN : ");hel$;:INPUT "",beta$:blad=VAL(beta$):PRINT zcht$;
2070 PRINT FNat$(23,0,eol$):PRINT FNat$(23,0,"GEEF AANTAL LIJNEN OVER TE SPRINGEN : ");hel$;:INPUT "",beta$:sprong=VAL(beta$):PRINT zcht$;
2080 PRINT FNat$(23,0,eol$);"<W>EGSCHRIJVEN - <K>ONTROLEREN - <A>NNULEREN : DRUK DE JUISTE TOETS";
2090 antw$="WKA":beta$=INKEY$:IF INSTR(antw$,UPPER$(beta$))=0 OR beta$="" THEN 2090
2100 IF UPPER$(beta$)="K" THEN 2010 ELSE IF UPPER$(beta$)="A" THEN CLEAR:RUN ELSE 2110
2110 REM
2120 GOSUB 1470
2130 RETURN
2140 REM subroutine veldinfo afdrukken
2150 PRINT FNat$(7,0,eol$);MID$(nm$(veld(ind,t)),1,20);TAB(32);srt$(veld(ind,t));TAB(45);lngt(veld(ind,t));TAB(59);srtr$(veld(ind,t));"/";rank$(veld(ind,t));TAB(75);lngtsrtvld(veld(ind,t))
2160 RETURN
2170 PRINT FNat$(23,0,eol$+"foutkode :");ERR
2180 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
2190 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 2190
2200 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 2210 ELSE RESUME NEXT
2210 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
 THEN 2190
2200 IF at$="H" THEN RESUME ELSE IF at$="M" THEN