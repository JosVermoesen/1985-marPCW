10 ON ERROR GOTO 1260
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 eol$=CHR$(27)+"[K"
60 OPTION RUN:hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 y=20
80 DIM lngt(y),srt$(y),srtr$(y),lngtsrtvld(y),nm$(y),rank$(y),subt$(y),sort(y),mini$(y),maxi$(y),av(y),veld(y,y),pct(y,y),subtot$(y,y),inhoud$(y,y),ff$(y),zoekstring$(y,y),bst$(30)
90 DIM field$(y),ttsub(y),tsub(y,y)
100 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,pcdrive$:CLOSE 1
110 PRINT cls$:PRINT FNat$(0,13,"UITDRUKKEN LIJSTEN GEGENEREERD MET RAPPORTGENERATOR"):PRINT STRING$(79,CHR$(172))
120 GOSUB 940
130 GOSUB 1170
140 PRINT FNat$(t+6,71,"");hel$;:INPUT "",keuze$:PRINT zcht$;:keuze=VAL(keuze$):IF keuze=0 THEN RUN"menu.mar" ELSE IF keuze<1 OR keuze+1>t THEN 140 ELSE 150
150 file$=bst$(keuze)
160 GOSUB 1050:keuze=rapport
170 GOSUB 700
180 GOSUB 650
190 GOSUB 810
200 LPRINT CHR$(27);"C";CHR$(blad);
210 LPRINT CHR$(27);"N";CHR$(sprong);
220 IF UPPER$(condens$)="J" THEN LPRINT CHR$(15);
230 LPRINT CHR$(14);titel$:LPRINT CHR$(14);STRING$(LEN(titel$),"-"):LPRINT
240 FOR tind=1 TO ind
250 GOSUB 560
260 kontrole=SEEKRANK(1,2,sort(tind))
270 kontrole=SEEKKEY(1,2,sort(tind),mini$(tind))
280 WHILE NOT FETCHKEY$(1)>maxi$(tind)
290  keynr$=FETCHKEY$(1)
300 kontrole=SEEKKEY(1,2,sort(tind),keynr$):IF kontrole<>0 THEN exit ELSE GET 1
310 GOSUB 880
320 tt=0
330 WHILE tt<av(tind)+1
340 tt=tt+1
350 IF UPPER$(srt$(veld(tind,tt)))="A" AND UPPER$(inhoud$(tind,veld(tind,tt)))="J" THEN IF INSTR(field$(veld(tind,tt)),zoekstring$(tind,tt))<>0 THEN switch=0 ELSE tt=99 ELSE 360
360 WEND
370 IF tt=99 THEN 430
380 FOR tt=1 TO av(tind)
390 IF UPPER$(srt$(veld(tind,tt)))="N" THEN field$(veld(tind,tt))=STR$(ROUND(VAL(field$(veld(tind,tt)))*pct(tind,tt)/100,0)) ELSE 400
400 IF UPPER$(subtot$(tind,tt))="J" THEN tsub(tind,tt)=tsub(tind,tt)+VAL(field$(veld(tind,tt))) ELSE 410
410 IF UPPER$(srt$(veld(tind,tt)))="N" THEN GOSUB 770 ELSE LPRINT field$(veld(tind,tt));" ";
420 NEXT tt:LPRINT
430 kontrole=SEEKNEXT(1,sort(tind))
440 IF kontrole<>0 THEN 280 ELSE GET 1
450 GOTO 310
460 WEND
470 LPRINT STRING$(tabul,"-")
480 FOR tt=1 TO av(tind)
490 IF UPPER$(subtot$(tind,tt))="J" THEN afdruk$=STR$(tsub(tind,tt)) ELSE afdruk$=SPACE$(lngt(veld(tind,tt)))
500 IF UPPER$(subtot$(tind,tt))="J" THEN GOSUB 1010 ELSE LPRINT afdruk$;" ";
510 ttsub(tt)=ttsub(tt)+tsub(tind,tt):tsub(tind,tt)=0
520 NEXT tt:LPRINT
530 IF UPPER$(ff$(tind))="J" THEN LPRINT CHR$(12);
540 NEXT tind
550 RUN
560 REM subroutine afdrukken hoofding
570 LPRINT:LPRINT:LPRINT CHR$(14);subt$(tind):LPRINT
580 tabul=1
590 FOR tt=1 TO av(tind)
600 LPRINT TAB(tabul);UPPER$(MID$(nm$(veld(tind,tt)),1,lngt(veld(tind,tt))));
610 tabul=tabul+lngt(veld(tind,tt))+1
620 NEXT tt:LPRINT
630 LPRINT STRING$(tabul,"-"):LPRINT
640 RETURN
650 REM bestin.bas=routine bestandsdefinitie inladen
660 IF nbstnd$="" THEN PRINT "eerst een bestand selecteren a.u.b.":RETURN ELSE OPEN "i",1,+pcdrive$+nbstnd$+".fil",2
670 INPUT #1,trl,drive$,a:FOR t=1 TO a:INPUT #1,nm$(t),lngt(t),srt$(t),srtr$(t),lngtsrtvld(t),rank$(t)
680 NEXT t:CLOSE 1
690 RETURN
700 REM bestkeuz.bas=routine bestand selecteren
710 IF FIND$(pcdrive$+"999.rnd")="" THEN PRINT "nog geen bestandsdefinitie voor handen":RETURN ELSE OPEN "r",1,pcdrive$+"999.rnd",30
720 FIELD 1,8 AS rc.nbstnd$
730 GET 1,1:FOR t=2 TO VAL(rc.nbstnd$):GET 1,t:NEXT t
740 GET 1,keuze+1:nbstnd$=rc.nbstnd$
750 CLOSE 1
760 RETURN
770 REM subroutine nummerisch formatteren
780 mask$=STRING$(lngt(veld(tind,tt)),"#")
790 LPRINT USING mask$;VAL(field$(veld(tind,tt)));:LPRINT " ";
800 RETURN
810 REM subroutine openen bestanden
820 IF a=0 THEN RETURN
830 BUFFERS 20
840 OPEN "k",1,drive$+nbstnd$+".rnd",drive$+nbstnd$+".key",2,trl
850 lf=0:FOR t=1 TO a:lf=lf+lngt(t):NEXT t
860 FIELD 1,lf AS a$
870 RETURN
880 REM subroutine velden uit record distileren
890 zk=1
900 FOR t=1 TO a
910 field$(t)=MID$(a$,zk,lngt(t)):zk=zk+lngt(t)
920 NEXT t
930 RETURN
940 REM routine rapporten zoeken
950 i=0:bst$(0)=" "
960 WHILE bst$(i)<>""
970 i=i+1
980 bst$(i)=FIND$(pcdrive$+"*.rap",i)
990 WEND
1000 RETURN
1010 REM subroutine nummerisch formatteren
1020 mask$=STRING$(lngt(veld(tind,tt)),"#")
1030 LPRINT USING mask$;VAL(afdruk$);:LPRINT " ";
1040 RETURN
1050 REM subroutine inladen rapportdefinitie
1060 OPEN "i",1,pcdrive$+file$,2
1070 INPUT #1,rapport,titel$,ind
1080 FOR t=1 TO ind
1090 INPUT #1,subt$(t),sort(t),mini$(t),maxi$(t),av(t),ff$(t)
1100 FOR tt=1 TO av(t)
1110 INPUT #1,veld(t,tt),pct(t,tt),subtot$(t,tt),inhoud$(t,tt),zoekstring$(t,tt)
1120 NEXT tt
1130 NEXT t
1140 INPUT #1,totlengte,condens$,blad,sprong
1150 CLOSE 1
1160 RETURN
1170 REM subroutine titels rapporten inladen
1180 FOR t=1 TO i-1
1190 OPEN "i",1,pcdrive$+bst$(t),2
1200 INPUT #1,rapport,titel$
1210 CLOSE 1
1220 PRINT FNat$(t+4,8,UPPER$(titel$));" "+STRING$(60-LEN(titel$),".")+" ";hel$;t;zcht$;
1230 NEXT t:PRINT FNat$(t+4,8,"TERUG NAAR HOOFDMENU ")+STRING$(40,".")+" ";hel$;0;zcht$;
1240 PRINT FNat$(t+6,59,"UW KEUZE : ");
1250 RETURN
1260 PRINT FNat$(23,0,eol$+"foutkode :");ERR
1270 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
1280 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 1280
1290 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 1300 ELSE RESUME NEXT
1300 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
 T