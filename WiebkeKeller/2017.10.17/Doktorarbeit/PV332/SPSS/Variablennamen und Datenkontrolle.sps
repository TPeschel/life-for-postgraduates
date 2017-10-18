*Varibalen benennen 

rename variables (lfdn=VP).

rename variables (v_1=Zustimmung).

*Demographische Daten allgemein 

rename variables (v_2=Alter).

rename variables (v_3=Geschlecht).

rename variables (v_4=Nationalität).

rename variables (v_5=höchsterAbschluss).

rename variables (v_6=Ausbildung).

rename variables (v_7=Studium).

rename variables (v_8=SonstigesAusbildung).

rename variables (v_9=AktuellBerufstätigkeit).

rename variables (v_10=Teilzeitals).

rename variables (v_11=AUzeit).

rename variables (v_12=Vollals).

rename variables (v_13=arbeitslos_seit).

rename variables (v_14=Beziehung).

rename variables (v_15=Kinder).

rename variables (v_16=AnzahlKinder).

rename variables (v_17=Sprachkenntnis).

rename variables (v_18=DerzeitigeBehandlung).

rename variables (v_19=SonstigeBehandlung).

rename variables (v_20=T_Dauer).

rename variables (v_21=Institution).

rename variables (v_22=Anzahl_T).

rename variables (v_23=Sonstige_Anzahl).

rename variables (v_24=Vorherige_B).

rename variables (v_25=Sonstige_B).

*Diagnosen

rename variables (v_26=Sucht_D).

rename variables (v_27=Ess_D).

rename variables (v_28=Art_EssD).

rename variables (v_29=Sonstige_ess).

*Drogen

rename variables (v_30=Alkohol). 

rename variables (v_31=THC).

rename variables (v_32=Kokain).

rename variables (v_33=Heroin).

rename variables (v_34=Speed).

rename variables (v_35=Crystal).

rename variables (v_36=LSD).

rename variables (v_37=Medis).

rename variables (v_38=Ecstasy).

rename variables (v_39=Spiel).

rename variables (v_40=Sucht_Andere).

rename variables (v_41=Sucht_andere2).

rename variables (v_42=Erste_Droge).

rename variables (v_43=Konsumalter).

rename variables (v_44=Letzte_Droge).

rename variables (v_45=Datum_letzterKonsum).

rename variables (v_46=Abstinenzzeiten).

rename variables (v_47=Dauer_Abstinenz).

rename variables (v_48=Krankheiten).

rename variables (v_49=Welche_Krankheiten).

rename variables (v_50=Haft).

rename variables (v_51=Zeit_Haft).

*Essstörungen

rename variables (v_52=Alter_Ess_Beginn).

rename variables (v_53=K_Dauer).

rename variables (v_54=Erbrechen).

rename variables (v_55=Laxantien).

rename variables (v_56=Diuretika).

rename variables (v_57=A_Zügler).

rename variables (v_58=Sport).

rename variables (v_59=Hungern).

rename variables (v_60=Andere_red).

rename variables (v_61=andere_red2).

rename variables (v_62=Keine_Red).

*EDE-Q

rename variables (v_63 TO v_74=EDE_Q1 TO EDE_Q12).

rename variables (v_76=EDE_Q13).

rename variables (v_78=EDE_Q14).

rename variables (v_80=EDE_Q15).

rename variables (v_82=EDE_Q16).

rename variables (v_84=EDE_Q17).

rename variables (v_86=EDE_Q18).

rename variables (v_93 TO v_102=EDE_Q19 TO EDE_Q28).

rename variables (v_103=EDE_Gewicht).

rename variables (v_104=EDE_Größe).

rename variables (v_105=EDE_Periode).

rename variables (v_106=EDE_AnzahlPeriode).

rename variables (v_107=EDE_Pille).


*VIA-120

rename variables (v_155 TO v_274=VIA_1 TO VIA_120).
*SWLS

rename variables (v_483 TO v_487=SWLS_1 TO SWLS_5).

*PANAS

rename variables (v_488 TO v_507=PANAS_1 TO PANAS_20). 

*OTH

rename variables (v_508 TO v_525=OTH_1 TO OTH_18).

*REL

rename variables (v_526 TO v_530=REL_1 TO REL_5).

*ACC

rename variables (v_531 TO v_535=ACC_1 TO ACC_5).

*RSES

rename variables (v_536 TO v_545=RSES_1 TO RSES_10). 

*VIA Skalenwerte

rename variables (c_0001=Kreativität).

rename variables (c_0002=Neugier).

rename variables (c_0003=Urteilsvermögen).

rename variables (c_0004=Liebe_Lernen).

rename variables (c_0005=Weitsicht).

rename variables (c_0006=Tapferkeit).

rename variables (c_0007=Ausdauer).

rename variables (c_0008=Authentizität).

rename variables (c_0009=Tatendrang).

rename variables (c_0010=Liebe).

rename variables (c_0011=Freundlichkeit).

rename variables (c_0012=Soziale_Intelligenz).

rename variables (c_0013=Teamfähigkeit).

rename variables (c_0014=Fairness).

rename variables (c_0015=Führungsvermögen).

rename variables (c_0016=Vergebung).

rename variables (c_0017=Bescheidenheit).

rename variables (c_0018=Vorsicht).

rename variables (c_0019=Selbstregulation).

rename variables (c_0020=Sinn_Schöne).

rename variables (c_0021=Dankbarkeit).

rename variables (c_0022=Hoffnung).

rename variables (c_0023=Humor).

rename variables (c_0024=Spiritualität).


*VP ohne Essstörung und Sucht rausnhemen und nach Erkrankung sortieren

SORT CASES  BY Ess_D Sucht_D.
SPLIT FILE SEPARATE BY Ess_D Sucht_D.

SELECT IF NOT (Sucht_D=2 AND Ess_D=2). 

SELECT IF NOT (Sucht_D=1 AND Ess_D=1). 


*Datenkontrolle

*Wertebereich 

*EDE_Q 1-12
DATASET ACTIVATE DatenSet1.
FREQUENCIES VARIABLES=EDE_Q1 EDE_Q2 EDE_Q3 EDE_Q4 EDE_Q5 EDE_Q6 EDE_Q7 EDE_Q8 EDE_Q9 EDE_Q10 
    EDE_Q11 EDE_Q12
  /ORDER=ANALYSIS.

*EDE-Q 19-21

FREQUENCIES VARIABLES=EDE_Q19 EDE_Q20 EDE_Q21 EDE_Q22 EDE_Q23 EDE_Q24 EDE_Q25 EDE_Q26 EDE_Q27 
    EDE_Q28
  /ORDER=ANALYSIS.

*VIA-120


FREQUENCIES VARIABLES=VIA_1 VIA_2 VIA_3 VIA_4 VIA_5 VIA_6 VIA_7 VIA_8 VIA_9 VIA_10 VIA_11 VIA_12 
    VIA_13 VIA_14 VIA_15 VIA_16 VIA_17 VIA_18 VIA_19 VIA_20 VIA_21 VIA_22 VIA_23 VIA_24 VIA_25 VIA_26 
    VIA_27 VIA_28 VIA_29 VIA_30 VIA_31 VIA_32 VIA_33 VIA_34 VIA_35 VIA_36 VIA_37 VIA_38 VIA_39 VIA_40 
    VIA_41 VIA_42 VIA_43 VIA_44 VIA_45 VIA_46 VIA_47 VIA_48 VIA_49 VIA_50 VIA_51 VIA_52 VIA_53 VIA_54 
    VIA_55 VIA_56 VIA_57 VIA_58 VIA_59 VIA_60 VIA_61 VIA_62 VIA_63 VIA_64 VIA_65 VIA_66 VIA_67 VIA_68 
    VIA_69 VIA_70 VIA_71 VIA_72 VIA_73 VIA_74 VIA_75 VIA_76 VIA_77 VIA_78 VIA_79 VIA_80 VIA_81 VIA_82 
    VIA_83 VIA_84 VIA_85 VIA_86 VIA_87 VIA_88 VIA_89 VIA_90 VIA_91 VIA_92 VIA_93 VIA_94 VIA_95 VIA_96 
    VIA_97 VIA_98 VIA_99 VIA_100 VIA_101 VIA_102 VIA_103 VIA_104 VIA_105 VIA_106 VIA_107 VIA_108 
    VIA_109 VIA_110 VIA_111 VIA_112 VIA_113 VIA_114 VIA_115 VIA_116 VIA_117 VIA_118 VIA_119 VIA_120
  /ORDER=ANALYSIS.

*SWLS 


FREQUENCIES VARIABLES=SWLS_1 SWLS_2 SWLS_3 SWLS_4 SWLS_5
  /ORDER=ANALYSIS.

*PANAS


FREQUENCIES VARIABLES=PANAS_1 PANAS_2 PANAS_3 PANAS_4 PANAS_5 PANAS_6 PANAS_7 PANAS_8 PANAS_9 
    PANAS_10 PANAS_11 PANAS_12 PANAS_13 PANAS_14 PANAS_15 PANAS_16 PANAS_17 PANAS_18 PANAS_19 PANAS_20
  /ORDER=ANALYSIS.

*PERMA


FREQUENCIES VARIABLES=OTH_1 OTH_2 OTH_3 OTH_4 OTH_5 OTH_6 OTH_7 OTH_8 OTH_9 OTH_10 OTH_11 OTH_12 
    OTH_13 OTH_14 OTH_15 OTH_16 OTH_17 OTH_18 REL_1 REL_2 REL_3 REL_4 REL_5 ACC_1 ACC_2 ACC_3 ACC_4 
    ACC_5
  /ORDER=ANALYSIS.

*RSES


FREQUENCIES VARIABLES=RSES_1 RSES_2 RSES_3 RSES_4 RSES_5 RSES_6 RSES_7 RSES_8 RSES_9 RSES_10
  /ORDER=ANALYSIS.

*Antwortbias 

*EDE-Q1-12 und 19-28

COUNT.
COUNT EDE_Q1er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (1). 
COUNT EDE_Q2er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (2). 
COUNT EDE_Q3er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (3). 
COUNT EDE_Q4er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (4). 
COUNT EDE_Q5er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (5). 
COUNT EDE_Q6er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (6). 
COUNT EDE_Q7er = EDE_Q1 TO EDE_Q12 AND EDE_Q19 TO EDE_Q28 (7). 
EXECUTE.

FREQUENCIES VARIABLES=EDE_Q1er EDE_Q2er EDE_Q3er EDE_Q4er EDE_Q5er EDE_Q6er EDE_Q7er
  /ORDER=ANALYSIS.

*VIA-120

COUNT.
COUNT VIA_1er = VIA_1 TO VIA_120 (1). 
COUNT VIA_2er = VIA_1 TO VIA_120 (2). 
COUNT VIA_3er = VIA_1 TO VIA_120 (3). 
COUNT VIA_4er = VIA_1 TO VIA_120 (4). 
COUNT VIA_5er = VIA_1 TO VIA_120 (5). 
EXECUTE.

FREQUENCIES VARIABLES=VIA_1er VIA_2er VIA_3er VIA_4er VIA_5er
  /ORDER=ANALYSIS.

*SWLS

COUNT.
COUNT SWLS_1er = SWLS_1 TO SWLS_5 (1). 
COUNT SWLS_2er = SWLS_1 TO SLWS_5 (2). 
COUNT SWLS_3er = SWLS_1 TO SWLS_5 (3). 
COUNT SWLS_4er = SWLS_1 TO SWLS_5 (4). 
COUNT SWLS_5er = SWLS_1 TO SWLS_5 (5). 
COUNT SWLS_6er = SWLS_1 TO SWLS_5 (6). 
COUNT SWLS_7er = SWLS_1 TO SWLS_5 (7). 
EXECUTE.

FREQUENCIES VARIABLES=SWLS_1er SWLS_2er SWLS_3er SWLS_4er SWLS_5er SWLS_6er SWLS_7er
  /ORDER=ANALYSIS.

*PANAS

COUNT.
COUNT PANAS_1er = PANAS_1 TO PANAS_20 (1). 
COUNT PANAS_2er = PANAS_1 TO PANAS_20  (2). 
COUNT PANAS_3er = PANAS_1 TO PANAS_20  (3). 
COUNT PANAS_4er = PANAS_1 TO PANAS_20  (4). 
COUNT PANAS_5er = PANAS_1 TO PANAS_20  (5). 
EXECUTE.

FREQUENCIES VARIABLES=PANAS_1er PANAS_2er PANAS_3er PANAS_4er PANAS_5er 
  /ORDER=ANALYSIS.

*OTH

COUNT.
COUNT OTH_1er = OTH_1 TO OTH_18 (1). 
COUNT OTH_2er = OTH_1 TO OTH_18  (2). 
COUNT OTH_3er = OTH_1 TO OTH_18  (3). 
COUNT OTH_4er = OTH_1 TO OTH_18  (4). 
COUNT OTH_5er = OTH_1 TO OTH_18  (5). 
EXECUTE.

FREQUENCIES VARIABLES=OTH_1er OTH_2er OTH_3er OTH_4er OTH_5er 
  /ORDER=ANALYSIS.

*REL

COUNT.
COUNT REL_1er = REL_1 TO REL_5 (1). 
COUNT REL_2er = REL_1 TO REL_5 (2). 
COUNT REL_3er = REL_1 TO REL_5 (3). 
COUNT REL_4er = REL_1 TO REL_5 (4). 
COUNT REL_5er = REL_1 TO REL_5 (5). 
EXECUTE.

FREQUENCIES VARIABLES=REL_1er REL_2er REL_3er REL_4er REL_5er 
  /ORDER=ANALYSIS.

*ACC 

COUNT.
COUNT ACC_1er = ACC_1 TO ACC_5 (1). 
COUNT ACC_2er = ACC_1 TO ACC_5 (2). 
COUNT ACC_3er = ACC_1 TO ACC_5 (3). 
COUNT ACC_4er = ACC_1 TO ACC_5 (4). 
COUNT ACC_5er = ACC_1 TO ACC_5 (5). 
EXECUTE.

FREQUENCIES VARIABLES=ACC_1er ACC_2er ACC_3er ACC_4er ACC_5er 
  /ORDER=ANALYSIS.

*RSES

COUNT.
COUNT RSES_1er = RSES_1 TO RSES_10 (1). 
COUNT RSES_2er = RSES_1 TO RSES_10 (2). 
COUNT RSES_3er = RSES_1 TO RSES_10 (3). 
COUNT RSES_4er = RSES_1 TO RSES_10 (4). 
EXECUTE.

FREQUENCIES VARIABLES=RSES_1er RSES_2er RSES_3er RSES_4er 
  /ORDER=ANALYSIS.


*Faktorenanalyse

*Daten transponieren

FLIP VARIABLES=EDE_Q1 TO EDE_Q12 EDE_Q19 TO EDE_Q28 VIA_1 TO VIA_120 SWLS_1 TO SWLS_5 PANAS_1 TO PANAS_20 OTH_1 TO OTH_18 REL_1 TO REL_5 ACC_1 TO ACC_5 RSES_1 TO RSES_10
  /NEWNAMES=VP.
DATASET NAME DatenSet2 WINDOW=FRONT.


*Anmerkung für uns: Neu machen und VP aufteilen 

FACTOR
  /VARIABLES K_8 K_9 K_10 K_16 K_17 K_18 K_19 K_20 K_21 K_22 K_24 K_25 K_27 K_28 K_31 K_32 K_33 
    K_35 K_36 K_38 K_40 K_42 K_43 K_44 K_45 K_46 K_47 K_48 K_49 K_50 K_51 K_53 K_54 K_55 K_56 K_57 K_58 
    K_60 K_61 K_63 K_66 K_67 K_69 K_70 K_71 K_73 K_76
  /MISSING LISTWISE 
  /ANALYSIS K_8 K_9 K_10 K_16 K_17 K_18 K_19 K_20 K_21 K_22 K_24 K_25 K_27 K_28 K_31 K_32 K_33 K_35 
    K_36 K_38 K_40 K_42 K_43 K_44 K_45 K_46 K_47 K_48 K_49 K_50 K_51 K_53 K_54 K_55 K_56 K_57 K_58 K_60 
    K_61 K_63 K_66 K_67 K_69 K_70 K_71 K_73 K_76
  /PRINT INITIAL KMO AIC EXTRACTION
  /PLOT EIGEN ROTATION
  /CRITERIA FACTORS(2) ITERATE(25)
  /EXTRACTION PC
  /ROTATION NOROTATE
  /SAVE REG(ALL)
  /METHOD=CORRELATION.



