
      SUBROUTINE PROPER(TV)

************************************************************************
* LAST CHANGE AT :                          BY:                        *
************************************************************************
* CALLED BY : COMBUS, CHARPR, ENERGY                                   *
* CALLS     :                                                          * 
************************************************************************
* COMMON in :                                                          *
* out       : HEATCG   heat capacity of specy (J/molK)
*             ENTROP   entropy of specy (J/molK)                       *
*             ENHAL    enthalpy of specy (J/mol)                       *
* in & out  :                                                          *
************************************************************************
* received  : TV      temperature                                      *
* returned  :                                                          *
************************************************************************
* PURPOSE   : calculation of the gas properties cp, h, s               *
************************************************************************

C*    !!!!!!!! The last five species are not evident !!!!!!!

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      TV, TX, TX2, TX3 ! auxiliaries
      INTEGER   J                ! specy indicator

      TX  = TV / 100.
      TX2 = TX * TX
      TX3 = TX2 * TX

C*     CH4
      HEATCG(CH4) = 4.1005+1.4388*TX+0.004201*TX2-0.0017803*TX3
      ENTROP(CH4) = 36.082+3.1386*TX-0.10471*TX2+0.0021556*TX3
      ENTHAL(CH4) = 0.2737*TX+0.09625*TX2-0.0016183*TX3-1.6187

C*     SO2
      HEATCG(SO2) = 5.9236+1.5003*TX-0.10531*TX2+0.0026419*TX3
      ENTROP(SO2) = 49.2+4.0019*TX-0.21114*TX2+0.0049779*TX3
      ENTHAL(SO2) = 0.74626*TX+0.044822*TX2-0.0011166*TX3-2.6065

C*     O2
      HEATCG(O2) = 6.1261+0.31446*TX-0.0093772*TX2-0.0000037879*TX3
      ENTROP(O2) = 41.868+2.8851*TX-0.1667*TX2+0.0041699*TX3
      ENTHAL(O2) = 0.60401*TX+0.016796*TX2-0.00035402*TX3-1.9402

C*     N2
      HEATCG(N2) = 7.0155-0.10279*TX+0.029194*TX2-0.0011061*TX3
      ENTROP(N2) = 38.798+2.8474*TX-0.17166*TX2+0.0044327*TX3
      ENTHAL(N2) = 0.63096*TX+0.0082485*TX2-0.000056527*TX3-1.9468

C*     NO2
      HEATCG(NO2) = 5.5322+1.3184*TX-0.079589*TX2+0.0017147*TX3
      ENTROP(NO2) = 48.115+3.6395*TX-0.1861*TX2+0.0043389*TX3
      ENTHAL(NO2) = 0.64435*TX+0.0474*TX2-0.0011384*TX3-2.3202

C*     NO
      HEATCG(NO) = 6.9301-0.0065059*TX+0.022272*TX2-0.00098397*TX3
      ENTROP(NO) = 43.234+2.8944*TX-0.17134*TX2+0.0043631*TX3
      ENTHAL(NO) = 0.62397*TX+0.012339*TX2-0.00020105*TX3-1.9592

C*     H2O
      HEATCG(H2O) = 7.7246+0.015807*TX+0.029945*TX2-0.0010291*TX3
      ENTROP(H2O) = 37.04+3.2692*TX-0.19068*TX2+0.0049639*TX3
      ENTHAL(H2O) = 0.71015*TX+0.012826*TX2+0.000055944*TX3-2.228

C*     H2
      HEATCG(H2) = 6.8475+0.023963*TX-0.00065734*TX2+0.00019814*TX3
      ENTROP(H2) = 24.106+2.9244*TX-0.18448*TX2+0.0048345*TX3
      ENTHAL(H2) = 0.7014*TX-0.001671*TX2+0.0001792*TX3-2.0818

C*     CO2
      HEATCG(CO2) = 5.1859+1.5157*TX-0.096572*TX2+0.0023054*TX3
      ENTROP(CO2) = 41.614+3.7224*TX-0.18886*TX2+0.0044172*TX3
      ENTHAL(CO2) = 0.6605*TX+0.048568*TX2-0.0010994*TX3-2.383

C*     CO
      HEATCG(CO) = 6.8632-0.040783*TX+0.024789*TX2-0.0010178*TX3
      ENTROP(CO) = 40.24+2.8415*TX-0.1694*TX2+0.004344*TX3
      ENTHAL(CO) = 0.6189*TX+0.010572*TX2-0.0001317*TX3-1.93 

C       the following values for h and s are not evident.......!!!!!!!

C*     C2H6
      HEATCG(C2H6) = 0.98832+4.3656*TX+0.180258*TX2+0.0026479*TX3
      ENTROP(C2H6) = 0.
      ENTHAL(C2H6) = 0.63096*TX+0.0082485*TX2-0.000056527*TX3-1.9468

C*     NH3
      HEATCG(NH3) = 6.46663+0.63121*TX+0.0199784*TX2-0.001397179*TX3
      ENTROP(NH3) = 58.915    !cal/mol*K, at 1000 K
      ENTHAL(NH3) = -3.169    !kcal/mol, at 1000 K;   0.63096*TX+0.0082485*TX2-0.000056527*TX3-1.9468

C*     N2O
      HEATCG(N2O) = 5.43004+1.55628*TX-0.1047332*TX2+0.002640968*TX3
      ENTROP(N2O) = 66.157    !cal/mol*K, at 1000 K
      ENTHAL(N2O) = 27.763    !kcal/mol, at 1000 K;   0.63096*TX+0.0082485*TX2-0.000056527*TX3-1.9468

C*     H2S
      HEATCG(H2S) = 7.4273+0.1580728*TX+0.0358632*TX2-0.001661138*TX3
      ENTROP(H2S) = 60.373    !cal/mol*K, at 1000 K
      ENTHAL(H2S) = 1.788     !kcal/mol, at 1000 K;  0.63096*TX+0.0082485*TX2-0.000056527*TX3-1.9468

C*     convert CP to (J/mol*K)
C*     convert S to     "
C*     convert H to  (J/mol)
      DO 10  J = 1, NSPECY
         HEATCG(J) = HEATCG(J) * 4.184
         ENTROP(J) = ENTROP(J) * 4.184
         ENTHAL(J) = ENTHAL(J) * 4184.
 10   CONTINUE

      RETURN
      END          ! (PREPOP) 



