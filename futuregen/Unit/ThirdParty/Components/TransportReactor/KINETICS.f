C*                    *****  KINETICS.FTN  *****
C*
C*            units used:
C*        length         (m)
C*        weight         (kg)
C*        time           (s)
C*        temperature    (K)
C*        mole           (mol)
C*  --->  pressure       (Pa)
C*          etc.
C*
C***********************************************************************
C*
C*       constants 
C*
      REAL*8  KH2OG,KCO2G,KH2G

      PARAMETER (KH2OG = 9.1784E-3)     ! C + H2O pre-exp. factor
      PARAMETER (KCO2G = 9.1784E-3)     ! C + CO2 pre-exp. factor
      PARAMETER (KH2G = 9.8692E-2)      ! C + H2 pre-exp. factor, 9.8692E-6

C*
C***********************************************************************
