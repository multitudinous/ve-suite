
      REAL*8 FUNCTION Z_QGEN(I,T,TP,XXO2,RTT,RTWP,RTCP,RTHP)

************************************************************************
* CALLED BY : CHARTR                                                   *
* CALLS     : Z_SPLT                                                   * 
************************************************************************
* COMMON in :   MWS(COALC)   molar weight of carbon (kg/m3)            *
*               ENTHAL(J)    enthalpy of gas (J/mol)                   *
*               HEATCP(M)    heat capacity of solids (J/kgK)           *
* out       :   -                                                      *
* in & out  :                                                          *
************************************************************************
* received  : I         size indicator                                 *
*             T         bulk temperature (K)                           *
*             TP        particle temperature (K)                       *
*             XXO2      molar fraction of oxygen (-)                   *
*             RRT       reaction rate (moleC/s)                        *
* returned  : Z_QGEN    generated energy (W)                           *
************************************************************************
* PURPOSE   : calculates heat generation due to burning rate RT        *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    Z_SPLT, FCO2  ! fraction of CO2 in combust. products
      REAL*8    HC,           ! enthalpy of coke
     &          HRCO,         ! heat of C + 0.5 O2 = CO 
     &          HRCO2,        ! heat of C + O2 = CO2
     &          HRW,          ! heat of C + H2O = CO + H2
     &          HRC,          ! heat of C + CO2 = 2 CO
     &          HRH,          ! heat of C + 2 H2 = CH4
     &          HTOT,         ! total enthalpy
     &          RTT,          ! carbon combustion rate (mol/s)
     &          RTWP,         ! H2O gasification rate (mol/s)
     &          RTCP,         ! CO2 gasification rate (mol/s)
     &          RTHP,         ! methanation rate (mol/s)
     &          T,            ! bulk temperature 
     &          TP,           ! particle temperature
     &          XXO2          ! oxygen molar fraction
      INTEGER   I             ! size indicator

    
C*     molar weight of carbon MC = 0.012

C*     mole fractions FCO2 (mole CO2/(mole CO + mole CO2))
!zc      FCO2 = Z_SPLT (I,T,XXO2) - 1.	! This is not correct
      FCO2 = 2.0/Z_SPLT (I,T,XXO2) - 1.

C*     enthalpy of coke HC  (J/mol C)
      HC = HEATCP(COKE) * (TP - 298.) * MWS(COALC)

C*     heat of the two reactions HRCO and HRC2  (J/mole C)
      HRCO  =(ENTHAL(CO)  - 0.5 * ENTHAL(O2) - HC + HEATFM(CO))
      HRCO2 = (ENTHAL(CO2) -       ENTHAL(O2) - HC + HEATFM(CO2)) 

      HRW = (ENTHAL(CO)+HEATFM(CO))+ENTHAL(H2)
     &      -(ENTHAL(H2O)+HEATFM(H2O))-HC                                    !C + H2O
      HRC = 2*(ENTHAL(CO)+HEATFM(CO))-(ENTHAL(CO2)+HEATFM(CO2))-HC           !C + CO2
      HRH = (ENTHAL(CH4)+HEATFM(CH4))-2*ENTHAL(H2)-HC                        !C + CO2

C*     heat generation HTOT  (J/mole C)
      HTOT = (1. - FCO2) * HRCO + FCO2 * HRCO2

C*     heat generation rate QGENRT  (W)
      Z_QGEN = - HTOT * RTT - HRW*RTWP - HRC*RTCP - HRH*RTHP

      RETURN
      END          ! (Z_QGEN)