
      SUBROUTINE HETGR(X,HETRAT)

************************************************************************
* LAST CHANGE AT : 20.11.96                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : FX                                                       *
* CALLS     :                                                          *
************************************************************************
* COMMON in : AREA(K), CORAVG(K), EMUAVG(K), HEIGHT(K), KCOMMON,       *
*             MCAO(K,P), MCOK(K,P), MCOKD(K,P), MINR(K,P), NPOST,      *
*             NSPECY, POSTPR, PRESS(1), RG, RHOS(M), TEMP(K,P)         *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : C       = gas concentrations in phases (mol/m3)          *
*             HETRAT  = heterogeneous reaction rate  (mol/m3 s)        *
************************************************************************
* PURPOSE   : delivers the heterogeneous gas reaction rates (mol/m3)   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    C(50,3),      ! gas concentrations (mol/m3)
     &          X(50),        ! gas fractions (mol/mol)
     &          HETRAT(15,2), ! heterog. reaction rate (mol/m3s)
     &          REA(20),      ! rectio constant temp. dependent (dependent)
     &          KO(20),       ! reaction constant (dependent)
     &          ER(20),       ! activation energy div. by gas constant E/R
     &          EX(20,15),    ! exponent for reaction order of specy
     &          PR(20,15),    ! steciometric coefficient +=product, -=educt
     &          VOLUME,       ! cell volume (m3)
     &          Z_EXP         ! function A**EX
      INTEGER   J,            ! specy indicator
     &          K,            ! cell indicator
     &          LR,           ! reaction counter
     &          N,            ! reduced species counter
     &          NPHASE(2),    ! fraction array pointer
     &          P,            ! index for core and annulus
     &          NREA          ! number of reactions considered
      LOGICAL   AD(20,15)     ! does the specy influence the reaction (t,f)

      REAL*8 ak3,f3
      INTEGER Kold,kkk
      common /aaa/Kold

C        write(*,*) '     HETGR'

        K    = KCOMMON

        IF (.NOT. POSTPR) RETURN

        N = NPOST

        NPHASE(CORE) = 0     ! pointers in array and jacobian for
        NPHASE(ANNU) = N     ! Core and Annulus 

C*        calc. of species concentrations in gas (mol/m3)
       DO P = 1,2
        DO J=1,NSPECY
           C(J,P) = X(J+NPHASE(P)) * PRESS(1)/RG/TEMP(K,P)
           IF (C(J,P) .LT. 0.) C(J,P) = 0.
       ENDDO
      ENDDO  

       DO P = 1,2
         DO J = 1,NSPECY
               HETRAT(J,P) = 0.
         ENDDO
       ENDDO

      DO 100 P=1,2             

C*     heterogeneous gas reactions
C* J=  1   2   3   4   5   6   7   8   9   10  11   12  13  14
C*     CO  CO2 H2O H2  N2  SO2 NO  NO2 CH4 O2  C2H6 NH3 N2O H2S

C*        number of reactions implemented
        NREA = 14


          DO J=1, NSPECY
           DO LR = 1, NREA
             AD(LR,J) = .FALSE.
             EX(LR,J) = 0.
             PR(LR,J) = 0.
           ENDDO
          ENDDO


C*  1.   NH3 + 3/4 O2 --> 1/2 N2 + 3/2 H2O  (VII)

         KO(1)     =   MCOK(K,P) / RHOS(COKE) * 9.83E6 ! * 0. ! JH
         ER(1)     = -10000.
           AD(1,NH3)  = .TRUE.
             EX(1,NH3)  = 1.
           PR(1,NH3)  = -1.
           PR(1,O2)  = -0.75
           PR(1,N2)  = 0.5
           PR(1,H2O) = 1.5

C*  2.   NH3 + 3/4 O2 --> 1/2 N2 + 3/2 H2O    (VIII) 

         KO(2)     =    MCAO(K,P) / RHOS(LIME) * 1.39E6
         ER(2)     = -10000.
           AD(2,NH3)  = .TRUE.
             EX(2,NH3)  = 1.
           PR(2,NH3)  = -1.
           PR(2,O2)  = -0.75
           PR(2,N2)  = 0.5
           PR(2,H2O) = 1.5

C* 3.      NH3 + 1.25 O2 --> NO + 1.5 H2O      (IX)

         KO(3)     =   MCOK(K,P) / RHOS(COKE) * 1.09E7
         ER(3)     = -10000.
           AD(3,NH3) = .TRUE.
             EX(3,NH3)  = 1.
           PR(3,NH3)  = -1.
           PR(3,O2)   = -1.25
           PR(3,NO)   = 1.
           PR(3,H2O)  = 1.5

C* 4.      NH3 + 1.25 O2 --> NO + 1.5 H2O    (X)

         KO(4)     =    MCAO(K,P) /RHOS(LIME) * 5.74E6
         ER(4)     = -10000.
           AD(4,NH3) = .TRUE.
             EX(4,NH3)  = 1.
           PR(4,NH3)  = -1.
           PR(4,O2)   = -1.25
           PR(4,NO)   = 1.
           PR(4,H2O)  = 1.5

C* 5.    NO + CO --> 1/2 N2 + CO2      (XVII)

       KO(5)     =  MCOK(K,P) / RHOS(COKE) * 5.21E8 * 3.71  !*5.0E5 ! (mol/kmol)^0.19  
       ER(5)     =  -20900.
       AD(5,NO) = .TRUE.
       AD(5,CO)  = .TRUE.
       EX(5,NO) =  0.43
       EX(5,CO)  = 0.38
       PR(5,NO) =  -1.
       PR(5,CO)  = -1.
       PR(5,N2)  =  0.5
       PR(5,CO2) =  1.

C* 6.    NO + CO --> 1/2 N2 + CO2      (XVIII)

       KO(6)     =    MCAO(K,P) / RHOS(LIME) * 2.13E7 /1000. !(mol/kmol) 
       ER(6)     =  -8920.
       AD(6,NO) = .TRUE.
       AD(6,CO)  = .TRUE.
       EX(6,NO) =  1.
       EX(6,CO)  = 1.
       PR(6,NO) =  -1.
       PR(6,CO)  = -1.
       PR(6,N2)  =  0.5
       PR(6,CO2) =  1.
       
C* 7.   NO + H2 --> 1/2 N2 + H2O    (XX)

       KO(7)     =    MCAO(K,P) /RHOS(LIME) * 2.E10 /1000. !(mol/kmol)
       ER(7)     =  -8623.
       AD(7,NO)  = .TRUE.
       AD(7,H2)  = .TRUE.
       EX(7,NO)  = 1.
       EX(7,H2)  = 1.
       PR(7,NO)  = -1.
       PR(7,H2)  = -1.
       PR(7,N2)  =  0.5
       PR(7,H2O) =  1.

C* 8.   N2O --> N2 + 0.5 O2        (XXI)

       KO(8)     =  MCOKD(K,P) * 43.5 
       ER(8)     =  -10000.
       AD(8,N2O) = .TRUE.
       EX(8,N2O) = 1.
       PR(8,N2O) = -1.
       PR(8,N2)  =  1.
       PR(8,O2)  =  0.5

C* 9.   N2O --> N2 + 0.5 O2      (XXII)

       KO(9)     =  MINR(K,P) * 1.7E6
       ER(9)     =  -22150.
       AD(9,N2O) = .TRUE.
       EX(9,N2O) = 1.
       PR(9,N2O) = -1.
       PR(9,N2)  =  1.
       PR(9,O2)  =  0.5

C*              reducing reactions

C* 10.   NO + 2/3 NH3 --> 5/6 N2 + H2O      (V)

       KO(10)     =  MCOK(K,P) * 2.36E6 *1.62 * 0.
       ER(10)     =  -10000.
       AD(10,NO)  = .TRUE.
       AD(10,NH3) = .TRUE.
       EX(10,NO) = 0.64
       EX(10,NH3) = 0.29
       PR(10,NO)  = -1.
       PR(10,NH3) = -0.6666
       PR(10,N2)  =  0.8333
       PR(10,O2)  =  1.

C* 11.   NO + 2/3 NH3 --> 5/6 N2 +  H2O     (VI)

       KO(11)     =  MCAO(K,P) * 2.3E10 / 1000. * 0.
       ER(11)     =  -10000.
       AD(11,NO)  = .TRUE.
       AD(11,NH3) = .TRUE.
       EX(11,NO)  =  1.
       EX(11,NH3) =  1.
       PR(11,NO)  = -1.
       PR(11,NH3) = -0.66666
       PR(11,N2)  =  0.83333
       PR(11,O2)  =  1.

C* 12.   NO + CO --> 1/2 N2 +  CO2      (XVI2)

       KO(12)     =  MCOK(K,P) *2.4E5 
       ER(12)     =  -20400.
       AD(12,NO)  = .TRUE.
       EX(12,CO)  =  1.
       PR(12,NO)  = -1.
       PR(12,CO)  = -1.
       PR(12,N2)  =  0.5
       PR(12,CO2) =  1.

C* 13.   NO + CO --> 1/2 N2 +  CO2      (XVI3)

       KO(13)     =  MCOK(K,P) * 8.9E5 * 1000.
       ER(13)     =  -31700.
       PR(13,NO)  = -1.
       PR(13,CO)  = -1.
       PR(13,N2)  =  0.5
       PR(13,CO2) =  1.

C* 14.   H2O + CO <--> H2 +  CO2      (New)

       f3=1.0E-3*MINR(K,P)*exp(-8.91+5553./TEMP(K,P))
     &   *(PRESS(1)/101325)**(0.5-PRESS(1)/(250.*101325))
       ak3=exp(-4.033+4393.83/TEMP(K,P))

       KO(14) = 1.95636E+9*f3*(C(CO,P)*C(H2O,P)-C(CO2,P)*C(H2,P)/ak3)/
     &         (PRESS(1)/(8.314*TEMP(K,P)))**2.
       ER(14)     =  -13969.
       PR(14,H2O)  = -1.
       PR(14,CO)  = -1.
       PR(14,H2)  =  1.
       PR(14,CO2) =  1.

C*     cell volume
       IF (P .EQ. 1) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))*CORAVG(K)*EMUAVG(K)
       ELSE IF (P .EQ. 2) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))*(1.-CORAVG(K))
       ELSE IF (P .EQ. 3) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                     *CORAVG(K)*(1.-EMUAVG(K))
       ENDIF


C*     calculate reaction rates (mol/m3s) and jacobian matrix

      DO 10 LR = 1, NREA
         if(LR.le.4.and.C(O2,P).eq.0.0) KO(LR) = 0.0  !zc
         if(LR.eq.12.OR.LR.eq.13) then
          if(C(CO,P).eq.0.0.OR.C(NO,P).eq.0.0) KO(LR) = 0.0  !zc
         endif
         REA(LR) = KO(LR) * EXP(ER(LR)/TEMP(K,P)) * VOLUME

         IF(LR.eq.14) GOTO 5

           DO J=1,NSPECY
              IF (AD(LR,J)) THEN
                REA(LR) = REA(LR)*Z_EXP(C(J,P),EX(LR,J))
              ENDIF
           ENDDO

   5   CONTINUE

           DO J = 1, NSPECY
              HETRAT(J,P) = HETRAT(J,P) + REA(LR) * PR(LR,J)
c      IF(J.LE.4.AND.K.NE.Kold) THEN
c      KKK = 100 + J
c      WRITE(kkk,111)LR,REA(LR),REA(LR) * PR(LR,J)
c      ENDIF
c111   FORMAT(I4,4E15.6)
           ENDDO
  10   CONTINUE
 100  CONTINUE

      RETURN
      END         ! (HETGR)

