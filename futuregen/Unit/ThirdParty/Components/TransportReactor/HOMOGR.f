
      SUBROUTINE HOMOGR(X,HOMRAT)

************************************************************************
* LAST CHANGE AT : 20.11.96                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : FX                                                       *
* CALLS     :                                                          *
************************************************************************
* COMMON in : AREA(K), CORAVG(K), EMUAVG(K), HEIGHT(K), KCOMMON,       *
*             NSPECY, POSTPR, PRESS(1), RG, TEMP(K,P)                  *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : X       = molar gas fractions in phases (mol/mol)        *
*             HOMRAT  = homogeneous reaction rate    (mol/s)           *
************************************************************************
* PURPOSE   : delivers the homogeneous gas reaction rates              *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    C(50,3),      ! gas concentrations (mol/m3)
     &          X(50),        ! gas fractions (mol/mol)
     &          HOMRAT(15,3), ! homog. reaction rate (mol/s)
     &          REA(20),      ! rectio constant temp. dependent (dependent)
     &          KO(20),       ! reaction constant (dependent)
     &          ER(20),       ! activation energy div. by gas constant E/R
     &          EX(20,15),    ! exponent for reaction order of specy
     &          PR(20,15),    ! steciometric coefficient +=product, -=educt
     &          VOLUME,       ! volume of actual cell
     &          Z_EXP         ! function A**EX
      INTEGER   J,            ! specy indicator
     &          K,            ! cell indicator
     &          LR,           ! reaction counter
     &          N,            ! reduced number of species
     &          NPHASE(3),    ! pointer for fraction array
     &          P,            ! index for core, annulus and bubble
     &          NREA          ! number of reactions considered
      LOGICAL   AD(20,15)     ! does the specy influence the reaction (t,f)

        K = KCOMMON

        IF (POSTPR) THEN
            N = NPOST
        ELSE
            N = NPRE
        ENDIF

        NPHASE(CORE) = 0     ! pointers in array and jacobian for
        NPHASE(ANNU) = N     ! COre ANnulus and BUbble
        NPHASE(BUBB) = 2*N   ! relative values

C*        calc. of species concentrations in gas (mol/m3)
       DO P = 1,NP
        DO J=1,N
           C(J,P) = X(J+NPHASE(P)) * PRESS(1)/RG/TEMP(K,P)
        ENDDO  
       ENDDO

       DO P=1,3
         DO J = 1,NSPECY
           IF (C(J,P) .LT. 0.) C(J,P) = 0.
          ENDDO
       ENDDO

C*     homogeneous gas reactions
C* J=  1   2   3   4   5   6   7   8   9   10  11   12  13  14
C*     CO  CO2 H2O H2  N2  SO2 NO  NO2 CH4 O2  C2H6 NH3 N2O H2S

C*        number of reactions implemented
        NREA = 9

C*            initialization
          DO J=1, NSPECY
           DO LR = 1, NREA
             AD(LR,J) = .FALSE.
             EX(LR,J) = 0.
             PR(LR,J) = 0.
           ENDDO
          ENDDO

C*  1.   CO + 0.5 O2 --> CO2  (Howard & Fine) [CO]^1 [O2]^0.5 [H2O]^0.5

       KO(1)     =   1.E7 ! 5.E6 ! 1.3E4    !  1.E4  !  1.3E8   
         ER(1)     = -15106.  !-8057.  !  - 15106.
           AD(1,CO)  = .TRUE.
           AD(1,O2)  = .TRUE.
           AD(1,H2O) = .TRUE.
             EX(1,CO)  = 1.
             EX(1,O2)  = 0.3
             EX(1,H2O) = 0.5
           PR(1,CO)  = -1.
           PR(1,O2)  = -0.5
           PR(1,CO2) = 1.

           IF (.NOT. POSTPR) GOTO 200

C* 2.      H2 + 0.5 O2 --> H2O   

       KO(2)     = 1.E6
         ER(2)     = - 10000.
           AD(2,H2)  = .TRUE.
           AD(2,O2)  = .TRUE.
             EX(2,H2)  = 1.
             EX(2,O2)  = 0.5
           PR(2,H2)  = -1.
           PR(2,O2)  = -0.5
           PR(2,H2O) = 1.

C* 3.     CH4 + 1.5 O2 --> CO + 2 H2O  (Dryer & Glassman) [CH4]^0.8 [O2]^0.7

       KO(3)     =  0. ! 1.E9
       ER(3)     = - 24000.
       AD(3,CH4) = .TRUE.
       AD(3,O2)  = .TRUE.
       EX(3,CH4) = 0.7
       EX(3,O2)  = 0.8
       PR(3,CH4) = -1.
       PR(3,O2)  = -1.5
       PR(3,CO)  =  1.
       PR(3,H2O) =  2.
       
C* 4.     C2H6 + 2.5 O2 --> 2 CO + 3 H2O 

       KO(4)      =  1.E9
         ER(4)      = -10000.    !too small
           AD(4,C2H6) = .TRUE.
           AD(4,O2)   = .TRUE.
             EX(4,C2H6) =  1.
             EX(4,O2)   =  1.
           PR(4,C2H6) = -1.
           PR(4,O2)   = -2.5
           PR(4,CO)   =  2.
           PR(4,H2O)  =  3.

C*  5.    H2S + 1.5 O2 --> SO2 + H2O

       KO(5)     = 0.  ! 1.E9
         ER(5)     = - 24000.
           AD(5,H2S) = .TRUE.
           AD(5,O2)  = .TRUE.
             EX(5,H2S) =  1.
             EX(5,O2)  =  1.
          PR(5,H2S) = -1.
          PR(5,O2)  = -1.5
          PR(5,SO2) =  1.
          PR(5,H2O) =  1.

C*  6.       N20 --> N2 + 0.5 O2

       KO(6)     = 5.2E9 
         ER(6)     = - 27000.
           AD(6,N2O) = .TRUE.
             EX(6,N2O) =  1.
          PR(6,N2O) = -1.
          PR(6,N2)  =  1.
          PR(6,O2)  =  0.5

C*  7.       NO + 2/3 NH3 --> 5/6 N2 +  H2O   (I)

       KO(7)     =  2.45E14 
         ER(7)     = - 27680.
           AD(7,NO) = .TRUE.
           AD(7,NH3) = .TRUE.
             EX(7,NO)  =  1.
             EX(7,NH3) =  1.
          PR(7,NO)  = -1.
          PR(7,NH3) = -0.66
          PR(7,N2)  =  0.8333
          PR(7,H2O) =  1.

C*  8.       NH3 + 5/4 O2 --> NO + 1.5 H2O        (III)

       KO(8)     = 2.21E14 
         ER(8)     = - 38160.
           AD(8,NH3) = .TRUE.
             EX(8,NH3) =  1.
          PR(8,NH3) = -1.
          PR(8,O2)  = - 1.25
          PR(8,NO)  =  1.
          PR(8,H2O) =  1.5

C*  9.       CH4 + H2O --> CO + 3 H2        (New)
 
       KO(9)     = 0.0  !3.00E+11
         ER(9)     = - 15097.
           AD(9,CH4) = .TRUE.
           AD(9,H2O) = .TRUE.
           EX(9,CH4) =  1.
           EX(9,H2O) =  1.
          PR(9,CH4) = -1.
          PR(9,H2O) = -1.
          PR(9,CO)  =  1.
          PR(9,H2) =  3.

 200     CONTINUE

         DO P=1,3
           DO J = 1,NSPECY
               HOMRAT(J,P) = 0.
           ENDDO
         ENDDO

C*     calculate reaction rates (mol/s) and jacobian matrix

      DO 100 P=1,3             

C*     cell volume
       IF (P .EQ. 1) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))*CORAVG(K)*EMUAVG(K)
       ELSE IF (P .EQ. 2) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))*(1.-CORAVG(K))
       ELSE IF (P .EQ. 3) THEN
           VOLUME = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                     *CORAVG(K)*(1.-EMUAVG(K))
       ENDIF

      DO 10 LR = 1, NREA
         if(LR.eq.8.and.C(O2,P).eq.0.0) KO(LR) = 0.0     !zc
         REA(LR) = KO(LR) * EXP(ER(LR)/TEMP(K,P)) * VOLUME
           DO J=1,N
              IF (AD(LR,J)) THEN
                REA(LR) = REA(LR)*Z_EXP(C(J,P),EX(LR,J))
              ENDIF
           ENDDO
      
           DO J = 1, N
              HOMRAT(J,P) = HOMRAT(J,P) + REA(LR) * PR(LR,J)
           ENDDO

  10   CONTINUE
 100  CONTINUE

      RETURN
      END         ! (HOMOGR)

