
      SUBROUTINE CHARTR (T,XXO2,RT,PHIS)

************************************************************************
* LAST CHANGE AT : 14.01.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : COMBUS, CHARPR                                           *
* CALLS     : Z_SHWO, Z_SPLT, Z_QGEN, Z_QTRN, CRRECT                   *
************************************************************************
* COMMON in : ACHAR, ECHAR, KDIFFU, MWS(1), PI, RG, TEMP(K), PRESS(1), *
*             COALM(FIXED), NCLASS, RHOS(M), DIAM(I), WRTOUT(3),       *
*             WRTSCR                                                   *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed :    T       bulk temperature (K)                             *
*             XXO2    molar oxygen fraction (-)                        *
*             RT(I)   reaction rate (mol/kgs)                          *
*             PHIS(I) mol C per molO2 used                             *
************************************************************************
* PURPOSE   : Calculates char burnout times and rates                  *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'
      INCLUDE 'KINETICS.FTN'

      REAL*8  Z_SPLT,  ! used C per reacted O2
     &      Z_SHWO,  ! sherwood number
     &      Z_QGEN,  ! heat generated
     &      Z_QTRN   ! heat transported

      REAL*8  BP,      ! auxiliary
     &      BPM,     ! auxiliary
     &      Cc,      ! auxiliary
     &      DIFF,    ! diffusion coefficient normalized (m2/s)
     &      FACTOR   ! density correction factor (-)

      REAL*8  KD,      ! diffusion coefficient (m2/s)
     &      KR,      ! reaction coefficient (m2/s)
     &      KS,      ! reaction per surface (kg/m2sPa)
     &      QG,      ! heat generated by reaction (W)
     &      QT,      ! heat transferred to gas  (W)
     &      RTP      ! combustion rate of a single particle (kg/S)

      REAL*8  SHW,        ! Sherwood number    (-)    
     &      SMO,        ! initial particle mass (kg)
     &      SK,         ! surface coefficient (m/kg**1/3)
     &      T,          ! bulk temperature    (K)
     &      TBOUND,     ! particle boundary temperature (K)
     &      TP(NI),     ! particle temperature (K)
     &      XXO2        ! average oxygen concentration

      REAL*8  BT(NI),     ! particle burnout time (s)
     &      PHIS(NI),   ! CO - CO2 split
     &      RS(NI),     ! particle shrinking rate    (m/s)
     &      RT(NI)      ! specific combustion rate  (mol/kgs)

      REAL*8  DELTP,      ! regula falsi stepsize
     &      X1, X2, E1, E2, EMAX, ! iteration variables
     &      ERR         ! itearation error

      REAL*8  RTW(NI),  ! specific H2O gasification rate (mol/kgs)
     &        RTC(NI),  ! specific CO2 gasification rate (mol/kgs)
     &        RTH(NI),  ! specific methanation rate (mol/kgs)
     &        RTWP,     ! H2O gasification rate of a single particle (kg/S)
     &        RTCP,     ! CO2 gasification rate of a single particle (kg/S)
     &        RTHP      ! methanation rate of a single particle (kg/S)
      
      REAL*8  XXH2O,XXCO,XXCO2,XXH2,XXCH4,XH2S,XH2OS,XCO2S
      COMMON /KINE/FACTOR,KD,SMO

      INTEGER  I,       ! size class indicator
     &         INDEX,   ! iteration index 
     &         MLOOP    ! iteration loop counter

       IF (WRTSCR) write(*,*) '  CHARTR '

       DO 50 I   = 1,NCLASS
C*            Initialize loop counter MLOOP, convergence indicator INDEX,
C*            increment for particle temperature DELTP, and error limit EMAX
           MLOOP = 1
           INDEX = 0
           DELTP = 20.
           EMAX  =  0.001

C*            Starting mass SMO of a char particle of size DIAM(I) (kg)
           SMO = PI * DIAM(I)**3. * RHOS(COKE) / 6.

C*            Surface area coefficient SK  (m/kg**1/3)
          IF (COALM(FIXED) .GT. 0.) THEN
           SK = (3. / (4. * PI * RHOS(COAL) * COALM(FIXED))) ** (1./3.)
          ELSE
           SK = 1. / RHOS(COKE) ** (1./3.)
          ENDIF

C*            Sherwood-Number
           SHW = Z_SHWO(I,T)

C*            Molar ratio of burnt carbon to needed oxygen PHI  (mol C/mol O2)
C*            (dependent on particle size DIAM(I))
            PHIS(I) = Z_SPLT(I,T,XXO2)

C*            First estimation for the particle temperature TP  (K)
           TP(I) = T + 0.1

   10     CONTINUE

C*            Temperature of the boundary film TBOUND  (K)
           TBOUND = (T + TP(I)) / 2.

           IF (TP(I)-T .LT. 0.01) GOTO 20

C*            Diffusion rate coefficient of oxygen DIFF (m2/s) at TBOUND and
C*            diffusion rate KD  (mol/m*s)
           DIFF = KDIFFU * (TBOUND / 298.)**1.5
           KD = 2. * PI * SHW * PHIS(I) * DIFF * PRESS(1) /(RG * TBOUND)

C*            Reaction rate coeff. on the char surface KS (kg C/m2*s*Pa O2)
           KS = ACHAR * EXP(ECHAR / TP(I)) 

C*            Kinetic reaction rate KR on the char surface  (mol/m2*s)
C*            (reduction of the kinetic reaction rate due to ash on the
C*            particle surface by FACTOR)
           FACTOR = RHOS(COAL) * COALM(FIXED) / RHOS(COKE)
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)

C*            Combustion rate of a single particle (mol/s)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XXO2 /
     &                 (KD + KR * DIAM(I) / 2.) 
       
C*            Weight specific combustion rate at the beginning of burning 
C*                                                  (mol/kg s)
           RT(I) = RTP / SMO

C*            Particle shrinking rate at the beginning of burning RS(I) (m/s)
           RS(I) = 2. / 3. * SK * SMO**(-2. / 3.) * RTP * MWS(1)

C*            Burnout time BT(I)  (s)
           BP  = 2./3. * KD * KR * MWS(COALC) * SK**2.
           BPM = (SMO * COALM(FIXED)) ** (1./3.)

           IF ((BP .GT. 0.) .AND. (XXO2 .GT. 0.)) THEN
             BT(I)  = (SK * KR * BPM**2. + 2. * KD * BPM) / (BP * XXO2)
           ELSE
             BT(I) = 0.
           ENDIF

!zc
      XXH2O = 0.05         !used only to cal. particle temp.
      XXCO = 0.15
      XXCO2 = 0.075
      XXH2 = 0.10
      XXCH4 = 0.005
      Cc = COALM(FIXED)/0.012

      XH2OS = XXH2*XXCO*PRESS(1)**2./(1.01325E5*exp(17.29-16326/TP(I)))
      RTWCO(I) = KH2OG*exp(-22645./TP(I))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTW(I) = max(0.0,RTWCO(I)*(XXH2O*PRESS(1)-XH2OS))        !mol C/kg s  C + H2O = CO + H2

      XCO2S = (XXCO*PRESS(1))**2/(1.01325E5*exp(20.92-20282./TP(I)))
      RTCCO(I) = KCO2G*exp(-22645./TP(I))*Cc
      RTCAN(I) = RTCCO(I)
      RTC(I) = max(0.0,RTCCO(I)*(XXCO2*PRESS(1)-XCO2S))        !mol C/kg s  C + CO2 = 2CO

      XH2S = sqrt(1.01325E5*XXCH4*PRESS(1)/exp(-13.43+10999/TP(I)))	  !Pa
      RTHCO(I) = KH2G*exp(-7.087-8078./TP(I))*Cc
      RTHAN(I) = RTHCO(I)
      RTH(I)= max(0.0,RTHCO(I)*(XXH2*PRESS(1)-XH2S))           !mol C/kg s   C + 2H2 = CH4

      RTWP = RTW(I)*SMO          !mol C/s
      RTCP = RTC(I)*SMO          !mol C/s
      RTHP = RTH(I)*SMO          !mol C/s
!zc
      GOTO 20     !Assume Tp = Tbed

C*           Calc. heat generation rate QG (W) and heat transfer rate QT (w)

           QG = Z_QGEN(I,T,TP(I),XXO2,RTP,RTWP,RTCP,RTHP)

           QT = Z_QTRN(I,T,TP(I))


!zc           IF (QG .LE. 0.) GOTO 20

           ERR = (QG - QT) / QG

C*            Adjust estimated particle temperature TP  (K)
           CALL CRRECT (MLOOP, INDEX, DELTP, X1, X2, TP(I), E1, E2, ERR,
     &               EMAX)

C*            Repeat calculation with a new estimation for TP, if INDEX = 1
           IF (INDEX .NE. 2)  THEN
C*               In case of loop count MLOOP exceeds limit
             IF (MLOOP .EQ. 100)  THEN
                WRITE (*,*)'Iteration fails in CHARTR at Class ',I
                WRITE(*,*) 'X',XXO2,' TP',TP(I),' ERR ',ERR
                    TP(I) = TBED
                    RT(I) = 0.
                GOTO 20
             END IF
              MLOOP = MLOOP + 1
               GOTO 10
           ENDIF

 20     CONTINUE
      TP(I) = TBED   !Assume Tp = Tbed

 50   CONTINUE

C*     **************** TESTWRITER *********************************

       IF (.NOT. WRTOUT(3)) RETURN

       OPEN(UNIT=32,FILE='CHARTR.DAT',STATUS='UNKNOWN')

       WRITE(32,*) 'dia(my) tmp.dff(K) brnout t(s) ',
     &   ' shrk rt (m/s) cmb.rt(mol/kgs) , phi '
 
      DO I = 1, NCLASS
      write(32,1001) DIAM(I)*1.E6,TP(I)-TBED,BT(I),RS(I),RT(I),PHIS(I)
      ENDDO

 1001  FORMAT(F7.0,2X,F7.2,2X,F10.2,2X,F10.7,2X,F12.5,2X,F10.7)

      CLOSE(32)

      RETURN
 
      END   ! (CHARTR)


