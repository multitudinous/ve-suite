
      SUBROUTINE CYCLON(M) 

************************************************************************
* LAST CHANGE AT : 01.03.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : PREPOP, POPUL                                            *
* CALLS     : Z_RHOG, Z_VIS, Z_VELO                                    *
************************************************************************
* COMMON in : AREA(K), CMP(1) ,CYCDAT(1-9),DIAM(I), EFFTOP(I,M),       *
*             ELFLUX(I,M), MEAND(M), NCLASS, NCYC, PART(I,M)           *
*             PI, PRESS(1), PRESS(3), RHOS(M), TBED, U(K), WRTOUT(17)  *
* out       : EFFCYC(I,M),                                             *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates cyclone separation efficiency according to    *
*            Krambrock, W., Aufbereitungstechnik, Vol.12, No.7 (1971)  *
*            391-401                                                   *
************************************************************************

       INCLUDE 'PARAMTR.FTN'
       INCLUDE 'COMMONBL.FTN'

       REAL*8  Z_RHOG,RHOG ! gas density (kg/m3)
       REAL*8  Z_VIS,VISG  ! gas viscosity 
C      REAL*8  Z_VELO  ! terminal particle velocity (m/s)
       REAL*8  ALFA,   ! acceleration coefficient (-)
     &         CONST,  ! separation constant
     &         FF,     ! aux
     &         HH,     ! aux
     &         REYNOL  ! Reynolds number
       REAL*8  HT,     ! aux
     &         UT,     ! aux
     &         VR,     ! aux
     &         RR,     ! aux
     &         RT,     ! aux
     &         SUM     ! auxiliary
       REAL*8  UU,     ! aux
     &         VT,     ! aux
     &         XI,     ! aux
     &         XI_E,   ! aux
     &         XI_T,   ! aux
     &         W50,    ! weight fraction of 50 % separation (-)
     &         DD(NM)  ! average particle diameter entering cyclone (m)
       REAL*8  GASFLO, ! gas flow (m3/s)
     &         VIN,    ! gas entrance velocity (m/s)
     &         CARY,   ! load of solid in gas (kg(s)/kg(g))
     &         CARYMX, ! maximum load of solid in gas (kg(s)/kg(g))
     &         FRICT,  ! friction coefficient (-)
     &         EFIN,   ! entrance separation efficiency (-)
     &         EFEDDY(NI), ! eddy separation efficiency (-)
     &         cst     ! constant used to adjust cyclone efficiency
       INTEGER I,      ! particle size indicator
     &         M       ! solid material indicator

         cst = 1.0  !zc
       
       IF (WRTSCR) write(*,*) ' CYCLON' 
       
      RHOG = Z_RHOG(TBED,PRESS(1))
      VISG = Z_VIS(TBED)

C*       estimate average terminal velocity /(m/s)
      W50 = 0.1
  
C*       gas entering cyclone
      GASFLO = U(NCELLS)*AREA(NCELLS) / NCYC

      HH   = CYCDAT(1) / CYCDAT(4)
      HT   = CYCDAT(3) / CYCDAT(4)
      RR   = CYCDAT(2) / CYCDAT(4)
      RT   = CYCDAT(4)
      FF   = CYCDAT(6)*CYCDAT(7) / (PI*CYCDAT(4)**2)
      ALFA = CYCDAT(8)
      CONST= CYCDAT(9)

C*       cyclone entrance velocity
      VIN = GASFLO/CYCDAT(6)/CYCDAT(7)

!zc      IF (PRESS(3) .LE. 0.) THEN
          PRESS(3) = 33.33 * VIN**2.
!zc      ENDIF

C*       carry of kg_solid per kg_gas 
             CARY = 0.
             SUM = 0. 
          DO I=1,NCLASS
            CARY = CARY + ELFLUX(I,BED)*AREA(CMP(1))/GASFLO/RHOG
     &                     *PART(I,BED)*(1.-EFFTOP(I,BED))
          ENDDO

C*                wall friction coefficient
      FRICT = 0.005 
C*                  original equation 
C*           FRICT = 0.005 * (1. + 3.* SQRT(CARY))

      UU = 1./ (FF*ALFA/RR + FRICT*HH)

C*       pressure loss coefficient entrance
      XI_E = UU**2/RR * (1./(1.-FRICT*HH*UU)**2. - 1.)

C*       pressure loss coefficient eddy
      XI_T = 2.2 * UU**1.7

C*      pressure loss coeff.
      XI   = XI_E + XI_T

C*      inner circumference velocity
      VT   = SQRT(PRESS(3)/XI/RHOG*2.) 

C*      check for allowance friction coefficient
      REYNOL = 2.* CYCDAT(4)*VT*RHOG/VISG/2./HH/(RR-1.)
      IF (REYNOL .LT. 500.) THEN 
          WRITE(*,*)' Re-No may be too low for cyclone model:  Re =  ',
     &    REYNOL
      ENDIF

C*       maximum load entering eddy (term. velo chosen set eq. fluidization)
         CARYMX = FRICT*CONST*RR/ALFA**1.5/SQRT(PI*FF)
     &             *VIN/W50

C*           separation efficiency of entrance
          IF (CARY .LT. CARYMX .OR. CARY .LE. 0.) THEN
             EFIN = 0.
          ELSE    
             EFIN = 1. - CARYMX/CARY
          ENDIF

      UT = UU * VT
      VR = VT/2./HT

C*          separation diameter of particles
      DD(M) = SQRT(18.*VISG*VR*RT/(RHOS(M)-RHOG)/UT**2.)      
C*             cylone efficiency (-)

       DO I=1,NCLASS,1

		 IF ((DIAM(I)-DD(M))/DD(M).GT.500.0) THEN
		  EFEDDY(I) = 0.999999
	     ELSE
            EFEDDY(I) = 1./(1./EXP( (DIAM(I)-DD(M))/DD(M))+1. )
		 ENDIF
           IF (CARYMX .GT. CARY) THEN
             EFFCYC(I,M) = cst*EFEDDY(I)
           ELSE
             EFFCYC(I,M) = cst*(EFIN + EFEDDY(I)*CARYMX/CARY)
           ENDIF
        ENDDO

C*        ***************** TESTWRITER **************************

        IF (.NOT. WRTOUT(17)) RETURN

       OPEN(UNIT=44,FILE='CYCLON'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')

        WRITE(44,*)'Cyclone eddy efficiency calculation, Material ',M
        WRITE(44,*) 'NO    DIAM      EFFeddy      EFFTOT    '
        WRITE(44,*) '     [æm]    ' 

       DO I = 1,NCLASS 
        WRITE(44,2001) I, DIAM(I)*1.E6, EFEDDY(I), EFFCYC(I,M)
       ENDDO
 2001   FORMAT(I3,2X,F7.0,2X,4(F8.6,2X))

        WRITE(44,*)
        write(44,*)
        write(44,*)'Volumetric gas flow to cyclone [m3/s]: ', GASFLO
        write(44,*)'Entrance velocity to cyclone   [m3/s]: ', VIN 
        write(44,*)'Solid load        [kg solid / kg gas]: ', CARY
        write(44,*)'Maximum load carried by eddy    [ " ]: ', CARYMX
        write(44,*)'Efficiency by bypass separation    [-]:', EFIN
        write(44,*)'Wall friction coefficient          [-]:', FRICT 
        write(44,*)'separation diam. of particles d50 [æm]:',DD(M)*1.E6

        CLOSE(44)


      RETURN
      END            ! (CYCLON)
