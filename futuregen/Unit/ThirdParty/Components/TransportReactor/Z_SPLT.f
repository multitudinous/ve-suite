
      REAL*8 FUNCTION Z_SPLT (I, TX, XXO2)

************************************************************************
* LAST CHANGE AT :  22.10.1993              BY:   J. Hannes            *
************************************************************************
* CALLED BY : POPUL, PREPOP                                            *
* CALLS     : Z_UMF                                                    * 
************************************************************************
* COMMON in : DIAM(I)    particle diameter (m)                         *
*             PRESS(1)   absolute pressure (Pa)                        *
* out       : -                                                        *
* in & out  : -                                                        *
************************************************************************
* received  : I         size indicator                                 *
*             TX        bulk temperature (K)                           *
*             XXO2      gas fraction O2 (-)                            *
* returned  : Z_SPLT    used mol C per mol O2                          *
************************************************************************
* PURPOSE   : calculation of CO2 - CO split during char combustion     *
*             dependent on particle size and temperature of the        *
*             surrounding                                              *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8     ARTHUR,  ! auxiliary
     &         PHIS,    ! C per O2 used
     &         TX,      ! bulk temperature (K)
     &         XXO2     ! oxygen molar fraction in gas (-)
      REAL*8     E_R,     ! activation energy (ARTHUR) (K)
     &         E_R0,    ! activation energy (Sarofim) (K)
     &         CON,     ! reaction constant (Arthur) (1/s)
     &         CON0,    ! reaction constant (Sarofim) (1/s)
     &         DSMALL,  ! distinction diameter (m)
     &         DBIG,    !      "           
     &         DREF,    ! reference diameter (m)
     &         X        ! exponent (Sarofim)
      INTEGER  I

      E_R = 6240. 
      E_R0= 2980.
      CON = 0.0004
      CON0 = 0.02
      X    = 0.2
      DSMALL = 5.E-5
      DBIG   = 1.E-3
      DREF   = 5.E-4

      IF (XXO2 .LT. 0.) THEN
         XXO2 = 0.
      ENDIF

C*    Temperature influence on CO2/CO ratio (according to Arthur (1951))
C      ARTHUR = CON * EXP(E_R/TX)

C*      Sarofim extension with O2 concentration
!zc       ARTHUR = CON0 * (PRESS(1)/1.01325E5*XXO2)**X * EXP(E_R0/TX)

C*     Molar ratio of burnt carbon to needed oxygen PHI (molC/molO2)
!zc      PHIS = 2. * (1. + ARTHUR) / (1. + 2. * ARTHUR)

C     Size influence on CO2/CO ratio if 50E-6 < DIAM(I) < 1E-3  (m)
C      IF (DIAM(I) .LE. DSMALL)  THEN
C              PHIS = PHIS
C      ELSE IF (DIAM(I) .GT. DSMALL .AND. DIAM(I) .LT. DBIG) THEN
C              PHIS = PHIS + (1.-PHIS)*(DIAM(I)-DSMALL)/(DBIG-DSMALL)
C      ELSE IF (DIAM(I) .GE. 1.E-3) THEN
C              PHIS = 1.
C      ENDIF

C*          approximation of above
!zc        PHIS = PHIS + (1.-PHIS)*(1.-1./(1.+(DIAM(I)/DREF)))

C*     Molar ratio mol molC/molO2 
      PHIS = 2.0
      Z_SPLT =  PHIS

      RETURN
      END            ! (Z_SPLT)



