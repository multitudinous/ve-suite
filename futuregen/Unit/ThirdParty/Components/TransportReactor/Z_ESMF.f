
      REAL*8 FUNCTION Z_ESMF(DP,M)

************************************************************************
* WRITTEN        :                          BY:                        *
* LAST CHANGE AT : 05.03.95                 BY: J. Hannes              *
************************************************************************
* CALLED BY : PREPAR, DENSBD, ELUTRI, CORFAC, BOTTOM, Z_CORE, Z_EPSA   *
* CALLS     : Z_RHOG, Z_VIS                                            *
************************************************************************
* COMMON in : G          acceleration due to gravity           (m/s2)  *
*             RHOS(M)    particle density                      (kg/m3) *
*             SPHR(M)    particle sphericity                   (-)     *
*             TBED       average riser temperature             (K)     *
*             PRESS(1)   ambient pressure                      (Pa)    *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* ARGUMENTS PASSED: M = MATERIAL INDICATOR                             *
*                   I = SIZE INDICATOR                                 *
*                   Z_ESMF = solids fraction at minimum fluidization   *
************************************************************************
* PURPOSE   : Calculate bed porosity at minimum fluidization velocity  *
*             according to Broadhurst and Becker, AIChE 21(1975)238    *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8     Z_RHOG,RHOG, ! gas density (kg/m3)
     &         Z_VIS, VISG  ! gas viscosity (kg/ms)
      REAL*8     AR,          ! archimedes no
     &         EMF,         ! voidage at minimum fluidization
     &         DP           ! particle diameter (m)
      INTEGER  M            ! material indicator

      RHOG = Z_RHOG(TBED,PRESS(1))
      VISG = Z_VIS(TBED)

C*      archimedes number AR  (-)
      AR = G * DP**3 * RHOG * (RHOS(M) - RHOG) / VISG**2

C*      bed porosity Eminf at Uminf (-)
      EMF = 0.586 * SPHR(M)**(-0.72) * AR**(-0.029) 
     &        * (RHOG/RHOS(M))**0.021

C*      solids volume fracton at minimum fluidization state (-)
      Z_ESMF = 1. - EMF


      RETURN
      END          ! (Z_ESMF)


