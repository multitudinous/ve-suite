
      REAL*8 FUNCTION Z_UMF (DP,M)

************************************************************************
* LAST CHANGE AT :                         BY :
************************************************************************
* CALLED BY : BOTTOM, PREPAR, DENSBD, ELUTRI, CORFAC, Z_BUBB, Z_CORE,  *
*             Z_EPSA, Z_ESMF, Z_NUSL                                   *
* CALLS     :                                                          *
************************************************************************
* COMMON in : G, RHOS(M)                                               *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : M       = material indicator                             *
*             I       = particle diameter indicator                    *
* returned  : Z_UMF   = minimum fluidization velocity (m/s)            *
************************************************************************
* PURPOSE   : calculates minimum fluidization velocity from mead diam. *
*             according to Preto (1987), Ph.D. thesis p.56 eqn 3.2.2.  *
************************************************************************

        INCLUDE 'PARAMTR.FTN'
        INCLUDE 'COMMONBL.FTN'

        REAL*8     Z_RHOG, RHOG, ! gas density (kg/m3) 
     &           Z_VIS,  VISG, ! gas viscosity (kg/ms)
C     &           Z_VELO,       ! termonal particle velocity (m/s)
     &           Z_EXP         ! safe exponent function
        REAL*8     AR,           ! archimedes number
     &           DP,           ! particle diameter (m)
     &           RHS           ! auxiliary
        INTEGER  M             ! material indicator

         RHOG = Z_RHOG(TBED,PRESS(1))
         VISG = Z_VIS(TBED)

C*             Archimedes number AR  (-)
       AR = G * DP**3. * RHOG * (RHOS(M) - RHOG) / VISG**2.

C*             Evaluate RHS (Right Hand Side of Preto equation)
!zc       RHS = 52400. * Z_EXP(AR,-0.82) * (RHOS(M)/RHOG)**0.22 + 1.65
		RHS = SQRT(27.2**2.+0.0408*AR)-27.2

C*             Finally extract UMF  (m/s)
!zc       Z_UMF = SQRT((G * (RHOS(M) - RHOG) * DP) / (RHS * RHOG))
		Z_UMF = RHS*VISG/(RHOG*DP)

C*             proposed correlation by WIRTH
C      Z_UMF = 0.05 * Z_VELO(DP,M)


      RETURN
      END        ! (Z_UMF)



