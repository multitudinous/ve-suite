
      REAL*8 FUNCTION Z_SHWO (I,T)

************************************************************************
* LAST CHANGE AT :                          BY:                        *
************************************************************************
* CALLED BY : POPUL, PREPOP                                            *
* CALLS     : Z_ESMF, Z_UMF, Z_RHOG, Z_VIS                             * 
************************************************************************
* COMMON in : DIAM(I)    particle diameter (m)                         *
*             KDIFFU     diffusion coefficient (m2/s)                  *
*             U(K)       superficial gas velocity in cell K (m/s)      *
* out       : -                                                        *
* in & out  : -                                                        *
************************************************************************
* received  : I   -  Particle diameter indicator                       *
*             T   -  Bed temperature (K)                               *
*             IND -  Indicator for calculation: IND = 1 => bed         *
*                                               IND = 2 => freeboard   *
* returned  : Z_SHWO sherwood number of a particle                     *
************************************************************************
* PURPOSE   : Returns the Sherwood-Number of a particle                *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_ESMF, ESMF, ! solids volume fraction at min. fl.
     &          Z_RHOG, RHOG, ! gas density (kg/m3)
     &          Z_UMF,  UMF,  ! min. fl. velocity (m/s)
     &          Z_VIS, VISG   ! gas viscosity (kg/ms)
      REAL*8      SHWO1, SHWO2  ! sherwood no
      REAL*8      CON,          ! auxiliary
     &          DIFF,         ! diffusion coefficient (m2/s)
     &          DPRAT,        ! particle ratio (-)
     &          EMF,          ! voidage (-)
     &          RE,           ! Reynolds number (superficial )
     &          REMF,         ! Reynolds number minimum fluidization
     &          SC,           ! Schmidt number
     &          T,            ! bulk temperature
     &          XMM           ! auxiliary
      INTEGER   IND,          ! model indicator
     &          I             ! size indicator

      IND = 1   ! 1= bubble bed 2= freeboard

      RHOG = Z_RHOG(T,PRESS(1))
      VISG = Z_VIS(T)
      UMF  = Z_UMF(DIAM(I),BED) 
      ESMF = Z_ESMF(DIAM(I),BED)

C*     Schmitt-Number
      DIFF   = (T / 298.0)**1.5 * KDIFFU
      SC     = VISG / ( RHOG * DIFF )

      DPRAT = DIAM(I) / MEAND(BED)
      REMF   = UMF * MEAND(BED) * RHOG / ( VISG * ESMF)

      IF (IND .EQ. 1) THEN

C*                for small particles
            SHWO1 = 2. * (1.-ESMF)


C*                 for large particles
            XMM    = 0.35 + 0.29 * DPRAT**(-.5)
            EMF    = ESMF / (1.-ESMF)
            CON    = 1.505 * DPRAT**(-0.05) + 0.105 * DPRAT
            SHWO2  = CON * EMF * SC**0.3333 * REMF**(1.-XMM)

C*                 superposition
           Z_SHWO = SHWO1/(1.+DPRAT) + SHWO2/(1.+1./DPRAT)

      ELSE
C*                freeboard calculation
C*                forced convection around a single sphere (Rowe 1965)
            RE = RHOG*DIAM(I)*U(CMP(1))/VISG
            Z_SHWO = 2 + 0.69*SC**(1./3.)*RE**0.5

      END IF

      RETURN
      END            ! (Z_SHWO)



