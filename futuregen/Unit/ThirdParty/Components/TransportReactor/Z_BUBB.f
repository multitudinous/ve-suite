
      REAL*8 FUNCTION Z_BUBB(DP,M)

************************************************************************
* CALLED BY : PREPAR, DENSBD, PRESOL, BOTTOM,  GASMIX                  *
* CALLS     : Z_UMF  minimum  fluidization velocity (m/s)              *
************************************************************************
* COMMON in : U(K)                                                     *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : M      = material indicator                              *
*           : DP     = particle diameter (m)                           *
* returned  : Z_BUBB = bubble volume fraction at bottom bed (-)        *
************************************************************************
* PURPOSE   : calculates the bubble fraction in the bottom bed         *
*             of the riser according to F. Johnsson, Powder Tech. 68   *
*             (1991) 117-123                                           *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_UMF, ! min. fl. velocity (m/s)
     &            DP     ! paticle diameter (m)
      REAL*8      F,Z_BUBB1      ! correlation by Filip Johnson (Chalmers)
      INTEGER     M      ! material indicator


      IF (Z_UMF(DP,M) .LT. U(1)) THEN

            F = ( 0.26 + 0.70 * EXP(-3300. * DP) )
     &            *( 0.15 + (U(1)-Z_UMF(DP,M)) )**(-0.33)
            Z_BUBB = 1. / ( 1. + 1.3/F*(U(1)-Z_UMF(DP,M))**(-0.8))

      ELSE

            Z_BUBB = 0. 

      ENDIF

!      IF (Z_UMF(DP,M) .LT. U(1)) THEN
!Glicksman et al., CES, 46, 1561-1571, 1991.          
!zc	 K = 1 + 2 * Z_BUBB is the through-flow factor
!zc	 Uo = K Umf + Z_BUBB * Ub
!       Z_BUBB = (U(1)-Z_UMF(DP,M))/(2.*Z_UMF(DP,M)+UBUBBL(1))
!      ELSE
!       Z_BUBB = 0. 
!      ENDIF
      RETURN
      END         ! (Z_BUBB)