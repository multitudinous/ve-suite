
      REAL*8 FUNCTION Z_EPSA(I,K,M)

************************************************************************
* LAST CHANGE AT : 10.03.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : Z_CORE, CHARTR                                           *
* CALLS     : Z_EPSS, Z_ESMF                                           *
************************************************************************
* COMMON in : NBED, MEAND(M)                                           *
*       out :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : I  = size                                                *
*             K = compartment                                          *
*             M = material indicator                                   *
* returned  : Z_EPSA    solid volume fraction in annulus of cell       *
************************************************************************
* PURPOSE   : calculates annulus solids concentration at the level H in*
*             compartment K for the size class I of material M         *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8   Z_EPSS, ! cross section averaged solid volume fraction (-)
     &         Z_ESMF  ! solid volume fraction at min. fluidization (-)
      INTEGER  I,      ! particle size indicator
     &         K,      ! riser cell indicator
     &         M       ! solid material indicator
      INTEGER           MODEL ! indicator for model used

C*     solids volume fraction models
C*     1 = Seiter (1990) EpsAnnulus = K * EpsAverage
C*     2 =               EpsAnnulus = EPS min. fluidization
C*     3 =               EpsAnnulus = const.

           MODEL = 2

        IF (MODEL .EQ. 1) THEN 
C*              approach according to Seiter (1990), Erlangen
               Z_EPSA = 2.3 * Z_EPSS(I,K,M)

       ELSE IF (MODEL .EQ. 2) THEN
C*            approach wall layer has min. fluid. cond.
                Z_EPSA = Z_ESMF(MEAND(BED),BED)

       ELSE IF (MODEL .EQ. 3) THEN
C*            fix value
              IF (Z_EPSS(I,K,M) .GT. 0.2) THEN
                Z_EPSA = Z_EPSS(I,K,M)
              ELSE
                Z_EPSA = 0.2
              ENDIF
       ENDIF

      RETURN
      END            ! (Z_EPSA)



