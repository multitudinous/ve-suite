
      REAL*8 FUNCTION Z_CORE (I,K,M)

************************************************************************
* LAST CHANGE AT : 22.10.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : PRESOL, SOLCON                                           *
* CALLS     : Z_EPSS    average solid volume fraction at level         *
*             Z_EPSA    solid volume fraction in annulus               *
************************************************************************
* COMMON in : AREA(K)     cross section in cell K              (m3)    *
*             PART(I,M)   size class weight fraction           (-)     *
*             RHOS(M)     particle density                     (kg/m3) *
*             USOLID(I,K,M)solids upward velocity              (m/s)   *
*             USOLA(I,M)  solids downward velocity in annulus  (m/s)   *
*             MNETTO(I,K,M) net solids flow of class in cell   (kg/s)  *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    :  I       particle size indicator                         *
*              K       cell indicator                                  *
*              M       material indicator                              *
* returned  :  Z_CORE  = fraction of cross sec. area covered by core(-)*
************************************************************************
* PURPOSE   : calculates the faction of the cross sectional area which *
*             is covered by core flow for one size class               *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8     Z_EPSA,  ! solid volume fraction in annulus
     &           Z_EPSS   ! average solid volume fraction in cell
      INTEGER    I,       ! size class indicator
     &           K,       ! riser cell indicator
     &           M        ! bed material indicator

          
      IF (PART(I,M) .GT. 1.E-6) THEN

          Z_CORE = 1. 
     &      - ( Z_EPSS(I,K,M)*USOLID(I,K,M) 
     &          - MNETTO(I,K,M)/RHOS(M)/AREA(K)/PART(I,M) )
     &      / Z_EPSA(I,K,M) / ( USOLID(I,K,M)+USOLA(I,K,M) )

      ELSE

          Z_CORE = 1.

      ENDIF

      IF (Z_CORE .LT. 0. .OR. Z_CORE .GT. 1.) THEN

        WRITE(LOG,*) 'CORE is with I,K,M ',Z_CORE,I,K,M
        WRITE(LOG,*) ' Set artificially to 0.99 '
          Z_CORE = 0.99 
      ENDIF

      RETURN
      END         ! (Z_CORE)




