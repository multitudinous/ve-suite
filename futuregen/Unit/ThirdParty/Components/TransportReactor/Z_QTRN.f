
      REAL*8 FUNCTION Z_QTRN (I,T,TP)

************************************************************************
* LAST CHANGE AT :  22.10.1993              BY:   J. Hannes            *
************************************************************************
* CALLED BY : CHARTR                                                   *
* CALLS     : Z_NUSL, Z_THC                                            * 
************************************************************************
* COMMON in : PI         3.14                                          *  
*             MEAND(M)   average particle diameter (m)                 * 
*             DIAM(I)    particle diameter (m)                         *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* received  : I         size indicator                                 *
*             T         bulk temperature (K)                           *
*             TP        particle temperature (K)                       *
* returned  : Z_QTRN    heat transport rate out of particle (W)        *
************************************************************************
* PURPOSE   : calculates heat transfer rate from burning char particle *
*             to fluidized bed                                         *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_THC, ! themal conductivity of gas (W/mK)
     &          Z_NUSL ! nusselt number
      REAL*8      ALPHA, ! heat transfer coefficient (W(m2K)
     &          TP,    ! particle temperature (K)     
     &          T      ! bulk temperature (K)
      INTEGER   I      ! size indicator

 
C*      total heat transfer coefficient ALPHA  (W/m2*K)
      ALPHA = Z_NUSL(I,T,TP) * Z_THC(T) / MEAND(BED)

C*     heat transfer rate QTRANS  (W)
      Z_QTRN = ALPHA * PI * DIAM(I)**2. * (TP - T)

      RETURN
      END         ! (Z_QTRN)


