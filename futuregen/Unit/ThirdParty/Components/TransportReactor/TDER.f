
      SUBROUTINE TDER (I, TP, PMASS, DR)

************************************************************************
* LAST CHANGE AT : 19.11.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : DRY                                                      *
* CALLS     : Z_NUSL, Z_THC                                            *
************************************************************************
* COMMON in : DIAM(I), HEATCP(COAL), MEAND(COAL), PI, TBED             *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed:   I      -  Particle size class                              *
*           TP     -  Current particle temperature (K)                 *
*           TBED   -  Bed temperature (K)                              *
*           PMASS  -  Particle mass (kg)                               *
*           THCG   -  Themal conductivity gas                          *
* returned: DR     -  Temperature derivative of a solid particle (K/sec)
************************************************************************
* PURPOSE   : Returns the temperature derivative of a solid particle   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    Z_THC, ! themal conductivity of gas (W/mK)
     &          Z_NUSL ! nusselt-number for drying
      REAL*8    ALPHA, ! heat transfer coefficient (W/m2K)
     &          DR,    ! temperature derivative (K/s)
     &          PMASS, ! mass of particle (kg)
     &          THF,   ! total heatr flux (W)
     &          TP     ! particle temperature (K)
      INTEGER   I      ! size identifier

C*             total heat transfer coefficient ALPHA  (W/m2*K)
      ALPHA = Z_NUSL(I,TBED,TP) * Z_THC(TBED) / MEAND(COAL)

C*             total heat flux THF  (W)
      THF = ALPHA * PI * DIAM(I)**2 * (TBED - TP)

C*             temperature derivative DR  (K/sec)
      DR = THF / (PMASS * HEATCP(COAL))

      RETURN
      END      ! (TDER)



