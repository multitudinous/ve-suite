
      REAL*8 FUNCTION Z_NUSL(I,T,TP)

************************************************************************
* LAST CHANGE AT :  22.10.1993              BY:   J. Hannes            *
************************************************************************
* CALLED BY : Z_QTRN                                                   *
* CALLS     : Z_RHOG, Z_VIS, Z_THC, Z_UMF                              * 
************************************************************************
* COMMON in :  G            gravity acceleration    (m/s2)             *
*              MWG(J)       molar weight gas        (kg/mol)           *
*              HEATCG(J)    gas specy heat capacity (J/molK)           *
*              RHOS(M)      solids density          (kg/m3)            *
*              MEAND(M)     average particle diameter  (m)             *
*              DIAM(I)      particle diameter          (m)             *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* received  : I         size indicator                                 *
*             T         bulk temperature                               *
*             TP        particle temperature                           *
* returned  : Z_NUSC    nusselt number combustion                      *
************************************************************************
* PURPOSE   : returns the Nusselt-Number of a particle from            *
*             sieve class I in a fluidized bed                         *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_RHOG, RHOG,   ! gas density (kg/m3)
     &          Z_VIS,  VISG,   ! gas viscosity (kg/ms)
     &          Z_THC,  THC,    ! gas thermal conductivity (W/mK)
     &          Z_UMF,  UMF     ! min. fl. velocity (m/s)
      REAL*8      NUSCON,         ! nusselt no convection
     &          NUSRAD          ! nusselt no radiation
      REAL*8      AR,             ! archimedes -no
     &          DPRAT,          ! auxiliaries
     &          EXP, FT, REYN,  !     "
     &          CPM, PRAN       !     "
      REAL*8      ALFRAD,         ! heat transfer coeff. due to radiation (W/m2K)
     &          TP,             ! particle temperature (K)
     &          T               ! bulk temperature (K)
      REAL*8      NUSL1, NUSL2    ! nusselt numbers according to different models
      INTEGER   I               ! size indicator

      RHOG = Z_RHOG(T,PRESS(1))
      VISG = Z_VIS(T)
      THC  = Z_THC(T)        
      UMF  = Z_UMF(MEAND(BED),BED)

C*         Equation according to Prins (1987)
         DPRAT  = DIAM(I) / MEAND(BED)
         AR     = G * MEAND(BED)**3. * RHOS(BED) * RHOG / VISG**2.
         EXP    = 0.105 * DPRAT**0.082
         FT     = 0.844 + 0.0756 * (T / 273.15)

         NUSL1 = 3.539 * DPRAT**(-0.257) * AR**EXP * FT


C*        Equation according to Botterill
C*        (forced convection around a sphere plus radiation)
         REYN = DIAM(I) * UMF * RHOG / VISG
         CPM  = HEATCG(O2)*.1/MWG(O2) + HEATCG(N2)*.8/MWG(N2)+
     &          HEATCG(CO2)*.1/MWG(CO2)
         PRAN = VISG * CPM / THC
         NUSCON = 2. + .69 * REYN**.5 * PRAN**.333

C*        Radiation
         ALFRAD = SBOLTZ*0.85*(TP**4.-T**4.)/(TP-T)
         NUSRAD = ALFRAD * DIAM(I) / THC

C*         Nusselt Number related to the averaged bed particle diameter
         NUSL2 = (NUSCON + NUSRAD) * MEAND(BED) / DIAM(I)

C*         superposition of the models accoding to their range of validation
         Z_NUSL = NUSL1 / (1. + MEAND(BED)/DIAM(I)    )
     &          + NUSL2 / (1. + DIAM(I)   /MEAND(BED) )

      RETURN
      END       ! (Z_NUSC)



