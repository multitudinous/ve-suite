
      SUBROUTINE DRY

************************************************************************
* LAST CHANGE AT : 15.06.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : PYROLY                                                   *
* CALLS     : TDER, Z_THC                                              *
************************************************************************
* COMMON in : COALM(MOIS), DIAM(I), HEATCP(M), NCLASS, RHOS(M),        *
*             SIVFED, TAMB, TBED, WRTOUT, WRTSCR                       *
* out       : CONDRY(I), CONDEV(I), TDRY(I), TDEVOL(I)                 *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : calculates drying and devolatilization rates via times   *
*             for dry out, evaporation and heat up till 900 K          *
*             of a coal particle                                       *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8   Z_THC,THCG ! thermal conductivity of gas
      REAL*8   PMASS(NI), ! wet particle mass (kg)
     &         PMASSD(NI) ! dry particle mass (kg)
      REAL*8   RHOPAR,    ! wet particle density (kg/m3)
     &         TINC(NI),  ! time increment in integration (s)
     &         TP,        ! particle temperature
     &         TV, TD, DR ! auxiliaries
      REAL*8   TBAR, DRB, !    "
     &         RATE, WP   !    "
      REAL*8   TDV,       !    "
     &         SUM        !    "
      INTEGER  I

       IF (WRTSCR) write(*,*) ' DRY '

C*             thermal conductivity of gas
      THCG = Z_THC(TBED)

C*              particle density on wet basis
      RHOPAR = RHOS(COAL) / (1. - COALM(MOIS))

            SUM = 0.
C*               calc mean diameter of coal feed particles
         DO  I = 1,NCLASS
            SUM = SUM + (SIVFED(I,COAL) / DIAM(I))
         ENDDO

         IF (SUM .GT. 0.)  THEN
              MEAND(COAL) = 1.0 / SUM
         ELSE
              MEAND(COAL) = 0.
         ENDIF


      DO I = 1, NCLASS
C*              particle mass (kg)
           PMASS(I)  =  PI * DIAM(I)**3 * RHOPAR / 6.
           PMASSD(I) = PMASS(I) * (1. - COALM(MOIS))

C*              time increment TINC for sub-routine TDER depends 
C*              on particle size and particle properties
           TINC(I) = DIAM(I)**2 * RHOPAR * HEATCP(COAL) 
     &               / (300. * THCG * (2. + 120. * SQRT(DIAM(I)) ) )
      ENDDO


      DO 30 I=1,NCLASS

C*              start temperature (K)
            TP = TAMB

        IF (TP .LT. 373. .AND. TBED .GT. 373.)  THEN
C*              loop to calculate dry-out time TD till 373 K is reached
            TD  = 0.

C*                  get the temperature derivative and integrate by euler
   10       CALL TDER (I, TP, PMASS(I), DR)
              TBAR = TP + (TINC(I) * DR)

            CALL TDER (I, TBAR, PMASS(I), DRB)
              TP = TP + (TINC(I) / 2.) * (DRB + DR)

C*             increment of time counter
            TD = TD + TINC(I)

C*             check current temperature and maximum time
         IF (TP .LT. 373. .AND. TD .LT. 1000.)  GOTO 10

C*             current heating rate (J/s)
         RATE = DR * HEATCP(COAL) * PMASS(I)

C*             heat required to evaporate water dependent on pressure (J)
         WP = COALM(MOIS) * PMASS(I) * 2257000.

C*             time for dry-out and evaporation CPROP(I,3)
         TDRY(I) = TD + WP / RATE
      ELSE
         TDRY(I) = 0.
      END IF

 30   CONTINUE

      DO 50 I = 1,NCLASS

C*             loop to calculate total time TV till 900 K is reached

      IF (TBED .GT. 900.) THEN
           TV = TDRY(I)
C*             get the temperature derivative
   40     CALL TDER (I, TP, PMASSD(I), DR)
             TBAR = TP + TINC(I) * DR
          CALL TDER (I, TBAR, PMASSD(I), DRB)
             TP = TP + (TINC(I) / 2.) * (DRB + DR)
C*             increment of time counter
           TV = TV + TINC(I)

C*             check current temperature
          IF (TP .LT. 900. .AND. TV .LT. 1000.)  GOTO 40

C*             old module VOLATM
C*             calculation of the devolatilization time 
C*              for a coal particle
C*             according to Pillai J. Inst. Energy Sept.(1981) and 
C*              Preto (1987), Ph.D. thesis pp 94-95

C*             reference temperature TREF and constants Z1 and Z2 
C*             according to Pillai
C*               ref. temperature = 1048.
C*               Z1 = 10.
C*               Z2 = 0.75

C*             devolatilization time TDV (sec)
         TDV       = 10.*(1048./TBED)**3.8 
     &                * (1000. * DIAM(I))**(0.75/1048.*TBED)
         TDEVOL(I) = TDV + TV
       ELSE
         TDEVOL(I) = 0.
       ENDIF

 50   CONTINUE


C*             kinetical constants for drying and devolatilization (1/s)
C*             factor 2 combines the linear with the reciproke exp curve
C*             as first estimation, better approach needed

        DO I = 1, NCLASS

          IF (TDRY(I) .GT. 1.E-6) THEN
              CONDRY(I) = 1. / TDRY(I)   
          ELSE
              CONDRY(I) = 0.
          ENDIF
          IF (TDEVOL(I) .GT. 1.E-6) THEN
              CONVOL(I) = 1. / TDEVOL(I)
          ELSE
              CONVOL(I) = 0.
          ENDIF

        ENDDO

        IF (WRTSCR) WRITE(*,*) ' END DRY '

      RETURN
      END          ! (DRY)


