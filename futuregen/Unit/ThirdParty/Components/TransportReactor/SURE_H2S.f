
      SUBROUTINE SURE
      
************************************************************************
* LAST CHANGE AT :  24.05.1995              BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_RHOG, Z_VIS, Z_EXP                                     * 
************************************************************************
* COMMON in : COALM(), DIAM(I), FEED(M), MANN, MCOR, MWS(), NCLASS,    *
*             NCELLS, NSPECY, PI, PRESS(1), RG, RHOS(M), RSURE, SUREF, *
*             TBED, U(K), WRTOUT, WRTSCR, XCACO3, XGAS,SBET, ALFMAX,   *
*             KSUR                                                     *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : sulfur retention model                                   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8 CON0   
      REAL*8 VPORE, TORTUO, RPORE
      REAL*8 H2SMEAN, O2MEAN, MMC(NK), MMA(NK), MMM, DKNUD, DAB 
C      REAL*8 PCO2EQ, PCO2, XCO2, TCALC(NI), TT(NI)
      REAL*8 XCALCIN(NI)
      REAL*8 REP, SCHMID, STANTO, CKSO2, RDELTA, DPORE, DSO2S, DSO3S
      REAL*8 DSO2F, DSO3F, CC1, CC2, CC3, CC4, ALFA, ALFA1, ALFA2
      REAL*8 DELALF, HINV1, HINV2, ALFAVG(NI), SFAVG, SSAVG
      REAL*8 RRSURE(NI), MM(NI)
      REAL*8 Z_RHOG, Z_VIS
      REAL*8 Z_EXP

      INTEGER I,J,K,P

          REAL*8 sum, sum2, sum1


       IF (WRTSCR) write(*,*) '  SURE '

        CON0 = 1.0               ! SO2-SO3 equilibrium const.
        VPORE = 1.E-6            ! pore volume (m3/g)
        TORTUO = 3.              ! tortuosity  (-)
        RPORE  = 2.*VPORE/SBET   ! pore radius (m)


C*          Knudsen-diffusion
       DKNUD = 4./3.*RPORE*SQRT(2.*RG*TBED/PI/0.1)

C*          two compounds diffusion
       DAB   = 1.E-7 * TBED**1.75 * SQRT((64.+28.)/64./28.)
     &         /  (PRESS(1)*1.E-5) 
     &         / ( 41.6**0.33 + 17.9**0.33 )**2.

C*           total pore diffusion
        DPORE = RHOS(2)*VPORE/TORTUO/(1./DKNUD + 1./DAB)

C*          SO2 and SO3 diffusion
         DSO2S = DPORE
         DSO3S = DPORE * 0.9
         DSO2F = DAB
         DSO3F = DAB

         H2SMEAN = 0.
         O2MEAN  = 0.

C         PCO2EQ = 1.2E12*EXP(-19124./TBED)
C         PCO2   = PRESS(1)*(0.065 + (TBED-1098.)*1.05E-3)
C         XCO2   = 0.1

C*            calcination degree of limestone
          DO I = 1, NCLASS
C            TCALC(I) = DIAM(I)/2.*0.1/0.00207
C     &                 /(PCO2EQ-PCO2-PRESS(1)*XCO2)
C            TT(I) = TTT(I,LIME)/TCALC(I)
C            XCALCIN(I) = 3.*TT(I)-6.*TT(I)**2.
C     &                 +6.*TT(I)**3.*(1.-EXP(1./TT(I)))
            XCALCIN(I) = 1.
          ENDDO

C*           mass of each limestone class in the riser
          DO I = 1, NCLASS
            MM(I) = 0.
           DO K = 1,NCELLS
            MM(I) = MM(I) + MCOR(I,K,2) + MANN(I,K,2)
           ENDDO
          ENDDO

C*           mass of limestone in each cell MMC and MMA and 
C*           total mass of limestone in the riser
            MMM = 0.
         DO K=1,NCELLS
            MMC(K) = 0.
            MMA(K) = 0.
          DO I=1,NCLASS
            MMC(K) = MMC(K) + MCOR(I,K,2) 
            MMA(K) = MMA(K) + MANN(I,K,2)    
          ENDDO   
            MMM = MMM + MMC(K) + MMA(K)
         ENDDO

C*         check if any limestone in the riser
        IF (MMM .LE. 0.) GOTO 150

      DO K=1,NCELLS
       H2SMEAN = H2SMEAN
     &     +(XGAS(H2S,K,CORE)*MMC(K)+XGAS(H2S,K,ANNU)*MMA(K))/MMM 
       O2MEAN  = O2MEAN 
     &     +(XGAS(O2,K,CORE) *MMC(K)+XGAS(O2,K,ANNU) *MMA(K))/MMM 
      ENDDO
       H2SMEAN = H2SMEAN *PRESS(1)/RG/TBED
       O2MEAN  = O2MEAN  *PRESS(1)/RG/TBED
       

       DO 100 I=1,NCLASS

         REP = Z_RHOG(TBED,PRESS(1))*U(CMP(1))*DIAM(I)/Z_VIS(TBED)
         SCHMID = Z_VIS(TBED)/Z_RHOG(TBED,PRESS(1))/DAB
         STANTO = 0.81 / SQRT(REP)/SCHMID**(2./3.)
         CKSO2  = STANTO * U(CMP(1))

         RDELTA = 1./(CKSO2/DAB-2./DIAM(I))

c         GOTO 80        !zc
c!zc
        IF (H2SMEAN .LE. 0.) GOTO 90
         
         if(O2MEAN.eq.0.0) O2MEAN = 1.0E-6
         CC1 = H2SMEAN/RHOS(2)/XCALCIN(I)/XCACO3*0.1
         CC2 = (DIAM(I)/2.)/KSUR/CON0/SQRT(O2MEAN)
         CC3 = (DIAM(I)/2.)**2./(DSO2S+DSO3S*CON0*SQRT(O2MEAN))
         CC4 = (DIAM(I)/2.)**3./(DSO2F+DSO3F*CON0*SQRT(O2MEAN))
     &         *( 2./DIAM(I) -2./(DIAM(I)+2.*RDELTA) )

         ALFA = 0.
         DELALF = 1.E-9
         ALFAVG(I) = 0.

 10     CONTINUE

        IF (ALFA+DELALF .GT. ALFMAX) THEN
          DELALF = ALFMAX - ALFA
        ENDIF

         ALFA1 = ALFA
         ALFA2 = ALFA + DELALF

         HINV1 = (  CC2        * (1.-(1.-ALFA1)**0.33)
     &             +CC3 * 0.5  *((1.-(1.-ALFA1)**0.66)-0.66*ALFA1)
     &             +CC4 * 0.33 *         ALFA1        )
     &                   /TTT(I,2)/CC1

         HINV2 = (  CC2        * (1.-(1.-ALFA2)**0.33)
     &             +CC3 * 0.5  *((1.-(1.-ALFA2)**0.66)-0.66*ALFA2)
     &             +CC4 * 0.33 *         ALFA2        )
     &                   /TTT(I,2)/CC1
      
         ALFAVG(I) = ALFAVG(I) + ( EXP(-HINV1) + EXP(-HINV2))/2.*DELALF

         ALFA = ALFA + DELALF

         DELALF = DELALF * 1.1

         IF ((ALFA+DELALF) .LT. ALFMAX) THEN
              GOTO 10
         ENDIF
c!zc
C*          average surface fraction
         SFAVG = (1.-ALFAVG(I))**0.66
      
c  80     SFAVG = 0.50          !zc
C*             average specific reactive surface area of sorbent (m2/kg)
         SSAVG = SFAVG*6./DIAM(I)/RHOS(2)*XCACO3

  90     CONTINUE
C*           specific SO2 ret. in class by mole fraction of SO2
C*                 per kg of limestone
!zc        RRSURE(I) = KSUR*SSAVG*CON0*XCALCIN(I)*(PRESS(1)/RG/TBED)**1.5
        RRSURE(I) = KSUR*EXP(-2.60E5/(RG*TBED))*SSAVG*XCALCIN(I)
     &              *PRESS(1)/RG/TBED
   
 100     CONTINUE

      DO P=1,2
        DO K = 1, NCELLS
         RSURE(K,P) = 0.
         DO I = 1,NCLASS
          IF (MM(I) .GT. 0.) THEN
            RSURE(K,P) = RSURE(K,P) + RRSURE(I)*MCOR(I,K,2)
          ENDIF
         ENDDO
        ENDDO
      ENDDO
          
          DO J=1,NSPECY
            SUREF(J) = 0.
          ENDDO
!zc            SUREF(O2)  = -0.5
            SUREF(H2S) = -1.
            SUREF(CO2) =  1.
            SUREF(H2O) =  1.

C*           some test balances SUM,SUM1,SUM2

           SUM = 0.
          DO K=1,NCELLS
           SUM = SUM + (RSURE(K,CORE)+RSURE(K,ANNU))
     &          *H2SMEAN/(PRESS(1)/RG/TBED)
          ENDDO

           SUM1 = 0.
        DO K=1,NCELLS
         SUM1 = SUM1 
     &       +RSURE(K,CORE)*XGAS(H2S,K,CORE) !*Z_EXP(XGAS(O2,K,CORE),0.5)
     &       +RSURE(K,ANNU)*XGAS(H2S,K,ANNU) !*Z_EXP(XGAS(O2,K,ANNU),0.5)
        ENDDO


           SUM2 = FEED(COAL)*(1.-COALM(MOIS))*COALM(5)/MWS(5)


 150     CONTINUE

C******************* testwriter

        IF (.NOT. WRTOUT(18)) RETURN

       OPEN (Unit=85,FILE='SURE.DAT',STATUS='UNKNOWN')
       write(85,*) ' H2S  O2  ', H2SMEAN, O2MEAN
       write(85,*)
       write(85,*) ' I         AlfaAVG          RRSURE '
       DO I=1, NCLASS
       write(85,*) I, ALFAVG(I), RRSURE(I)
       ENDDO
       write(85,*)
       write(85,*) ' K       RSURE core      RSURE annu '
       do K=1,ncells
       write(85,*) k,rsure(k,CORE),rsure(K,ANNU)
       enddo
       write(85,*)
       write(85,*) ' SUM in , SUM ret ' ,SUM2, SUM
       CLOSE(85)

 1177  FORMAT(I3,5(2X,F12.2))

      RETURN
           
      END         ! (SURE)




