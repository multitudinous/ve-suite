
      SUBROUTINE PYROLY

************************************************************************
* LAST CHANGE AT :  22.09.94                BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : VOLCOM, DRY, LINEQS                                      *
************************************************************************
* COMMON in : CONDRY(I), CONVOL(I), NCLASS, NCELLS, COALM(), PART(I,M) *
*             MINL(I,K,M), MOUT(I,K,M), MUP(I,K,M), MDWN(I,K,M),       *
*             MX(I,k,M), MCA(I,K,M), MAC(I,K,M), MCAX(I,K,M),          *
*             MCYC(I,K,M), MEHE(I,K,M), MCOR(I,K,M), MANN(I,K,M)       *
*             CMP(6), YH2O(K), YVOLA(K), YO2(K), WRTSCR, WRTOUT        *
*             TEMP(K,P), FEED(M), COALM(),AREA(K), HEIGHT(K)           *
*       out : TMCVOL(K), TMAVOL(K), TMCH2O(K), TMAH2O(K)               *
*  in & out :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calc. drying and devolatilization of coal in each cell   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER I,            ! part. size indicator
     &        J,
     &        K,            ! cell indicator
     &        KK,           ! cell indicator
     &        INDEX,        ! 1=drying, 2=devolatilization
     &        Idev          ! devolatilization model: 1=IEA, 2=CPD
      REAL*8  A(NK,NK),     ! matrix for dry and devol. lin. eq. sys.
     &        CH2O(NI,NK),  ! H2O mass fraction in coal      (-)
     &        CVOL(NI,NK)   ! volatile mass fraction in coal (-)
      REAL*8  CON(NI),      ! release rate constant (1/s)
     &        B(NK)         ! right hand array of lin. q.sys.
      REAL*8  TTH2O,        ! auxiliary
     &        TTVOL,        ! auxiliary
     &        VOLMOL,       ! molVolatiles/kgVolatiles
     &        H2OMOL,       ! molH2O/kgMoisture
     &        O2MOL,        ! molO2/kgVolatiles
     &        fgasx,        ! mass fraction of coal released as volatiles
     &        gh2o,gco2,gch4,gco    !species mole numbers in volatiles

         Idev = 1

      IF (WRTSCR)  WRITE(*,*) ' PYROLY '

C*          calculate preliminary volatile composition (indicator 0)           
        CALL VOLCOM(1,H2OMOL,VOLMOL,O2MOL)      !zc using MERRICK's model

C*          calculate volatile evaporation rate
        CALL DRY


       DO 2000 INDEX=1,2

       DO 1000 I=1, NCLASS

             DO K=1,2*NCELLS
                B(K) = 0.
             ENDDO

C*                skip iteration if no coal in size class
             IF (PART(I,1) .LE. 0.) THEN
               GOTO 30
             ENDIF

C*                   distinction index drying=1,  devolatilization=2
          IF (INDEX .EQ. 1) THEN
                    CON(I) = CONDRY(I)
                DO K=1,NCELLS
                    B(K) = MINL(I,K,1)*COALM(MOIS)
                ENDDO
          ELSE
                    CON(I) = CONVOL(I)
                DO K=1,NCELLS
                    B(K) = MINL(I,K,1)*(1.-COALM(MOIS))*COALM(VOLAT)
                ENDDO
          ENDIF

C*               reset cells
       DO K=1,2*NCELLS
         DO KK=1,2*NCELLS
          A(K,KK) = 0.
         ENDDO
       ENDDO

       K = 1
          A(K+KCO,K+KCO)   =   CON(I)*MCOR(I,K,1) + MUP(I,K,1)
     &                       + MX(I,K,1)   + MCA(I,K,1)
     &                       + MCAX(I,K,1) + MOUT(I,K,1)
          A(K+KCO,K+KCO+1) = - MX(I,K,1)
          A(K+KCO,K+KAN)   = - MAC(I,K,1) - MCAX(I,K,1)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,1) - MEHE(I,K,1)

          A(K+KAN,K+KAN)   =   CON(I)*MANN(I,K,1) + MDWN(I,K,1) 
     &                       + MAC(I,K,1) + MCAX(I,K,1)    
          A(K+KAN,K+KAN+1) = - MDWN(I,K+1,1)
          A(K+KAN,K+KCO)   = - MCA(I,K,1) - MCAX(I,K,1)

       DO K=2,NCELLS-1
          A(K+KCO,K+KCO-1) = - MUP(I,K-1,1)   - MX(I,K-1,1)
          A(K+KCO,K+KCO)   =   CON(I)*MCOR(I,K,1) 
     &                       + MUP(I,K,1)     + MX(I,K-1,1)
     &                       + MX(I,K,1) 
     &                       + MCA(I,K,1) + MCAX(I,K,1) 
     &                       + MOUT(I,K,1)
          A(K+KCO,K+KCO+1) = - MX(I,K,1)
          A(K+KCO,K+KAN)   = - MAC(I,K,1) - MCAX(I,K,1)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,1) - MEHE(I,K,1)

          A(K+KAN,K+KAN)   =   CON(I)*MANN(I,K,1)
     &                       + MDWN(I,K,1) 
     &                       + MAC(I,K,1) + MCAX(I,K,1)    
          A(K+KAN,K+KAN+1) = - MDWN(I,K+1,1)
          A(K+KAN,K+KCO)   = - MCA(I,K,1) - MCAX(I,K,1)
       ENDDO

       K = NCELLS 
          A(K+KCO,K+KCO-1) = - MUP(I,K-1,1)       - MX(I,K-1,1)
          A(K+KCO,K+KCO)   =   CON(I)*MCOR(I,K,1)
     &                       + MX(I,K-1,1) + MCA(I,K,1)
     &                       + MCAX(I,K,1) + MOUT(I,K,1)
          A(K+KCO,K+KAN)   = - MAC(I,K,1)  - MCAX(I,K,1)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,1) - MEHE(I,K,1)

          A(K+KAN,K+KAN)   =   CON(I)*MANN(I,K,1) + MDWN(I,K,1) 
     &                       + MAC(I,K,1)  + MCAX(I,K,1)    
          A(K+KAN,K+KCO)   = - MCA(I,K,1) - MCAX(I,K,1)


        CALL LINEQS(A,2*NCELLS,NK,B)

 30     CONTINUE

C*                 write results to concentration arrays
        IF (INDEX .EQ. 1) THEN
           DO K=1,2*NCELLS
             CH2O(I,K) = B(K)
           ENDDO
        ELSE
!zc
      IF(Idev.eq.2) THEN
            DO J=1,NSPECY
               !  XMV(J) = 0.0
               XVOLA(J) = 0.0 !XMV(J) /VOLMOL
            ENDDO 
            call cpd(I,fgasx,gh2o,gco2,gch4,gco)
            VOLMOL = gh2o+gco2+gch4+gco
               ! XMV(H2O) = gh2o
               ! XMV(CO2) = gco2
               ! XMV(CH4) = gch4
               ! XMV(CO) = gco
               XVOLA(H2O) = gh2o/VOLMOL
               XVOLA(CO2) = gco2/VOLMOL
               XVOLA(CH4) = gch4/VOLMOL
               XVOLA(CO) = gco/VOLMOL
           DO K=1,2*NCELLS
             CVOL(I,K) = fgasx
           ENDDO
      ENDIF
!zc
           DO K=1,2*NCELLS
             CVOL(I,K) = B(K)
           ENDDO
        ENDIF

 1000   CONTINUE
 2000   CONTINUE
      
C*        release moisture and water vapour relative to cell holdup
        DO K = 1, NCELLS
          YH2O(K) = 0.
          YVOLA(K) = 0.
          YO2(K) = 0.         !zc
         DO I = 1, NCLASS
               
          YH2O(K) = YH2O(K) 
     &     + H2OMOL * CONDRY(I)
     &       *(CH2O(I,K+KCO)*MCOR(I,K,1)+CH2O(I,K+KAN)*MANN(I,K,1))

          YVOLA(K) = YVOLA(K) 
     &     + VOLMOL * CONVOL(I)
     &       *(CVOL(I,K+KCO)*MCOR(I,K,1)+CVOL(I,K+KAN)*MANN(I,K,1))

          YO2(K) = YO2(K)
     &     + O2MOL * CONVOL(I)
     &       *(CVOL(I,K+KCO)*MCOR(I,K,1)+CVOL(I,K+KAN)*MANN(I,K,1))
     
         ENDDO

       ENDDO

       IF (WRTSCR)   write(*,*) ' END PYROLY '


C ************************ testwriter ********************************

       IF (.NOT. WRTOUT(11)) RETURN


             TTH2O = 0.
             TTVOL = 0.
      DO K = 1, NCELLS
             TMCH2O(K) = 0.
             TMCVOL(K) = 0.
             TMAH2O(K) = 0.
             TMAVOL(K) = 0.  
        DO I = 1, NCLASS
         TMCH2O(K) = TMCH2O(K) + CH2O(I,K)*CONDRY(I)*MCOR(I,K,1)
         TMCVOL(K) = TMCVOL(K) + CVOL(I,K)*CONVOL(I)*MCOR(I,K,1)
         TMAH2O(K) = TMAH2O(K) + CH2O(I,K+KAN)*CONDRY(I)*MANN(I,K,1)
         TMAVOL(K) = TMAVOL(K) + CVOL(I,K+KAN)*CONVOL(I)*MANN(I,K,1)
        ENDDO
         TTH2O = TTH2O + (TMCH2O(K)+TMAH2O(K))/(HEIGHT(K)-HEIGHT(K-1))
         TTVOL = TTVOL + (TMCVOL(K)+TMAVOL(K))/(HEIGHT(K)-HEIGHT(K-1))
      ENDDO
         IF (TTH2O .LE. 0.) THEN
               TTH2O = 1.
         ENDIF
         IF (TTVOL .LE. 0.) THEN
               TTVOL = 1.
         ENDIF

       OPEN(UNIT=44,FILE='PYRO.DAT',STATUS='UNKNOWN')
        WRITE(44,*) ' Cells, release rates (mol/m3s)'
        WRITE(44,*) 'cell height   H2Ocore     VOLcore     H2Oannu    '
     &             ,'VOLannu'
       DO K = 1,NCELLS
 1076  FORMAT(I3,2X,F7.2,4(2X,E10.2))
        Write(44,1076) K,HEIGHT(K),
     &                TMCH2O(K)/TTH2O/(HEIGHT(K)-HEIGHT(K-1))/AREA(K),
     &                TMCVOL(K)/TTVOL/(HEIGHT(K)-HEIGHT(K-1))/AREA(K),
     &                TMAH2O(K)/TTH2O/(HEIGHT(K)-HEIGHT(K-1))/AREA(K),
     &                TMAVOL(K)/TTVOL/(HEIGHT(K)-HEIGHT(K-1))/AREA(K)
       ENDDO
        CLOSE(44)


       RETURN
      END       ! (PYROLY)



