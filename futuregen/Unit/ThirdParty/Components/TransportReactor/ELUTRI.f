
      SUBROUTINE ELUTRI(M)

************************************************************************
* WRITTEN        : 1991                     BY:   J. Hannes            *
* LAST CHANGE AT : 10.07.1992               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : SUB01, SUB02, SUB03                                      *
************************************************************************
* ARUMENTS PASSED: M = Material indicator                              *
************************************************************************
* COMMON in : CMP(1), DIAM(I), EPSSHM(M), G, PRESS(1), NCLASS,         *
*             PART(I,M), RHOS(M), U(K), WRTSCR                         *
* out       : ELFLUX(IM), EPSELU(I,M)                                  *
* in & out  :                                                          *
************************************************************************
* passed    : M = material indicator                                   *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculation of the elutriation flow and the volume       *
*             fraction of the solids at the riser's top                *
*             for every particle diameter and material                 *
*             according to Wirth, K.E. 'Zirkulierend Wirbelschichten'  *
*             Springer Verlag (1990)                                   *
*             !!! includes 3 own subrotines !!!                        *
*             or Yang(1988) and Wen & Chen(1982)                       *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_RHOG, Z_VIS, Z_VELO, Z_EXP
      REAL*8      AR, DUMMY, EPSFIX, FR, FRMX, FRUMF, FRWF
      REAL*8      PHIHM, PHIO, RHOG, VISG, VSV 
      REAL*8      VELOAV, SUMPRT, VELO, AA, UU, EE
      INTEGER     I, M, STATE(NI),K


      REAL*8     A, HB, HT, EI, ED, Z_EPSS11  ! auxiliaries


      IF (WRTSCR) write (*,*) 'ELUTRI    ',M

C*     option(1)=0 --> Wirth,   = 1 --> Wen and Chen

      IF (OPTION(1) .EQ. 0.) GOTO 5

       DO I=1,NCLASS

       UU = U(CMP(1)) - Z_VELO(DIAM(I),M)


      IF (UU .GT. 0.) THEN
       AA = 1. + 6.15 / G / RHOS(M) / SQRT(DIAM(I)) / Z_EXP(UU,0.5)
       EE = -1./4.7
       EPSELU(I,M) = 1. - Z_EXP(AA,EE)
      ELSE
       EPSELU(I,M) = 1.E-6
      ENDIF
      
            IF (((EPSSHM(M)-EPSELU(I,M))/EPSSHM(M)) .LT. 0.001) THEN
                  EPSELU(I,M) = EPSSHM(M) * 0.999
            ENDIF

C*         saturation elutriation flux [kg/m^2s]
        ELFLUX(I,M) = EPSELU(I,M)*(U(CMP(1))-Z_VELO(DIAM(I),M))*RHOS(M)
            IF (ELFLUX(I,M) .LT. 1.E-8) THEN
                  ELFLUX(I,M) = 1.E-8
            ENDIF

       ENDDO

       GOTO 20

 5     CONTINUE

      RHOG = Z_RHOG(TBED,PRESS(1))
      VISG = Z_VIS(TBED)

      IF (M .EQ. 4) THEN
          VELOAV = 0.
          SUMPRT=0.
         DO I=1,NCLASS
             VELO = Z_VELO(DIAM(I),5)
            IF (VELO .LT. U(CMP(1))) THEN
                VELOAV = VELOAV + Z_VELO(DIAM(I),5)*PART(I,5)
                SUMPRT = SUMPRT + PART(I,5)
            ENDIF
         ENDDO
           VELOAV = VELOAV / SUMPRT
      ENDIF

         DO 10 I=1,NCLASS

C*           voidage in clusters
         EPSFIX  = 0.4

         PHIHM = 1. - EPSSHM(M) / (1.-EPSFIX)

C*       superficial Fr-number and Ar-number [-]
         FR = U(CMP(1))/((RHOS(M)-RHOG)*DIAM(I)*G/RHOG)**.5
         AR = (RHOS(M)-RHOG)*RHOG*(DIAM(I))**3.*G/(VISG**2.)

C*       terminal Fr-number
        IF (M .NE. 4) THEN
          FRWF = Z_VELO(DIAM(I),M)/((RHOS(M)-RHOG)*DIAM(I)*G/RHOG)**.5
         ELSE
           FRWF = VELOAV           /((RHOS(M)-RHOG)*DIAM(I)*G/RHOG)**.5
         ENDIF

C*       Fr of minimal fluidization
         FRUMF= .05*FRWF

         IF (FR .LE. FRWF) THEN
            VSV = 0.
            PHIO = 1.
            STATE(I) = 1
         ELSE
C*           iteration maximum Fr-Number (-)
             FRMX  = 1.

             CALL SUB02 (PHIHM, FRWF, FRUMF, FRMX)

C*           iteration strand-volume ratio PHIO (-)
             PHIO = .9
             CALL SUB03 (PHIHM,FR,FRWF,FRUMF,FRMX,PHIO)

             IF (PHIO .GE. 1.-1.E-7) THEN
                VSV = 0.
                STATE(I) = 1
             ELSE
                CALL SUB01 (FR,FRWF,FRUMF,PHIO,VSV,DUMMY)
                IF (ABS(PHIO-PHIHM) .LE. 1.E-4) THEN
                  STATE(I) = 3
                ELSE
                  STATE(I) = 2
                ENDIF
             ENDIF
          ENDIF

C*         saturation solid part of fraction [-]
           EPSELU(I,M) = (1.-PHIO)*(1.-EPSFIX)
            IF (ABS((EPSSHM(M)-EPSELU(I,M))/EPSSHM(M)) .LT. 0.001) THEN
                  EPSELU(I,M) = EPSSHM(M) * 0.999
            ELSE IF (EPSELU(I,M) .LT. 1.E-6) THEN
                  EPSELU(I,M) = 1.E-6
            ENDIF

C*         saturation elutriation flux [kg/m^2s]
           ELFLUX(I,M) = VSV*RHOS(M)*(1.-EPSFIX)*U(CMP(1))
            IF (ELFLUX(I,M) .LT. 1.E-8) THEN
                  ELFLUX(I,M) = 1.E-8
            ENDIF

 10        CONTINUE

 20        CONTINUE

       IF (WRTSCR) write(*,*) ' END ELUTRI '

C*********************** TESTWRITER **************************************


!     OPEN(UNIT=17,FILE='ELUTRI'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')
!        WRITE(17,*) 'Solids concentration at riser top by Wirth, ',
!     &               '  Material',M
!        WRITE(17,*) 'diameter    EPS         sat.carr    term velo    ',
!     &              'state fluid.'    
!        WRITE(17,*) ' [æm]        (-)      (kg/m2s)      (m/s)      see'
!     &  ,' below'
!
!       DO I=1,NCLASS
!         WRITE(17,1001) DIAM(I)*1000, EPSELU(I,M),
!     &                  ELFLUX(I,M) , Z_VELO(DIAM(I),M), STATE(I)
!       ENDDO      

! 1001   FORMAT(F10.3,2X,F10.6,2X,F10.4,2X,F10.6,2X,I10)
!        WRITE(17,*)
!        WRITE(17,*) ' EPS homogeneous ',EPSSHM(M)
!        WRITE(17,*)
!        WRITE(17,*) ' state of fluidization for each sieve class:'
!        WRITE(17,*) ' 0=not calc. 1=bubbl.bed  2=circ.bed  3=pn.transp.'

!      CLOSE(17)
        
       RETURN

       END


      SUBROUTINE SUB01 (FR, FRWF, FRUMF, PHI, VSV, XI)

C*    this routine solves the 2nd order equation for the volumetric 
C*    flow ratio (VSV) and delivers the deviation of the stability 
C*    criterion

      REAL*8  LAMBDA,EPSFIX,FR,FRWF,FRUMF
      REAL*8  PHI,VSV, VSV2,Xi,AA,BB,CC

      EPSFIX = .4
      LAMBDA = .0053

C*    solution of equation 2nd degree

      AA = (EPSFIX*(1.-PHI))**2+PHI*EPSFIX*(1.-PHI)

      BB = 2.*EPSFIX*(1.-PHI)*(FRUMF/FR*(1.-PHI)**2.-1.)
     &     +EPSFIX*(1.-PHI)*PHI*FRWF/FR+PHI*FRUMF/FR*(1.-PHI)**2.-PHI

      CC = (FRUMF/FR*(1.-PHI)**2.-1.)**2.
     &     +PHI*(FRUMF/FR*(1.-PHI)**2.-1.)
     &     *FRWF/FR-((1.-EPSFIX)*PHI**3.*(1.-PHI))/(LAMBDA*FR**2.)  

C*      VSV1 = (1.-PHI)*(-.5*BB/AA+((.5*BB/AA)**2.-CC/AA)**(.5))

      VSV2 = (1.-PHI)*(-.5*BB/AA-((.5*BB/AA)**2.-CC/AA)**(.5))

      VSV  = VSV2 
C*    Only VSV2 makes mathematical sense !

C*    calculation of deviation Xi by stability equation

      XI =   FR**2.*(1.-EPSFIX*VSV)
     &         *(VSV *( PHI*(4.*PHI-2.) + EPSFIX*(1.-PHI)*(4.*PHI-3.) )
     &          -(1.-PHI)*(4.*PHI-3.)  )
     &    +  FR*FRWF*(1.-EPSFIX*VSV)
     &         *(1.-PHI)*PHI*(3.*PHI-2.)  
     &    +  FR*FRUMF*(1.-PHI)**3. 
     &         *( (1.-EPSFIX*VSV)*(4.*PHI-6.) + VSV*2.*PHI )
     &    +  FRUMF*FRWF*(1.-PHI)**3.*(2.-PHI)*PHI
     &    +  FRUMF**2.*3.*(1.-PHI)**5.

      END


      SUBROUTINE SUB02 (PHIH, FRWF, FRUMF, FRMX)

C*    calculation of the stable FR number FRMX by Newton Raphson

      REAL*8   PHIH,FRWF,FRUMF,FRMX,DUMMY
      REAL*8   XI,FRMX1,XI1,FRMX2,XI2,XIY
      INTEGER            N, NMAXI
        
        N    = 0
        NMAXI = 50

C*    calculation of FRmx by modified NEWTON-algoritm

C**** *************REPEAT**************************
10    CALL SUB01 (FRMX, FRWF, FRUMF, PHIH, DUMMY,XI)

      FRMX1 = (1.-0.01)*FRMX
        CALL SUB01 (FRMX1, FRWF, FRUMF, PHIH, DUMMY,XI1)
 
      FRMX2 = (1.+0.01)*FRMX
        CALL SUB01 (FRMX2, FRWF, FRUMF, PHIH, DUMMY , XI2)
 
      FRMX = FRMX-(FRMX2-FRMX1)*XI/(XI2-XI1)
        N  = N+1
      IF (N .GE. NMAXI) RETURN

      FRMX1 = (1.-1.E-7)*FRMX
        CALL SUB01 (FRMX1, FRWF, FRUMF, PHIH, DUMMY, XI1)
 
      FRMX2 = (1.+1.E-7)*FRMX
        CALL SUB01 (FRMX2, FRWF, FRUMF, PHIH, DUMMY, XI2)
 
      XIY = XI1*XI2
      IF (XIY .GT. 0.) GOTO 10
C**** ****************UNTIL*****************************
      END


      SUBROUTINE SUB03 (PHIH, FR, FRWF, FRUMF, FRMX, PHIO)

C*    calculation of the strand-free fraction in the flow at 
C*    the riser's top

      REAL*8  PHIH,PHIO,FR,FRWF,FRUMF,FRMX
      REAL*8  DUMMY,XI,PHIO1,XI1,PHIO2,XI2,XIY
      INTEGER           N, NMAXI

      IF (FR .LE. FRWF) THEN
         PHIO =1.
         RETURN                       
      ENDIF

      IF (FR .GE. FRMX) THEN
         PHIO = PHIH
         RETURN
      ENDIF

      N     = 0
      NMAXI = 50

C*    Calculation of PHIo by modified NEWTON-algoritm

C**** *************REPEAT*****************************
30      CALL SUB01 (FR, FRWF, FRUMF, PHIO, DUMMY, XI)

           PHIO1 = 1.-(1.-PHIO)*(1.-0.01)
           CALL SUB01 (FR, FRWF, FRUMF, PHIO1, DUMMY, XI1)

           PHIO2 = 1.-(1.-PHIO)*(1.+0.01)
           CALL SUB01 (FR, FRWF, FRUMF, PHIO2, DUMMY, XI2)

         PHIO = PHIO-(PHIO2-PHIO1)*XI/(XI2-XI1)
         N    = N+1

      IF (N .GE. NMAXI) RETURN

           PHIO1 = 1.-(1.-PHIO)*(1.-1.E-7)
           CALL SUB01 (FR, FRWF, FRUMF, PHIO1, DUMMY, XI1)

           PHIO2 = 1.-(1.-PHIO)*(1.+1.E-7)
           CALL SUB01 (FR, FRWF, FRUMF, PHIO2, DUMMY, XI2)

        XIY = XI1*XI2
      IF (XIY .GT. 0.) GOTO 30
C**** **********UNTIL*************************************

      END   ! (ELUTRI0)

