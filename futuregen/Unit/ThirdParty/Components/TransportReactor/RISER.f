
      SUBROUTINE RISER

************************************************************************
* LAST CHANGE AT : 19.09.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_RHOG                                                   *
************************************************************************
* COMMON in : AEXPIN(M), ADDAIR(K), ADDFLU(K), AREA(K),                *
*             CMP(), COALM(), FEED(M), MWG(J), NCELL(K), NCOMP,        *
*             PRESS(1-2), RHOS(M), RG, TAP, TBED, WRTOUT, WRTSCR, XGAS *
* out       : AEXPO(M), BEDMAS, EPSSHM(M), NCELLS, U(K), UANNU(K),     *
*             UBUBBL(K), TEMP(K), KCO, KAN, KBU                        *
* in & out  : HEIGHT(K), WIDTH(K), LENGTH(K), WALL(K), TUBE(K)         *
************************************************************************
* passed    : -                                                        *
* rturned   : -                                                        *
************************************************************************
* PURPOSE   : calculation of the riser's superficial gas velocity,     *
*             the volumetric gas flow, the cross sectional area per    *
*             compartment, the bed mass and the riser volume           *
*             the riser is divided into the number of cells per        *
*             compartment, as given in the input file                  *
*             the indicators CMP(K) are adapted                        *
************************************************************************

       INCLUDE 'PARAMTR.FTN'
       INCLUDE 'COMMONBL.FTN'

       REAL*8   Z_RHOG, RHOG, ! gas density (kg/m3)
     &          GASFL,        ! gas flow rate (m3/s)
     &          COMVOL        ! combustor volume (m3)
       REAL*8   HEINEW(0:NK), ! height of riser (temporary variable)
     &          WIDNEW(0:NK), ! width  ""
     &          LENNEW(0:NK), ! lenth  "" 
     &          ADANEW(0:NK), ! air added at level (m3/s)
     &          ADFNEW(0:NK), ! flue gas added at level (m3/s)
     &          RELNEW(0:NK), ! gas released by solids (m3/s)
     &          RELEAS(0:NK)  !    "  "
       REAL*8   H2OREL(NK)    ! moisture released (m3/s)
       REAL*8   TUBNEW(0:NK), ! new tube surface ratio (m2/m3)
     &          WALNEW(0:NK), ! new water wall ratio (m2/m2)
     &          UMIN,         ! slowest gas velocity in riser (m/s)
     &          UMAX          ! highest gas velocity in riser (m/s)
       INTEGER  II,           ! particle size indicator
     &          K,            ! cell indicator
     &          KC,           ! cell indicator
     &          KK,           ! cell indicator
     &          KCMP(6),      ! indicator for I/O cells
     &          M,            ! material indicator
     &          KMIN,         ! pointer to cell with slowest gas velocity
     &          KMAX          ! pointer to cell with highest gas velocity

       IF (WRTSCR) write(*,*) '  RISER GEOMETRY '

       RHOG = Z_RHOG(TBED,PRESS(1))

        DO K=1,NCOMP
C*              estimate volatiles and moisture release 
         IF (K .EQ. CMP(3)) THEN
          H2OREL(K) = FEED(COAL)*COALM(MOIS)/MWG(H2O)*RG*TBED/PRESS(1)
         ELSE
          H2OREL(K) = 0.
         ENDIF
        ENDDO

C*              rename compartment indicators of input file 
C*              to cell indicators considering chosen number of cells 
C*              per compartment
           DO II = 1 ,6
               KCMP(II) = 0
             DO K = 1, (CMP(II)-1)
               KCMP(II) = KCMP(II) + NCELL(K)
             ENDDO
               CMP(II) = KCMP(II)
           ENDDO

             KK = 0
           HEINEW(0) = HEIGHT(1)
           WIDNEW(0) = WIDTH(1)
           LENNEW(0) = LENGTH(1)
           ADANEW(0) = 0.
           ADFNEW(0) = 0.
           RELNEW(0) = 0.
           TUBNEW(0) = 0.
           WALNEW(0) = 0.

      DO 10 K = 1, NCOMP
                                             

         DO KC = 1, NCELL(K)
             KK=KK+1
           HEINEW(KK) = HEIGHT(K) + KC*(HEIGHT(K+1)-HEIGHT(K))/NCELL(K)
           WIDNEW(KK) = WIDTH(K)  + TAP(K) * (HEINEW(KK)-HEIGHT(K))
     &                  /(HEIGHT(K+1)-HEIGHT(K))*(WIDTH(K+1)-WIDTH(K))
           LENNEW(KK) = LENGTH(K+1)    !LENGTH(K)
           WALNEW(KK) = WALL(K)                  
           TUBNEW(KK) = TUBE(K)  

           IF (KC .EQ. 1) THEN
               ADANEW(KK) = ADDAIR(K)
               ADFNEW(KK) = ADDFLU(K)
               RELNEW(KK) = H2OREL(K) 
           ELSE
               ADANEW(KK) = 0.
               ADFNEW(KK) = 0.
               RELNEW(KK) = 0.
           ENDIF
         ENDDO

 10   CONTINUE

           NCELLS = KK

         DO K=0, NCELLS
           HEIGHT(K) = HEINEW(K)
           WIDTH(K)  = WIDNEW(K)
           LENGTH(K) = LENNEW(K)
           AREA(K)   = WIDTH(K)*LENGTH(K)
           TUBE(K)   = TUBNEW(K)                   
           WALL(K)   = WALNEW(K)
           ADDAIR(K) = ADANEW(K)
           ADDFLU(K) = ADFNEW(K)
           RELEAS(K) = RELNEW(K)
         ENDDO

         
           GASFL    = 0.
         DO K = 1, NCELLS
           GASFL     = GASFL + ADDAIR(K) + ADDFLU(K) + RELEAS(K)
           U(K)      = GASFL/AREA(K)*1.01325E5/PRESS(1)*TBED/273.15
           UANNU(K) = U(K) / 3.
C*           provisorical bubble velocity, exact calculation in GASMIX
           UBUBBL(K)= U(K)
	   ENDDO
C*           set first and last cells
         U(0)             = U(1)
         UANNU(0)         = UANNU(1)
         UBUBBL(0)        = UBUBBL(1)
         U(NCELLS+1)      = U(NCELLS)
         UANNU(NCELLS+1)  = UANNU(NCELLS)
         UBUBBL(NCELLS+1) = UBUBBL(NCELLS)

C*             total combustor volume (m3)
         COMVOL = 0.
      DO K=1, NCELLS
        COMVOL = COMVOL + (HEIGHT(K)-HEIGHT(K-1))*AREA(K)
      ENDDO

C*       the gas velocity responsible for elutriation can be chosen either
C*       as minimum or maximum velocity
      IF (CMP(1) .EQ. 0) THEN
              UMIN = U(1)
              UMAX = U(1)
         DO K=1,NCELLS
          IF (U(K) .LT. UMIN) THEN
              UMIN = U(K)
              KMIN = K
          ENDIF
          IF (U(K) .GT. UMAX) THEN
              UMAX = U(K)
              KMAX = K
          ENDIF
         ENDDO
C*           CMP(1) = KMIN                     
             CMP(1) = KMAX                     
       ENDIF
!zc
           DO II = 1 ,6
              IF(CMP(II).EQ.0) CMP(II) = 1
           ENDDO
!zc


C*       exponential decay constant
      DO M = 1,4
         AEXPO(M) = AEXPIN(M)/U(CMP(1))
      ENDDO
          
C*          calc. homogeneous solids fraction                               
      DO M = 1,4
        EPSSHM(M) = PRESS(2)/(HEIGHT(NCELLS)-HEIGHT(0))
     &              /G/(RHOS(M)-Z_RHOG(TBED,PRESS(1)))
      ENDDO
                             
C*         first guess for mass in riser                             
      BEDMAS = EPSSHM(INERT)*COMVOL*RHOS(INERT)


C*          define pointers for core, annulus and bubbles
         KCO = 0
         KAN = NCELLS
         KBU = NCELLS + NCELLS
 
C*         starting values for riser temperature
      DO K=1, NCELLS 
         TEMP(K,CORE) = TBED
         TEMP(K,ANNU) = TBED  
         TEMP(K,BUBB) = TBED
         XGAS(O2,K,BUBB) = 0.05
         XGAS(O2,K,BUBB) = 0.05
         XGAS(O2,K,BUBB) = 0.05
      ENDDO 



C*    ************** TESTWRITER *******************************

      IF (.NOT. WRTOUT(13)) RETURN

      OPEN(UNIT=55,FILE='RISER.DAT',STATUS='UNKNOWN')

      WRITE(55,*)  'Combuster geometry, local gas velocities and flows'
      WRITE(55,*)  'No   LEVEL     AREA      GAS-VELOCITY '

      DO K=0,NCELLS
         WRITE(55,1010) K,  HEIGHT(K),AREA(K) ,U(K)
      ENDDO

      WRITE(55,*)     
      WRITE(55,*)'No   LEVEL     GAS-CORE  GAS-ANNU  GAS-BUBBLE '
      WRITE(55,*)  '  Gas - Velocities '

      DO K=1,NCELLS
         WRITE(55,1020) K, HEIGHT(K), U(K), UANNU(K), UBUBBL(K)
      ENDDO

         WRITE(55,*)
         WRITE(55,*) ' cell responsible for'
         WRITE(55,*) ' elutriation   ',CMP(1)
         WRITE(55,*) ' lime feed     ',CMP(2)
         WRITE(55,*) ' coal feed     ',CMP(3)
         WRITE(55,*) ' cyclone back  ',CMP(4)
         WRITE(55,*) ' ext. heat ex  ',CMP(5)
         WRITE(55,*) ' riser exit    ',CMP(6)
         WRITE(55,*) 
         WRITE(55,*) 'number of cells  =  ',NCELLS
         WRITE(55,*)
         WRITE(55,*) 'mass in riser    =  ',BEDMAS
         WRITE(55,*)
         WRITE(55,*) 'conbustor volume =  ',COMVOL

      CLOSE(55)

 1010  FORMAT(I3,2X,F6.2,4X,F6.2,4X,F6.3)
 1020  FORMAT(I3,2X,F6.2,3(4X,F6.3))

      RETURN
      END       ! (RISER)
