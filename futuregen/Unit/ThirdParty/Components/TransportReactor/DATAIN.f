
      SUBROUTINE DATAIN

************************************************************************
* LAST CHANGE AT : 13.11.96                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_NAME                                                   *
************************************************************************
* COMMON in :                                                          *
* out       : ACHAR, ADDAIR(K), ADDFLU(K), AEXPIN, ECHAR, KDIFFU,      *
*             ALFMAX, SBET, KS, XCACO3, ALFATB, ALFAWL, ATTCON(M),     *
*             CMP(1-5), COALM(1-9), CYCDAT(1-9), FRGFAC, HEATCP(M),    *
*             HEIGHT(K), LENGTH(K), MESH(I),NCELL(K), NCOMB, NCYC,     *
*             PRESS(1-4), OPTION(1-9), REHE, RHOS(M),                  *
*             SPHR(M), SIVFED(I,M), FEED(M), TAP(K), TBED, TEHE, TIN,  *
*             TAMB, TFLU, TUBE(K), WALL(K), WIDTH(K), WRTOUT, WRTSCR   *
* in & out  :                                                          *
************************************************************************
* passed    : FLNAME = name of input file                              *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : reads input data file and performs transformations to    * 
*             SI-units                                                 *
************************************************************************
!  M - Material Index, 1 = coal; 2 = lime; 3 = inert; 4 = char; 5 = bed

        INCLUDE 'PARAMTR.FTN'
        INCLUDE 'COMMONBL.FTN'
 
        CHARACTER*12       Z_NAME
        INTEGER            FLAG1, DUMMY, I, K, L, M, IERR
        REAL*8             T1, T2, T3, T4, T5, WOUT, WSCR, Gs

	  COMMON /Solids/Gs
C*                get name of input file
         FLNAME = Z_NAME()

C*                 check file exists, and can be opened
        OPEN (UNIT = 66, FILE = FLNAME, STATUS = 'OLD', IOSTAT = IERR)
                 IF (IERR .NE. 0)  THEN
                   WRITE (IOUT,*) 'ERROR OPENING FILE ',FLNAME
                   STOP
                 END IF
        REWIND(66)

      READ(66,*)     ! line 1
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*)     ! line 8
      READ(66,*)     ! line 9
C*                          combustor lines 10 - 29
                   NCOMP = 0
          DO 10 K=1,20
C*                     level     width     length    air      flue gas
      READ(66,*)DUMMY,HEIGHT(K),WIDTH(K),LENGTH(K),ADDAIR(K),ADDFLU(K),
C*
C*              tapered?  tube wall?  tube bundles?  numb of cells per comp
     &          TAP(K),   WALL(K),    TUBE(K),       NCELL(K)

C*                   estimate last value of level as combustor height
          IF (K .GT. 1 .AND. HEIGHT(K) .LE. 0. .AND. NCOMP .EQ. 0) THEN
             NCOMP = K-2
          ENDIF  
          IF (NCELL(K) .LE. 0) THEN 
             NCELL(K) = 10.
          ENDIF
10        CONTINUE      

      READ(66,*)     !  line 30
      READ(66,*)
      READ(66,*)     !  line 32

C*            corresponding compartment for: 
C*            elutriation, coalfeed, limefeed, recycle flow, external flow
      READ(66,*) CMP(1), CMP(2), CMP(3), CMP(4), CMP(5), CMP(6)

      READ(66,*)   ! line 34
      READ(66,*)
      READ(66,*)   ! line 36

C*            cyclone data:
C*               number of cyclones
      READ(66,*) NCYC

      READ(66,*)
C*               height    ht of tube   wall diam    tube diam
      READ(66,*) CYCDAT(1),CYCDAT(2),   CYCDAT(3),   CYCDAT(4)
      READ(66,*)

C*                rad. inlet  ht inlet   wdt inlet   acc coeff
      READ(66,*) CYCDAT(5),  CYCDAT(6), CYCDAT(7),  CYCDAT(8)

      READ(66,*)
      READ(66,*) CYCDAT(9)

      READ(66,*)
      READ(66,*)
      READ(66,*)


C*             feed rates of materials [kg/s]
      READ(66,*) (FEED(M), M=1,3)

      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*) REHE,TEHE

      READ(66,*)
      READ(66,*)
      READ(66,*)

C*               sieve results 20 sieve classes
                   FLAG1 = 0
          DO 20  I = 1, 20
      READ(66,*) DUMMY,MESH(I),(SIVFED(I,M), M = 1, 3)
            IF (MESH(I) .LE. 0. .AND. FLAG1 .EQ. 0) THEN
                NCLASS = I
                FLAG1 = 1         
            ENDIF
		  IF(I.EQ.20.AND.MESH(I).NE.0.) THEN
                NCLASS = I
		  ENDIF
   20     CONTINUE

      READ(66,*)
      READ(66,*)
      READ(66,*)

C*             temperature (BED, AIR INLET, AMBIENT, FLUE GAS RECIRC, WALL)
      READ(66,*) T1, T2, T3, T4, T5

C*                    pressure + pressure drop in riser + cyclone
      READ(66,*)
      READ(66,*)

      READ(66,*) PRESS(1), PRESS(2), PRESS(3)

      READ(66,*)
      READ(66,*)
      READ(66,*) ALFATB, ALFAWL
      READ(66,*)
      READ(66,*)
      READ(66,*)      

      READ(66,*) (RHOS(M)  , M = 1, 4)
      READ(66,*)
      READ(66,*) (SPHR(M)  , M = 1, 4)
      READ(66,*)
      READ(66,*) (HEATCP(M), M = 1, 4)
      READ(66,*)
      READ(66,*) (ATTCON(M), M = 1, 4)
      READ(66,*)
      READ(66,*) (FRGFAC(M), M = 1, 4)
      READ(66,*)
      READ(66,*) (AEXPIN(M), M = 1, 4)

C*              coal chemical analysis
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*) (COALM(I), I = 1, 5)
      READ(66,*)
      READ(66,*)
      READ(66,*) (COALM(I), I = 6, 9)

C*                 NO/N split in char N release, rest gives N2O
      READ(66,*)
      READ(66,*)
      READ(66,*)
      READ(66,*) SPLTN
C*                 Combustion:    A, E0/R,  D
      READ(66,*)
      READ(66,*)
      READ(66,*) ACHAR, ECHAR, KDIFFU
C*                SO2:    BET    Alfmax   ks     Xcaco3
      READ(66,*)
      READ(66,*)
      READ(66,*) SBET, ALFMAX, KSUR, XCACO3

C*       perform population balance 1=yes, 0=no
C*       perform gas phase
      READ(66,*)
      READ(66,*)
      DO L=1,10
      READ(66,*)
      READ(66,*) OPTION(L)
      ENDDO
      READ(66,*)
      READ(66,*)
      READ(66,*) WSCR
      READ(66,*)
      READ(66,*)
      READ(66,*) WOUT
      READ(66,*)
      READ(66,*)
      READ(66,*) Gs
      READ(66,*)
C ADD XFEED READ BY YANG
      READ(66,*)
      READ(66,*) (XFEED(L), L=1, 14)   
      CLOSE(66)

C************************* transformations ****************************

      IF (WSCR .EQ. 1.) THEN
       WRTSCR = .TRUE.
      ELSE
       WRTSCR = .FALSE.
      ENDIF

      IF (WOUT .EQ. 1.) THEN
       DO L=1,20
        WRTOUT(L) = .TRUE.
       ENDDO
      ELSE
       DO L=1,20
        WRTOUT(L) = .FALSE.
       ENDDO
      ENDIF

C*                 transform diameter from (µm) to (m)
               DO I = 1,NCLASS
                   MESH(I) = MESH(I)/1.E6
                  DO M=1,3
                   SIVFED(I,M) = SIVFED(I,M) / 100.
                  ENDDO
               ENDDO

C*                     transform degree Celsius to Kelvin
                    TBED = T1 + 273.15
                    TIN  = T2 + 273.15
                    TAMB = T3 + 273.15
                    TFLU = T4 + 273.15
                    TWALL= T5 + 273.15
                    TEHE = TEHE + 273.15

                    PRESS(4) = PRESS(2)

                DO I=1,9
                   COALM(I) = COALM(I)/100.
                ENDDO


            RETURN
           END        ! (DATAIN)


