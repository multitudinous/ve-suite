
      SUBROUTINE PREPOP 

************************************************************************
* LAST CHANGE AT :  22.10.1993              BY:   J. Hannes            *
************************************************************************
* CALLED BY : PREPAR                                                   *
* CALLS     : BEDHGT, BOTTOM, REXIT, CYCLON, ATTRIT, LINEQS            * 
*             REXIT, ELUTRI0                                           *
************************************************************************
* COMMON in : AREA(K), BEDMAS, DIAM(I), EFFCYC(I,M),                   * 
*             EFFTOP(I,M), ELFLUX(I,M), FEED(M), OPTION(3)             *
*             NI, NCLASS, SIVFED(I,M), TTT(I,M), WRTSCR                *
* out       : PART(I,M), BAGCL(I,M), RECCL(I,M), OVERCL(I,M), MEAND(M) *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : averaged pre-calculation of a virtual population         *
*             to get better starting values in popul                   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8     AA(NI,NI),      ! lin. eqs. matrix
     &           Y(NI)           ! lin. eqs. array
      REAL*8     X(NI,NI,NM)     ! attrition result matrix
      REAL*8     SEGREG(NI,NM),  ! segregation funct. (-)
     &           SIFTER(NI,NM)   ! sifter funct. (-)
      REAL*8     RELAX,          ! relaxation factor
     &           RESIDU,         ! iteration residue
     &           OVERSM, SUM     ! auxiliaries
      REAL*8     BAG, OVER, RECY ! total mass flows flue-,coarse-,recycle-ash
      INTEGER    I, II, L        ! loop counters

      IF (WRTSCR) WRITE(*,*) 'PREPOP '

C*               elutriation rates
!zc        CALL ELUTRI(BED)

C*               initial guess for size distribution
           DO  I=1,NCLASS
                PART(I,BED)   = 0. ! SIVFED(I,BED)
                BAGCL(I,BED)  = 0.
                OVERCL(I,BED) = 100.
           ENDDO  
                 PART(1,BED) = 1.
                RESIDU = 1.
C*      *************** REPEAT **************************
       DO 400 L = 1,1000

         RELAX = 0.9
                     
C*               mean particle diameter                     
              SUM = 0.
         DO  I = 1,NCLASS
              SUM = SUM + (PART(I,BED) / DIAM(I))
         ENDDO
         IF (SUM .GT. 0.)  THEN
              MEAND(BED) = 1.0 / SUM
         ELSE
              MEAND(BED) = 0.
         ENDIF

C*               average residence times

          CALL BEDHGT
          CALL ELUTRI(BED)
          CALL BOTTOM(BED,SEGREG,SIFTER)

C*              calculate size changes due to attrition and combustion
C*              no shrinking due to reaction for bed particles
C*              separation efficiency of the riser exit
          CALL REXIT(BED)

C*                 cyclone efficiency
          CALL CYCLON(BED)
        DO I=1,NCLASS
            BAGCL(I,BED) = ELFLUX(I,BED) * AREA(CMP(1))
     &               * (1.-EFFTOP(I,BED)) * (1.-EFFCYC(I,BED))
            RECCL(I,BED) =  ELFLUX(I,BED) * AREA(CMP(1))
     &               * (1.-EFFTOP(I,BED)) * EFFCYC(I,BED)
            TTT(I,BED) = BEDMAS /(BAGCL(I,BED)+OVERCL(I,BED))
        ENDDO                        
        
          CALL ATTRIT(BED,X)

          BAG    = 0.
          RECY   = 0.
          OVERSM = 0.

C*           ..CL  specific rates for bag, recirculation, overflow, reaction
C*                    total rates for "   "
        DO I=1,NCLASS
            BAG     = BAG    + BAGCL(I,BED)  * PART(I,BED)
            RECY    = RECY   + RECCL(I,BED)  * PART(I,BED)
            OVERSM  = OVERSM + SEGREG(I,BED) * SIFTER(I,BED)*PART(I,BED)
        ENDDO
                          
             SUM = 0.             
        DO II = 1,NCLASS
          DO I = 1, NCLASS
             SUM = SUM + X(I,II,BED)*PART(I,BED)
          ENDDO   
             SUM = SUM + BAGCL(II,BED)*PART(II,BED)
        ENDDO    

          OVER = FEED(BED) - SUM                        

      IF (OVER .LT. 0.) THEN
         WRITE(*,*) 'Bed material too fine to run steady state'
         WRITE(*,*) 'Check attrition, cyclone or feed rate and ',
     &              'distribution'
          write(*,*) 'FEED  ',FEED(BED)
          write(*,*) 'BAG   ',BAG
          write(*,*) 'OVER  ',OVER
         STOP 
      ENDIF

      IF (OPTION(3) .GT. 0) THEN
          DO I = 1,NCLASS
            OVERCL(I,BED) = OVER*SEGREG(I,BED)*SIFTER(I,BED)/OVERSM 
          ENDDO          
      ELSE
          DO I = 1,NCLASS
            OVERCL(I,BED) = OVER
          ENDDO          
      ENDIF

C*            define right hand array (Y) of linear eq. set
C*            prepare matrix fractions (X) of Matrix (AA) 
         DO I = 1, NCLASS
            Y(I) = SIVFED(I,BED)*FEED(BED)
            X(I,I,BED) = X(I,I,BED) + OVERCL(I,BED) + BAGCL(I,BED) 
         ENDDO

C*            initialize matrix (AA)
        DO I=1,NCLASS
         DO II=1,NCLASS
          AA(I,II) = 0.
         ENDDO
        ENDDO

C*           define matrix (AA) with fractions (X)
C*           change I,II due to FORTRAN reading sequence, first columns
C*                                                        then lines 
          DO I = 1,NCLASS
            DO II=1,NCLASS
              AA(II,I) = X(I,II,BED)
            ENDDO
          ENDDO

C*            linear equation solver
          CALL LINEQS(AA,NCLASS,NI,Y)

           SUM    = 0.
           RESIDU = 0.
         DO I=1,NCLASS
            RESIDU = RESIDU + ABS(Y(I)-PART(I,BED))
            PART(I,BED) = PART(I,BED) + (Y(I)-PART(I,BED))*RELAX
            SUM = SUM + PART(I,BED)
         ENDDO

C*               normalize Part(I,BED)
             DO I=1,NCLASS
               PART(I,BED) = PART(I,BED)/SUM
                IF (PART(I,BED) .LT. 0.) THEN
                   WRITE(*,*) 'Part negative at (',I,BED,')'
                ENDIF
             ENDDO


         WRITE(IOUT,1001) L,ABS(SUM-1.),RESIDU, OVER
 1001    FORMAT(I5,'  Error= ',E12.6,'  Residue= ',E12.6,' OVER=',
     &             E12.6)

         IF (RESIDU .LT. 1.E-5 .AND. L .NE. 1) THEN
             LPOPUL = L
             GOTO 500
         ENDIF

 400  CONTINUE
C*     ****************** UNTIL ******************************

C*       normal exit indicates failure of convergence
      WRITE(*,*)' convergence error in PREPOP, Residue = ',RESIDU
      WRITE(*,*)' the combustor can not be run steady state under these
     &conditions'
      WRITE(*,*)'Possible reasons: Baghouse loss higher than feed rate'
      WRITE(*,*)'                  Attrition const. too high'
      WRITE(*,*)'                  Cyclone model not applicable'
      PAUSE

 1002 FORMAT(A1)

 500  CONTINUE
 
      write(*,*) ' PREPOPULATION BALANCE DONE after ', 
     &             LPOPUL, ' iterations !'
      write(*,*)        

      RETURN                     
      
      END       ! (PREPOP)


