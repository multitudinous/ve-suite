
      SUBROUTINE POPUL 

************************************************************************
* LAST CHANGE AT :  24.05.1995              BY:   J. Hannes   RWTH     *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : REXIT, BOTTOM, CYCLON, ATTRIT, LINEQS                    * 
************************************************************************
* COMMON in : AREA(K), BEDMAS, CMP(1), EFFCYC(I,M), EFFTOP(I,M),       *
*             ELFLUX(I,M), FEED(M), NCLASS, NI, SIVFED(I,M), WRTSCR    *
*             OPTION(3), CMAS                                          *
* out       : PART(I,M), BAGCL(I,M), RECCL(I,M), OVERCL(I,M), TTT(I,M) *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : calculation of the population balance of each material   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8   AA(3*NI,3*NI),! input matrix for linear eq. solver
     &         Y(3*NI)      ,! input and return array for lin.eq.solv.
     &         X(NI,NI,NM)  ,! array carrying attrition information
     &         XX(NI,NI,NM) ,!           "           "
     &         SEGREG(NI,NM),! segregation coefficient at bottom bed
     &         SIFTER(NI,NM) ! wind sifter coefficient at bottom bed

      REAL*8   RELAX        ,! relaxation factor
     &         RESIDU        ! residue

      REAL*8   OVERSM       ,! auxiliary 
     &         SUM,SUMA,SUMP ! auxiliary

      REAL*8   BAG          ,! total mass flow to baghouse (kg/s)
     &         OVER         ,! total coarse ash discharge (kg/s)
     &         RECY          ! total recirculation flow (kg/s)

      INTEGER  I, II, L, M   ! counters for size, loop and material

      IF (WRTSCR) WRITE(*,*) ' POPUL '


C*      population of "virtual coal" 
C*      coal is calculated to move only inside the riser to
C*      get the lokal voaltile and moisture concentration and their
C*      release rates

C*                      separation efficiency of the riser exit
             CALL REXIT(COAL)

             SUM = 0.

C*              no coal in fly ash
        DO I=1,NCLASS
            PART(I,COAL)  = SIVFED(I,COAL)
            BAGCL(I,COAL) =  0.
            RECCL(I,COAL) =  0.
            OVERCL(I,COAL) = FEED(COAL)
            SUM = SUM + SIVFED(I,COAL)
        ENDDO
        DO I=1,NCLASS
         DO II=1, NCLASS
          DO M=2,4
           X(I,II,M) = 0.
          ENDDO
         ENDDO
        ENDDO

*********************************************************************

C*       population balance of lime, inert and char together

       IF (LMASS .GT. 1) GOTO 10

  5   CONTINUE

      DO I=1,NCLASS
        PART(I,LIME)  = PART(I,BED)*FEED(LIME)
     &                   / (FEED(LIME)+FEED(INERT)+FEED(COKE))
        PART(I,INERT) = PART(I,BED)*FEED(INERT)
     &                   / (FEED(LIME)+FEED(INERT)+FEED(COKE))
        PART(I,COKE)  = PART(I,BED)*FEED(COKE)
     &                   / (FEED(LIME)+FEED(INERT)+FEED(COKE))
      ENDDO

  10  CONTINUE

      DO M  =2,4
C*                segregation at bottom bed
        CALL BOTTOM(M,SEGREG,SIFTER)
C*                separation efficiency of the riser exit
        CALL REXIT(M)
C*                      cyclone efficiency
        CALL CYCLON(M)
      ENDDO    
C*           set start residue
       RESIDU = 1.
C*      *************** REPEAT **************************
       DO 400 L = 1,1000

C*          adapt relaxation to state of iteration
          IF (RESIDU .LE. 1.E-2 .AND. L .GT. 1) THEN
                                         RELAX = 0.8
          ELSE IF(RESIDU .LE. 1.E-1 .AND. L .GT. 1) THEN
                                         RELAX = 0.5
          ELSE
                                         RELAX = 0.2
          ENDIF
      
      DO M=2,4
        DO I=1,NCLASS,1
C*               unweighted flow to filter (kg/s/class fraction)
            BAGCL(I,M) = ELFLUX(I,M) * AREA(CMP(1))
     &               * (1.-EFFTOP(I,M)) * (1.-EFFCYC(I,M))
C*               unweighted recirculation flow (kg/s/class fraction)     
            RECCL(I,M) =  ELFLUX(I,M) * AREA(CMP(1))
     &               * (1.-EFFTOP(I,M)) * EFFCYC(I,M)
        ENDDO
      ENDDO
C*                  sum up flows to (kg/s) and 
C*                  calculate guess for discharge rate OVERSM (kg/s)
           SUM = 0.                                            
           BAG = 0.
           RECY = 0.
           OVERSM = 0.
      DO M = 2, 4
        DO II = 1, NCLASS
          DO I = 1, NCLASS
            SUM = SUM + X(I,II,M)*PART(I,M)
          ENDDO
             SUM = SUM + BAGCL(II,M)*PART(II,M) 
             BAG  = BAG  + BAGCL(II,M) *PART(II,M)
             RECY = RECY + RECCL(II,M) *PART(II,M)
             OVERSM = OVERSM + SEGREG(II,M) * SIFTER(II,M) * PART(II,M)
        ENDDO
      ENDDO     
                                                            
C*             calculate exact discharge rate OVER(kg/s)
          OVER = FEED(LIME)+FEED(INERT)+FEED(COKE) - SUM 
       
      IF (OVER .LT. 0.) THEN
         WRITE(*,*) 'Bed material too fine to run steady state'
         WRITE(*,*) 'Check attrition, cyclone or feed rate and ',
     &              'distribution'
          write(*,*) 'FEED  ',FEED(2)+FEED(3)+FEED(4)
          write(*,*) 'BAG   ',BAG
          write(*,*) 'OVER  ',OVER
          write(*,*) 'SUM   ',SUM
          GOTO 5
      ENDIF

C*          if segregation takes place, correct bottom distribution 
C*            OVERSM with OVER
C*          aim is unweighted discharge flow OVERCL (kg/s/class fraction)
      IF (OPTION(3) .GT. 0) THEN
        DO M = 2,4
          DO I = 1,NCLASS
            OVERCL(I,M) = OVER*SEGREG(I,M)*SIFTER(I,M)/OVERSM
          ENDDO          
        ENDDO
      ELSE
        DO M = 2,4
          DO I = 1,NCLASS
            OVERCL(I,M) = OVER
          ENDDO          
        ENDDO
      ENDIF

C*                 particle residence time (s)
       DO M=2,4
          DO I = 1, NCLASS
            TTT(I,M) = BEDMAS/(BAGCL(I,M)+OVERCL(I,M))
          ENDDO
C*                    attrition rates              
             CALL ATTRIT(M,X)
       ENDDO

C*        define right hand array (Y) of linear eq. set
C*        prepare Matrix (AA) from matrix fractions (X) with attrition terms  
        DO M=2,4
         DO I = 1, NCLASS
           Y(I+NCLASS*(M-2)) = SIVFED(I,M)*FEED(M)
           DO II = 1, NCLASS
              XX(I,II,M) = X(I,II,M)
           ENDDO   
           XX(I,I,M)=XX(I,I,M) + OVERCL(I,M) + BAGCL(I,M) 
         ENDDO
        ENDDO 

C*            initialize matrix (AA)
        DO I=1,3*NCLASS
         DO II=1,3*NCLASS
          AA(I,II) = 0.
         ENDDO
        ENDDO

C*           define matrix (AA) with fractions (XX)
C*           change I,II due to FORTRAN reading sequence, first columns
C*                                                        then lines 
        DO M=2,4
          DO I = 1,NCLASS
            DO II=1,NCLASS
              AA(II+NCLASS*(M-2),I+NCLASS*(M-2)) = XX(I,II,M) 
            ENDDO
          ENDDO
        ENDDO  

C*            linear equation solver, deliver matrix AA and right hand array y
C*            the solution array is given back by array Y 
          CALL LINEQS(AA,3*NCLASS,3*NI,Y)

           SUM    = 0.
           RESIDU = 0.
        DO M=2,4
         DO I=1,NCLASS
            RESIDU = RESIDU + ABS(Y(I+NCLASS*(M-2))-PART(I,M))
            PART(I,M) = PART(I,M) + (Y(I+NCLASS*(M-2))-PART(I,M))
     &                               * RELAX 
            SUM = SUM + PART(I,M)
         ENDDO
        ENDDO

C*           normalize PART(I,M) to enforce convergence and avvoid negative values
         DO M=2,4
             DO I=1,NCLASS
               PART(I,M) = PART(I,M)/SUM
                IF (PART(I,M) .LT. 0.) THEN
                   WRITE(*,*) 'Part negative at (',I,M,')'
                   PART(I,M) = 0.
                ENDIF
             ENDDO
           ENDDO
         
         WRITE(IOUT,1001) L,ABS(SUM-1.),RESIDU, OVER
 1001    FORMAT(' ³Loop=',I4,'³Error= ',E12.6,'³Residue= ',E12.6,
     &          '³OVER=',E12.1,'³')

         IF (RESIDU .LT. 1.E-4 .AND. L .NE. 1) THEN
             LPOPUL = L
             GOTO 500
         ENDIF

 400  CONTINUE




C*     ****************** UNTIL ******************************

C*       normal exit indicates failure of convergence
      WRITE(*,*)' convergence error in POPUL, Residue = ',RESIDU
      WRITE(*,*)' the combustor can not be run steady state under these
     &conditions'
      WRITE(*,*)'Possible reasons: Baghouse loss higher than feed rate'
      WRITE(*,*)'                  Attrition const. too high'
      WRITE(*,*)'                  Cyclone model not applicable'
C      CALL BEEP
C      READ(*,1002) CH
C     STOP

 500  CONTINUE

C*               mean particle diameter                     
              SUMA = 0.
          DO I=1,NCLASS
              SUMA = SUMA + (PART(I,2)+PART(I,3)+PART(I,4)) / DIAM(I)
          ENDDO

         IF (SUMA .GT. 0.)  THEN
              MEAND(BED) = 1.0 / SUMA
         ELSE
              MEAND(M) = 0.
         ENDIF

         DO M=2,4
            SUM = 0.
            SUMP = 0.
           DO  I = 1,NCLASS
              SUM  = SUM  + PART(I,M) / DIAM(I)
              SUMP = SUMP + PART(I,M)
           ENDDO

           IF (SUM .GT. 0.)  THEN
              MEAND(M) = 1.0 / SUM * SUMP
           ELSE
              MEAND(M) = 0.
           ENDIF
         ENDDO

       write(99,*) ' population iterations : ',L

       WRITE(*,*) '      Done after ',LPOPUL,' iterations' 
       WRITE(*,*)

      RETURN                     
      END     ! (POPUL)


