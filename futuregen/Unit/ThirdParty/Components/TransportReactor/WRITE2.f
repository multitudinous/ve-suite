
      SUBROUTINE WRITE2 

************************************************************************
* LAST CHANGE AT : 14.2.1994                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_VELO, Z_UMF, Z_ESMF, Z_BUBB                            *
************************************************************************
* ARUMENTS PASSED:                                                     *
************************************************************************
* COMMON in : AREA(K), BAGCL, DIAM(I), FEED(M), HEIGHT(K), MEAND(M),   *
*             MANN, MCOR, MESH, NCLASS, NMAT, OVERCL, PART(I,M),       *
*             RECCL, RHOS(M), SIVFED, WRTSCR                           *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : write results of calculations to files                   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

       REAL*8            CUMFED, CUMOV, CUMREC, CUMBAG, CUMPRT
       REAL*8            SUMF, SUMO, SUMR, SUMB, SUMC
       REAL*8            SUMMF, SUMMO, SUMMR, SUMMB, SUMMC 
       REAL*8            SUMFED
       REAL*8            BAG, OVER, RECY, REAK
       REAL*8            PRTBAG(NI,NM),PRTOVR(NI,NM),PRTREC(NI,NM)
       REAL*8            PRTREA(NI,NM)
       INTEGER           I, K, M
       IF (WRTSCR) WRITE(*,*) ' WRITE2'

C*    ************* POPUL  ***********************

      OPEN(UNIT=40,FILE='PART.DAT',STATUS='UNKNOWN')
                  SUMMF=0.
                  SUMMO=0.
                  SUMMR=0.
                  SUMMB=0.
                  SUMMC=0.

          BAG  = 0.
          OVER = 0.
          RECY = 0.
          REAK = 0.
      DO M=2,4
        DO I = 1, NCLASS
          BAG  = BAG  + PART(I,M) * BAGCL(I,M)
          OVER = OVER + PART(I,M) * OVERCL(I,M)
          RECY = RECY + PART(I,M) * RECCL(I,M)
          REAK = REAK + PART(I,M) * REAKCL(I,M)
        ENDDO
      ENDDO

      DO 320 M=2,4
        DO  I = 1,NCLASS
            IF (BAG .LE. 1.E-12) THEN
               PRTBAG(I,M) = 0.
            ELSE
               PRTBAG(I,M) = BAGCL(I,M) * PART(I,M) / BAG 
            ENDIF
            IF (RECY .LE. 1.E-12) THEN
               PRTREC(I,M) = 0.
            ELSE
               PRTREC(I,M) = RECCL(I,M) * PART(I,M) / RECY 
            ENDIF
            IF (OVER .LE. 1.E-12) THEN
               PRTOVR(I,M) = 0.
            ELSE
               PRTOVR(I,M) = OVERCL(I,M) * PART(I,M) / OVER 
            ENDIF
            IF (REAK .LE. 1.E-12) THEN
               PRTREA(I,M) = 0.
            ELSE
               PRTREA(I,M) = REAKCL(I,M) * PART(I,M) / REAK 
            ENDIF
         ENDDO
      
	WRITE(40,*)' M = 1 (coal), 2 (lime), 3 (inert), 4 (coke), 5 (bed)'
	WRITE(40,*)
         WRITE(40,*) ' Material = ',M
         WRITE(40,*) ' Sieve parts after population balance '      
         WRITE(40,*) 'No  Mesh     Diam      Feed       Overfl     ',
     &               'Recirc.    Baghs      Content  '
                  SUMF=0.
                  SUMO=0.
                  SUMR=0.
                  SUMB=0.
                  SUMC=0.
                  SUMFED = FEED(2)+FEED(3)+FEED(4)
         DO I = 1,NCLASS
            WRITE(40,3001) I,MESH(I)*1.E6, DIAM(I)*1.E6, 
     &            SIVIN0(I,M)*100.*FEED(M)/SUMFED,PRTOVR(I,M)*100.,
     &            PRTREC(I,M)*100., PRTBAG(I,M)*100.,
     &            PART(I,M)*100.    
                  SUMF = SUMF + SIVIN0(I,M)*FEED(M)*100./SUMFED
                  SUMO = SUMO + PRTOVR(I,M)*100.
                  SUMR = SUMR + PRTREC(I,M)*100.
                  SUMB = SUMB + PRTBAG(I,M)*100.
                  SUMC = SUMC + PART(I,M)  *100.
         ENDDO
        WRITE(40,*)'Sum test'
        WRITE(40,3001) 0,MEAND(M)*1.E6,MEAND(M)*1.E6,
     &                   SUMF,SUMO,SUMR,SUMB,SUMC
            SUMMF = SUMMF + SUMF
            SUMMO = SUMMO + SUMO
            SUMMR = SUMMR + SUMR
            SUMMB = SUMMB + SUMB
            SUMMC = SUMMC + SUMC
 320  CONTINUE

        WRITE(40,*)'Sum test total'
        WRITE(40,3001) 0,MEAND(BED)*1.E6,MEAND(BED)*1.E6,
     &                          SUMMF,SUMMO,SUMMR,SUMMB,SUMMC

          WRITE(40,*)
          WRITE(40,*) ' mass flow (kg/s): FEED      OVERFLOW   '
     &                ,'RECIRC.    BAGHOUSE'
          WRITE(40,3002) SUMFED, OVER, RECY,BAG
          WRITE(40,*)

      CLOSE(40)

      OPEN(UNIT=60,FILE='SUM.DAT',STATUS='UNKNOWN')

         WRITE(60,*) ' Sum curves after population balance ',
     &               '(for material fractions see PART.DAT)'
         WRITE(60,*) 'No  Mesh     Diam     Feed       Overflow    ',
     &               'Recirc.    Filter     Content'

         CUMFED = 1.
         CUMOV  = 1.
         CUMREC = 1.
         CUMBAG = 1.
         CUMPRT = 1.

        WRITE(60,3001) 1,MESH(1)*1.E6, DIAM(1)*1.E6, CUMFED*100.,
     &        CUMOV*100.,CUMREC*100.,CUMBAG*100.,CUMPRT*100.

      DO 330 I=2,NCLASS
        CUMFED = CUMFED - ( SIVFED(I-1,2)*FEED(2)+SIVFED(I-1,3)*FEED(3)
     &                    +SIVFED(I-1,4)*FEED(4) )/SUMFED
        CUMOV  = CUMOV  - PRTOVR(I-1,2)-PRTOVR(I-1,3)-PRTOVR(I-1,4)
        CUMREC = CUMREC - PRTREC(I-1,2)-PRTREC(I-1,3)-PRTREC(I-1,4)
        CUMBAG = CUMBAG - PRTBAG(I-1,2)-PRTBAG(I-1,3)-PRTBAG(I-1,4)
        CUMPRT = CUMPRT - PART(I-1,2)  -PART(I-1,3)  -PART(I-1,4)

          WRITE(60,3001) I,MESH(I)*1.E6, DIAM(I)*1.E6, CUMFED*100.,
     &    CUMOV*100.,CUMREC*100., CUMBAG*100.,CUMPRT*100. 
 330  CONTINUE

      write(60,*)
      write(60,*) ' mass flow (kg/s): FEED     OVERFLOW   RECICULATION',
     &                   '  BAGHOUSE    REACTION'
      write(60,3002) SUMFED, OVER, RECY, BAG
          WRITE(60,*)

      CLOSE(60)

 3001 FORMAT(I2,2(2X,F7.0),5(2X,F9.5))
 3002 FORMAT(15X,4(F12.7))    
       RETURN
       END        ! (WRITE2)



