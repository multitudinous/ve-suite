
      SUBROUTINE REDIST

************************************************************************
* LAST CHANGE AT : 22.04.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : NMAT                                                     *
* out       : SIVIN0(I,M)                                              *
* in & out  : DIAM(I), MESH(I), SIVFED(I,M), NCLASS                    *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : redistributes the input size distribution to a sqrt 2    *
*             distribution, to smooth the size distances               *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    MLIMIT(NI),   ! new mesh sizes
     &          MESHIN(0:NI), ! input mesh sizes
     &          MESH0(NI),    ! old mesh sizes stored and changed
     &          SIVORG(NI,NM) ! old size classes stored and changed
      REAL*8    XX            ! fraction of size class distibuted to new ones
      REAL*8    SUM1, SUM2, SUM3,       ! intermediate sums
     &          SUMIN1, SUMIN2, SUMIN3  !    " of input
      INTEGER   I, J, M, NNEW  ! counters and indicators

C*          mesh sizes are fixed in the program
      DATA MLIMIT /        16.  ,  11. ,  8.0  , 5.6  , 4.0  ,
     &                     2.8  ,  2.0 ,  1.4 ,  1.0  , .71  ,
     &                     .50  , .355 , .250 , .180  , .125 ,
     &                     .090 , .063 , .044 , .031  , .0  /

       NNEW = 20
       MESHIN(0) = 100. * MESHIN(1)
       MESH(0)   = 2.   * MLIMIT(1)/1000.

C*    store mesh and siv in temporary arrays
      DO I=1,NCLASS,1
                 MESH0(I) = MESH(I)          
         DO M=1,3
                 SIVIN0(I,M) = SIVFED(I,M)
         ENDDO
      ENDDO

C*    initialize new distribution
      DO J=1,NNEW,1
        MESH(J) = MLIMIT(J)/1000.
      ENDDO

      DO 70 M=1,NMAT,1

        DO I=1,NCLASS
           MESHIN(I)  = MESH0(I)
           SIVORG(I,M) = SIVIN0(I,M)
        ENDDO

        I = 1 
        DO 60 J = 1,NNEW,1
          SIVFED(J,M) = 0.
   50     CONTINUE
           IF (I .LE. NCLASS) THEN
                IF (MESH(J) .LE. MESHIN(I)) THEN
C*                     old range included in new range      
                  SIVFED(J,M) = SIVFED(J,M) + SIVORG(I,M)
                  I = I + 1
                  IF (I .GT. NCLASS) GOTO 60
                  GOTO 50   
                ELSE
C*                     old range partly distributed to new range
                  IF (MESH(J) .LE. MESHIN(I-1)) THEN
                    XX = (MESH(J)-MESHIN(I-1))/(MESHIN(I)-MESHIN(I-1))
                    SIVFED(J,M) = SIVFED(J,M) + XX * SIVORG(I,M)
                    SIVORG(I,M) = (1-XX) * SIVORG(I,M)
                    MESHIN(I-1) = MESH(J)
                  ENDIF
                ENDIF
           ELSE
                SIVFED(J,M) = 0.
           ENDIF
   60   CONTINUE
   70 CONTINUE

      NCLASS = NNEW


C*              convert the meaning of  SIV  from sieve openings 
C*              calc mean diameter (m) from sieve openings Mesh(m)
           DIAM(1) = MESH(1)
      DO I = 2, NCLASS
           DIAM(I) = (MESH(I) + MESH(I-1)) / 2.
      ENDDO        


C*     ******************** Testwriter ******************************

      IF (.NOT. WRTOUT(12)) RETURN

      OPEN (UNIT=13,FILE='REDIST.DAT',STATUS='UNKNOWN')

      WRITE(13,*) ' Changes in distribution '
      WRITE(13,1000)


      DO I=1,NCLASS
            SUM1=SUM1 + SIVFED(I,1)*100. 
            SUM2=SUM2 + SIVFED(I,2)*100. 
            SUM3=SUM3 + SIVFED(I,3)*100.
            SUMin1=SUMIN1 + SIVIN0(I,1)*100. 
            SUMin2=SUMIN2 + SIVIN0(I,2)*100. 
            SUMin3=SUMIN3 + SIVIN0(I,3)*100.

            WRITE(13,1001) MESH(i)*1000.,
     &          SIVFED(I,1)*100.,SIVFED(I,2)*100.,SIVFED(I,3)*100., 
     &                 MESH0(i)*1000.,
     &          SIVIN0(I,1)*100.,SIVIN0(i,2)*100,SIVIN0(i,3)*100.
      ENDDO
            WRITE(13,1002) SUM1,SUM2,SUM3,SUMIN1,SUMIN2, SUMIN3
  
      CLOSE(13)

 1000 FORMAT(' Meshnew coal    lime    bed        Meshold coal    lime  
     &  bed')
 1001 FORMAT(1X,F6.3,1X,3(F7.3,1X),4X,F6.3,1X,3(F7.3,1X))
 1002 FORMAT(8X,3(F7.3,1X),11X,3(F7.3,1X))

      RETURN
      END        ! (REDIST)

