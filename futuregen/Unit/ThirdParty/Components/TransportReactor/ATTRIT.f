
      SUBROUTINE ATTRIT(M,X) 

************************************************************************
* LAST CHANGE AT :  22.10.1993              BY:   J. Hannes            *
************************************************************************
* CALLED BY : POPUL, PREPOP                                            *
* CALLS     : Z_UMF                                                    * 
************************************************************************
* COMMON in : ATTCON, BEDMAS, CMP(1), DIAM(I), MESH(I), NCLASS, NI, NM,*
*             PART(I,BED), SEE(I,M), TTT(I,M),  U(K), WRTOUT(), WRTSCR *
* out       : -                                                        *
* in & out  : -                                                        *
************************************************************************
* received  : M         material indicator                             *
*             TTT(I,M)  average residence time of each particle class  *
* returned  : X(I,II,M) shrinking and attrition rate matrix            *
************************************************************************
* PURPOSE   : Calculation of the attrition and shrinking influence     *
*             on population balances of each material                  *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8  Z_UMF, UMF,   ! minimum fluidization velocity (m/s)
     &        ATTR(NI,NM),  ! attrition rate (kg/s)
     &        CUM(0:NI),    ! cumulative size fraction up to class i 
     &        DELDIA(NI,NM) ! change in diameter  (m)
      REAL*8              
     &        X(NI,NI,NM),  ! class change matrix for population balance
     &        DELTST(NI,NM) ! test change in diameter for coal shrinking

      INTEGER I, II,        ! size class indicators
     &        M,            ! material indicator
     &        NFINES        ! boundary class between coarse and fines

      IF (WRTSCR) WRITE (*,*) 'ATTRIT '

        NFINES = 14
                          
         UMF = Z_UMF(MEAND(BED),BED)                  
                          
        DO I=1,NCLASS
         DO II = 1,NCLASS
           X(I,II,M)= 0.
         ENDDO
        ENDDO   
              
C*             cummulative sieve classes            
           CUM(0) = 1.         
         DO I = 1, NFINES                      
           CUM(I) = CUM(I-1)-PART(I,BED)   
         ENDDO     

         DO I = NFINES,NCLASS                      
           CUM(I) = 0.   
         ENDDO     

C*        ***** shrinking + abrasion ********
      
          DO 150 I = 1,NCLASS

C*           inplementation of COMBUSTION NEEDED ********************

C*                diameter loss with averaged residence time (m)
C*                total attrition rate (kg/s)

            IF (UMF .LE. U(CMP(1))) THEN 
                DELDIA(I,M) = DIAM(I) 
     &             *( 1. -EXP( -( ATTCON(M)/3.
     &                *CUM(I) * (U(CMP(1))-UMF) )*TTT(I,M))    ) 
                ATTR(I,M)=ATTCON(M)*CUM(I)*(U(CMP(1))-UMF)*BEDMAS

                DELTST(I,M) = DIAM(I) 
     &             *( 1. -EXP( -( ATTCON(M)/3.
     &                *CUM(I) * (U(CMP(1))-UMF) )*TTT(I,M))    ) 

            ELSE
                   DELDIA(I,M) = 0.
                   DELTST(I,M) = 0.
                   ATTR(I,M)   = 0.
            ENDIF
          
 150      CONTINUE
         
C*           define matrix for linear eq.system X(I,II,M)=A,SIV(I,M)=Y(I)

        DO 250 I = 1, NCLASS
           DO 240 II = I, NCLASS
             IF ((MESH(I-1)-DELDIA(I,M)) .LE. MESH(II)) THEN
                     X(I,II,M) = 0.
                     SEE(I,II,M) = 0
             ELSE IF ((MESH(I)-DELDIA(I,M)) .GE. MESH(II-1)) THEN
                     X(I,II,M) = 0.
                     SEE(I,II,M) = 0
             ELSE IF ((MESH(I-1)-DELDIA(I,M)) .LE. MESH(II-1)) THEN
               IF ((MESH(I)-DELDIA(I,M)) .GE. MESH(II)) THEN
                     X(I,II,M) = 1. 
                     SEE(I,II,M) = 1
               ELSE
                     X(I,II,M) = (MESH(I-1)-DELDIA(I,M)-MESH(II))/
     &                         (MESH(I-1)-MESH(I))
                     SEE(I,II,M) = 2
               ENDIF
             ELSE
               IF ((MESH(I)-DELDIA(I,M)) .LE. MESH(II)) THEN
                     X(I,II,M) = (MESH(II-1)-MESH(II))/
     &                         (MESH(I-1)-MESH(I))
                      SEE(I,II,M) = 3
               ELSE
                     X(I,II,M) = (MESH(II-1)-(MESH(I)-DELDIA(I,M)))/
     &                         (MESH(I-1)-MESH(I)) 
                      SEE(I,II,M) = 4
               ENDIF
             ENDIF

             X(I,II,M) = X(I,II,M) * (-BEDMAS/TTT(I,M))

           IF (I .EQ. II) THEN
             X(I,II,M) = X(I,II,M) + BEDMAS/TTT(I,M) + ATTR(I,M)
           ENDIF

           IF (II .GT. I .AND. II .GT. NFINES) THEN
              SEE(I,II,M) = 8
             X(I,II,M) = X(I,II,M) 
     &                  - ATTR(I,M)/MIN0(NCLASS-NFINES,NCLASS-I)
           ENDIF

 240       CONTINUE
 250      CONTINUE

                                 
         IF (.NOT. WRTOUT(1)) RETURN

C*       *************** TESTWRITER *************************************

        OPEN (UNIT=70,FILE='ATTR'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')

         write(70,*) ' Diameter decrease during residence '
         write(70,*) ' No   Diameter   delta Diameter'
        DO I = 1, NCLASS
          write(70,1001)I,DIAM(I)*1.E6,DELDIA(I,M)*1.E6,DELTST(I,M)*1.E6
        ENDDO
 1001    FORMAT(I3,2X,F7.0,2X,F8.1,2X,F10.4)

       write(70,*)
       write(70,*) 'Attrition constants and average residence time (s)'
       write(70,*) ' No  DIAMETER Attr con   residence time '
       write(70,*) '       (æm)     (m/s)        (s) '
      DO I = 1, NCLASS
       write(70,1002) I, DIAM(I)*1.E6,ATTR(I,M),TTT(I,M)
      ENDDO
 1002    FORMAT(I3,2X,F7.0,2X,E10.1,2X,F10.2)
       
       CLOSE(70)


      RETURN
      END


