
      SUBROUTINE NETFLO(M)

************************************************************************
* LAST CHANGE AT : 19.09.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : PRESOL, SOLIDS                                           *
* CALLS     :                                                          *
************************************************************************
* COMMON in : MINL(I,K,M), MOUT(I,K,M), MCYC(I,K,M), MEHE(I,K,M),      *
*             CMP(4+5+6), FEED(M), BAGCL(I,M), OVERCL(I,M), RECCL(I,M) *
*             PART(I,M), NCELLS, NCLASS, WRTSCR                        *
* out       : MNETTO(I,K,M)                                            *
* in & out  :                                                          *
************************************************************************
* passed    : M  material indicator                                    *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : deliveres the net mass flow of solids in any cell        *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER  I, ! size indicator
     &         K, ! cell indicator
     &         M  ! material indicator
	REAL*8 A1,A2,A3

      IF (WRTSCR) write(*,*) '  NETFLO '

C*            flows into and out of the bed at actual cells          
      DO K = 0, NCELLS + 1
        DO I = 1, NCLASS                                           
             MINL(I,K,M)   = 0.
             MOUT(I,K,M)   = 0.
             MCYC(I,K,M)   = 0.
             MEHE(I,K,M)   = 0.
             MNETTO(I,K,M) = 0.
         ENDDO    
      ENDDO

	A1=0.0
        A2=0.0
        A3=0.0

      DO 200 K = 1, NCELLS 
        DO 100 I = 1, NCLASS                                           

C*                 bottom bed discharge
          IF (K .EQ. 1) THEN
             MOUT(I,K,M) = OVERCL(I,M)*PART(I,M)
          ENDIF
C*                 flow to riser exit
          IF (K .EQ. CMP(6)) THEN
            MOUT(I,K,M) = (RECCL(I,M)+BAGCL(I,M))*PART(I,M)

		A1=A1+RECCL(I,M)*PART(I,M)
		A2=A2+BAGCL(I,M)*PART(I,M)

          ENDIF
                 
C*                  feed flows limestone            
          IF (K .EQ. CMP(2)) THEN
           IF (M .EQ. 2) THEN
            MINL(I,K,M) = (BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
           ELSE IF (M .EQ. 5) THEN
            MINL(I,K,M) = MINL(I,K,M)+(BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
     &                   * FEED(2) / ( FEED(2) + FEED(3) + FEED(4) )
           ENDIF
          ENDIF

C*                  feed coal and char
          IF (K .EQ. CMP(3)) THEN
           IF (M .EQ. 1 .OR. M .EQ. 4) THEN
            MINL(I,K,M) = (BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
           ELSE IF (M .EQ. 5) THEN
            MINL(I,K,M) = MINL(I,K,M)+(BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
     &        * ( FEED(4) ) / ( FEED(2) + FEED(3) + FEED(4) )
          ENDIF
         ENDIF

C*                  feed coal, inert and char
          IF (K .EQ. 1) THEN
           IF (M .EQ. 3) THEN
            MINL(I,K,M) = (BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
           ELSE IF (M .EQ. 5) THEN
            MINL(I,K,M) = MINL(I,K,M)+(BAGCL(I,M)+OVERCL(I,M))*PART(I,M)
     &        * ( FEED(3)) / ( FEED(2) + FEED(3) + FEED(4) )
          ENDIF
         ENDIF
            
C*                cyclone recycle flows
          IF (K .EQ. CMP(4)) THEN
              MCYC(I,K,M) =  RECCL(I,M)*PART(I,M)*(1.-REHE)

		A3=A3+RECCL(I,M)*PART(I,M)*(1.-REHE)

          ENDIF    
C*                ext. heat exchanger recycle flows
          IF (K .EQ. CMP(5)) THEN
              MEHE(I,K,M) =  RECCL(I,M)*PART(I,M)*REHE
          ENDIF    

            MNETTO(I,K,M) = MNETTO(I,K-1,M) 
     &                      + MINL(I,K,M) - MOUT(I,K,M)
     &                      + MCYC(I,K,M) + MEHE(I,K,M)
            IF (K .GT. (CMP(6))) THEN
             MNETTO(I,K,M) = 0.
            ENDIF
 100     CONTINUE          
 200   CONTINUE

      RETURN
      END       ! (NETFLO)



