
      SUBROUTINE ENERGY
      
************************************************************************
* LAST CHANGE AT : 21.03.1995               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : CRRECT, HEATTR, PROPER, LINEQS                           *
************************************************************************
* COMMON in : AREA(K), HEIGHT(K), LENGTH(K), WIDTH(K), TUBE(K), NCELLS *
*             NCLASS, CORAVG(K), TIN, TEHE, TBED, HEATCG(J), HEATFM(J),*
*             YFEEDA(K), YFEEDF(K), YH2O(K), YVOLA(K), YO2(K), YOUT(K),*
*             YCORE(K), YANNU(K), YBUBB(K), YCOANN(K), YANNCO(K),      *
*             YCOANMX(K), XFEED(J), XH2O(K), XVOLA(J), XO2(J),         *
*             XGAS(J,K,P), NSPECY,                                     *
*             MUP(I,K,M), MDWN(I,K,M), MX(I,K,M), MMCA(I,K,M),         *
*             MAC(I,K,M), MCAX(I,K,M), MINL(I,K,M), MOUT(I,K,M),       *
*             MCYC(I,K,M), MEHE(I,K,M), MSCREL(K), MSAREL(K),          *
*             TMCH2O(K), TMAH2O(K), VAPH2O, WRTSCR, KCO, KAN, TWALL    *
* out       : TEMP(K,P), HTTUBE, HTWALL, HTGAS, HTRECY                 *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : performs energy balance considering solids and gas flows *
*             and change in gas species by formation enthalpies        *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8           AA(2*NK,2*NK),BB(2*NK),XX(2*NK)
      REAL*8           AWALL(0:NK) , ACTUBE(0:NK),  AATUBE(0:NK) 
      REAL*8           ALFAT(0:NK) , ALFAW(0:NK)
      REAL*8           TTUBE(0:NK)
      REAL*8           MSUP(0:NK)  , MSDWN(0:NK) ,  MSX(0:NK)
      REAL*8           MSCA(0:NK)  , MSCAX(0:NK)
      REAL*8           MSAC(0:NK)  
      REAL*8           MSIN(0:NK)  , MSOUT(0:NK)
      REAL*8           MSCYC(0:NK) , MSEHE(0:NK)
      REAL*8           MSCREL(0:NK), MSAREL(0:NK)

      REAL*8           NSFCOR(0:NK), NSCOR(0:NK)
      REAL*8           NSFANN(0:NK), NSANN(0:NK)
      REAL*8           NSFCA(0:NK) , NSCA(0:NK)
      REAL*8           NSFCAX(0:NK), NSCAX(0:NK)
      REAL*8           NSFAC(0:NK) , NSAC(0:NK)
      REAL*8           NSFACX(0:NK), NSACX(0:NK)
      REAL*8           NSFFED(0:NK), NSFED(0:NK)
      REAL*8           NSFFLU(0:NK), NSFLU(0:NK)
      REAL*8           NSFOUT(0:NK), NSOUT(0:NK)
      REAL*8           DUMMY,        VOLUM(0:NK)
      REAL*8           EMAX, ERR, X1, X2, E1, E2, DT
      INTEGER          I, J, K, KK, M ,INDX, L
      
      IF (WRTSCR) WRITE(*,*) ' ENERGY '
                                                      
          DUMMY = 0.                                           
      DO K=1, NCELLS
           TTUBE(K)  = TWALL
      ENDDO
                                                      
C*         heat transfer to tubes and walls

          CALL HEATTR(ALFAT,ALFAW)

      DO K=1, NCELLS
           AWALL(K)  = 2.*(WIDTH(K)+LENGTH(K))*(HEIGHT(K)-HEIGHT(K-1))
           VOLUM(K)  = (AREA(K)+AREA(K-1))/2.*(HEIGHT(K)-HEIGHT(K-1))
           ACTUBE(K) = TUBE(K)*VOLUM(K)* CORAVG(K)
           AATUBE(K) = TUBE(K)*VOLUM(K)*(1.-CORAVG(K)) 
      ENDDO

C ****************** Heat transfer due to mass flows *******************

      DO K=0, NCELLS+1
           MSUP(K)   = 0.
           MSDWN(K)  = 0.
           MSX(K)    = 0.
           MSCA(K)   = 0.
           MSAC(K)   = 0.
           MSCAX(K)  = 0.
           MSIN(K)   = 0.
           MSCYC(K)  = 0.
           MSEHE(K)  = 0.
           MSOUT(K)  = 0.
       ENDDO

       DO K=1,NCELLS
        DO M = 2, 4
          DO I = 1, NCLASS
            MSUP(K)  = MSUP(K)  + MUP(I,K,M)  * HEATCP(M)      
            MSDWN(K) = MSDWN(K) + MDWN(I,K,M) * HEATCP(M)
            MSX(K)   = MSX(K)   + MX(I,K,M)   * HEATCP(M) 
            MSCA(K)  = MSCA(K)  + MCA(I,K,M)  * HEATCP(M)
            MSAC(K)  = MSAC(K)  + MAC(I,K,M)  * HEATCP(M)
            MSCAX(K) = MSCAX(K) + MCAX(I,K,M) * HEATCP(M)  
            MSIN(K)  = MSIN(K)  + MINL(I,K,M) * HEATCP(M)
            MSCYC(K) = MSCYC(K) + MCYC(I,K,M) * HEATCP(M)  
            MSEHE(K) = MSEHE(K) + MEHE(I,K,M) * HEATCP(M)
            MSOUT(K) = MSOUT(K) + MOUT(I,K,M) * HEATCP(M)
          ENDDO
        ENDDO

C*             water and volatile release rates
        MSCREL(K) = TMCH2O(K)*VAPH2O + TMCVOL(K) * 0.
        MSAREL(K) = TMAH2O(K)*VAPH2O + TMAVOL(K) * 0.

      ENDDO


C************* heat flows by feed air ****************************
      CALL PROPER(TIN)
      DO K=1,NCELLS
        NSFFED(K) = 0.
        NSFED(K)  = 0.
        NSFFLU(K) = 0.
        NSFLU(K)  = 0.
       DO J=1, NSPECY
        NSFFED(K)   = NSFFED(K)+ ( YFEEDA(K)* XFEED(J)
     &                           + YH2O(K)  * XH2O(J)
     &                           + YVOLA(K) * XVOLA(J)
     &                           - YO2(K)   * XO2(J) ) * HEATFM(J)
     &                           - YH2O(K)  * XH2O(J) *45000.
C*                      45000. J/mol(H2O) = molar evaporation enthalpy

        NSFFLU(K)   = NSFFLU(K) + YFEEDF(K)*XGAS(J,CMP(6),CORE)
     &                                                 * HEATFM(J)

        NSFED(K)    = NSFED(K) + ( YFEEDA(K)* XFEED(J)
     &                           + YH2O(K)  * XH2O(J)
     &                           + YVOLA(K) * XVOLA(J)
     &                           - YO2(K)   * XO2(J)   )* HEATCG(J)
        NSFLU(K)    = NSFLU(K) +  YFEEDF(K)*XGAS(J,CMP(6),CORE)
     &                                                  * HEATCG(J)
       ENDDO                    
      ENDDO
            
C       CALL PROPER(TBED) 
 
C **************** Heat flows in core **********************************
           NSFCOR(0) = 0.          
           NSCOR(0)  = 0.          

      DO 300 K = 1, NCELLS

C         CALL PROPER(TEMP(K,CORE))

           NSFCOR(K)   = 0.
           NSCOR(K)    = 0.        
           NSFCA(K)    = 0.
           NSCA(K)     = 0.
           NSFCAX(K)   = 0.
           NSCAX(K)    = 0.
           NSFOUT(K)   = 0.
           NSOUT(K)    = 0.
       DO J=1, NSPECY

        NSFCOR(K) = NSFCOR(K) + 
     &     (YCORE(K)*XGAS(J,K,CORE)+YBUBB(K)*XGAS(J,K,BUBB)) * HEATFM(J)
        NSFCA(K)    = NSFCA(K) + YCOAN(K)*XGAS(J,K,CORE) * HEATFM(J)
        NSFCAX(K)   =  NSFCAX(K) + COANMX(K) * AREA(K) 
     &             * (HEIGHT(K)-HEIGHT(K-1))*XGAS(J,K,CORE)*HEATFM(J)
        NSFOUT(K)  =  NSFOUT(K) + YOUT(K) * XGAS(J,K,CORE) * HEATFM(J)


        NSCOR(K) = NSCOR(K) +
     &     (YCORE(K)*XGAS(J,K,CORE)+YBUBB(K)*XGAS(J,K,BUBB)) * HEATCG(J)
        NSCA(K)     = NSCA(K)  + YCOAN(K)*XGAS(J,K,CORE) * HEATCG(J)
        NSCAX(K)    =  NSCAX(K) + COANMX(K) * AREA(K) 
     &                * (HEIGHT(K)-HEIGHT(K-1))*XGAS(J,K,CORE)*HEATCG(J)
        NSOUT(K)   =  NSOUT(K)  + YOUT(K) * XGAS(J,K,CORE) * HEATCG(J)

       ENDDO
                                                         
 300  CONTINUE

C ****************** Heat flows in Annulus ******************************

           NSFANN(0)   = 0.
           NSANN(0)    = 0.           
 
      DO 400 K=1, NCELLS

C         CALL PROPER(TEMP(K,ANNU))

           NSFANN(K)   = 0.
           NSANN(K)    = 0.
           NSFAC(K)    = 0.
           NSAC(K)     = 0.
           NSFACX(K)   = 0.
           NSACX(K)    = 0.
         
       DO J = 1, NSPECY
        NSFANN(K) =  NSFANN(K) + YANNU(K)*XGAS(J,K,ANNU)*HEATFM(J)
        NSFAC(K)  =  NSFAC(K)  + YANCO(K)*XGAS(J,K,ANNU)*HEATFM(J)
        NSFACX(K) =  NSFACX(K) + COANMX(K) * AREA(K) 
     &                * (HEIGHT(K)-HEIGHT(K-1))*XGAS(J,K,ANNU)*HEATFM(J)
 
        NSANN(K)  =  NSANN(K)  + YANNU(K)*XGAS(J,K,ANNU)*HEATCG(J)
        NSAC(K)   =  NSAC(K)   + YANCO(K)*XGAS(J,K,ANNU)*HEATCG(J)
        NSACX(K)  =  NSACX(K)  + COANMX(K) * AREA(K) 
     &                * (HEIGHT(K)-HEIGHT(K-1))*XGAS(J,K,ANNU)*HEATCG(J)
        ENDDO                    
 400   CONTINUE

C*************** define linear equation system AA*T=BB ********************
               
       INDX = 0
       DT   = 10.
       EMAX = 0.001

       DO 700 L = 1,3000

          DO K=1,2*NCELLS
           DO KK=1,2*NCELLS
             AA(K,KK) = 0.
           ENDDO
             BB(K) = 0.
          ENDDO   

      DO 500 K=1,NCELLS
                        
C*        flow of lower cell to actual                        
      IF(K .GT. 1) THEN
        AA(K+KCO,K+KCO-1) = AA(K,K-1)
     &    -  MSUP(K-1)  
     &    -  MSX(K-1)     
     &    -  NSCOR(K-1)

        AA(K+KAN,K+KAN-1) = AA(K+KAN,K+KAN-1)
     &    -  NSANN(K-1)        
      ENDIF      
            
C*          flow of upper cell to actual 
       IF(K .LT. NCELLS) THEN
        AA(K+KCO,K+KCO+1)   = AA(K+KCO,K+KCO+1)
     &                           - MSX(K)    
        AA(K+KAN,K+KAN+1)   = AA(K+KAN,K+KAN+1)
     &                           - MSDWN(K+1)
       ENDIF 

C*              cyclone reflow
         AA(K+KCO,CMP(6)) = AA(K+KCO,CMP(6))
     &                      - MSCYC(K)  

C*          flows leaving actual core cell
        AA(K+KCO,K+KCO) = AA(K+KCO,K+KCO) 
     &    +  MSUP(K) 
     &    +  MSCA(K) 
     &    +  MSCAX(K)                        
     &    +  MSX(K-1) + MSX(K)
     &    +  MSOUT(K)
     &    +  NSCOR(K) 
     &    +  NSCA(K) 
     &    +  NSCAX(K)
     &    +  NSOUT(K)
     &    +  ALFAT(K) * ACTUBE(K) 
 
C*          flows leaving actual annulus cell
         AA(K+KAN,K+KAN) = AA(K+KAN,K+KAN)
     &    +  MSDWN(K) 
     &    +  MSCAX(K) 
     &    +  MSAC(K)
     &    +  NSANN(K) 
     &    +  NSAC(K) 
     &    +  NSACX(K) 
     &    +  ALFAT(K) * AATUBE(K)
     &    +  ALFAW(K) * AWALL(K)
 
C*         flow from annulus to core
        AA(K+KCO,K+KAN) = AA(K+KCO,K+KAN)        
     &    -  MSAC(K)
     &    -  MSCAX(K)   
     &    -  NSAC(K)
     &    -  NSACX(K)     

C*           flow from core to annulus
        AA(K+KAN,K+KCO) = AA(K+KAN,K+KCO)
     &    -  MSCA(K)       
     &    -  MSCAX(K)      
     &    -  NSCA(K)   
     &    -  NSCAX(K)  

 500   CONTINUE        


       DO 600 K = 1,NCELLS

C*            independent right hand vector core
         BB(K+KCO) =    
     &       MSIN(K) * TIN
     &    +  MSEHE(K) * TEHE 
     &    +  NSFCOR(K-1) - NSFCOR(K)
     &    +  NSFAC(K)    - NSFCA(K) 
     &    +  NSFACX(K)   - NSFCAX(K)
     &    -  NSFOUT(K)
     &    +  NSFFED(K) 
     &    +  NSFFLU(K)
     &    +  NSFED(K) * TIN
     &    +  NSFLU(K) * TFLU
     &    +  ALFAT(K) * ACTUBE(K) * TTUBE(K)

C*             independent right hand vector annulus
          BB(K+KAN) = 
     &       NSFANN(K-1) - NSFANN(K)
     &    +  NSFCA(K)    - NSFAC(K) 
     &    +  NSFCAX(K)   - NSFACX(K)
     &    +  ALFAT(K) * AATUBE(K) * TTUBE(K)
     &    +  ALFAW(K) * AWALL(K)  * TWALL

 600  CONTINUE

       CALL LINEQS(AA,2*NCELLS,2*NK,BB) 
!zc         CALL GSITRN(AA,2*NCELLS,2*NK,BB,XX)

       DO K=1,NCELLS
         TEMP(K,CORE) = BB(K+KCO)
         TEMP(K,ANNU) = BB(K+KAN)
         TEMP(K,BUBB) = BB(K+KCO)
       ENDDO                   

         ERR = (TBED-TEMP(1,CORE))/TBED
        CALL CRRECT(L,INDX,DT,X1,X2,TEHE,E1,E2,ERR,EMAX)
	IF((L/50)*50.EQ.L) THEN
		IF((L/300)*300.EQ.L) THEN
      WRITE(*,*)'Iteration      T(1,CORE)         Tp(cyc)      Error'
		ENDIF
	WRITE(*,605)L,TEMP(1,CORE),TEHE,ERR
	ENDIF
 605	FORMAT(I6,2(6X,F12.3),3X,F8.4)
          IF (INDX .EQ. 2) GOTO 800
      
 700  CONTINUE

         WRITE(*,*) 'TEMP iteration failed'

 800  CONTINUE

         HTTUBE = 0.
         HTWALL = 0.
         HTGAS  = 0.
         HTRECY = 0.
        DO K = 1,NCELLS
          HTTUBE = HTTUBE + ALFAT(K)*ACTUBE(K)*(TEMP(K,CORE)-TTUBE(K))
     &                    + ALFAT(K)*AATUBE(K)*(TEMP(K,ANNU)-TTUBE(K))
          HTWALL = HTWALL + ALFAW(K)*AWALL(K)*(TEMP(K,ANNU)-TWALL)
          HTGAS  = HTGAS  + NSOUT(K)*(TEMP(K,CORE)-TFLU)
          HTRECY = HTRECY + MSOUT(K)*TEMP(K,CORE)
     &                    - MSCYC(K)*TEMP(CMP(6),CORE)
     &                    - MSEHE(K)*TEHE
        ENDDO

      RETURN

      END        ! (ENERGY)

