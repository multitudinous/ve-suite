
      SUBROUTINE VOLCOM(N,H2OMOL,VOLMOL,O2MOL)
      
************************************************************************
* LAST CHANGE AT :  24.05.1995              BY:   J. Hannes            *
************************************************************************
* CALLED BY : PYROLY                                                   *
* CALLS     : LINEQS                                                   * 
************************************************************************
* COMMON in : COALM(1-10), MWS(M), MWG(J), NSPECY, TBED, WRTSCR, XNVOL *
*             WRTOUT                                                   *
* out       : XVOLA(J),XH2O(J), XO2(J)                                 *
* in & out  :                                                          *
************************************************************************
* passed    : N       model indicator                                  *
* returned  : VOLMOL  molVolatiles/kgVolatiles                         *
*             H2OMOL  molH2O/kgMoisture                                *
*             O2MOL   molO2/kgVolatiles                                *
************************************************************************
* PURPOSE   : calculation of the volatiles composition in mass         *
*             fractions of the devolatilization rate (kg/s)            *
*             assuming a certain order of reaction. If there remain    *
*             residues of C and S, oxygen from the air is taken to     *
*             produce CO and SO2                                       *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8  CC,HH,OO,NN,SS, ! atomic species fractions in mole/kgVolatiles 
     &        XMV(NJ),        ! molecular species fractions ""
     &        OONEED          ! needed oxygen from air      ""

      REAL*8  VOLMOL,         ! molVolatiles/kgVolatiles
     &        H2OMOL,         ! molH2O/kgMoisture
     &        O2MOL           ! molO2/kgVolatiles

      REAL*8  A(10,10), AA(10,10), B(10) ! variables for linear equation solver
      REAL*8  WTOT, SUM, W_V(NJ), W_CHAR ! auxiliaries
      REAL*8  FF                         ! auxuliaries
      INTEGER J, L, LL, N                ! counters

       IF (WRTSCR) write(*,*) '  VOLCOM '

       FF    = 3545. * EXP(-10300./TBED)
       XNVOL = (0.486*COALM(4)*(1.-COALM(MOIS))*(1.-COALM(ASH)) + FF)/
     &            ( 1. + FF)
  
C*           choose selected model         
      IF (N .EQ. 1) THEN
          GOTO 10
       ELSE IF (N .EQ. 2) THEN
          GOTO 20
       ELSE
            RETURN
       ENDIF
        
                 
 10   CONTINUE


C*     C  H  O  N  S  GOING TO VOLATILES
C*     MOISTURE RELEASE
C*     O2 CONSUMED FOR DEVOLATILIZATION

C*      convert kg/kgVolat to mole/kgVOLATILES fractions of volatile 
C*      species in coal volatiles

      IF (COALM(VOLAT) .GT. 0.) THEN
       CC = (COALM(1) - COALM(FIXED)) / COALM(VOLAT) / MWS(1)
       HH = COALM(2)  / COALM(VOLAT) / MWS(2)     
       OO = COALM(3)  / COALM(VOLAT) / MWS(3)     
       NN = COALM(4) * XNVOL / COALM(VOLAT) / MWS(4)     
       SS = COALM(5) / COALM(VOLAT) / MWS(5)
      ELSE
       CC = 0.
       HH = 0.
       OO = 0.
       NN = 0.
       SS = 0.
      ENDIF

C*         initioalize volatile fractions mole/kgVolatiles
          DO J = 1, NSPECY
             XMV(J) = 0.
          ENDDO

C*        initialize needed oxygen
           OONEED = 0.

C*         distribute solids flow of CHONS 
C*         to gas flow of compounds (species) (mols/s)

C*        convert C to CO
           XMV(CO) = CC
           OONEED = OONEED + CC
           CC = CC - CC

C*         convert O to O2
          XMV(O2) = OO / 2.
          OONEED = OONEED + 0.
          OO = OO - OO

!zcC*         convert S to SO2
!zc          XMV(SO2) = SS
!zc          OONEED = OONEED + 2. * SS
!zc          SS = SS - SS
C*         convert S to H2S
          XMV(H2S) = SS
          HH = HH - 2.*SS
          SS = SS - SS

C*         convert N to NH3
          IF (NN .GE. HH/3.) THEN
            XMV(NH3) = HH/3.
            HH = HH - HH
            NN = NN - HH/3.
          ELSE
            XMV(NH3) = NN
            HH = HH - 3.*NN
            NN = NN-NN
          ENDIF  

C*         convert rest N to NO
          XMV(NO) = NN 
          OONEED = OONEED + NN
          NN = NN  

C*          convert H to H2
           XMV(H2) = HH / 2.
           HH = HH - HH

C*           needed molecular oxygen (mole/kgCoal)
           O2MOL = OONEED / 2.

C*               sum up molar fractions of gas species (mole/mole)
             VOLMOL = 0.
          DO J = 1,NSPECY
             VOLMOL = VOLMOL + XMV(J)
          ENDDO

C*               molar flow of water vapour (mole/kgcoal)
           H2OMOL = 1. / MWG(H2O)

C*         get release rate fractions (mole specy / moles total)

         IF (VOLMOL .GT. 0.) THEN
            DO J=1,NSPECY
               XVOLA(J) = XMV(J) /VOLMOL
            ENDDO 
         ELSE
            DO J=1,NSPECY
               XVOLA(J) = 0.
            ENDDO 
         ENDIF 

C*         drying and consumed oxygen fractions
          DO J=1,NSPECY
             XH2O(J)  = 0.
             XO2(J)   = 0.
          ENDDO 
             XH2O(H2O) = 1.
             XO2(O2)   = 1.

        IF (WRTSCR)  WRITE(*,*) ' END VOLCOM '

C **** ******** TESTWRITER ********************

        IF (.NOT. WRTOUT(16)) RETURN

        OPEN(UNIT=27,FILE='VOLCOM.DAT',STATUS='UNKNOWN')
        WRITE(27,*)' Species distribution for devolatilization'
        WRITE(27,*)
  
       DO J=1,NSPECY
           WRITE(27,*) SPECY(J), XH2O(J), XVOLA(J),XO2(J)
       ENDDO
        WRITE(27,*) 'RELEASE      H2O         Volatiles      O2 needed'
        WRITE(27,*) 'RATE   ', H2OMOL,     VOLMOL,         O2MOL 
        WRITE(27,*)
        WRITE(27,*) 'fixC', COALM(FIXED)
        WRITE(27,*) 'N in fix', 1.-XNVOL
        CLOSE(27)

        RETURN
 
*****************************************************************************

C       DEVOLATILIZATION MODEL ACCORDING TO MERRICK

 20     CONTINUE
                
C     The B(10) array contains following species respectively
C     1     2    3     4   5    6    7   8    9    10
C     coke  CH4  C2H6  CO  CO2  tar  H2  H2O  NH3  H2S 

C     order of MATRIX(columns,rows)  
      DATA AA /
     & 0., 0.75, 0.8, 0.4286, 0.2727, 0.85 , 0., 0.    , 0.    , 0.    ,
     & 0., 0.25, 0.2, 0.    , 0.    , 0.082, 1., 0.1111, 0.1765, 0.0588,
     & 0., 0.  , 0. , 0.5714, 0.7273, 0.049, 0., 0.8889, 0.    , 0.    ,
     & 0., 0.  , 0. , 0.    , 0.    , 0.009, 0., 0.    , 0.8283, 0.    ,
     & 0., 0.  , 0. , 0.    , 0.    , 0.01 , 0., 0.    , 0.    , 0.9412,
     & 1., 9*0.,
     & 0., 1.  , 8*0.,
     & 0., 0.  , 1.  , 7*0.,
     & 0., 0.  , 0.  , 1.  , 6*0.   ,
     & 0., 0.  , 0.  , 0.  , 1.     , 5*0. /

      WRITE(*,*) ' MERRIK '

C*    transpose matrix (due to fortran reading order)
       DO L=1,10
         DO LL=1,10
          A(L,LL) = AA(LL,L)
         ENDDO
       ENDDO

       DO  L=1,5
         A(1,L) = 1.
         B(L)   = COALM(L)
       ENDDO

         A(1,4) = (1.-XNVOL)*COALM(4)

        B(6) = 1. - COALM(VOLAT)
        B(7) = 1.31 * COALM(COALH)
        B(8) = 0.22 * COALM(COALH)
        B(9) = 0.32 * COALM(COALO)
        B(10)= 0.15 * COALM(COALO)

      CALL LINEQS(A,10,10,B)

      W_V(CO)   = B(4)   
      W_V(CO2)  = B(5)
      W_V(O2)   = 0.
      W_V(H2O)  = B(8)
      W_V(NO)   = 0.
      W_V(NO2)  = 0.
      W_V(N2)   = 0.
      W_V(H2)   = B(7)
      W_V(SO2)  = 0.
      W_V(CH4)  = B(2)
      W_V(C2H6) = B(3)
      W_V(NH3)  = B(9)
      W_V(N2O)  = 0.
      W_V(H2S)  = B(10)   
      
      W_CHAR    = B(1)
   
         SUM=0
      DO J = 1,NSPECY
         SUM = SUM + W_V(J)
      ENDDO
      SUM = SUM 

      DO J =1,NSPECY
         W_V(J)    = W_V(J)/SUM

         XVOLA(J) = W_V(J)          !zc
         XH2O(J) = 0.0              !zc
         XO2(J) = 0.0               !zc
      ENDDO


         H2OMOL = 1. / MWG(H2O)     !zc
         VOLMOL = SUM               !zc
         O2MOL = 0.0                !zc

         XH2O(H2O) = 1.             !zc
         XO2(O2)   = 1.             !zc

        COALM(FIXED) =   B(1)*(1.-COALM(ASH))
    
C **** ******** TESTWRITER ********************

        OPEN(UNIT=17,FILE='MERRIK.DAT',STATUS='UNKNOWN')
        WRITE(17,*)' Species distribution for devolatilization'
        WRITE(17,*)
  
        WTOT=0.
       DO J=1,NSPECY
        WRITE(17,*) SPECY(J), W_V(J),XVOLA(J)
        WTOT = WTOT + W_V(J)
       ENDDO
    
        WRITE(17,*)
        WRITE(17,*) 'SUM ', WTOT
        WRITE(17,*)
        WRITE(17,*) 'fixC', COALM(FIXED)

        CLOSE(17)

      RETURN
           
      END         ! (VOLCOM)




