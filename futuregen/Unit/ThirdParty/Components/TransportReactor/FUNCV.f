
      SUBROUTINE FUNCV(NN,X,FVEC)

************************************************************************
* LAST CHANGE AT : 28.12.93                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : NWTSVD                                                   *
* CALLS     : HOMOGR, HETGR                                            *
************************************************************************
* COMMON in : AREA(K), HEIGHT(K), KCOMMON, PRESS(1), RG,               *
*             TEMP(K,P), YCORE(K), YANNU(K), YBUBB(K)                  *
*             XGAS(J,K,P), YFEEDA(K), YFEEDF(K), YH2O(K), YVOLA(K)     *  
*             YO2(K), YOUT(K), YCOBU(K), YBUCO(K), YCOAN(K), YANCO(K)  *
*             COBUMX(K), COANMX(K), XFEED(J), XH2O(J), XVOLA(J), XO2(J)*
*             RRCOMB(J,K,P), RSURE(K,P), SUREF                         *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : N     number of equations to be solved simultaneously    *
*             X     variable array                                     *
* returned  : FVEC(J)  root funct. array                               *
************************************************************************
* PURPOSE   : delivers root funct. (  and jacobians if necessary )     *
*             to the nonlinear equation solver dependent on status     *
*             pre- or postprocessing                                   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER N,NN
      REAL*8  FVEC(50)        ! vector of zero funct.
      REAL*8  X(50)           ! gas specy fractions
      REAL*8  HOMRAT(15,3),   ! homogeneous reaction rates
     &        HETRAT(15,2)    ! heterogeneous reaction rates
      REAL*8  MIXCB,          ! core-bubble mixing rate
     &        MIXCA           ! core annulus mixing rate

      REAL*8  Z_EXP                            !   "
      INTEGER J, K, NCO, NAN, NBU, NCOUNT,kkk,Kold  !   "
      common /aaa/Kold
        K      = KCOMMON

        IF (POSTPR) THEN
            N = NPOST
        ELSE
            N = NPRE
        ENDIF

            NCO = 0        ! pointers for reduced arrays
            NAN = N
            NBU = 2*N           
            NCOUNT = N

       DO J=1,3*N
           IF (X(J) .LT. 0.) THEN
              X(J) = 0.
           ENDIF
       ENDDO

       CALL HOMOGR(X,HOMRAT)
       CALL HETGR(X,HETRAT)
                  
C*       Lateral gas mixing gas exchange between phases (mol/s)
            MIXCB =  COBUMX(K)*AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                        *PRESS(1) / RG / TEMP(K,CORE)
            MIXCA =  COANMX(K)*AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                        *PRESS(1) / RG / TEMP(K,CORE)

C------------------------------------------------------------------------
C*        Create funct.

C*          create funct. to become  zero [mol/s]
      DO J=1,N

        FVEC(J+NCO)   = YCORE(K-1)*XGAS(J,K-1,CORE) 
     &                - YCORE(K)  *X(J)
     &                + YFEEDA(K) *XFEED(J)
     &                + YFEEDF(K) *XGAS(J,CMP(6),CORE)
     &                + YH2O(K)   *XH2O(J)
     &                + YVOLA(K)  *XVOLA(J)
     &                - YO2(K)    *XO2(J)
     &                - YOUT(K)   *X(J+NCO)
     &                - YCOBU(K)  *X(J+NCO) + YBUCO(K)  *X(J+NBU) 
     &                - YCOAN(K)  *X(J+NCO) + YANCO(K)  *X(J+NAN)   
     &                - MIXCB     *X(J+NCO) + MIXCB     *X(J+NBU) 
     &                - MIXCA     *X(J+NCO) + MIXCA     *X(J+NAN)   
     &                + RRCOMB(J,K,CORE)  ! * X(O2+NCO)
     &                + HOMRAT(J,CORE) 
     &                + HETRAT(J,CORE)
     &                + RSURE(K,CORE)*SUREF(J)
     &                       *X(H2S+NCO)  !*Z_EXP(X(O2+NCO),0.5)

c      if(J.le.4.and.K.ne.Kold) then
c      kkk = 100+J
c      write(kkk,111)k,RRCOMB(J,K,CORE),HOMRAT(J,CORE),HETRAT(J,CORE)
c      endif
c111   format(i4,3e15.6)

       FVEC(J+NAN)  = YANNU(K-1)*XGAS(J,K-1,ANNU) 
     &                - YANNU(K)  *X(J+NAN)
     &                - YANCO(K)  *X(J+NAN)  + YCOAN(K) *X(J+NCO)
     &                - MIXCA     *X(J+NAN)  + MIXCA    *X(J+NCO)         
     &                + RRCOMB(J,K,ANNU)  ! * X(O2+NAN)
     &                + HOMRAT(J,ANNU)
     &                + HETRAT(J,ANNU) 
     &                + RSURE(K,ANNU)*SUREF(J)
     &                       *X(H2S+NAN)  !*Z_EXP(X(O2+NAN),0.5)

c      if(J.le.4.and.K.ne.Kold) then
c      kkk = 105+J
c      write(kkk,111)k,RRCOMB(J,K,CORE),HOMRAT(J,CORE),HETRAT(J,CORE)
c      endif

       FVEC(J+NBU)  = YBUBB(K-1)*XGAS(J,K-1,BUBB)
     &                - YBUBB(K)  *X(J+NBU)
     &                - YBUCO(K)  *X(J+NBU) + YCOBU(K) *X(J+NCO)
     &                - MIXCB     *X(J+NBU) + MIXCB    *X(J+NCO)   
     &                + HOMRAT(J,BUBB)  

      ENDDO
         Kold = K
      RETURN

      END         ! (FUNCV)




      