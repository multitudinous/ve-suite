
      SUBROUTINE LINEQS(A,N,NP,B)

************************************************************************
* LAST CHANGE AT : 10.10 1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : MERRIK, POPUL                                            *
* CALLS     :                                                          *
************************************************************************
* COMMON in : -                                                        *
* out       : -                                                        *
* in & out  :                                                          *
************************************************************************
* passed    : A     - NxN matrix for equation system                   *
*           : B     - vector of independent terms                      *
*           : INDX  - internal                                         *
*           : N     - number of equations                              *
*           : NP    - dimension of vector                              *
* returned  : B     - solution vector of independent variables         *
************************************************************************
* PURPOSE   : solves a set of N linear equations by LU decomposition   *
*             and separate solution according to                       *
*             LUDCMP Numerical Recipies pp. 35                         *
*             LUBKSB Numerical Recipies pp. 36                         *
*             LINear EQuation Solver, needs matrix A(N,N)              *
*             and vector B(N) delivers the solution vector in B(N)     *
*             CAUTION!!! FORTRAN 77 reads rows before lines -> A(N,N)  *
*             must be already transponed                               *
************************************************************************

      INTEGER            N, NP 
      INTEGER            INDX(N)
      REAL*8             A(NP,NP), B(NP), D
         
        CALL LUDCMP(A,N,NP,INDX,D)
        CALL LUBKSB(A,N,NP,INDX,B)
 
      RETURN
      END            

*************************************************************************

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      INTEGER N,NP,INDX(N)
      REAL*8 A(NP,NP),B(N)
      INTEGER I,II,J,LL
      REAL*8 SUM
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        DO 13 J=I+1,N
          SUM=SUM-A(I,J)*B(J)
13      CONTINUE
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

*************************************************************************

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      INTEGER N,NP,INDX(N),NMAX
      REAL*8 D,A(NP,NP),TINY
      PARAMETER (NMAX=400,TINY=1.0E-20)
      INTEGER I,IMAX,J,K,L
      REAL*8 AAMAX,DUM,SUM,VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
         IF (AAMAX.EQ.0.) THEN
            PAUSE 'SINGULAR MATRIX IN LUDCMP'
                DO L=1,N
              WRITE(*,*) 'AA ', L, A(L,L)
                ENDDO 
         ENDIF
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        DO 14 I=1,J-1
          SUM=A(I,J)
          DO 13 K=1,I-1
            SUM=SUM-A(I,K)*A(K,J)
13        CONTINUE
          A(I,J)=SUM
14      CONTINUE
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          DO 15 K=1,J-1
            SUM=SUM-A(I,K)*A(K,J)
15        CONTINUE
          A(I,J)=SUM
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(A(J,J).EQ.0.)A(J,J)=TINY
        IF(J.NE.N)THEN
          DUM=1./A(J,J)
          do 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      RETURN
      END       ! (LINEQS)

