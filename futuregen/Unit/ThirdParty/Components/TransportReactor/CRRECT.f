
      SUBROUTINE CRRECT (I, INDEX, DX, X1, X2, XNEW, E1, E2, E, EMAX)

************************************************************************
* LAST CHANGE AT :                          BY:   M. Horio             *
************************************************************************
* CALLED BY : CHARTR                                                   *
* CALLS     :                                                          * 
************************************************************************
* COMMON in :                                                          *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* received  : I         loop counter                                   *
*             INDEX     0=regula falsi,1=Newton raphson,2=root found   *
*             DX        regula falsi steps                             *
*             X1, X1    interval root variables                        *
*             XNEW      new root variable                              *
*             E1, E2    zero funct. interval variables                 *
*             E         actual error of zero funct.                    *
*             EMAX      error limit                                    *
* returned  : INDEX, X1,X2,XNEW,E1,E2,E                                *
*                                                                      *
*  To start iteration put CRRECT into a loop with counter I and a max. *
*  number of loops. Give starting value XNEW, steps DX (DX*Imax must be*
*  bigger than allowed interval) and the allowed error EMAX. Set Index *
*  to zero. Root is found when INDEX returnes as 2.                    *
************************************************************************
* PURPOSE   : Updated to FORTRAN 77 from a routine given by Horio et al*
*             Fluidized Bed Combustor Modelling - NASA Report, Jan 1977*
************************************************************************

      REAL*8  DX, X1, X2, XNEW, E1, E2, E, Emax ! description see above
      INTEGER I, INDEX                          ! description see above

C*       first check to see if convergence is achieved
      IF (ABS(E) .LE. EMAX)  THEN
         INDEX = 2
         RETURN
      END IF

C*        calculate new value for XNEW by increment  (INDEX = 0)
      IF (INDEX .EQ. 0)  THEN

C*          check if sign of error value E has changed
         IF (I .NE. 1 .AND. E1 * E .LE. 0.)  THEN
            INDEX = 1
            GOTO 10
         END IF

C*          store current values of error E and variable XNEW for next loop
         E1 = E
         X1 = XNEW

C*          increment and exit
         IF (E .GT. 0.)  THEN
            XNEW = XNEW + DX
         ELSE
            XNEW = XNEW - DX
         END IF

         RETURN
      END IF

C*       calculate new value for XNEW by interpolation  (INDEX = 1)
   10 IF (E1 * E .LE. 0.)  THEN
         E2 = E
         X2 = XNEW
      ELSE
         E1 = E
         X1 = XNEW
      END IF

C*       calculate new value with Newton-Raphson
      XNEW = X2 - (X2 - X1) * E2 / (E2 - E1)

      RETURN

      END   ! (CRRECT)
