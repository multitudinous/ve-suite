
      REAL*8 FUNCTION Z_EXP(A,EX)

************************************************************************
* LAST CHANGE AT :                          BY:                        *
************************************************************************
* CALLED BY :                                                          *
* CALLS     :                                                          * 
************************************************************************
* received  : A   -  Value                                             *
* returned  : Z_EXP  power of a number if positive, else zero          *
************************************************************************
* PURPOSE   : Returns the safe result of A**EX                         *
************************************************************************

      REAL*8  A ,EX, TINY 
      PARAMETER (TINY=1.E-30)


      IF (A .GE. 0.) THEN
         Z_EXP = A**EX
      ELSE
         Z_EXP = TINY
      ENDIF

      RETURN

      END

