
      FUNCTION Z_NAME()

************************************************************************
* LAST CHANGE AT :  16.09.94                BY:   J. Hannes            *
************************************************************************
* CALLED BY : DATAIN                                                   *
* CALLS     :                                                          *
************************************************************************
* COMMON in :                                                          *
*       out :                                                          *
*  in & out :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : Z_NAME input file name                                   *
************************************************************************
* PURPOSE   : SUPPLIES INPUT FILE NAME                                 *
************************************************************************

      CHARACTER * 12   Z_NAME 
      LOGICAL          EX

C*              interface for starting CFBC from BASE
C*              menu handles data flow if CFBC is
C*              called directly without BASE

          OPEN(1,file=' ',status='old',iostat=ierr)
       IF (IERR .EQ. 0) THEN 
             REWIND(1)
             READ(1,'(A)') 
             READ(1,'(A)')
             READ(1,'(A)')
             READ(1,'(A)') Z_NAME
             WRITE(*,*) 'The input file is ',Z_NAME
       ELSE 
             WRITE(*,*) ' File does not exist !'
             WRITE(*,*) ' Please check NAME or PATH !'

 110       CONTINUE
             WRITE(*,*)
             WRITE(*,*)' Type Name of input data file ! ' 
             READ (*, '(A)') Z_NAME
             INQUIRE(FILE=Z_NAME,EXIST=EX)

             IF (.NOT. EX)  THEN
               WRITE(*,*) ' File does not exist, please try again !!! '
              GOTO 110
             ENDIF
       ENDIF

            CLOSE(1)

      RETURN

      END       ! (Z_NAME)



