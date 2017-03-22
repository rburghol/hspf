C
C
C
      PROGRAM   HSPFBAT
C
C     + + + PURPOSE + + +
C     Batch HSPF, file management and system dependent stuff here,
C     then call generic code shared with interactive
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FILES(15),I,I0,USRFL,MESSU,
     $             RETCOD,RCL,SCLU,SGRP
      CHARACTER*64 FILNAM,VERSN,HMSNAM,FNAME
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRMT
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBFIN, XGTARG, FILOPN, FILBLK, HSPF, HDMESI, SCCLAL
      EXTERNAL     ZIPI, DSSCLO, UCIINP, HDMESC, HDMEST, WDBOPN
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A64)
C
C     + + + END SPECIFICATIONS + + +
C
C     version info and unix what info
      INCLUDE 'versn.inc'
C
C     vax - dont print output conversion error messages
CVAX  call errset (63,.true.,.false.,.false.,.false.,15)
C
C     OPEN (UNIT=99,FILE='ERROR.FIL')
C
C     initialize wdm file common block
      CALL WDBFIN
C
C     get input file name - this is a system-specific routine
      FILNAM= 'hspfuci.inp'
      CALL XGTARG
     M            (FILNAM)
C
C     open message file
      I= 1
      INCLUDE 'fhsmes.inc'
      CALL WDBOPN (MESSFL,HMSNAM,I,
     O             RETCOD)
      IF (RETCOD .NE. 0) THEN
C       problem with opening message file, prompt for other file name
        WRITE (*,*) 'Problem:',RETCOD,MESSFL,I,' with ',HMSNAM
        WRITE (*,*)
        WRITE (*,*) 'NAME OF WDM MESSAGE FILE: '
        READ (*,1000) FNAME
        CALL WDBOPN (MESSFL,FNAME,I,
     O               RETCOD)
      END IF
C
      IF (RETCOD .EQ. 0) THEN
C       do opening message
C       clear screen
        CALL SCCLAL
        SCLU= 201
        SGRP= 50
        CALL HDMESC (MESSFL,SCLU,SGRP,FILNAM)
C       open input file
        USRFL= 7
        ACC= 'SEQUENTIAL'
        FRMT= 'FORMATTED'
        RCL= 0
        STAT= 'OLD'
        CALL FILOPN
     I              (ACC,FRMT,RCL,STAT,USRFL,FILNAM,
     O               RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         input file opened, process files block in input file
C         initialize files to closed
          I= 14
          I0= 0
          CALL ZIPI (I,I0,
     O               FILES)
          FILES(15)= MESSFL
          CALL FILBLK
     I                (USRFL,
     M                 FILES,
     O                 RETCOD)
C         back to beginning of input file
          REWIND (USRFL)
C
          IF (RETCOD .EQ. 0) THEN
C           file block processed without error - beginning message
            CALL SCCLAL
            SGRP= 51
            CALL HDMEST (MESSFL,SCLU,SGRP)
C           read users uci file
            MESSU= FILES(1)
            CALL UCIINP (USRFL,MESSFL,MESSU)
C           close users input file
            CLOSE (UNIT=USRFL)
C           proceed to run model
            CALL HSPF (FILES,FILNAM,
     O                 RETCOD)
C           simulation complete
C           clear screen
            CALL SCCLAL
            IF (RETCOD .EQ. 0) THEN
              SGRP= 52
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 1) THEN
C             runfg=0 in global block - must stop
              SGRP= 53
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 2) THEN
C             errors in input file -  must stop
              SGRP= 54
              CALL HDMEST (MESSFL,SCLU,SGRP)
            ELSE IF (RETCOD .EQ. 3) THEN
C             no run keyword found in input file
              SGRP= 55
              CALL HDMEST (MESSFL,SCLU,SGRP)
            END IF
          ELSE
C           error in files block
            SGRP= 56
            CALL HDMESI (MESSFL,SCLU,SGRP,RETCOD)
          END IF
        ELSE
C         error opening uci file
          SGRP= 57
          CALL HDMESI (MESSFL,SCLU,SGRP,RETCOD)
        END IF
C
C       close any dss files
        CALL DSSCLO
      END IF
C
      STOP
      END
