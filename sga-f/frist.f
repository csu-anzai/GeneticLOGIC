C -------------------------------------------------------
      SUBROUTINE FRIST(CPU)
C -------------------------------------------------------
C
      REAL*4 CPU, CPUO, ETIME
      REAL*4 ACPU(2)
      DATA CPUO/172800.00/
      CPU=ETIME(ACPU)
      CPU=CPUO - CPU
      RETURN
      END
