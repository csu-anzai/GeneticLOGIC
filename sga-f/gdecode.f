C -------------------------------------------------------
C     SUBROUTINE FRIST(CPU)
C -------------------------------------------------------
C
C     REAL*4 CPU, CPUO, ETIME
C     REAL*4 ACPU(2)
C     DATA CPUO/172800.00/
C     CPU=ETIME(ACPU)
C     CPU=CPUO - CPU
C     RETURN
C     END
c
c -----------------------------------------------------------------------
c       gdecode
c -----------------------------------------------------------------------
c
        real function gdecode( chromosome, lbits )
c
c               calculate fitness from chromosom
c
        byte            chromosome ( lbits )
        integer         j,lbits
        real            accum, powerof2
c
        accum           = 0.0
        powerof2        = 1.0
c
        do j = 1,lbits
c               if ( chromosome(j) ) accum = accum +powerof2
                accum = accum + float(chromosome(j))*powerof2
                powerof2 = powerof2 * 2.0
        enddo
        gdecode =accum
        return
        end
