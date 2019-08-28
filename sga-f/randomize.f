c
c -----------------------------------------------------------------------
c       randomize
c -----------------------------------------------------------------------
c
        subroutine randomize(randseed)
        integer randomseed, randseed
	real    a, ran2
        common /ranco/ randomseed
        randomseed=randseed
        A       = ran2(randomseed)
        return
        end
