c
c -----------------------------------------------------------------------
c       mutation
c -----------------------------------------------------------------------
c
        subroutine mutation ( alleleval,allelenew,pmutation, nmutation )
        integer         nmutation
        byte            alleleval, allelenew
        real            pmutation
c
        integer         flip
c
        if ( flip ( pmutation ) .eq. 1 ) then
                allelenew       = 1 - alleleval
                nmutation       = nmutation + 1
        else
                allelenew       = alleleval
        endif

        end
