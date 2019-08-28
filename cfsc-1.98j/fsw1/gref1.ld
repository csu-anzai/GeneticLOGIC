; file to load Gref1 environment, set FSW1 parameters, and designate 
; next best states.
le gref1.env
ecmd eas 2
ecmd dftr 1
ecmd dftm 1
ecmd htr 1
set echo=0
ecmd nbs  0, 5
ecmd nbs  1, 6
ecmd nbs  2, 6
ecmd nbs  3, 6
ecmd nbs  4, 8
ecmd nbs  5, 9
ecmd nbs  6, 10
ecmd nbs  7, 10
ecmd nbs  8, 13
ecmd nbs  9, 13
ecmd nbs 10, 13
ecmd nbs 11, 13
ecmd nbs 12, 0
ecmd nbs 13, 0
ecmd nbs 14, 0
ecmd nbs 15, 0
set echo=1
