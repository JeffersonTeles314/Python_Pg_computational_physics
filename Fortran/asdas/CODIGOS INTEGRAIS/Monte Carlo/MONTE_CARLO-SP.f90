program MonteCarlo
open(15, file="Monte_Carlo-SP.txt")
         idum = -1
        N = 10**7
          PI = ACOS(-1.0)
          EXATO = --
        SUMF = 0.
        SUMF2 = 0.
        A = --
        B = --
        DO 20 IX = 1, N
           X = A + (B-A)*ran3(idum)
           FX = FUNC(X)
           SUMF = SUMF + FX
           SUMF2 = SUMF2 + FX**2
        20 CONTINUE
        FAVE = SUMF/N
        F2AVE = SUMF2/N
        SIGMA = SQRT(abs(F2AVE - FAVE**2)/N)
        ERRO = ABS(FAVE - EXATO)
        ERRO2 = ABS(FAVE - EXATO)/EXATO *100
        write(15,*) "Integral =", FAVE, "Exato =", EXATO
        write(15,*) "Erro estimado:", SIGMA,"Erro obtido:", ERRO, "Erro Percentual:", ERRO2
        write(15,*) "N =",N
end

function FUNC(X)
   FUNC =
return
end

FUNCTION ran3(idum)
      INTEGER idum
      INTEGER MBIG,MSEED,MZ
      REAL ran3,FAC
      PARAMETER (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1./MBIG)
      INTEGER i,iff,ii,inext,inextp,k
      INTEGER mj,mk,ma(55)
      SAVE iff,inext,inextp,ma
      DATA iff /0/
      if(idum.lt.0.or.iff.eq.0)then
        iff=1
        mj=abs(MSEED-abs(idum))
        mj=mod(mj,MBIG)
        ma(55)=mj
        mk=1
        do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.MZ)mk=mk+MBIG
          mj=ma(ii)
11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.MZ)ma(i)=ma(i)+MBIG
12        continue
13      continue
        inext=0
        inextp=31
        idum=1
      endif
      inext=inext+1
      if(inext.eq.56)inext=1
      inextp=inextp+1
      if(inextp.eq.56)inextp=1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.MZ)mj=mj+MBIG
      ma(inext)=mj
      ran3=mj*FAC
      return
      END

