PROGRAM CO_42021
    INTEGER, PARAMETER :: Mcat = 1200, Mnu = 4550, Mru = 200, Nhanh = 400
    INTEGER, PARAMETER :: Ngio = 750, MMnu = 2 * Mnu, Ninv = 30, MOCO = 1500
    INTEGER, PARAMETER :: Nqtt = 801, Nbien = 10, MaxLink = 8, NKql = 500
    INTEGER, PARAMETER :: NTSo = 14
    INTEGER, PARAMETER :: NTdoan = 120, Nrain = 12, NtabRu = 10, Mband = 25
    INTEGER, PARAMETER :: KBsize = Mcat * NtSo, KB1size = Mcat * NtSo
    INTEGER KEP(38000)
    INTEGER  JCO1(MNu * Mband), NCO1(MNu * Mband)
    !-----
    REAL FEN(15000), FY(17000), PEN(46500), F2(43900), F3(8500)
    REAL :: time_begin, time_end
    REAL BB(KBsize), BB1(KB1size), HMa(Mcat), Hav(Mcat), HMi(Mcat)
    REAL TG1(Mcat), TG2(Mcat), AJ(Mnu, Mband), Hbot(Mcat), XX(Mcat)
    REAL QDU(Mcat), QAM(Mcat), H(MCat + Mru), Q(Mcat), HH(Mcat), YY(Mcat)
    REAL QLA(Mcat), WLNgap(Mnu), DELTAX(MCat), SOGIO(MOCO)
    REAL A1(Mcat), E1(Mcat), B2(Mcat), D2(Mcat), E2(Mcat), A3(Mcat), &
            &     B3(Mcat), C3(Mcat), D3(Mcat), PN(Mcat), QN(Mcat), RN(Mcat)
    !-------
    COMMON/S9/ QTI(5000), CHU(5000), SIZ(5000)
    COMMON/LT/l15, l16, l17, NAC, MBAN
    COMMON/DIMK/L1, L2, L3, L4, L5, L6, L7, L8, L10, L11, L12
    COMMON/DIMF/LQ, LH, LPN, LQN, LRN, LA3, LB3, LC3, LD3, LA1, LE1, &
            & LB2, LD2, LE2, LDI, LRG, LBO, LR, LK2, LK1, LRO, LBS, LBS1, LBS2
    COMMON/DIMP/MAM, MQL, NAP, MZD, MRZ, MHC, LFAL, LFZD, &
            &   MQR, MQQ, MFQ, MFS, MFR, MVL, LPS, MU, MQA, MQT, LQL, LQRU
    COMMON/KPR/ITER, MOA, IP, NUM, NCK, IR1, DT, DNT, &
            &       INDIS, JHQS, MDT, DNR, MPLU, MBOC, IM1
    COMMON/DAT/ITO, ING, ITH, IYE, KKS, JKT, DELT
    COMMON/SS /PTO(5000), PTN(5000), NUCO
    COMMON/ONE/NA, NT, NH, NOC, MCON, NES, NC, KN, LN, MQC, NH1, &
            & NSM, NTD, NRK, NTR, NNC, NRH, NSQ, KQL, MAXNOI, JNU, NHA, DCO
    COMMON/PRIN/ INHQ(400), INHRU(400), INQRU(400)
    CHARACTER*20 FF
    real, dimension(:), allocatable :: CH
    !------
    Open(90, file = './output/console.txt', status = 'unknown')
    CALL CORRES(F2, F3, PEN, FEN, FY, KEP, Mband, MaxLink, NInv)
    CALL INPUT(H, Q, BB(1), BB1(1), F2(LBO), &
            & F3(LBS), B2, A1, E1, FEN(LPS), PEN, PEN(MFR), &
            & PEN(MVL), KEP, FY(1), FEN(LQL), FEN(LQRU), F2(LR), FEN, HH, &
            & F3(LK1), F3(LK2), FY(LRO), F3(LBS1), F3(LBS2), HMa, Hav, HMi, HBot, &
            & XX, YY, WLNgap, DELTAX)
    CALL XEP(KEP, KEP(L15), JCO1(1), KEP(L17), LN, L3, L4, L5)
    CALL CPU_TIME(time_begin)
    CALL TEN
    1 ITER = ITER + 1
    CALL COMHQ(H, Q, A3, B3, C3, D3, PN, QN, RN, A1, E1, B2, D2, &
            & E2, AJ, BB(1), BB1(1), F2(LBO), FEN(LPS), PEN(MFR), F2(LR), &
            & PEN, KEP, KEP(L16), KEP(L15), NCO1(1), JCO1(1), KEP(L17), FEN(LQRU), &
            & FEN, HH, F3(LBS), F3(LK1), F3(LK2), FY(LRO), F3(LBS1), F3(LBS2), &
            & Hma, Hav, Hmi, QDu, QAm, WLNgap, DELTAX, SOGIO)
    IF (MOA == 0.AND.ITER > IR1)THEN
        CALL RESULT(NES, NC, IP, H, Q, CHU, TG1, TG2, &
                & KEP, MPLU, PEN(MU + 1), FY(1), MBOC, &
                & NRK, MAXNOI, FEN(LQRU), MCON, LN, F3(LBS1))
        !	             CALL TEN4(iter/Num,NH1,DT)
    END IF
    CALL TEN
    IF(ITER.LT.NCK) GOTO 1
    CALL CPU_TIME(time_end)
    totaltime = time_end - time_begin
    igio = totaltime / 3600.
    ifut = (totaltime - igio * 60) / 60.
    igiay = totaltime - igio * 3600. - ifut * 60.
    Write(90, 75)igio, ifut, igiay
    Write(12, 75)igio, ifut, igiay
    CALL FINI(NA, NC, IP, MCON, L6, L7, L10, LN, &
            &  H, Q, INDIS, KEP, NRK, MAXNOI, FEN(LQRU), CH, &
            &  Hma, Hav, HMi, QDU, QAM, XX, YY)
    75 Format(/, 5x, 'Time of operatiom was (h:m:s):', i2, ':', i2, ' :', i3, /)
    !================================
    IF(MCON /= 0)Then
        Write(6, *)' ============================='
        Write(6, *)' DO NGAP MAX (m) TAI CAC HO GA '
        WRITE(6, *)' ThTu     HO GA    DO NGAP(m)  Thoi gian ngap (phut) '
        DO K = NUCO, LN
            MM = K - NUCO + 1
            WRITE(6, 447)MM, K, WLNgap(K), SOGIO(K) * DT / 60
        END DO
        447  FORMAT(I5, 4x, I6, 4x, F8.2, F8.0)
    End if
    Close(90)
END PROGRAM
!      *************************
SUBROUTINE TEN
    !      -------------------------
    Write(90, *) '      PROGRAM PIPES DEVELOPED BY Prof. Ng.T.DAC'
    Write(90, *) '      Apr21 VERSION MODIFIED IN 2021'
    Write(90, *) '      ******* APR_21 RUNNING *******'
End Subroutine
!      ******************
SUBROUTINE TEN4(kk, NH, DT)
    !      ------------------
    Write(90, *)
    Write(90, *)
    Write(90, 80)kk, NH, DT
    80    Format(15x, 'Running at ', I6, '  of  ', I7, '  (Hours)', &
            &' ; Each Time Step =', F6.0, ' Seconds')
End Subroutine
!     ****************************************************
SUBROUTINE CORRES(F2, F3, PEN, FEN, FY, KEP, Mband, MaxLink, NInv)
    !     ****************************************************
    REAL F2(*), F3(*), PEN(*), FEN(*)
    REAL FY(*)
    INTEGER KEP(*)
    INTEGER(2) IYR, IMON, IDAY, IGIO, IPHUT, IGIAY, IPHA
    COMMON/LT/L15, L16, L17, NAC, MBAN
    COMMON/DIMK/L1, L2, L3, L4, L5, L6, L7, L8, L10, L11, L12
    COMMON/DIMF/LQ, LH, LPN, LQN, LRN, LA3, LB3, LC3, &
            &LD3, LA1, LE1, LB2, LD2, LE2, LDI, LRG, LBO, &
            &LR, LK2, LK1, LRO, LBS, LBS1, LBS2
    COMMON/DIMP/MAM, MQL, NAP, MZD, MRZ, MHC, LFAL, LFZD, &
            & MQR, MQQ, MFQ, MFS, MFR, MVL, LPS, MU, MQA, MQT, LQL, LQRU
    COMMON/ONE/NA, NT, NH, NOC, MCON, NES, NC, KN, LN, MQC, NH1, &
            & NSM, NTD, NRK, NTR, NNC, NRH, NSQ, KQL, MAXNOI, JNU, NHA, DCO
    COMMON/KPR/ITER, MOA, IP, NUM, NCK, IR1, DT, &
            &     DNT, INDIS, JHQS, MDT, DNR, MPLU, MBOC, IM1
    CHARACTER*80 TITLE, FTopo, FTvan, FPRO, FTCong
    CHARACTER*20  DAU, CODE
    INTEGER, DIMENSION(8) :: TIME_VALUES
    ! DANH SACH FILES:  TopoSong : 1; TVanSong: 2 ; HHH: 3 ; QQQ: 4 ; TopoTvanCong: 5
    !      Ngap: 6 ; Velocity : 7 ; DKDau: 8  ; ???: 9; QPL: 10 ; HPL: 11; SODO:12
    !      TAM: 13 ;  HSUM: 14 ; QSUM: 15; DKsau: 18 ;  FVao: 27
    Mcode = 5  ! DTLAM
    CALL DATE_AND_TIME(VALUES = TIME_VALUES)

    IYR = TIME_VALUES(1)
    IMON = TIME_VALUES(2)
    IDAY = TIME_VALUES(3)
    IGIO = TIME_VALUES(5)
    IPHUT = TIME_VALUES(6)
    IGIAY = TIME_VALUES(7)
    IPHA = TIME_VALUES(8)

    OPEN(27, FILE = './input/Fvao.txt', STATUS = 'OLD')
    Read(27, *)
    Read(27, *)CODE   ! Ma chuong trinh
    MTEN = LEN_TRIM(CODE)
    IF(MTEN /= MCODE)Then
        Write(90, *)' CODE ERROR ', MTEN
        STOP
    ELSE
        Goto 666
    END IF
    666  Read(27, *)
    READ(27, '(A)')FPRO  ! Project Name
    Read(27, *)
    READ(27, '(A)')FTopo
    Read(27, *)
    READ(27, '(A)')FTvan
    Read(27, *)
    READ(27, '(A)')FTCong
    Read(27, *)
    READ(27, '(A)')DAU     ! SCENARIO
    MKI = LEN_TRIM(DAU)
    OPEN(1, FILE = FTopo, STATUS = 'OLD')  ! Dia Hinh
    READ(1, '(A)') TITLE
    READ(1, *)NC, NA, LN, KN, NOC, NRH, NRK, NT, NTR
    OPEN(2, FILE = FTvan, STATUS = 'OLD')  ! Thuy van
    READ(2, '(A)') TITLE
    READ(2, *)MCON, MQC, NHA
    READ(2, *)NSM, NTD, NH, NH1
    READ(2, *)DNT, DNR, DCO, MDT, DT    ! DELT in Hours, DT in seconds
    !	DCO=DNT
    OPEN(3, FILE = 'output/' // DAU(1:MKI) // '.HHH', STATUS = 'Replace')
    Write(3, 30)Igio, Iphut, iday, imon, iyr
    OPEN(4, FILE = 'output/' // DAU(1:MKI) // '.QQQ', STATUS = 'Replace')
    Write(4, 30)Igio, Iphut, iday, imon, iyr
    OPEN(14, FILE = 'output/' // DAU(1:MKI) // '.HSUM', STATUS = 'Replace')
    Write(14, 30)Igio, Iphut, iday, imon, iyr
    OPEN(15, FILE = 'output/' // DAU(1:MKI) // '.QSUM', STATUS = 'Replace')
    Write(15, 30)Igio, Iphut, iday, imon, iyr
    Open(12, FILE = 'output/' // DAU(1:MKI) // '.SODO', STATUS = 'Replace')
    Open(13, FILE = 'output/TAM.txt', STATUS = 'Replace')
    IF(MCON.NE.0)THEN
        OPEN(5, FILE = FTCong, STATUS = 'OLD')  ! Dia Hinh TV cong
        OPEN(6, FILE = 'output/' // DAU(1:MKI) // '.NGAP', STATUS = 'Replace')
        Write(6, 30)Igio, Iphut, iday, imon, iyr
    END IF
    IF(NRK.NE.0)THEN
        Open(10, FILE = 'output/' // DAU(1:MKI) // '.QPL', STATUS = 'REPLACE')
        Write(10, 30)Igio, Iphut, iday, imon, iyr
        Open(11, FILE = 'output/' // DAU(1:MKI) // '.HPL', STATUS = 'REPLACE')
        Write(11, 30)Igio, Iphut, iday, imon, iyr
    ENDIF
    !-------------------------------
    Write(12, 30)Igio, Iphut, iday, imon, iyr
    Write(13, 30)Igio, Iphut, iday, imon, iyr
    Write(12, 60) TITLE
    Write(12, 75)DAU
    Write(12, 65)FTopo
    IF(MCON.NE.0)Write(12, 66)FTCong
    Write(12, 67)FTvan
    Write(12, 40)NC, NA, NOC, KN, LN, MCON, NT, NH, NSM, NRK
    30 Format(6x, 'Tinh luc', I3, ' Gio', I3, ' Phut  Ngay', i3, ' Thang', i3, &
            &' Nam ', i5)
    40 FORMAT(10X, 'CROSS SECTIONS =', I5, 2X, '//BRANCHES =', &
            & I5, 2X, '//STRUCTURES =', I3, 2X, /10x, 'BOUN.NODES =', I3, 2X, &
            & '//INTER.NODES+CONG =', I5, '  // CONG NGAM =', I5, &
            &  /6X, 'WATER LEVEL SCALES =', I3, 2X, ' /;BOUNDARY DURATION =', I5, &
            & ' HOURS'/5x, ' RAIN =', I5, 2X, '// PLAIN =', I5, 2x)
    60 FORMAT(A80)
    65 FORMAT(18X, 'TOPO-FILE : ', A80)
    66 FORMAT(16X, 'TOPO-TVCong : ', A80)
    67 FORMAT(19X, 'TVanSong : ', A80)
    75 FORMAT(18X, '  SCENARIO: ', A6)
    MaxNoi = MaxLink
    MRKI = NRK * MAXNOI
    NAC = NA + NOC + MCON
    NNC = NC + NRK
    L1 = NC + Ninv + NRK + 1
    L2 = L1 + KN
    L3 = L2 + NAC
    L4 = L3 + NAC
    L5 = L4 + NAC
    L6 = L5 + NAC
    L7 = L6 + NAC + MRKI
    L8 = L7 + NAC + MQC + MRKI
    L10 = L8 + NRK + 1
    L11 = L10 + Ninv
    L12 = L11 + NA + NRK
    L15 = L12 + KQL + 1
    L16 = L15 + LN + 1
    L17 = L16 + LN + 1
    LEN1 = L17 + NAC * 4
    DO 15 J = 1, LEN1
    15 KEP(J) = 0
    LQ = 2 + NC + NC
    LH = LQ + NNC
    LRO = LH + NNC + 1
    LQH = LRO + LN + 1
    DO  JJ = 1, LQH
        FY(JJ) = 0.
    END DO
    LBO = 1
    LR = LBO + NH * KN
    LEF2 = LR + 2 * LN + MCON + 1
    DO 18 IK1 = 1, LEF2
    18  F2(IK1) = 0.
    LK1 = 1 + MCON
    LK2 = LK1 + 2 * MCON
    LBS = LK2 + 2 * MCON + 1
    LBS1 = LBS + NHA * MQC + 1
    LBS2 = LBS1 + LN + 1
    LEN2 = LBS2 + 3 * LN + 1
    DO 25 J2 = 1, LEN2
    25 F3(J2) = 0.
    MAM = NC + NRK  ! Bo NC
    MQL = MAM + 2 * NC
    NAP = MQL + 2 * NC
    MZD = NAP + NOC
    MRZ = MZD + NOC + MRKI
    MQA = MRZ + NOC + MRKI
    MQT = MQA + NSQ
    MHC = MQT + NSQ
    MQR = MHC + NSM
    MQQ = MQR + NNC
    MFR = MQQ + NC + 1
    MVL = MFR + NRK * NTR + 1
    MU = MVL + NRK
    LEN3 = MU + NC
    DO 27 J3 = 1, LEN3
    27 PEN(J3) = 0.
    MFQ = 1 + NES + NRK
    MFS = MFQ + NES + 1
    LPS = MFS + 1
    LQL = LPS + NSM * NTD + 1
    LQRU = LQL + KQL + 1
    LFAL = LQRU + MRKI + 1
    LFZD = LFAL + MRKI + 1
    LEN4 = LFZD + MRKI
    DO 28 J4 = 1, LEN4
    28 FEN(J4) = 0.
    IF(NH1.EQ.NH)NH1 = NH - 1
    Write(12, 50)LEN1, LEN2, LEN3, LEN4, LEF2, LQH
    50 FORMAT(10X, 'CORE: KEP =', I7, 3X, ';F3 =', I6, 3X, ';PEN =', I6, 3x, &
            &'FEN =', I6, 3X, /10x, ' F2 =', i6, 5x, ';FY =', I6)
END SUBROUTINE
!     ********************************************************
SUBROUTINE INPUT(H, Q, DIE, RON, BO, BS, B2, A1, E1, PSM, PEN, FRU, VOL, &
        & KEP, UMAX, QLA, PQR, R, FEN, HH, TT1, TT2, CH, CAO, DITI, HMa, Hav, Hmi, &
        & Hbot, XX, YY, WLNgap, DELTAX)
    !     ********************************************************
    COMMON/KPR/ITER, MOA, IP, NUM, NCK, IR1, DT, &
            &     DNT, INDIS, JHQS, MDT, DNR, MPLU, MBOC, IM1
    COMMON/DAT/ITO, ING, ITH, IYE, KKS, JKT, DELT
    COMMON/LT/l15, l16, l17, NAC, MBAN
    COMMON/SS /PTO(5000), PTN(5000), NUCO
    COMMON/ONE/NA, NT, NH, NOC, MCON, NES, NC, KN, LN, MQC, NH1, &
            & NSM, NTD, NRK, NTR, NNC, NRH, NSQ, KQL, MAXNOI, JNU, NHA, DCO
    COMMON/DIMP/MAM, MQL, NAP, MZD, MRZ, MHC, LFAL, LFZD, &
            & MQR, MQQ, MFQ, MFS, MFR, MVL, LPS, MU, MQA, MQT, LQL, LQRU
    COMMON/DIMK/L1, L2, L3, L4, L5, L6, L7, L8, L10, L11, L12
    COMMON/S9/ QTI(5000), CHU(5000), SIZ(5000)
    COMMON/PRIN/ INHQ(400), INHRU(400), INQRU(400)
    REAL H(*), Q(*), PEN(*), PSM(NSM, *), B2(*), CH(*), DELTAX(*), &
            & A1(*), E1(*), VOL(*), FRU(NRK, *), TT1(*), TT2(*), CAO(*), &
            & DIE(NC, *), RON(NC, *), BO(KN, NH), BS(MQC, NHA), UMAX(*), &
            & PQR(NRK, *), JCODE, QLA(*), R(*), FEN(*), HH(*), DITI(LN, 3), &
            & Hma(*), Hav(*), Hmi(*), HBot(*), XX(*), YY(*), WLNgap(*)
    INTEGER KEP(*), KQT(1000), MANI(1500, 20)
    CHARACTER*40 FP, FPR, NBR
    CHARACTER*12 DKDau
    MBOC = 1
    READ(2, *)ITO, ING, ITH, IYE, IP, IM1
    READ(2, *)MOUT, INDIS, JHQS, KVE, LGIO
    Write(12, 3050)MBOC, NH1
    Write(12, 3100)ITO, ING, ITH, IYE
    MPLU = 0
    IF(MPLU.EQ.1)THEN
        Write(90, *)' VELOCITY FILE ?'
        READ(*, '(A)')FP
        OPEN(7, FILE = FP, STATUS = 'REPLACE')
    END IF
    DELT = DT / 3600.
    NUM = 1. * MBOC / DELT
    KKS = 0
    JKT = NH1 + IP
    Read(2, *)NES
    NSQ = NES
    IF(NES.NE.0)THEN
        READ(2, *)(INHQ(I), I = 1, NES)  ! MAT CAT IN H Q
        WRITE(3, 4445)(InHQ(K), K = 1, NES)   ! In HH
        WRITE(4, 4444)(InHQ(K), K = 1, NES)   ! In QQ
        4444 FORMAT('  TTu TIME  DATE---Mcat:', I6, 250(3X, I6))  ! Q song
        4445 FORMAT('  TTu TIME  DATE---Mcat:', I5, 250(2X, I5))   ! In HH song
    END IF
    !-------
    IF(NRK.NE.0)THEN
        Read(2, *)JHRU
        IF(JHRU .NE.0) READ(2, *)(INHRU(I), I = 1, JHRU)  ! MAT CAT IN H  Ruong
        WRITE(11, 5555)(INHRU(I), I = 1, JHRU)
        Read(2, *)JQRU
        IF(JQRU .NE.0) READ(2, *)(INQRU(I), I = 1, JQRU)  ! MAT CAT IN Q  Ruong
        WRITE(10, 5560)(INQRU(I), I = 1, JQRU)
    ENDIF
    5555 FORMAT(5X, 300(2X, I3, 2X))
    5560 FORMAT(5X, 300(2X, I5, 2X))
    Write(12, 550) DT
    550 FORMAT(10X, 'TIME STEP DT=', F6.0, 1X, 'Seconds ')
    ITER = (IP - 1) * NUM
    NCK = (NH1 + IP - 1) * NUM
    IR1 = (IP + IM1 - 1) * NUM + 1
    KNODE = 0
    NODIS = 0
    XTOT = 0.
    IDC = 0
    !    MPLU=0
    IF(MPLU.EQ.1.AND.MOUT.EQ.5)WRITE(7, 2790)
    MNU = 0
    Write(12, *)
    If(mout==5)Write(12, 4432)
    4432   Format(5x, 'Nhanh', 4x, 'NuDau', 1x, 'NuCuoi', 2x, 'McDau', ' McCuoi', &
            &2x, 'SoMcat', 2x'Bien', 3x, 'Mua   ')
    MuaMax = 0
    Write(13, *)' So Nhanh song', NA
    NoBi = 0
    DO 12 N6 = 1, NA
        READ(1, '(A)')NBR
        READ(1, *)IDJ, I, J, NSEC, KTRAM, NT
        IDE = IDC + 1
        IDE1 = IDE + 1
        IDC = IDE + NSEC - 1
        KEP(L4 + N6) = I
        KEP(L5 + N6) = J
        IF(IDJ /= 0)Then
            NoBi = NoBi + 1
        ELSE
            LW = MAX0(I, J)
            MNU = MAX0(MNU, LW)
        End if
        KEP(L6 + N6) = IDE
        KEP(L7 + N6) = IDC
        KEP(L11 + N6) = KTRAM
        MuaMax = Max0(KTram, MuaMax)
        DO  K = IDE, IDC
            !     READ(1,*)TDX,XNh,HD1,PEN(NC+K),DIE(K,1),(RON(K,IK),IK=1,NT)
            READ(1, *)XX(K), YY(K), TDX, XNh, HD1, PEN(K), DIE(K, 1), (RON(K, IK), IK = 1, NT)
            DELTAX(K) = TDX ! PEN(K)=TDX      ! Chainage in Km
            PEN(K + MAM) = XNh  ! Manning*1000
            PEN(K + NC + MAM) = 0. ! D Nham
            HBot(K) = HD1       ! Hday
            ! ZHmi=PEN(K)
            DO 2 L = 2, NT
                IF(RON(K, L - 1).GT.RON(K, L))THEN
                    Write(90, 4200)(K - IDE + 1), L
                    STOP
                END IF
            2 DIE(K, L) = DIE(K, L - 1) + (RON(K, L - 1) + RON(K, L)) * DNT / 2.
        END DO
        4200 FORMAT(5X, ' ERROR, PLEASE CHECK WIDTH AT CROSS-SECTION', 2I6)
        if(mout == 5)Then
            Write(12, 4433)n6, i, j, ide, idc, nsec, idj, ktram
        End if
        4433  format(3x, 8i7)
        IF(MOUT.EQ.1)THEN
            Write(12, 4433)N6, I, J, IDE, IDC, IDJ, KTRAM
            Write(12, 850)(DELTAX(K), K = IDE1, IDC)
            850 FORMAT(6X, 'REACH LENGTH IN KM', 16F7.2)
            DO 22 K = IDE, IDC
            22 Write(12, 1300)K, PEN(K), DIE(K, 1), (RON(K, IK), IK = 1, NT)
            1300 FORMAT(2X, I4, 2X, F5.2, F8.1, 16F7.0)
            Write(12, 1950)(PEN(K + MAM), K = IDE, IDC), (PEN(K + NC + MAM), K = IDE, IDC)
            1950 FORMAT(5X, 'N,DN', 1X, 35F6.1)
        END IF
        DX0 = DELTAX(IDE)  ! PEN(IDE)
        DO 4 IN = IDE, IDC
        4   DELTAX(IN) = (DELTAX(IN) - DX0) * 1000.
        KEP(L3 + N6) = IDJ
        IF(IDJ.NE.0)THEN
            KNO = IABS(IDJ)
            KEP(KNO) = IDJ
            IF(IDJ.LT.0)NODIS = NODIS + 1
        END IF
        XTOT = XTOT + DELTAX(IDC)
        Write(90, *)' Doc Xong nhanh', N6
    12 CONTINUE
    Write(13, *)'  Doc xong Dia hinh cac nhanh song'
    IF(NC.NE.IDC)THEN
        Write(90, '(A)')' NC.NE.IDC'
        Write(90, *)IDC
        STOP
    ENDIF
    Write(12, 13)MNU
    Write(13, 13)MNU
    13 FORMAT(20X, 'Max River Junctions = ', I5)
    NUCO = MNU + 1
    IF(MCON.NE.0)Then
        Write(12, 14)NUCO, LN
        Write(13, 14)NUCO, LN
    END IF
    14 Format(20X, 'GATE NODE FROM ', I5, '  TO ', I5)
    Write(12, 1850) XTOT / 1000.
    1850 FORMAT(20X, 'TOTAL LENGTH IN KM', F12.2)
    Write(12, 1000)NODIS
    Write(12, *)
    !======================================
    IF(KN /= 0)Then    ! NB
        Write(12, *) '              NHANH BIEN:'
        Write(12, *)'        So Bo so lieu bien ', KN
        Write(12, *)'   So Luong cac nhanh bien ', NoBi
        Write(12, *)'       TTu  Nhanh   NuDau NuCuoi  McDau McCuoi  &
                & SoMcat  Bien   Mua'
        KdemB = 0
        Do Lj = 1, NA               !Lj
            IDJ = KEP(L3 + Lj)
            IF(IDJ /= 0)Then    ! Bien
                KdemB = KdemB + 1
                I = KEP(L4 + Lj)
                J = KEP(L5 + Lj)
                IDE = KEP(L6 + Lj)
                IDC = KEP(L7 + Lj)
                KMua = KEP(L11 + Lj)
                NSEC = IDC - IDE + 1
                Write(12, 4434)KdemB, Lj, i, j, ide, idc, nsec, idj, ktram
            End if               ! Bien
        End do                  ! Lj
    End if              ! NB
    4434  format(3x, 9i7)
    !=========================
    MA1 = MAM + 1
    DO 26 K = MA1, MQL
    26 PEN(K) = PEN(K) / 1000.
    !======================
    IF(NOC.NE.0)THEN     ! CONG TRINH
        Write(90, '(A)')' STRUCTURES'
        WRITE(13, *)'  Bat Dau Doc so lieu Cong Trinh'
        READ(1, '(A)')NBR
        DO K4 = 1, NOC
            NCON = NA + K4
            READ(1, *)KTT, IBC, I, J, NB, NGATE, ICON, ZD, Zmin, Amin, WD   !?????????????
            RZC = 0.38
            IF(IBC.NE.0)THEN
                KEP(IBC) = IBC
            END IF
            KEP(L4 + NCON) = I
            KEP(L5 + NCON) = J
            LW = MAX0(I, J)
            MNU = MAX0(LW, MNU)
            IF(I.EQ.KEP(L5 + NB))THEN  !Cung chieu nhanh thuong luu
                KEP(L6 + NCON) = KEP(L7 + NB)      ! Mat cat thuong luu  cong
                KEP(L7 + NCON) = 1
            ELSE                      ! Nguoc chieu nhanh thuong luu
                KEP(L6 + NCON) = KEP(L6 + NB)       ! Mat cat thuong luu cong
                KEP(L7 + NCON) = -1
            END IF
            KEP(L3 + NCON) = IBC     ! Loa cong
            KEP(L2 + K4) = ICON       ! chieu chay          !?????????
            PEN(MZD + K4) = ZD
            PEN(MRZ + K4) = RZC * WD * 11.51
            IDD = KEP(L6 + NCON)
            IF(MOUT.EQ.1)Write(12, 720)K4, I, J, IDD, WD, ZD, IBC
        END DO   ! Cong trinh
        720 FORMAT(/10X, 'STRUCTURE', I3, 2X, '(', I3, '/', I4, ')', 8X, &
                &'UP.SECTION', I4, 2X, 'WD=', F6.1, 2X, 'ZD=', F5.2, 2X, 'TYPE', I4)
    END IF
    Write(13, *)'  Doc Xong so lieu', NOC, ' cong trinh '
    Write(13, *)
    !==================================
    IF(MCON.NE.0)THEN     ! CONG NGAM
        WRITE(13, *)'*** Bat dau doc so lieu cong ngam ***'
        Write(90, *)' SO LUONG CONG NGAM ', MCON
        JNU = MNU + 1       !Nut cong ngam tu JNU
        READ(5, '(A)')NBR      ! Cong Ngam
        WRITE(6, 4447)(K, K = JNU, LN)
        4447 FORMAT('TIME DATE--CONG NODE:', 250(2X, I5))
        READ(5, '(A)')NBR      ! Dong Thong so
        IF(MOUT.EQ.5)Then
            Write(12, *)
            Write(12, *)'CONG NGAM      NUT     DF1(m)  DF2(m)  ZD1(m) ZD2(m) &
                    &  DAI(m)    D.KINH(m)'
        Endif
        MaxNuCo = 0
        DO KT = 1, MCON    !DCO
            LCON = NA + NOC + KT
            READ(5, *)kx, EMPI, I, J, DF1, DF2, ZD1, ZD2, DAI, DKIN
            MaxNuCo = Max0(MaxNuCO, I)
            MaxNuCo = Max0(MaxNuCO, J)
            KEP(L3 + LCON) = 0
            KEP(L4 + LCON) = I
            KEP(L5 + LCON) = J
            TT1(KT) = ZD1
            TT1(KT + MCON) = ZD2
            TT2(KT) = DAI
            TT2(KT + MCON) = DKIN * EMPI
            CH(I) = ZD1 + 0.05
            CH(J) = ZD2 + 0.05
            CAO(I) = DF1
            CHU(I) = DF1
            CAO(J) = DF2
            CHU(J) = DF2
            QTI(I) = 0.
            QTI(J) = 0.
            IF(MOUT.EQ.5)Then                      !1
                Write(12, 730)KT, I, J, DF1, DF2, ZD1, ZD2, DAI, DKIN
            END IF                                 !1
            730 FORMAT(5X, I5, 3X, I4, I5, 1X, 4F7.2, 3X, F7.1, 2X, F7.2)
        END DO   !DCO
        write(13, *)'       Doc xong cong ngam No', kx
    END IF            ! Cong Ngam
    Write(13, *)
    !-------------------------
    IF(MQC.NE.0)THEN  !MQC
        Kdem = 0
        READ(5, '(A)')NBR                    ! NUOC MUA VAO CONG NGAM
        Write(90, '(A)')NBR
        READ(5, '(A)')NBR
        DO KKK = 1, MQC        ! D1
            Kdem = Kdem + KKK
            READ(5, *)KTT, NUT, XD, (DITI(NUT, J), J = 1, 3), DIC, (BS(KKK, IK), IK = 1, NHA)
            Write(13, *)'Da doc thoi doan mua No:', KKK, ' Vao Nut', NUT
            KEP(L7 + NA + NOC + KKK) = NUT
            SIZ(NUT) = XD
            DO IIK = 1, NHA    ! D2
                BS(KKK, IIK) = BS(KKK, IIK) * DIC
            END DO         ! D2
            DO JJ = 1, 3        !D3
                DITI(NUT, JJ) = DITI(NUT, JJ) * 10000.
            END DO          !D3
            IF(MOUT.EQ.1)THEN  !II
                Write(12, 1600)NUT
                Write(12, 1550)(BS(KKK, IK), IK = 1, NHA)
            END IF       !! II
        END DO   !  D1
    END IF         !  MQC
    ! =========================================================
    IF(MOUT.EQ.5)THEN
        DO II = 1, LN
            LG = 0
            DO N = 1, NA + NOC + MCON
                KI = KEP(L4 + N)
                KJ = KEP(L5 + N)
                IF(KEP(L3 + N).EQ.0)THEN
                    IF(KI.EQ.II.OR.KJ.EQ.II)LG = LG + 1
                ELSE
                    IF(KI.EQ.II)LG = LG + 1
                END IF
            END DO
            IF(LG.EQ.0)THEN
                Write(90, 699)II
                699   FORMAT(2X, 'ERROR, PLEASE CHECK NODE :', I4)
                STOP
            END IF
            KEP(L16 + II) = LG
        END DO
    END IF
    !=============================
    IF(MOUT.EQ.5)THEN
        DO II = 1, LN         !  MNU  XP
            IDP = 0         ! Dem nhanh tai nut II
            DO N = 1, NA + NOC + MCON        ! **
                IDAU = KEP(L4 + N)
                ICUO = KEP(L5 + N)
                IDJ = KEP(L3 + N)
                IF(IDJ.EQ.0)THEN     !00  Nhanh trong
                    IF(IDAU.EQ.II.OR.ICUO.EQ.II)THEN   ! 1
                        IDP = IDP + 1
                        IF(N > NA + NOC)THEN  !&
                            KM = N + 5000 - NOC - NA
                        ELSE            !&
                            KM = N
                        END IF          !&
                        MANI(II, IDP) = KM   ! Chua Ten nhanh tai mot nut
                    END IF                            ! 1
                ELSE                    ! 00  Nhanh Bien
                    IF(IDAU.EQ.II)THEN        ! 22
                        IDP = IDP + 1
                        KM = N
                        MANI(II, IDP) = KM   ! Chua Ten nhanh tai mot nut
                    END IF                    ! 22
                END IF                   ! 00
            END DO      !**
            KQT(II) = IDP   ! Chua so nhanh tai mot nut
        END DO     !  MNU  XP
        Write(12, *)
        Write(12, *)'      SO DO NOI CAC NHANH SONG VA CONG TAI MOI NUT'
        Write(12, *)'--NUT    -SO NHANH-     -TEN SONG+CONG         &
                & (Ten cong them 5000)-'
        DO II = 1, LN         ! Do II
            IDP = KQT(II)   ! Dem so nhanh song tai nut !   AAAAAAAAAAAAAA
            Write(12, 708)II, IDP, (MANI(II, JJ), JJ = 1, IDP)
            708  FORMAT(1X, I5, 6X, I4, 5X, 10I6)
        END DO          ! Do II
    END IF
    !==================================================
    NUTCONG = MNU + 1
    !========================================================
    Write(13, *)
    WRITE(13, *)' SO LUONG O TRW HO = ', NRH
    If11 : IF(NRH.NE.0)THEN               ! 11   Storage Lay tu DELTA
        Write(90, '(A)')' STORAGE'
        !      WRITE(13,*)'   Doc so lieu Khu trw ho'
        Read(1, '(A)')NBR
        Loop22 : DO KR = 1, NRH               ! 22
            Read(1, *)KRH, NB, MC1, ZMIN, (A1(K), K = 1, NTR) ! Chieu rong m
            Write(90, *)'Rong trw thu', KR, ' ; Thuoc Nhanh', NB, 'NTRU=', NTR
            IDE = KEP(L6 + NB)
            IDC = KEP(L7 + NB)
            NSEN = IDC - IDE + 1
            IF(MC1.GT.NSEN)THEN         ! &&&
                Write(90, 711)KR, NB, NSEN, MC1
                711  FORMAT(2X, 'CHECK STORAGE No.', I4, '  ;Nhanh', I6, '  ;NSEC=', I4, '  ;MCAT=', I4)
                STOP
            END IF                     ! &&&
            MZ1 = IDE + MC1 - 1
            J = MZ1
            HI1 = PEN(NC + MZ1)            ! Zmin
            DO  KNT = 1, NT    ! $$
                E1(KNT) = 0.
            END DO          ! $$
            Loopxx : DO K = 1, NT                ! xx
                HI = HI1 + (K - 1) * DNT
                HREF = HI - ZMIN
                IF(HREF.GE.0.)THEN        ! %%
                    IM = HREF / DNR
                    I = IM + 1
                    IF(I.GE.NTR)THEN      ! *
                        E1(K) = A1(NTR)
                    ELSE
                        IK = I + 1
                        V3 = HREF / DNR - IM
                        E1(K) = A1(I) + (A1(IK) - A1(I)) * V3
                    END IF                ! *
                    RON(MZ1, K) = RON(MZ1, K) + E1(K)
                ENDIF                     ! %%
            END DO  Loopxx                 ! xx
        END DO Loop22             !  22
        Write(90, *)' Doc xong', NRH, ' ruong ho'
    END IF If11        !  11
    !===========================================================
    Write(13, *)
    WRITE(13, *)' SO LUONG RUONG KIN = ', NRK
    IF(NRK.NE.0)THEN  ! RUONG KIN
        Write(90, '(A)')' PLAINS'
        WRITE(13, *)'  Doc so lieu cac khu trw kin PLAINS'
        READ(1, '(A)')NBR
        DO KR = 1, NRK
            READ(1, '(A)')NBR
            KR1 = KR - 1
            READ(1, *)HC, NORU, KTRA, H(NC + KR), ZDA, (FRU(KR, K), K = 1, NTR)
            KEP(L8 + KR) = NORU
            PEN(NC + KR) = ZDA
            KEP(L11 + NA + KR) = KTRA
            DD = 100. * HC * SQRT(0.5 * (FRU(KR, 1) + FRU(KR, NTR)))
            DD = SQRT(DD)
            DO KK = 1, NORU
                READ(1, *)NNO, LLR, NB, MC1, MC2, ZNG, WD, ZD2, WD2, ZD3, WD3  ! Them
                ! LLR=0 : Song-Ruong ; LLR=1 : Ruong-Ruong
                HEF = 0.4  !!!!!!!!!!!!!!!!!
                Write(90, *)'Ruong Kin noi nhanh ', NB
                IDE = KEP(L6 + NB)
                MZ1 = IDE + MC1 - 1
                MZ2 = IDE + MC2 - 1
                KEP(L6 + NA + NOC + MAXNOI * KR1 + KK) = MZ1
                KEP(L7 + NA + NOC + MAXNOI * KR1 + KK) = MZ2
                PEN(MZD + NOC + MAXNOI * KR1 + KK) = ZNG
                PEN(MRZ + NOC + MAXNOI * KR1 + KK) = HEF * WD * 11.51 / DD
                FEN(LFAL + MAXNOI * KR1 + KK) = WD2 / WD
                FEN(LFZD + MAXNOI * KR1 + KK) = ZD2
                PQR(KR, KK) = 0.
            END DO
            IF(MOUT.EQ.1)THEN
                Write(12, 2600)KR, NORU, ZDA, KTRA
                Write(12, 2950)(FRU(KR, K), K = 1, NTR)
            END IF
        END DO
        Write(13, *)'  Doc xong so lieu cac khu trw kin Plains'
    END IF  ! Ruong Kin
    Write(13, *)
    !===========================================================
    IF(NSM.NE.0)THEN            ! MUA
        SSS = 11.57E-9  !: Daily Rainfall; SSS=2.777778E-7: Hourly
        Write(13, *) ' So lieu cua ', NSM, 'Tram Mua-So thoi doan mua', NTD
        READ(2, '(A)')NBR
        !	    WRITE(13,'(A)')NBR
        DO 24 IK = 1, NSM
            READ(2, *)NBR
            Write(90, *)NBR
            PEN(MHC + IK) = 0.0001
            READ(2, *)(PSM(IK, JJP), JJP = 1, NTD)
            Write(90, *)'Doc Xong Tram Mua No', IK
            DO  JP = 1, NTD
                PSM(IK, JP) = PSM(IK, JP) * SSS
            END DO
        24    Continue
        Write(13, *)'  Xong so lieu', NSM, ' Tram Mua'
        Write(13, *)
    END IF   !Mua
    Write(90, '(A)')'  Q.LATERAL'
    READ(2, '(A)')NBR    ! Ten
    READ(2, *)KQL
    IF(KQL.NE.0)THEN    !KQL      ! LUU LUONG GIA NHAP
        DO KK = 1, KQL
            READ(2, *)KT, NBB, JMC, QQL
            Write(90, *)NBB
            IKK = KEP(L6 + NBB) + JMC + 1
            PEN(MQL + IKK) = -QQL
        END DO
    END IF   ! KQL
    READ(2, *)KQHC
    READ(2, *)Nhuq, Ngia
    READ(2, *)NXaK
    READ(2, *)NXaT
    Write(13, *)' Doc xong so lieu QLateral'
    !----------------------------------------------------
    Write(13, *)
    READ(2, '(A)')NBR         !  BIEN  THUY LUC
    Write(90, *)' DOC BIEN THUY LUC'
    DO K = 1, KN
        Write(90, *)' DOC BIEN THUY LUC No', K
        READ(2, '(A)')NBR
        READ(2, *) JCODE, E1(K)
        READ(2, *)(BO(K, IK), IK = 1, NH)
        DO 42 IK = 1, NH
        42 BO(K, IK) = BO(K, IK) * JCODE + E1(K)
        IF(KEP(K).GE.0)THEN
            DO 54 IK = 1, NH
            54 BO(K, IK) = BO(K, IK) / 100.
            E1(K) = E1(K) / 100.
        END IF
        IF(MOUT.EQ.1)THEN
            Write(12, 1500) NBR, E1(K)
            IF(KEP(K).LT.0)THEN
                Write(12, 1570) (BO(K, IK), IK = 1, NH)
            ELSE
                Write(12, 1550) (BO(K, IK), IK = 1, NH)
            END IF
        END IF
    END DO
    Write(13, *)' Doc xong Bien Thuy luc '
    IF(JHQS.NE.0)THEN        ! DKD
        Write(90, '(A)') ' PREVIOUS RESULT?'
        READ(27, *)
        READ(27, '(A)')DKDau
        OPEN(8, FILE = DKDau, Status = 'OLD')
        READ(8, *)
        READ(8, *)(H(K), K = 1, NC)
        READ(8, *)(Q(K), K = 1, NC)
        IF(NRK.NE.0)THEN   ! NRK
            Read(8, *)
            READ(8, *)(H(K + NC), K = 1, NRK)
            READ(8, *)((PQR(K4, K5), K5 = 1, MAXNOI), K4 = 1, NRK)
        END IF       ! NRK
        Read(8, *)
        IF(MCON.NE.0)READ(8, *)(CH(K), K = 1, LN)
    ELSE    ! DKD
        DO 130 I = 1, NC
        130 Q(I) = 0.
        READ(2, *)(H(I), I = 1, NC)
        IF(MCON.NE.0)THEN  ! MCO
            DO KT = 1, MCON
                CH(KT) = TT1(KT) + 0.03 !  Muc nuoc ban dau tai moi nut cong
            END DO
        END IF             ! MCO
    END IF     ! DKD                         ! CUR
    CALL BAN(NC, NT, DNT, H, PEN, DIE, RON, A1, B2, E1, NRK)
    DO I = 1, NC
        IF(A1(I).LT.0.05)THEN
            PEN(MU + I) = 0.
        ELSE
            PEN(MU + I) = Q(I) / A1(I)
        END IF
        UMAX(I) = ABS(PEN(MU + I))
    END DO
    IF(NOC.NE.0)THEN
        DO ID = 1, NA
            I = KEP(L4 + ID)
            IDE = KEP(L6 + ID)
            R(LN + MCON + I) = H(IDE)
            IF(KEP(L3 + ID).EQ.0)THEN
                J = KEP(L5 + ID)
                IDC = KEP(L7 + ID)
                R(LN + J) = H(IDC)
            END IF
        END DO
    ENDIF
    DO I = 1, NC
        HH(I) = H(I)
    END DO
    !========================================
    NPP = (NH1 - IM1) * NUM + 1
    DO I = 1, NC
        HMA(I) = -100.   !H(I)
        HMI(I) = 100.    !  H(I)
        HAV(I) = 0.   !H(I)/NPP
    End do
    !=====================================
    IF(NRK == 0)Goto 3345
    DO KR = 1, NRK
        HPL = H(NC + KR)
        ZMI = PEN(2 * NC + KR)
        IF(HPL.GT.ZMI)THEN
            HD = HPL - ZMI
            IM = HD / DNR
            I = IM + 1
            IF(I.GT.NTR)THEN
                VV1 = FRU(KR, NTR)
                VOL(KR) = HD * (VV1 + FRU(KR, 1)) / 2.
            ELSE
                V3 = HD / DNR - IM
                VV1 = FRU(KR, I) + (FRU(KR, I + 1) - FRU(KR, I)) * V3
                VOL(KR) = HD * (VV1 + FRU(KR, 1)) / 2.
            END IF
        ELSE
            VOL(KR) = 0.
        END IF
    END DO
    DO K = 1, LN
        WLNgap(K) = 0.
    End do
    3345  Continue
    1000 FORMAT(20X, 'DIS.BOUN NODE =', I6)
    1450 FORMAT(5X, I3, 14F8.4)
    1500 FORMAT(15X, 'HYD.CONDITION AT ', A20, 5X, 'REGULATED', F9.2/)
    1550 FORMAT(12F9.2)
    1570 FORMAT(12F9.1)
    1600 FORMAT(30X, 'Q MUA TAI NUT', I5/)
    1750 FORMAT(5X, 15F6.0)
    1900 FORMAT(/7X, 20(1X, 'MC', I3))
    2150 FORMAT(8X, 14F8.0)
    2490 FORMAT(5X, 'BRACH', I4)
    2500 FORMAT(5X, I4, 2X, 14F7.0)
    2600 FORMAT(5X, 'PLAIN N0', I3, 2X, 'LINKS:', I3, 2X, 'ZBOTTOM', F5.2, &
            & 2X, 'RAIN STATION', I4)
    2700 FORMAT(10X, 'RAIN ST.', I4, 2X, 16F6.1)
    2790 FORMAT(6X, 'BRANCH', 5X, 'I', 5X, 'J', '  IDAU', ' ICUOI  NSEC   KTRAM')
    2800 FORMAT(6X, 8I6)
    2950 FORMAT(12F10.0)
    3000 FORMAT(15X, 'Q LATERAL AT BRANCH', I5, 3X, F9.2, /)
    3050 FORMAT(15X, 'ONE STEP = ', I5, 2X, 'HOUR(S)', 3X, 'COMP.STEPS=', I6)
    3100 FORMAT(10X, 'SIMULATION STARTED FROM', I3, 'H/', I4, '/', I3, '/', I5)
    4000 FORMAT(5X, 'ERROR, DX = 0 IN BRANCH ', I3)
    6666 FORMAT(200F10.2)
END SUBROUTINE
!     ********************************************************
SUBROUTINE XEP(KEP, IBAN, NCOL, KAX, LN, L3, L4, L5)
    !     ********************************************************
    COMMON/LT/L15, L16, L17, NAC, MBAN
    DIMENSION KEP(*), IBAN(*), NCOL(LN, *), IB(360), KAX(NAC, 4)
    NBAN = 1
    DO I = 1, LN
        M = 1
        IB(1) = I
        DO 240 N = 1, NAC
            IF(KEP(L3 + N).NE.0)GOTO 240
            KI = KEP(L4 + N)
            KJ = KEP(L5 + N)
            IF(KI.EQ.I)GOTO 230
            IF(KJ.EQ.I)GOTO 220
            GOTO 240
            220  KJ = KI
            230          DO  L = 1, M
                IF(IB(L).EQ.KJ)GOTO 240
            END DO
            M = M + 1
            IB(M) = KJ
        240  CONTINUE
        IBAN(I) = M
        NBAN = MAX0(NBAN, M)
        M1 = M - 1
        !  **** XEP LAI MANG IB ****
        DO L = M1, 1, -1
            DO J = 1, L
                KB = IB(J)
                IF(KB.GT.IB(J + 1))THEN
                    IB(J) = IB(J + 1)
                    IB(J + 1) = KB
                END IF
            END DO
        END DO
        DO J1 = 1, M
            !	Write(2,999)M,I,J1
            !  999 Format(5x,'M,I,J1',3I10)
            NCOL(I, J1) = IB(J1)
        END DO
    END DO
    ! **** TAO MANG CHI SO ****
    DO N = 1, NAC
        IN = KEP(L4 + N)
        M = IBAN(IN)
        DO K1 = 1, M
            KL1 = NCOL(IN, K1)
            IF(IN.EQ.KL1)GOTO 262
        END DO
        262  KAX(N, 1) = K1
        IF(KEP(L3 + N).EQ.0)THEN
            JN = KEP(L5 + N)
            DO K2 = 1, M
                KL2 = NCOL(IN, K2)
                IF(JN.EQ.KL2)GOTO 266
            END DO
            266  KAX(N, 2) = K2
            M = IBAN(JN)
            DO K3 = 1, M
                KL3 = NCOL(JN, K3)
                IF(JN.EQ.KL3)GOTO 270
            END DO
            270  KAX(N, 3) = K3
            DO  K4 = 1, M
                IF(IN.EQ.NCOL(JN, K4))GOTO 274
            END DO
            274  KAX(N, 4) = K4
        END IF
    END DO
    IF(NBAN.GT.7)THEN
        Write(90, 300)NBAN
    ENDIF
    MBAN = NBAN + 25
    300  FORMAT(10X, 'WARNING MBAN=', I6)
    RETURN
END
!      *****************************************************DCOT7
SUBROUTINE HESO(I1, I2, A1, E1, B2, D2, E2, H, Q, DT, MQQ, PEN, DELTAX)
    !      -----------------------------------------------------------
    Real :: A1(*), E1(*), B2(*), D2(*), E2(*), DELTAX(*)
    REAL :: H(*), Q(*), PEN(*)
    REAL :: TETA = 0.66667, G = 9.81
    INTEGER :: Mu = 4
    !------------------------------------
    I3 = I2 - 1
    DO I = I1, I3
        J2 = I + 1
        HSUM = H(J2) + H(I)
        HDIF = H(J2) - H(I)
        DX = DELTAX(J2) - DELTAX(I) ! DX=PEN(J2)-PEN(I)
        BSUM = B2(I) + B2(J2)
        BST = BSUM / 2.
        !   if(bsum.lt.0.1)bsum=0.5
        ASUM = A1(I) + A1(J2)
        AST = ASUM / 2.
        !       if(asum.lt.0.1)asum=0.5
        QSUM = Q(I) + Q(J2)
        QST = QSUM / 2.
        QDIF = Q(J2) - Q(I)
        AMS = E1(I) + E1(J2)
        FR1 = BST * (QST**2) / G
        FORUT = FR1 / (AST**3)
        XIG = 1 - (FORUT**Mu)
        IF(FORUT > 1.)XIG = 0.
        F1 = DX / DT
        F6 = G * ASUM
        F22 = DX * AMS * AMS * ABS(QSUM) * (BSUM**1.333)
        F2 = F22 / (4. * (ASUM**3.333))
        F3 = (TETA - 1.) * HDIF - 4. * QSUM * QDIF / (F6 * ASUM) - F2 * QSUM
        F4 = XIG * F1 / F6
        F5 = F4 + 2. * TETA * F2
        A1(I) = BSUM * F1 / 4.
        E1(I) = A1(I) * HSUM - (1. - TETA) * QDIF + PEN(J2 + MQQ)
        B2(I) = F5 - (8 * XIG * TETA * Q(I) / F6) / ASUM
        D2(I) = F5 + (8 * XIG * TETA * Q(J2) / F6) / ASUM
        E2(I) = F3 + B2(I) * Q(I) + D2(I) * Q(J2)
    END DO
END SUBROUTINE
!     *********************************************
SUBROUTINE SWEEP(I1, I2, P1, Q1, R1, T, V, Z, Y, A1, E1, B2, D2, E2)
    !    *********************************************
    DIMENSION P1(*), Q1(*), R1(*), T(*), V(*), Z(*), &
            & A1(*), E1(*), B2(*), D2(*), E2(*), Y(*)
    TETA = 0.66667
    K2 = I2 - 1
    P1(I1) = 0.
    Q1(I1) = 1.
    R1(I1) = 0.
    DO 1 J = I1, K2
        F = A1(J) * P1(J) - TETA
        JC = J + 1
        T(JC) = -TETA / F
        V(JC) = -A1(J) / F
        Z(JC) = -A1(J) * Q1(J) / F
        Y(JC) = (E1(J) - A1(J) * R1(J)) / F
        DET = A1(J) * B2(J) - TETA * TETA
        FZ = 2. * A1(J) * TETA / DET + V(JC)
        P1(JC) = -((D2(J) * A1(J) + TETA * TETA) / DET + T(JC)) / FZ
        Q1(JC) = -Z(JC) / FZ
        R1(JC) = ((A1(J) * E2(J) + E1(J) * TETA) / DET - Y(JC)) / FZ
    1 CONTINUE
    PN = 0.
    QN = 1.
    RN = 0.
    DO 2 JF = I1, K2
        J = I2 - JF + I1 - 1
        DET1 = A1(J) * D2(J) - TETA * TETA
        VA4 = 2. * A1(J) * TETA / DET1
        VA5 = -(A1(J) * B2(J) + TETA * TETA) / DET1
        VA6 = (A1(J) * E2(J) - TETA * E1(J)) / DET1
        FI = TETA + A1(J) * PN
        TN = TETA / FI
        VN = -A1(J) / FI
        ZN = -A1(J) * QN / FI
        SN = (E1(J) - A1(J) * RN) / FI
        GF = VA4 - VN
        PN = (TN - VA5) / GF
        QN = ZN / GF
        RN = (SN - VA6) / GF
    2 CONTINUE
    P1(I1) = PN
    Q1(I1) = QN
    R1(I1) = RN
END SUBROUTINE
!     ************************************************CO1018
SUBROUTINE BAN(NC, N, DNT, HJ, PEN, DIE, RON, AI, BI, AM, NRK)
    !     ****************************************************
    DIMENSION AI(*), BI(*), AM(*), HJ(*), PEN(*), DIE(NC, *), RON(NC, *)
    K1 = N - 1
    LJ = N / 2
    DO  K = 1, NC
        DN = 0.  ! PEN(2*NC+NRK+K)
        XNh = PEN(NC + NRK + K)
        RFF = PEN(K)
        IF(HJ(K).LT.RFF - 0.05)HJ(K) = RFF - 0.05
        HRE = HJ(K) - RFF
        IM = HRE / DNT
        I = IM + 1
        IF(I.LE.1) THEN
            V1 = 1. - (HRE / DNT)
            BI(K) = RON(K, 2) - (RON(K, 2) - RON(K, 1)) * V1
            AI(K) = DIE(K, 1) + (BI(K) + RON(K, 1)) * HRE / 2.
            AM(K) = XNh
        ELSE IF(I.GE.N) THEN
            BI(K) = RON(K, N)
            XX = BI(K) / (RON(K, LJ) + 1.)
            AI(K) = DIE(K, N) + BI(K) * (HRE - K1 * DNT)
            AM(K) = XNh + XX * DN
        ELSE
            IK = I + 1
            V3 = HRE / DNT - IM
            BBV = RON(K, I) + (RON(K, IK) - RON(K, I)) * V3
            BI(K) = BBV
            CCC = RON(K, LJ) + 1.
            XX = BBV / CCC
            AI(K) = DIE(K, I) + (BI(K) + RON(K, IK)) * V3 * DNT / 2.
            AM(K) = XNh + (I - 2) * DN + (V3 + XX) * DN
        END IF
        IF(AI(K).LT.0..OR.BI(K).LT.0.)THEN
            WRITE(13, *)
            WRITE(13, 80)K, RFF, HJ(K), AI(K), BI(K)
            Write(90, 80)K, RFF, HJ(K), AI(K), BI(K)
            AI(K) = DIE(K, 1) * 0.85
            BI(K) = RON(K, 1) * 0.85
            AM(K) = 100.
            HJ(K) = RFF - 0.05
        END IF
    END DO
    80 FORMAT(5X, 'DRY BED-MCat-ZMin-H-A-B=  ', I4, 2F6.2, 2F8.0)
END SUBROUTINE
!     ****************************************************************
SUBROUTINE COMHQ(H, Q, A3, B3, C3, D3, PN, QN, RN, A1, E1, B2, D2, E2, AW, &
        &DIE, RON, BO, PSM, FRU, R, PEN, KEP, IBAN, JBAN, NCOL, JCOL, KAX, PQR, FEN  &
        &, VV, BS, TT1, TT2, CH, CAO, DITI, HMa, Hav, Hmi, QDu, Qam, WLNgap, DELTAX, SOGIO)
    !     ****************************************************************
    COMMON/S9/ QTI(5000), CHU(5000), SIZ(5000)
    COMMON/DAT/ITO, ING, ITH, IYE, KKS, JKT, DELT
    COMMON/DIMK/L1, L2, L3, L4, L5, L6, L7, L8, L10, L11, L12
    COMMON/KPR/ITER, MOA, IP, NUM, NCK, IR1, DT, DNT, &
            &       INDIS, JHQS, MDT, DNR, MPLU, MBOC, IM1
    COMMON/ONE/NA, NT, NH, NOC, MCON, NES, NC, KN, LN, MQC, &
            &  NH1, NSM, NTD, NRK, NTR, NNC, NRH, NSQ, KQL, MAXNOI, JNU, NHA, DCO
    COMMON/DIMP/MAM, MQL, NAP, MZD, MRZ, MHC, LFAL, LFZD, MQR, &
            &    MQQ, MFQ, MFS, MFR, MVL, LPS, MU, MQA, MQT, LQL, LQRU
    COMMON/LT/L15, L16, L17, NAC, MBAN
    COMMON/SS /PTO(5000), PTN(5000), NUCO
    COMMON/PRIN/ INHQ(400), INHRU(400), INQRU(400)
    INTEGER KEP(*), IBAN(LN), JBAN(LN), NCOL(LN, *), JCOL(LN, *), KAX(NAC, 4)
    REAL H(*), Q(*), A3(*), B3(*), C3(*), D3(*), PN(*), QN(*), RN(*), &
            &A1(*), E1(*), B2(*), D2(*), AW(LN, *), FRU(NRK, NTR), E2(*), &
            &DIE(NC, *), RON(NC, *), BO(KN, NH), PSM(NSM, *), TT2(*), &
            &R(*), PEN(*), PQR(NRK, *), FEN(*), VV(*), DITI(LN, 3), &
            &BS(MQC, *), TT1(*), CH(*), CAO(*), HMa(*), Hav(*), Hmi(*)
    Real QDU(*), QAm(*), WLNgap(*), DELTAX(*), SOGIO(*)
    coef = 1.11
    DO IN = 1, LN
        R(IN) = 0.
        DO 5 IM = 1, MBAN
        5 AW(IN, IM) = 0.
    END DO
    IK = ITER / NUM
    IP = IK + 1
    MR = IP
    MOA = ITER - IK * NUM
    IF(NSM.NE.0)THEN   ! Tinh Q mua
        MRA = 1 + MR * MBOC / (24 * MDT)
        DETA = 3.078 / NUM
        DO 10 J = 1, NSM
            PP = PSM(J, MRA)
            PHC = PEN(MHC + J)
            PCC = DETA * (PP - PHC) + PHC**0.6666
            IF(PCC.LT.0.)PCC = 0.
        10 PEN(MHC + J) = PCC**1.5
    END IF
    DO 15 I = 1, LN
        IBAN(I) = JBAN(I)
        DO 15 K = 1, MBAN
        15 NCOL(I, K) = JCOL(I, K)
    DO 20 I = 1, NC
        IF(KQL.NE.0.and.ITER.LE.NHA)THEN
            PEN(MQQ + I) = PEN(MQL + I)
        ELSE
            PEN(MQQ + I) = 0.
        END IF
    20   CONTINUE
    !     *****WATER LEVELS AT INTERNAL NODES*****
    DO 40 ID = 1, NA
        N1 = KEP(L6 + ID)
        N21 = KEP(L7 + ID)
        N2 = N21 - 1
        N11 = N1 + 1
        INO = KEP(L4 + ID)
        JNO = KEP(L5 + ID)
        IJ1 = KAX(ID, 1)
        IJ2 = KAX(ID, 2)
        IJ3 = KAX(ID, 3)
        IJ4 = KAX(ID, 4)
        IBII = KEP(L3 + ID)
        KTR = KEP(L11 + ID)
        IF(KTR.NE.0)THEN
            PP = PSM(KTR, MRA)
            DO 30 IL = N1, N2
                JJ = IL + 1
                BSR = (RON(IL, NT) + RON(JJ, NT)) / 2.
                DEX = PEN(JJ) - PEN(IL)
                PEN(MQR + JJ) = BSR * PP * DEX
            30 PEN(MQQ + JJ) = PEN(MQR + JJ) + PEN(MQQ + JJ)
        END IF
        IF(NRK.NE.0)THEN
            DO 35 IL = N11, N21
            35 PEN(MQQ + IL) = PEN(MQQ + IL) + PEN(MQL + NC + IL)
        END IF
        CALL HESO(N1, N21, A1, E1, B2, D2, E2, H, Q, DT, MQQ, PEN, DELTAX)
        IF(IBII.EQ.0)THEN
            CALL SWEEP(N1, N21, PN, QN, RN, A3, B3, C3, D3, A1, E1, B2, D2, E2)
            AW(JNO, IJ3) = AW(JNO, IJ3) + 1. / PN(N21)
            AW(JNO, IJ4) = AW(JNO, IJ4) - QN(N21) / PN(N21)
            R(JNO) = R(JNO) + RN(N21) / PN(N21)
            AW(INO, IJ1) = AW(INO, IJ1) - 1. / PN(N1)
            AW(INO, IJ2) = AW(INO, IJ2) + QN(N1) / PN(N1)
            R(INO) = R(INO) - RN(N1) / PN(N1)
        ELSE
            K = IABS(IBII)
            IF(MOA.NE.0)THEN
                BU = BO(K, MR) + (BO(K, MR + 1) - BO(K, MR)) * MOA / NUM
            ELSE
                BU = BO(K, MR)
            END IF
            IF(IBII.GT.0)THEN
                CALL SWEEP(N1, N21, PN, QN, RN, A3, B3, C3, D3, A1, E1, B2, D2, E2)
                AW(INO, IJ1) = AW(INO, IJ1) - 1. / PN(N1)
                R(INO) = R(INO) - (RN(N1) + QN(N1) * BU) / PN(N1)
                H(N21) = BU
            ELSE
                CALL SWEEP(N1, N21, PN, QN, RN, A3, B3, C3, D3, A1, E1, B2, D2, E2)
                AW(INO, IJ1) = AW(INO, IJ1) + (QN(N1) * QN(N21) - 1.) / PN(N1)
                R(INO) = R(INO) - (RN(N1) + QN(N1) * (RN(N21) + PN(N21) * BU)) / PN(N1)
                Q(N21) = BU
            END IF
        END IF
    40 CONTINUE
    !                      *** GATE , DAM ***
    IF(NOC.NE.0)THEN
        DO 60 ID = 1, NOC
            INA = ID + NA
            IN = KEP(L4 + INA)
            JN = KEP(L5 + INA)
            H1 = R(LN + MCON + IN) - ZD
            H2 = R(LN + MCON + JN) - ZD
            AP = 0.55 * H1 + 0.45 * H2
            MChieu = KEP(L2 + ID)
            IF(MCHieu  == -5)GOTO 60
            IF(AP.GT.0.1)THEN
                ZD = PEN(MZD + ID)
                IJ1 = KAX(INA, 1)
                IJ2 = KAX(INA, 2)
                IJ3 = KAX(INA, 3)
                IJ4 = KAX(INA, 4)
                HES = PEN(MRZ + ID)
                H1 = R(LN + MCON + IN) - ZD
                IF(KEP(L3 + INA).EQ.0)THEN  !1
                    H2 = R(LN + MCON + JN) - ZD
                ELSE                      !1
                    K = KEP(L3 + INA)
                    IF(MOA.NE.0)THEN           !2
                        Z11 = (BO(K, MR + 1) - BO(K, MR)) / NUM
                        Z22 = BO(K, MR) + Z11 * MOA
                        HH = Z22 - Z11
                    ELSE                       !2
                        Z22 = BO(K, MR)
                        JH = MR - 1
                        IF(JH.EQ.0)JH = 1
                        Z11 = (BO(K, MR) - BO(K, JH)) / NUM
                        HH = Z22 - Z11
                        H2 = HH - ZD
                    END IF                     !2
                END IF                    !1
                HAF = H1 - H2
                IF(KEP(L2 + ID))45, 55, 50
                45  IF(H2.GT.H1 - 0.02) GOTO 60
                GOTO 55
                50  IF(H1.GT.H2 - 0.02) GOTO 60
                55       IF(HAF.LT.0.)THEN      !3
                    HMAX = H2
                    HMIN = H1
                    DAA = -1.
                ELSE IF(HAF.GT.0.)THEN !3
                    HMAX = H1
                    HMIN = H2
                    DAA = 1.
                ELSE                  !3
                    GOTO 60
                END IF                !3
                IF(HMAX.GT.0.02)THEN
                    HTEST = 0.6667 * HMAX
                    IF(HMIN.GT.HTEST)THEN
                        !          1***   SUBMERGED FLOW  ****************
                        IF(AP.GT.HMIN)THEN
                            !         11*** GATES OPEN-SUBMERGED WEIR FLOW ***
                            FJ = HES * ABS(HMIN)
                            QK = FJ * SQRT(HMAX - HMIN)
                            F1 = FJ * FJ / QK
                            IF(KEP(L3 + INA).EQ.0)THEN
                                AW(IN, IJ1) = AW(IN, IJ1) - F1
                                AW(IN, IJ2) = AW(IN, IJ2) + F1
                                AW(JN, IJ4) = AW(JN, IJ4) + F1
                                AW(JN, IJ3) = AW(JN, IJ3) - F1
                            ELSE
                                AW(IN, IJ1) = AW(IN, IJ1) - F1
                                R(IN) = R(IN) - F1 * Z22
                            END IF
                        ELSE
                            !        12** FLOW INFLUENCED BY GATES OPENING **
                            FJ = HES * ABS(AP)
                            QK = FJ * SQRT(HMAX - AP)
                            F1 = FJ * FJ / QK
                            IF(KEP(L3 + INA).EQ.0)THEN
                                !	Write(12,*)'IN,IJ1',IN,IJ1
                                AW(IN, IJ1) = AW(IN, IJ1) - F1
                                AW(IN, IJ2) = AW(IN, IJ2) + F1
                                AW(JN, IJ4) = AW(JN, IJ4) + F1
                                AW(JN, IJ3) = AW(JN, IJ3) - F1
                            ELSE
                                AW(IN, IJ1) = AW(IN, IJ1) - F1
                                R(IN) = R(IN) - F1 * Z22
                            END IF
                        END IF
                        !                                 2*** FREE FLOW ***
                    ELSE
                        IF(AP.GT.HTEST)THEN
                            !       21** GATES OPEN COMPLETELY FREE FLOW OVER WEIR **
                            F1 = .385 * HES * SQRT(HMAX)
                            F2 = F1 * ZD
                            IF(KEP(L3 + INA).EQ.0)THEN
                                R(IN) = R(IN) - DAA * F2
                                R(JN) = R(JN) + DAA * F2
                                IF(DAA.GT.0.)THEN
                                    AW(IN, IJ1) = AW(IN, IJ1) - F1
                                    AW(JN, IJ4) = AW(JN, IJ4) + F1
                                ELSE
                                    AW(IN, IJ2) = AW(IN, IJ2) + F1
                                    AW(JN, IJ3) = AW(JN, IJ3) - F1
                                END IF
                            ELSE
                                IF(DAA.GT.0.)THEN
                                    AW(IN, IJ1) = AW(IN, IJ1) - F1
                                    R(IN) = R(IN) - F2
                                ELSE
                                    R(IN) = R(IN) + F1 * (ZD - Z22)
                                END IF
                            END IF
                            !           22** FLOW INFLUENCED BY GATES BUT FREE **
                        ELSE
                            FS = HES * AP
                            QK = FS * SQRT(HMAX - AP)
                            F1 = FS * FS / QK
                            F2 = F1 * (ZD + AP)
                            ZD = ZD + AP
                            IF(KEP(L3 + INA).EQ.0)THEN
                                R(IN) = R(IN) - DAA * F2
                                R(JN) = R(JN) + DAA * F2
                                IF(DAA.GT.0.)THEN
                                    AW(IN, IJ1) = AW(IN, IJ1) - F1
                                    AW(JN, IJ4) = AW(JN, IJ4) + F1
                                ELSE
                                    AW(IN, IJ2) = AW(IN, IJ2) + F1
                                    AW(JN, IJ3) = AW(JN, IJ3) - F1
                                END IF
                            ELSE
                                IF(DAA.GT.0.)THEN
                                    AW(IN, IJ1) = AW(IN, IJ1) - F1
                                    R(IN) = R(IN) - F2
                                ELSE
                                    R(IN) = R(IN) + F1 * (ZD - Z22)
                                END IF
                            END IF
                        END IF
                    END IF
                END IF
            END IF
        60    CONTINUE
    END IF
    !====================================== CONG NGAM
    IF(MQC.NE.0)THEN
        DO L = 1, MQC      ! TINH NUOC TIEU
            NUT = KEP(L7 + NA + NOC + L)
            DC = SIZ(NUT)
            ZMIN = CAO(NUT)
            HMD = CHU(NUT)
            IM = (HMD - ZMIN) / DCO
            I = IM + 1
            IF(HMD.LT.ZMIN)THEN
                FCH = 1.
            ELSE IF(HMD.GE.2. * DCO + ZMIN)THEN
                FCH = DITI(NUT, 3)
            ELSE
                IK = I + 1
                V3 = (HMD - ZMIN) / DCO - IM
                FCH = DITI(NUT, I) + (DITI(NUT, IK) - DITI(NUT, I)) * V3
            END IF
            IF(FCH==0.)FCH = 1.    !!! Them
            DD1 = DT / FCH
            IF(ITER.LE.NHA)THEN
                BU = BS(L, ITER)
            ELSE
                BU = 0.
            END IF
            HTC = CH(NUT)
            IF(HTC.LE.ZMIN)HTC = ZMIN
            VG = coef * 7.09 * DC * SQRT(ABS(HMD - HTC))  ! Coef=1.11
            IF(HMD.LT.HTC)VG = -VG
            RRU = HMD + (BU - VG) * DD1
            IF(RRU.LE.ZMIN)RRU = ZMIN
            CHU(NUT) = RRU
            TUI = coef * 7.09 * DC * SQRT(ABS(RRU - HTC))  ! Coef=1.11
            IF(RRU.LT.HTC)TUI = -TUI
            QTI(NUT) = TUI
        END DO
    END IF
    !=============================================================
    IF(MCON.NE.0)THEN        ! TINH CONG NGAM  IF 0
        DO 652 ID = 1, MCON         ! CHAY TUNG CONG
            INA = ID + NA + NOC
            ZD1 = TT1(ID)
            ZD2 = TT1(ID + MCON)
            DAI = TT2(ID)
            DKI = TT2(MCON + ID)
            IN = KEP(L4 + INA)
            JN = KEP(L5 + INA)
            IJ1 = KAX(INA, 1)
            IJ2 = KAX(INA, 2)
            IJ3 = KAX(INA, 3)
            IJ4 = KAX(INA, 4)
            H1 = CH(IN)
            H2 = CH(JN)
            HAF = H1 - H2
            IF(HAF.GE.0.)THEN
                HMAX = H1 - ZD1
                HMIN = H2 - ZD2
            ELSE
                HMAX = H2 - ZD2
                HMIN = H1 - ZD1
            END IF
            !   ************ BAT DAU TINH--CONG
            IF(HMAX.GT.1.15 * DKI)THEN           ! X1
                IF(HMIN.GE.DKI)THEN          ! XX1
                    HE = 1. / SQRT(DAI / (45. * DKI) + 0.25)
                    HE1 = HE * 3.14159 * DKI * DKI * 4.429 / 4.
                    HDIF = CH(IN) - CH(JN)
                    HCFS = ABS(HDIF)
                    XX = SQRT(HCFS)
                    QKK = HE1 * XX + 0.0001
                    AA = 0.5 * HE1 * HE1 / QKK
                    AW(IN, IJ1) = AW(IN, IJ1) - AA
                    AW(IN, IJ2) = AW(IN, IJ2) + AA
                    AW(JN, IJ4) = AW(JN, IJ4) + AA
                    AW(JN, IJ3) = AW(JN, IJ3) - AA
                    R(IN) = R(IN) + AA * HDIF
                    R(JN) = R(JN) - AA * HDIF
                ELSE IF(HMIN.GT.0.1 * DKI.AND.HMIN.LT.DKI)THEN   !XX1
                    IF(HAF.GE.0.)THEN     !XX11
                        HE = 1. / SQRT(1. + DAI / (45. * DKI) + 0.55)
                        AR2 = 3.1416 * DKI * DKI / 8. + (HMIN - DKI / 2.) * SQRT(DKI * HMIN - HMIN**2)
                        HE1 = HE * AR2 * 4.429
                        HCFS = ABS(CH(IN) - ZD2 - 0.85 * DKI)
                        XX = SQRT(HCFS)
                        QKK = HE1 * XX + 0001
                        XIP = 0.85 * DKI + ZD2
                        AA = 0.5 * HE1 * HE1 / QKK
                        AW(IN, IJ1) = AW(IN, IJ1) - AA
                        AW(JN, IJ4) = AW(JN, IJ4) + AA
                        R(IN) = R(IN) + AA * (CH(IN) - 2. * XIP)
                        R(JN) = R(JN) - AA * (CH(IN) - 2. * XIP)
                    ELSE IF(HAF.LT.0.)THEN                  ! XX11
                        HE = 1. / SQRT(1. + DAI / (45. * DKI) + 0.55)
                        AR2 = 3.1416 * DKI * DKI / 8. + (HMIN - DKI / 2.) * SQRT(DKI * HMIN - HMIN**2)
                        HE1 = HE * AR2 * 4.429
                        HCFS = ABS(CH(JN) - ZD1 - 0.85 * DKI)
                        XX = SQRT(HCFS)
                        QKK = HE1 * XX + 0001
                        XIP = 0.85 * DKI + ZD1
                        AA = -0.5 * HE1 * HE1 / QKK
                        AW(IN, IJ2) = AW(IN, IJ2) - AA
                        AW(JN, IJ3) = AW(JN, IJ3) + AA
                        R(IN) = R(IN) + AA * (CH(JN) - 2. * XIP)
                        R(JN) = R(JN) - AA * (CH(JN) - 2. * XIP)
                    ELSE                      ! XX11
                        CONTINUE
                    END IF                 ! XX11
                ELSE               ! XX1
                    CONTINUE
                END IF             ! XX1
            ELSE                                  !  X1
                IF(HMIN.GE.0.75 * DKI)THEN      ! XX2
                    IF(HAF.GE.0.)THEN     ! XX21
                        HE = 0.65                           !----------------
                        HFF = HMIN
                        IF(HMIN.GT.DKI)HFF = DKI
                        AR2 = 3.1416 * DKI * DKI / 8. + (HFF - DKI / 2.) * SQRT(DKI * HFF - HFF**2)
                        HE1 = HE * AR2 * 4.429
                        HCFS = ABS(CH(IN) - ZD2 - 0.6667 * DKI)
                        XX = SQRT(HCFS)
                        QKK = HE1 * XX + 0001
                        XIP = 0.6667 * DKI + ZD2
                        AA = 0.5 * HE1 * HE1 / QKK
                        AW(IN, IJ1) = AW(IN, IJ1) - AA
                        AW(JN, IJ4) = AW(JN, IJ4) + AA
                        R(IN) = R(IN) + AA * (CH(IN) - 2. * XIP)
                        R(JN) = R(JN) - AA * (CH(IN) - 2. * XIP)
                    ELSE                  ! XX21
                        HE = 0.65
                        HFF = HMIN
                        IF(HMIN.GT.DKI)HFF = DKI
                        AR2 = 3.1416 * DKI * DKI / 8. + (HFF - DKI / 2.) * SQRT(DKI * HFF - HFF**2)
                        HE1 = HE * AR2 * 4.429
                        HCFS = ABS(CH(JN) - ZD1 - 0.6667 * DKI)
                        XX = SQRT(HCFS)
                        QKK = HE1 * XX + 0001
                        XIP = 0.6667 * DKI + ZD1
                        AA = -0.5 * HE1 * HE1 / QKK
                        AW(IN, IJ2) = AW(IN, IJ2) - AA
                        AW(JN, IJ3) = AW(JN, IJ3) + AA
                        R(IN) = R(IN) + AA * (CH(JN) - 2. * XIP)
                        R(JN) = R(JN) - AA * (CH(JN) - 2. * XIP)
                    END IF                 ! XX21
                ELSE                 !XX2
                    CONTINUE
                END IF             ! XX2
            END IF                    ! X1
        652    CONTINUE
    END IF
    !===================================================
    IF(MCON.NE.0)THEN
        Do LL = 1, MCON
            SOGIO(LL) = 0.
        END DO
        DO L = 1, MQC      ! TINH NUOC TIEU
            NUT = KEP(L7 + NA + NOC + L)
            R(NUT) = R(NUT) - QTI(NUT)
        END DO
        DO  ID = 1, MCON         !1
            ZD1 = TT1(ID)
            ZD2 = TT1(ID + MCON)
            INA = ID + NA + NOC
            IN = KEP(L4 + INA)
            JN = KEP(L5 + INA)
            IJ1 = KAX(INA, 1)
            IJ3 = KAX(INA, 3)
            IF(IN.GE.JNU)THEN       ! 2
                IF(AW(IN, IJ1).EQ.0.)THEN
                    AW(IN, IJ1) = 1.
                END IF
            END IF              ! 2
            IF(JN.GE.JNU)THEN           !3
                IF(AW(JN, IJ3).EQ.0.)THEN
                    AW(JN, IJ3) = 1.
                END IF
            END IF                !3
        END DO      ! 1
    END IF
    CALL SOLE(LN, MBAN, AW, R, NCOL, IBAN)
    !========================================================
    IF(MCON.NE.0)THEN      ! TINH LAI CONG NGAM
        DO I = 1, LN
            CH(I) = R(I)
        END DO
        DO  ID = 1, MCON
            ZD1 = TT1(ID)
            ZD2 = TT1(ID + MCON)
            INA = ID + NA + NOC
            IN = KEP(L4 + INA)
            JN = KEP(L5 + INA)
            IF(IN.GE.JNU)THEN
                IF(R(IN).LT.ZD1)CH(IN) = ZD1 + 0.03
            END IF
            IF(JN.GE.JNU)THEN
                IF(R(JN).LT.ZD2)CH(JN) = ZD2 + 0.03
            END IF
        END DO
    END IF
    IF(NOC.NE.0)THEN
        DO 65 II = 1, LN
        65   R(II + LN) = R(II)
    END IF
    !         ***** H,Q PROCEDURE FOR EACH BRANCH *****
    DO 70 ID = 1, NA
        INODE = KEP(L4 + ID)
        JNODE = KEP(L5 + ID)
        N1 = KEP(L6 + ID)
        N21 = KEP(L7 + ID)
        IBI = KEP(L3 + ID)
        H(N1) = R(INODE)
        IF(IBI.EQ.0)H(N21) = R(JNODE)
        IF(IBI.GE.0)THEN
            Q(N21) = (H(N21) - QN(N21) * H(N1) - RN(N21)) / PN(N21)
        END IF
        DO L = N21, N1 + 1, -1
            H(L) = PN(L) * Q(L) + QN(L) * H(N1) + RN(L)
            Q(L - 1) = A3(L) * Q(L) + B3(L) * H(L) + C3(L) * H(N1) + D3(L)
        END DO
    70 CONTINUE
    IF(NRK.NE.0)THEN
        CALL PLAIN(H, FRU, PEN, KEP, PSM, PQR, FEN, VV)
        DO I = 1, NC
            VV(I) = H(I)
        END DO
    END IF
    CALL BAN(NC, NT, DNT, H, PEN, DIE, RON, A1, B2, E1, NRK)
    DO  I = 1, NC
        IF(A1(I).GT.0.01)THEN
            C3(I) = Q(I) / A1(I)
        ELSE
            C3(I) = 0.
        END IF
        PEN(MU + I) = C3(I)
    END DO
    IF(ITER.GE.IR1)THEN    ! 55
        NPP = (NH1 - IM1) * NUM + 1
        Do i = 1, Nc
            Wle = H(i)
            If(Hma(i).Lt.Wle)Hma(i) = Wle
            If(Hmi(i).Gt.Wle)Hmi(i) = Wle
            Hav(i) = Hav(i) + Wle / Npp
        End do
    End if       ! 55
    !            IF(NSQ.NE.0)THEN  !2
    DO K = 1, NC             !1
        !      I4=InHQ(K)
        FVV = Q(K) / NPP    !Q(I4)
        IF(FVV.GT.0.)THEN
            Qdu(K) = Qdu(K) + FVV
        ELSE
            Qam(K) = Qam(K) + FVV
        END IF
    END DO                 !1
    !                   END IF            !2
    IF(MCON.NE.0)THEN
        CALL IHDATE(IP, NGIOTI, NGAYT, NTHANGT, NAMT)
        IHO = NGIOTI
        IDA = NGAYT
        IMO = NTHANGT
        INA = NAMT
        !=====================================
        NDelt = DT / 60
        MXX = NDelt * MOA
        DO k = NUCO, LN
            ST = SOGIO(k)
            HNga = CHU(k) - CAO(k)
            If(HNga /= 0.)ST = ST + 1
            SOGIO(k) = ST
            IF(HNga > WLNgap(k))WLNgap(k) = HNga
        END DO
        WRITE(6, 250)IHO, MXX, IDA, IMO, INA, (CHU(I) - CAO(I), I=NUCO, LN)
    END IF
    250 Format(I4':', I2, I4, I3, 1x, I5, 1x, 250F7.2)
    !            END IF
END SUBROUTINE
!     *****************************************
SUBROUTINE SOLE(NP, MBAN, A, B, NCOL, IBAN)
    !     *****************************************
    DIMENSION A(NP, *), NCOL(NP, *), B(NP), & !NCOL(NP,MBAN),A(NP,MBAN)   &
            & IBAN(*), NPIV(NP), NNCOL(3000), AA(NP)    !IBAN(NP),NNCOL(NP),AA(NP)
    ! IBAN(NP)=IBANDW chua so phan tu /= 0 tren moi dong,NPIV(NP)
    ! MINROW la hang co so phan tu khac khong nho nhat
    ! AA hang lam viec chua so phan tu cua matran A, chi so cot chua trong NNCOL
    ! NCOL chua chi so cot cua A(NP,*)
    MAXWID = 10
    DO IV = 1, NP
        NPIV(IV) = 0
        AA(IV) = 0.
    END DO
    NP1 = NP - 1
    DO LL = 1, NP1
        DO IG = 1, NP
            NNCOL(IG) = 0
        END DO
        DO IB = 1, NP
            AA(IB) = 0.
        END DO
        KS = 1000
        DO I = LL, NP     !Do I
            IC = IBAN(I)  ! SO PHAN TU /=0 TREN HANG I
            IF(IC.GT.0.AND.IC.LT.KS)THEN
                MINROW = I       !Hang co so phan tu it nhat
                KS = IC
            END IF
        END DO         !Do I
        LM = IBAN(LL)   ! So phan tu tren hang it nhat
        M = MINROW      ! Hang co so phan tu it nhat
        DO I = 1, LM
            NNCOL(I) = NCOL(M, I) ! Chi so cot hang lam viec
            NCOL(M, I) = NCOL(LL, I) !Doi ?
            AA(I) = A(M, I)         ! Hang khac 0
            A(M, I) = A(LL, I)       ! Doi
        END DO
        SAVA = B(LL)
        B(LL) = B(M)
        B(M) = SAVA
        IBAN(LL) = IBAN(M)
        IBAN(M) = LM
        NCC = IBAN(LL)
        MINROW = LL
        A7 = 0.
        DO J = 1, NCC
            AB = ABS(AA(J))
            IF(AB.GE.A7)THEN
                A7 = AB
                IY = J
            END IF
        END DO
        MAXCOL = NNCOL(IY)
        NPIV(LL) = MAXCOL
        X = AA(IY) + 0.00001
        DO J = 1, NCC
            AA(J) = AA(J) / X
            NCOL(MINROW, J) = NNCOL(J)
            A(MINROW, J) = AA(J)
        END DO
        B(MINROW) = B(MINROW) / X
        LL1 = LL + 1
        DO 22 I = LL1, NP
            IF(IBAN(I).EQ.0)GOTO 22
            NCC = IBAN(I)
            DO 21 J = 1, NCC
                IF(NCOL(I, J) - MAXCOL)21, 10, 22
                10     NOP = I
                JKOP = 1
                JKPI = 1
                C = -A(NOP, J)
                B(NOP) = B(MINROW) * C + B(NOP)
                11 CONTINUE
                IF(NNCOL(JKPI).EQ.0)GOTO 22
                IF(NCOL(NOP, JKOP).EQ.0)GOTO 12
                IF(NNCOL(JKPI) - NCOL(NOP, JKOP))12, 14, 20
                12 IBAN(I) = IBAN(I) + 1
                IF(MAXWID.LT.IBAN(I))MAXWID = IBAN(I)
                !===================================Chu y, hay gap loi
                IF(MAXWID.GT.MBAN)GOTO 31       !=====================
                II = IBAN(I)
                JKL = JKOP + 1
                13 IX = II - 1
                A(NOP, II) = A(NOP, IX)
                NCOL(NOP, II) = NCOL(NOP, IX)
                II = IX
                IF(IX.GE.JKL)GOTO 13
                A(NOP, JKOP) = AA(JKPI) * C
                NCOL(NOP, JKOP) = NNCOL(JKPI)
                IX = NCOL(NOP, JKOP)
                GOTO 19
                14 IX = NCOL(NOP, JKOP)
                IF(IX.EQ.MAXCOL)GOTO 15
                X = AA(JKPI) * C + A(NOP, JKOP)
                A(NOP, JKOP) = X
                IF(ABS(X).GT.1.E-20)GOTO 19
                15 IBAN(NOP) = IBAN(NOP) - 1
                IF(IBAN(NOP))16, 16, 17
                16 Write(90, 29)MINROW, MAXCOL, NOP
                STOP
                17 IX = IBAN(NOP)
                DO  NK = JKOP, IX
                    A(I, NK) = A(I, NK + 1)
                    NCOL(I, NK) = NCOL(I, NK + 1)
                END DO
                IX = IX + 1
                NCOL(I, IX) = 0
                A(I, IX) = 0.
                JKPI = JKPI + 1
                GOTO 11
                19 JKPI = JKPI + 1
                20 JKOP = JKOP + 1
                GOTO 11
            21 CONTINUE
        22 CONTINUE
    END DO
    NPIV(NP) = NCOL(NP, 1)
    DO I = 1, NP
        AA(I) = 0.
    END DO
    DO I = 1, NP
        II = NP - I + 1
        LM = IBAN(II)
        NPV = NPIV(II)
        IF(NPV.NE.0)THEN
            DO J = 1, LM
                NNZ = NCOL(II, J)
                IF(NNZ.NE.NPV)THEN
                    B(II) = B(II) - AA(NNZ) * A(II, J)
                ELSE
                    IJ = J
                END IF
            END DO
            AA(NPV) = B(II) / A(II, IJ)
        END IF
    END DO
    DO I = 1, NP
        B(I) = AA(I)
    END DO
    RETURN
    31 Write(90, 32)I, LL, MAXWID
    STOP
    29 FORMAT(5X, 'SINGULAR-MINROW=', I5, 2X, 'MAXCOL=', I5, 2X, 'ROW OPERATED=', I5)
    32 FORMAT(5X, 'COL.DIM.OF NCOL AND A EXCEEDED IN ROW', I5, 2X, ' ROW OPERATED=', I5, 'MAXWID=', i4)
END SUBROUTINE
!     ****************************************************
SUBROUTINE PLAIN(H, FRU, PEN, KEP, PSM, PQR, FEN, HH)
    !     ****************************************************
    COMMON/DIMK/L1, L2, L3, L4, L5, L6, L7, L8, L10, L11, L12
    COMMON/KPR/ITER, MOA, IP, NUM, NCK, IR1, ISI, DT, DNT, &
            &       INDIS, JHQS, MDT, DNR, MPLOT, MPLU, MBOC, IM1
    COMMON/ONE/NA, NT, NH, NOC, NES, NC, KN, LN, ITEST, NSS, &
            & NUU, NH1, NSM, NTD, NRK, NTR, NNC, NRH, NSQ, KQL, MAXNOI, JRK
    COMMON/DIMP/MAM, MQL, NAP, MZD, MRZ, MHC, LFAL, LFZD, MQR, &
            & MQQ, MFQ, MFS, MFR, MVL, LPS, MU, MQA, MQT, LQL, LQRU
    INTEGER KEP(*)
    REAL H(*), PEN(*), FRU(NRK, *), PSM(NSM, *), PQR(NRK, *), FEN(*), HH(*)
    LR6 = L6 + NA + NOC
    LR7 = L7 + NA + NOC
    NC2 = 2 * NC
    DETA = 0.55
    NLAP = 2
    LR1 = L11 + NA
    MRR = MQR + NC
    MLC = MQL + NC
    MZN = MZD + NOC
    MRH = MRZ + NOC
    DO 10 KK = 1, NC
    10  PEN(MLC + KK) = 0.
    DO 100 KR = 1, NRK
        KCR = KR + NC
        HSA = H(KCR)
        HRR = H(KCR)
        KR1 = KR - 1
        NORU = KEP(L8 + KR)
        ZMIN = PEN(NC2 + KR)
        KTRA = KEP(LR1 + KR)
        IF(KTRA.NE.0)THEN
            MRA = 1 + IP * MBOC / (24 * MDT)
            PP = PSM(KTRA, MRA)
            HEC = PEN(MHC + KTRA)
        ELSE
            PP = 0.
            HEC = 0.
        END IF
        PRL = 0.
        TF = NLAP * 10000. / DT
        DO 77 L = 1, NLAP
            IM = (HRR - ZMIN) / DNR
            I = IM + 1
            IF(I.LE.1)THEN
                V1 = 1. + (ZMIN - HRR) / DNR
                FNG = FRU(KR, 2) - (FRU(KR, 2) - FRU(KR, 1)) * V1
            ELSE IF(I.GE.NTR)THEN
                FNG = FRU(KR, NTR)
            ELSE
                IK = I + 1
                V3 = (HRR - ZMIN) / DNR - IM
                FNG = FRU(KR, I) + (FRU(KR, IK) - FRU(KR, I)) * V3
            END IF
            FKN = FRU(KR, NTR) - FNG
            PEN(MRR + KR) = (FNG * PP + FKN * HEC) * 10000.
            FF = FNG * TF
            DO 60 KK = 1, NORU
                MZ1 = KEP(LR6 + MAXNOI * KR1 + KK)
                MZ2 = KEP(LR7 + MAXNOI * KR1 + KK)
                ICOT = KEP(L1 + KN + MAXNOI * KR1 + KK)
                MN = MZ2 - MZ1
                IF(NN.EQ.0)NN = 1
                ZDA = PEN(MZN + MAXNOI * KR1 + KK)
                HEF = PEN(MRH + MAXNOI * KR1 + KK)
                ZDQ = FEN(LFZD + MAXNOI * KR1 + KK)
                ALL = FEN(LFAL + MAXNOI * KR1 + KK)
                HOLD = 0.5 * (HH(MZ1) + HH(MZ2))
                HNEW = 0.5 * (H(MZ1) + H(MZ2))
                DEH = (HNEW - HOLD) / NLAP
                HSO = HOLD + L * DEH
                HAF = HSO - HRR
                IF(HRR.GT.HSA)THEN           ! KIEM TRA MUC NUOC RUONG
                    IF(HSO.GT.HRR)GOTO 55
                END IF
                IF(ICOT)80, 85, 90
                80   IF(HRR.GT.HSO - 0.005)GOTO 55
                GOTO 85
                90   IF(HSO.GT.HRR - 0.005)GOTO 55
                85         IF(HAF.GT.0.)THEN
                    HMA = HSO - ZDA
                    HMI = HRR - ZDA
                    DIVI = 1.
                    AFA = 1.
                    BTA = 0.
                ELSE
                    HMA = HRR - ZDA
                    HMI = HSO - ZDA
                    DIVI = -1.
                    AFA = 0.
                    BTA = 1.
                END IF
                DIF = HMA - HMI
                IF(HMA.LT.0.005.OR.DIF.LT.0.005)THEN
                    PQR(KR, KK) = 0.
                ELSE
                    HTEST = 0.6667 * HMA
                    IF(HMA + ZDA.LE.ZDQ)ALL = 1.
                    TGG = (1. - ALL) * (ZDQ - ZDA)
                    GAM = 0.55
                    TT = ALL * ((3. * GAM - 1.) * HMA - 3. * GAM * HMI) - TGG
                    IF(TT.GT.0.)GAM = 0.
                    HTB = GAM * HMI + (1. - GAM) * HMA
                    IF(HMI.GT.HTEST)THEN
                        HE1 = HEF * DIVI
                        WW = ALL * HTB + TGG
                        CAN = SQRT(DIF)
                        CAN1 = 1. / (CAN + 0.0001)
                        XX1 = (AFA - BTA) * WW * CAN1 / 2.
                        XX2 = ALL * CAN * (AFA + GAM * (BTA - AFA))
                        FQ = WW * HE1 * CAN
                        PQR(KR, KK) = FQ + (XX1 + XX2) * HE1 * DETA * DEH
                        VV = (BTA - AFA) * WW * CAN1 / 2.
                        VV = VV + (BTA + GAM * (AFA - BTA)) * ALL * CAN
                        FF = FF - HE1 * DETA * VV
                    ELSE
                        AX1 = 3. * ALL + TGG / (HMA + 0.0001)
                        HE1 = HEF * DIVI * 0.386
                        WW = ALL * HMA + TGG
                        FXQ = HE1 * SQRT(HMA)
                        FQ = FXQ * WW
                        PQR(KR, KK) = FQ + DETA * AFA * FXQ * AX1 * DEH / 2.
                        FF = FF + DETA * FXQ * BTA * AX1 / 2.
                    END IF
                END IF
                LL = 1
                IF(MZ1.EQ.MZ2)LL = 0
                DO 20 KJ = MZ1 + LL, MZ2
                20 PEN(MLC + KJ) = -PQR(KR, KK) / NN
                PRL = PRL + PQR(KR, KK)
                GOTO 60
                55   PQR(KR, KK) = 0.
            60   CONTINUE
            HRR = (PRL + PEN(MRR + KR)) / FF + HRR
            PWL = HRR - ZMIN
            IF(PWL.LE.0.)HRR = ZMIN
        77   CONTINUE
        H(KCR) = HRR
    100     CONTINUE
    RETURN
END
!********************************************************************
SUBROUTINE IHDATE(IP, NGIOTI, NGAYTI, NTHANGT, NAMT)
    !  -------------------------------------------------------------------
    !  DOI SO GIO TINH THANH GIO-NGAY-THANG-NAM
    ! INPUT:ITO,ING,ITH,NAM == IP thu tu dem gio tinh
    ! OUTPUT:NGIOTI,NGAYTI,NTHANGT,NAMT
    INTEGER SOGIO(2, 13)
    LOGICAL L
    DATA SOGIO/0, 0, 744, 744, 1416, 1440, 2160, 2184, 2880, 2904, 3624, 3648, &
            & 4344, 4368, 5088, 5112, 5832, 5856, 6552, 6576, 7296, 7320, &
            & 8016, 8040, 8760, 8784/
    COMMON/DAT/ITO, ING, ITH, IYE, KKS, JKT, DELT
    NAMT = IYE
    L = (MOD(NAMT, 4) == 0).AND.(MOD(NAMT, 100) /= 0).OR.(MOD(NAMT, 400) == 0)
    LEAP = 1      ! Nam thuong
    IF(L)LEAP = 2  ! Nam Nhuan
    !	Write(11,*)'Leap,ITO,ING,ITH,IYE ',  Leap,ITO,ING,ITH,IYE
    NGIOT1 = SOGIO(LEAP, ITH) + (ING - 1) * 24 + ITO !GIO XUAT PHAT TINH
    NHTI = NGIOT1 + IP - 1 ! Gio dang tinh ke tu dau nam
    IF(NHTI + 1 > SOGIO(LEAP, 13))THEN
        NHTI = NHTI - SOGIO(LEAP, 13) + 1          !!!!!
        NAMT = NAMT + 1
        L = (MOD(NAMT, 4) == 0).AND.(MOD(NAMT, 100) /= 0).OR.(MOD(NAMT, 400) == 0)
        LEAP = 1
        IF(L)LEAP = 2
    END IF
    DO IK = 1, 12
        NGIO1 = SOGIO(LEAP, IK)
        NGIO2 = SOGIO(LEAP, IK + 1)
        IF(NHTI >= NGIO1.AND.NHTI < NGIO2)THEN
            NTHANGT = IK
        END IF
    END DO
    NXX = NHTI - SOGIO(LEAP, NTHANGT) !-1
    NGAYTI = NXX / 24 + 1
    NGIOTI = NXX - (NGAYTI - 1) * 24
END SUBROUTINE
!     *************************************************
SUBROUTINE RESULT(NES, NC, IP, H, Q, CHU, FH, FQ, KEP, &
        & MPLU, U, UMA, MBOC, NRK, MNO, PQR, MCON, LN, CAO)
    !     *************************************************
    COMMON/SS /PTO(5000), PTN(5000), NUCO
    COMMON/DAT/ITO, ING, ITH, IYE, KKS, JKT, DELT
    COMMON/PRIN/ INHQ(400), INHRU(400), INQRU(400)
    REAL H(*), Q(*), FH(*), FQ(*), U(*), CAO(*), UMA(*), PQR(NRK, *), CHU(*)
    INTEGER KEP(*)
    CALL IHDATE(IP, NGIOTI, NGAYT, NTHANGT, NAMT)
    IHO = NGIOTI
    IDA = NGAYT
    IMO = NTHANGT
    INA = NAMT
    !=========================================
    IP1 = IP - 1
    KKS = KKS + 1
    J5 = ITO + IP1 * MBOC
    J6 = J5 / 24
    LL1 = J5 - J6 * 24
    IF(MBOC.EQ.24)LL1 = LL1 - ITO
    IF(NRK.NE.0)THEN
        MOF = NRK * MNO
        DO KI = 1, NRK
            IC = (KI - 1) * MNO
            DO 5 JK = 1, MNO
            5    FH(IC + JK) = PQR(KI, JK)
        END DO
        WRITE(11, 400)IP1, (H(NC + I), I = 1, NRK)
        WRITE(10, 500)IP1, (FH(KK), KK = 1, MOF)
    ENDIF
    DO I = 1, NES
        K1 = INHQ(I)
        FH(I) = H(K1)
        FQ(I) = Q(K1)
    END DO
    WRITE(3, 200)IP1, IHO, IDA, IMO, INA, (FH(I), I = 1, NES)
    WRITE(4, 350)IP1, IHO, IDA, IMO, INA, (FQ(I), I = 1, NES)
    200 FORMAT(I5, 1x, I3, I4, I3, 1x, I5, 250F7.2)
    350 FORMAT(I5, 1x, I3, I4, I3, 1x, I5, 200F9.1)
    !               IF(MCON.NE.0)THEN
    !               END IF
    IF(MPLU.NE.0)THEN
        DO MI = 1, NC
            UM = ABS(U(MI))
            IF(UM.GE.UMA(MI))UMA(MI) = UM
        END DO
    END IF
    IF(LL1.EQ.23.OR.IP.EQ.JKT)THEN
        J7 = ING + J6
        J8 = J7 / (30 + 1)   !KIM=30
        LL2 = J7 - J8 * 30   !KIM=30
        LL3 = ITH + J8
        IF(MPLU.NE.0)THEN
            WRITE(7, 160)LL2, LL3, IYE
            WRITE(7, 170)(UMA(LK), LK = 1, NC)
            DO LK = 1, NC
                UMA(LK) = ABS(U(LK))
            END DO
        END IF
        KKS = 0
    ENDIF
    160 FORMAT(10X, 3I7)
    170 FORMAT(35F6.2)
    250 FORMAT(I5, 300F7.2)
    400 FORMAT(I5, 200F7.2)
    500 FORMAT(I5, 200F9.1)
END SUBROUTINE
!     *********************************************
SUBROUTINE FINI(NA, NC, IP, MCON, L6, L7, L10, LN, &
        &  H, Q, INDIS, KEP, NRK, MAXNOI, PQR, CH, Hma, Hav, HMi, &
        &  QDU, QAM, XT, YT)
    !  -----------------------------------------------
    DIMENSION H(*), Q(*), CH(*), QDU(*), QAM(*), &
            & PQR(NRK, *), Hma(*), Hav(*), Hmi(*), XT(*), YT(*)
    INTEGER      KEP(*)
    CHARACTER*40 F5
    Write(14, *)' Nhanh    Mcat       Hma      Hbq     Hmi    &
            &XX           YY '
    Do kj = 1, NA  ! Nhanh
        MCD = KEP(L6 + kj)
        MCC = KEP(L7 + kj)
        Do i = MCD, MCC
            Write(14, 909)Kj, i, Hma(i), Hav(i), Hmi(i), XT(i), YT(i)
        End do
    End do   ! Nhanh
    909     format(2i7, 2x, 3f9.2, 2f13.2)
    Write(15, *)' Nhanh    Mcat        Q+        Qsum      Qam      &
            & XX          YY '
    Do kj = 1, NA  ! Nhanh
        MCD = KEP(L6 + kj)
        MCC = KEP(L7 + kj)
        Do i = MCD, MCC
            Qss = Qam(i) + Qdu(i)
            Write(15, 910)Kj, i, Qdu(i), Qss, Qam(i), XT(i), YT(i)
        End do
    End do  ! Nhanh
    910     format(2i7, 1x, 3f11.2, 2f13.2)
    !==================================================
    IF(INDIS /= 0)THEN  ! INDIS
        Write(90, '(A)')'  Ten file Dieu Kien Dau cho lan tinh sau ?'
        READ(*, '(A)')F5
        OPEN(18, FILE = F5, STATUS = 'REPLACE')
        Write(18, *)' Dieu kien H,Q cua NC Mat cat, NC= ', NC
        WRITE(18, 700)(H(K), K = 1, NC)
        WRITE(18, 720)(Q(K), K = 1, NC)
        IF(NRK /=0)Then ! NRK
            Write(18, *)' Dieu kien H,Q cua NRK , NRK= ', NRK
            WRITE(18, 700)(H(J + NC), J = 1, NRK)
            WRITE(18, 720)((PQR(K4, K5), K5 = 1, MAXNOI), K4 = 1, NRK) ! (PQR(K2),K2=1,NOSUM)
        ENDIF  ! NRK
        IF(MCON.NE.0)Then
            Write(18, *)' Water level at LN nodee, LN=', LN
            WRITE(18, 700)(CH(K), K = 1, LN)
        End if
    END IF   ! INDIs
    Write(12, 600)IP
    600 FORMAT(/10X, 'H,Q ARE KEPT FOR NEXT COM. IP='I6)
    700 FORMAT(20F10.2)
    720 FORMAT(20F10.1)
END SUBROUTINE
