      SUBROUTINE CM4FIELD (PATH,UNIT,LOAD,INDX,GMUT,CORD,PRED,CURR,
     1                     COEF,NHMF,NLMF,UT,MUT,THETA,PHI,ALT,DST,
     2                     F107,BMDL,JMDL,GMDL,PERR,OERR,CERR)
*                                                                               
********************************************************************************
*                                                                               
* 1.  MODULE NAME - CM4FIELD                                                   
*                                                                               
* 2.  NAME DERIVATION - Comprehensive Model 4 magnetic FIELD evaluation
*                       subroutine        
*                                                                               
* 3.  FUNCTION - To evaluate the Comprehensive Model 4 (CM4) magnetic field
*                model at a given temporal and spatial location with the given
*                magnetic indices.  It returns the local X, Y, Z (North, East,
*                Down) components of the B field vector from the main, primary
*                and induced magnetosphere, primary and induced ionosphere, and
*                toroidal field sources.  Two evaluations of the main field are
*                accommodated per two given spherical harmonic degree ranges.
*                This is helpful when only sub-ranges of the full degree/order
*                range is of interest.  In addition, the J field vector
*                associated with the primary and induced ionosphere and the
*                toroidal B field sources is also returned in local X, Y, Z
*                components and coefficients from the various sources are also
*                computed and returned in various forms.  For details into the
*                nature of models of this type see Sabaka et al. (2002,2004).
*
* 4.  LANGUAGE - Fortran 77
*                                                                               
* 5.  ARGUMENTS -                                                               
*                                                                               
*     Name      Type  I/O                   Description                         
*     ----      ----  ---                   -----------                         
*
*     PATH(3)   Chr    I     File absolute paths:
*                            (1) Model coefficients.
*                            (2) Dst index.
*                            (3) F10.7 index.
*
*     UNIT(3)   Int    I     Logical file units:
*                            (1) Model coefficients.
*                            (2) Dst index.
*                            (3) F10.7 index.
*                                                                               
*     LOAD(3)   Log   I/O    File read flags:
*                            = T, read file
*                            = F, file previously read (return value).
*                            (1) Model coefficients.
*                            (2) Dst index.
*                            (3) F10.7 index.
*                                                                               
*     INDX(2)   Log    I     Index acquisition flags:
*                            = T, from file.
*                            = F, from argument.
*                            (1) Dst index (DST).
*                            (2) F10.7 index (F107).
*                                                                               
*     GMUT      Log    I     Magnetic dipole universal time (MUT) acquisition
*                            flag:
*                            = T, compute from UT.
*                            = F, from argument.
*                                                                               
*     CORD      Log    I     Observation coordinate system flag:
*                            = T, geodetic, i.e., local geographic on a
*                                 reference ellipsoid.
*                            = F, geocentric, i.e., local geographic on a
*                                 reference sphere.
*                                                                               
*     PRED(6)   Log    I     Model B magnetic field prediction source flags:
*                            = T, compute.
*                            = F, do not compute.
*                            (1) Main field.
*                            (2) Magnetospheric fields.
*                            (3) Ionospheric fields.
*                            (4) Toroidal field from:
*                                (5) Magsat (= T) or Oersted (= F).
*                                    (6) Dawn (= T) or dusk (= F) for Magsat.
*
*     CURR      Log    I     Model J current field prediction flag:
*                            = T, compute.
*                            = F, do not compute.
*
*                            J currents are only computed for certain external
*                            sources activated by PRED (see JMDL).
*
*     COEF      Log    I     Model coefficient generation flag:
*                            = T, compute.
*                            = F, do not compute.
*
*                            Coefficients are only computed for sources
*                            activated by PRED (see GMDL).
*                                                                               
*     NHMF(2)   Int    I     Maximum main field spherical harmonic
*                            degree:
*                            (1) for main field 1.
*                            (2) for main field 2.
*
*                            These values must be < or = 65.
*                                                                               
*     NLMF(2)   Int    I     Minimum main field spherical harmonic
*                            degree:
*                            (1) for main field 1.
*                            (2) for main field 2.
*
*                            If NHMF(i) < NLMF(i), then this degree
*                            range is not evaluated.
*                                                                               
*     UT        Dble   I     Geographic universal time (years A.D.):
*                            GMUT = T, used for secular, seasonal and diurnal
*                                      time scale variations.
*                            GMUT = F, used only for secular and seasonal time
*                                      scale variations.
*                                                                               
*     MUT       Dble  I/O    Magnetic dipole universal time (hours 0-24) used
*                            for diurnal time scale variations:
*                            GMUT = T, compute from UT and return.
*                            GMUT = F, from argument.
*                                                                               
*     THETA     Dble   I     Geodetic or geocentric colatitude (degrees).
*
*     PHI       Dble   I     Longitude (degrees).
*                                                                               
*     ALT       Dble   I     Geodetic or geocentric altitude (km).
*                                                                               
*     DST       Dble  I/O    Linearly interpolated hourly Dst magnetic index
*                            value (nT):
*                            INDX(1) = T, from file and return.
*                            INDX(1) = F, from argument.
*                                                                               
*     F107      Dble  I/O    Linearly interpolated 3-monthly means of absolute
*                            F10.7 solar radiation flux value
*                            (10^(-22)W/m^2/Hz):
*                            INDX(2) = T, from file and return.
*                            INDX(2) = F, from argument.
*                                                                               
*     BMDL(3,7) Dble   O     Array storing computed B field vectors from various
*                            sources (nT):
*                                                                               
*                            Row label:
*                            (1) X.
*                            (2) Y.
*                            (3) Z.
*                                                                               
*                            Column label:
*                            (1) Main field 1.
*                            (2) Main field 2.
*                            (3) Primary magnetospheric field.
*                            (4) Induced magnetospheric field.
*                            (5) Primary ionospheric field.
*                            (6) Induced ionospheric field.
*                            (7) Toroidal field.
*
*                            Note that toroidal B is produced by an insitu
*                            poloidal J field through which a satellite has
*                            sampled.  Therefore, it is only considered accurate
*                            near its associated RTAY shell radius.
*                                                                               
*     JMDL(3,4) Dble   O     Array storing computed J field vectors from certain
*                            external sources:
*                                                                               
*                            Row label:
*                            (1) X.
*                            (2) Y.
*                            (3) Z or current function Psi.
*                                                                               
*                            Column label:
*                            (1) Induced magnetospheric field.
*                            (2) Primary ionospheric field.
*                            (3) Induced ionospheric field.
*                            (4) Poloidal field.
*
*                            Note that the primary magnetospheric J field is a
*                            culmination of many J field sources which extend
*                            over a wide volume.  Hence, an equivalent sheet
*                            current representation is not considered useful
*                            in this case.  However, the induced magnetospheric
*                            J are confined, more or less, to a thin outer
*                            shell of the Earth.  Therefore, the J computed
*                            here is an equivalent current representation, which
*                            is toroidal and considered to flow in a spherical
*                            shell and RM.  Hence, this J has no Z component.
*                            Therefore, the equivalent current function Psi (a
*                            stream function), along whose contours J flows, is
*                            placed in JMDL(3,1) for the induced magnetospheric
*                            J field.  Both J and Psi are evaluated only on
*                            their shells, i.e., ALT is ignored.  For more
*                            information on equivalent currents and current
*                            functions see Chapman and Bartels (1940).
*
*                            Note that since primary and induced ionospheric J
*                            are equivalent currents, they are toroidal and
*                            considered to flow in spherical shells at RM+HION
*                            and RM, respectively, and so have no Z component.
*                            Therefore, the equivalent current function Psi (a
*                            stream function), along whose contours J flows, is
*                            placed in JMDL(3,2) and JMDL(3,3) for the primary
*                            and induced ionospheric J fields, respectively.
*                            Both J and Psi are evaluated only on their shells,
*                            i.e., ALT is ignored.
*
*                            Note that poloidal J is an insitu field through
*                            which a satellite has sampled and thus gives rise
*                            to a toroidal B field.  Therefore, they are only
*                            considered accurate near their associated RTAY
*                            shell radius.  For more information on poloidal J
*                            fields in thin sampling shells see Olsen (1997).
*
*                            Note all equivalent currents J are in A/m and all
*                            current functions Psi are in kA.  Poloidal J is in
*                            nA/m^2.
*                                                                               
*     GMDL(L,*) Dble   O     Array storing coefficients from various sources,
*                            where all coefficients are with respect to Schmidt-
*                            normalized spherical harmonics (see Langel, 1987):
*
*                            Row length L is the maximum number of coefficients
*                            N from the sources selected by PRED, NHMF and NLMF,
*                            such that:
*                            (1) Main field 1: N=NSHC(NHMF(1),NLMF(1))*(NXOR+1),
*                            (2) Main field 2: N=NSHC(NHMF(2),NLMF(2))*(NXOR+1),
*                            (3) Magnetospheric field: N=IXMG/2,
*                            (4) Ionospheric field:    N=IXSQ/2,
*                            (5) Toroidal field:       N=IXTO/2,
*                            where NSHC(NMAX,NMIN) is the number of spherical
*                            harmonic coefficients corresponding to NMAX and
*                            NMIN, and NXOR is the maximum order of B-splines
*                            describing secular variation.
*
*                            Column length is also a function of PRED, NHMF and
*                            NLMF such that the order and number of columns are:
*                            (1) Main field 1: 1 col if corresponding N>0,
*                            (2) Main field 2: 1 col if corresponding N>0,
*                            (3) Magnetospheric field: 2 cols (primary,induced),
*                            (4) Ionospheric field:    2 cols (primary,induced),
*                            (5) Toroidal field:       1 col.
*
*                            Note that columns are allocated only if the source
*                            is selected, and in the case of the main field,
*                            the number of coefficients are also greater than
*                            zero.  This allows for quasi-minimal memory
*                            allocation for GMDL.  If COEF = F, the calling
*                            routine may use a dummy argument for GMDL so that
*                            no additional memory is needed.
*
*                            The form and order of the coefficients is as
*                            follows:
*                            (1) Main fields 1 and 2:
*
*                                do j = 0, NXOR
*                                   do n = NLMF(), NHMF()
*                                      do m = 0, n
*                                         g(n,m,j) = jth time derivative of
*                                                    cos(m*phi) term at UT in
*                                                    geographic coordinates
*                                         if (m .gt. 0) then
*                                            h(n,m,j) = jth time derivative of
*                                                       sin(m*phi) term at UT in
*                                                       geographic coordinates
*                                         end if
*                                      end do
*                                   end do
*                                end do
*
*                            (2) Primary and induced magnetospheric fields:
*
*                                do n = 1, NXMG
*                                   do m = 0, min(n, MXMG)
*                                      q(n,m) = cos(m*phi) term at UT, MUT
*                                               and DST in dipole coordinates
*                                      if (m .gt. 0) then
*                                         s(n,m) = sin(m*phi) term at UT, MUT
*                                                  and DST in dipole coordinates
*                                      end if
*                                   end do
*                                end do
*
*                            (3) Primary and induced ionospheric fields:
*
*                                do n = 1, NXSQ
*                                   do m = 0, min(n, MXSQ)
*                                      c(n,m) = cos(m*phi) term at UT, MUT, ALT
*                                               and F10.7 in dipole coordinates
*                                      if (m .gt. 0) then
*                                         d(n,m) = sin(m*phi) term at UT, MUT,
*                                                  ALT and F10.7 in dipole
*                                                  coordinates
*                                      end if
*                                   end do
*                                end do
*
*                                Note that the ALT dependence here is simply
*                                binary; when ALT is above (below) HION, the
*                                primary coefficients are internal (external).
*                                Note also that c(n,m) and d(n,m) correspond
*                                to the usual spherical harmonic functions, NOT
*                                quasi-dipole functions.
*
*                            (4) Toroidal field:
*
*                                do n = 1, NXTO
*                                   do m = 0, min(n, MXTO)
*                                      u(n,m) = cos(m*phi) term at UT, MUT and
*                                               ALT in dipole coordinates
*                                      if (m .gt. 0) then
*                                         v(n,m) = sin(m*phi) term at UT, MUT
*                                                  and ALT in dipole coordinates
*                                      end if
*                                   end do
*                                end do
*
*                                Note that u(n,m) and v(n,m) correspond to the
*                                usual spherical harmonic functions, NOT quasi-
*                                dipole functions.
*                                                                               
*     PERR      Int    I     Error message print flag:
*                            = 0, do not print.
*                            = 1, print.
*                                                                               
*     OERR      Int    I     Logical unit for error message print.
*                                                                               
*     CERR      Int   I/O    Error return code:
*                            = 0, normal.
*                            > 0 and < 50, warning.
*                            > 50, fatal.
*                                                                               
* 6.  FILE FORMATS -
*                                                                               
*     a) Magnetic field model coefficients:
*
*        The magnetic field model file is an ASCII file composed of 25
*        logical records read from logical unit UMDL=UNIT(1):
*
*        READ(UMDL,1000) LSMF,LPOS,LCMF
*        READ(UMDL,1000) NMXI
*        READ(UMDL,1001) EPCH,RE,RP,RM
*        READ(UMDL,1002) (BORD(J),J=1,LSMF)
*        READ(UMDL,1002) (BKNO(J),J=1,LSMF)
*        READ(UMDL,1001) (BKPO(J),J=1,LPOS)
*        READ(UMDL,1001) (GAMF(J),J=1,LCMF)
*        READ(UMDL,1000) LCMG,LSMG
*        READ(UMDL,1000) PBMG,PEMG,PSMG,NXMG,MXMG,IXMG
*        READ(UMDL,1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM
*        READ(UMDL,1001) (((GPMG(I,J,K),I=1,LCMG),J=1,LSMG),K=1,2)
*        READ(UMDL,1001) (((GSMG(I,J,K),I=1,LCMG),J=1,LSMG),K=1,2)
*        READ(UMDL,1000) LCSQ,LSSQ
*        READ(UMDL,1000) PBSQ,PESQ,PSSQ,NXSQ,MXSQ,IXSQ
*        READ(UMDL,1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,HION
*        READ(UMDL,1001) (((GPSQ(I,J,K),I=1,LCSQ),J=1,LSSQ),K=1,2)
*        READ(UMDL,1001) ((GSSQ(I,J),I=1,LCSQ),J=1,LSSQ)
*        READ(UMDL,1000) LCTO_MG,LSTO_MG,LRTO_MG
*        READ(UMDL,1000) PBTO_MG,PETO_MG,NTAY_MG,PSTO,NXTO,MXTO,IXTO
*        READ(UMDL,1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,RTAY_DW,RTAY_DK
*        READ(UMDL,1001) ((((GCTO_MG(I,J,K,L),I=1,LCTO_MG),J=1,LSTO_MG),
*                        K=1,LRTO_MG),L=1,2)
*        READ(UMDL,1000) LCTO_OR,LSTO_OR,LRTO_OR
*        READ(UMDL,1000) PBTO_OR,PETO_OR,NTAY_OR,PSTO,NXTO,MXTO,IXTO
*        READ(UMDL,1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,RTAY_OR
*        READ(UMDL,1001) (((GCTO_OR(I,J,K),I=1,LCTO_OR),J=1,LSTO_OR),
*                        K=1,LRTO_OR)
*
*        with formats:
*
*        1000 FORMAT(10I8)
*        1001 FORMAT(4E22.15)
*        1002 FORMAT(22I4)
*
*        where:
*
*        Name                 Type              Description                         
*        ----                 ----              -----------                         
*                                                                               
*        LSMF                 Int   Total number of main field (MF) SHC's.
*                                                                               
*        LPOS                 Int   Total number of B-spline knot positions
*                                   for all MF SHC's.
*                                                                               
*        LCMF                 Int   Total number of MF model parameters.
*
*        NMXI                 Int   Maximum SH degree of MF model.
*
*        EPCH                 Dble  Epoch of MF model (years).
*
*        RE                   Dble  MF equatorial Earth radius (km).
*
*        RP                   Dble  MF polar Earth radius (km).
*
*        RM                   Dble  MF mean Earth radius (km).
*
*        BORD(LSMF)           Int   Array storing the order of the B-spline
*                                   basis (4=cubic) for each MF SHC (note:
*                                   zero value indicates a static SHC).
*
*        BKNO(LSMF)           Int   Array storing the interior knot number
*                                   of the B-spline basis for each MF SHC.
*
*        BKPO(LPOS)           Dble  Array storing the knot positions of the
*                                   B-spline basis (including both exterior
*                                   knots) for each MF SHC.
*
*        GAMF(LCMF)           Dble  Array storing the MF model parameters.
*                                                                               
*        LCMG                 Int   Total number of spatial/diurnal modulated
*                                   magnetospheric field (MG) model parameters.
*                                                                               
*        LSMG                 Int   Total number of MG fourier seasonal modes.
*                                                                               
*        PBMG                 Int   Minimum MG diurnal wavenumber.
*                                                                               
*        PEMG                 Int   Maximum MG diurnal wavenumber.
*                                                                               
*        PSMG                 Int   Maximum MG seasonal wavenumber.
*
*        NXMG                 Int   Maximum SH degree of MG model.
*
*        MXMG                 Int   Maximum SH order of MG model.
*                                                                        
*        IXMG                 Int   Total number of MG SHC's.
*                                                                               
*        CNMP                 Dble  MG colatitude of North magnetic
*                                   dipole (degree).
*                                                                               
*        ENMP                 Dble  MG east longitude of North magnetic
*                                   dipole (degree).
*                                                                               
*        OMGS                 Dble  MG seasonal fundamental angular
*                                   frequency (rads/year).
*                                                                               
*        OMGD                 Dble  MG diurnal fundamental angular
*                                   frequency (rads/hr).
*
*        RE                   Dble  MG equatorial Earth radius (km).
*
*        RP                   Dble  MG polar Earth radius (km).
*
*        RM                   Dble  MG mean Earth radius (km).
*
*        GPMG(LCMG,LSMG,2)    Dble  Array storing the MG primary model
*                                   parameters, where GPMG(i,j,1) is the ith
*                                   spatial/diurnal modulated coefficient of
*                                   the jth seasonal mode having no Dst
*                                   dependence, and GPMG(i,j,2) is the same,
*                                   but with Dst dependence.
*
*        GSMG(LCMG,LSMG,2)    Dble  Same as GPMG, except the MG induced model
*                                   parameters.
*
*        LCSQ                 Int   Total number of spatial/diurnal modulated
*                                   ionospheric field (SQ) model parameters.
*
*        LSSQ                 Int   Total number of SQ fourier seasonal modes.
*
*        PBSQ                 Int   Minimum SQ diurnal wavenumber.
*
*        PESQ                 Int   Maximum SQ diurnal wavenumber.
*
*        PSSQ                 Int   Maximum SQ seasonal wavenumber.
*
*        NXSQ                 Int   Maximum SH degree of SQ model.
*
*        MXSQ                 Int   Maximum SH order of SQ model.
*
*        IXSQ                 Int   Total number of SQ SHC's.
*
*        CNMP                 Dble  SQ colatitude of North magnetic
*                                   dipole (degree).
*
*        ENMP                 Dble  SQ east longitude of North magnetic
*                                   dipole (degree).
*
*        OMGS                 Dble  SQ seasonal fundamental angular
*                                   frequency (rads/year).
*
*        OMGD                 Dble  SQ diurnal fundamental angular
*                                   frequency (rads/hr).
*
*        RE                   Dble  SQ equatorial Earth radius (km).
*
*        RP                   Dble  SQ polar Earth radius (km).
*
*        RM                   Dble  SQ mean Earth radius (km).
*
*        HION                 Dble  Height of ionosphere where equivalent
*                                   currents conceptually flow (km).
*
*        GPSQ(LCSQ,LSSQ,2)    Dble  Array storing the SQ primary model
*                                   parameters, where GPSQ(i,j,1) is the ith
*                                   spatial/diurnal modulated coefficient of
*                                   the jth seasonal mode below RM+HION, and
*                                   GPSQ(i,j,2) is the same, but for above.
*
*        GSSQ(LCSQ,LSSQ)      Dble  Same as GPSQ, except the SQ induced model
*                                   parameters, all of which are for below
*                                   RM+HION.
*
*        LCTO_MG              Int   Total number of spatial/diurnal modulated
*                                   toroidal field (TO) model parameters.
*
*        LSTO_MG              Int   Total number of TO fourier seasonal modes.
*
*        LRTO_MG              Int   Total number of TO radial expansion
*                                   parameters.
*
*        PBTO_MG              Int   Minimum TO diurnal wavenumber.
*
*        PETO_MG              Int   Maximum TO diurnal wavenumber.
*
*        NTAY_MG              Int   Degree of radial Taylor series expansion.
*                                                                               
*        PSTO                 Int   Maximum TO seasonal wavenumber.
*
*        NXTO                 Int   Maximum SH degree of TO model.
*
*        MXTO                 Int   Maximum SH order of TO model.
*
*        IXTO                 Int   Total number of TO SHC's.
*
*        CNMP                 Dble  TO colatitude of North magnetic
*                                   dipole (degree).
*
*        ENMP                 Dble  TO east longitude of North magnetic
*                                   dipole (degree).
*
*        OMGS                 Dble  TO seasonal fundamental angular
*                                   frequency (rads/year).
*
*        OMGD                 Dble  TO diurnal fundamental angular
*                                   frequency (rads/hr).
*
*        RE                   Dble  TO equatorial Earth radius (km).
*
*        RP                   Dble  TO polar Earth radius (km).
*
*        RM                   Dble  TO mean Earth radius (km).
*                                                                               
*        RTAY_DW              Dble  Radial Taylor series reference radius
*                                   for Magsat dawn TO.
*                                                                               
*        RTAY_DK              Dble  Radial Taylor series reference radius
*                                   for Magsat dusk TO.
*
*        GCTO_MG(LCTO_MG,...  Dble  Array storing the Magsat TO model
*                LSTO_MG,...        parameters,where GCTO(i,j,k,1) is the ith
*                LRTO_MG,2)         spatial/diurnal modulated coefficient of the
*                                   jth seasonal mode of the kth radial
*                                   expansion coefficient for dawn local time,
*                                   and GCTO(i,j,k,2) is the same, but for
*                                   dusk local time (note: since this model
*                                   reduced only Magsat dawn and dusk data,
*                                   the diurnal dependence is not continuous
*                                   but is sampled at only two points (dawn
*                                   and dusk) which are broken out of the
*                                   first dimension and placed in the fourth
*                                   dimension of the array).
*
*        (Note:  The remaining 4 records hold TO information for Oersted which
*                is analogous to the 4 previous records for Magsat.  However,
*                the Oersted TO is continuous in diurnal time.)
*                                                                               
*     b) Dst index:
*
*        The Dst index file is a WDC format ASCII file composed of one logical
*        record per day read from logical unit UDST=UNIT(2):
*
*        READ(UDST,1000) YEAR,MONTH,DAY,(JDST(I),I=24)
*          "
*          "
*          "
*
*        with format:
*
*        1000 FORMAT(3X,2I2,1X,I2,10X,24I4)
*
*        where:
*
*        Name                 Type              Description                         
*        ----                 ----              -----------                         
*                                                                               
*        YEAR                 Int   Year since 1900.
*
*        MONTH                Int   Month of year.
*
*        DAY                  Int   Day of month.
*
*        JDST(24)             Int   Hourly means of Dst magnetic index values
*                                   (nT) for this day such that JDST(i) is mean
*                                   value over (i-1)th hour, thus values are
*                                   centered on the half-hour.
*
*        These files types may be retrieved in one-year segments from:
*
*             ftp://ftp.ngdc.noaa.gov/STP/GEOMAGNETIC_DATA/INDICES/DST/
*
*        as:
*
*             dstXXXX
*
*        where XXXX is the year A.D.  Note that if Dst is to be calculated at
*        times spanning year boundaries, then these one-year segment files
*        should simply be appended.  Of course, it is assumed that the
*        concatenated file is in increasing time order with no gaps.
*
*     c) F10.7 index:
*
*        The F10.7 index file is a MONTHPLT.ABS format ASCII file composed of
*        one logical record per month read from logical unit UFIN=UNIT(3):
*
*        READ(UFIN,1001) YEAR,MONTH,JF107
*          "
*          "
*          "
*
*        with format:
*
*        1000 FORMAT(I4,1X,I2,1X,I4)
*
*        where:
*
*        Name                 Type              Description                         
*        ----                 ----              -----------                         
*                                                                               
*        YEAR                 Int   Year A.D.
*
*        MONTH                Int   Month of year.
*
*        JF107                Int   (3-monthly means) x 10 of absolute F10.7
*                                   solar radiation flux values
*                                   (10^(-22)W/m^2/Hz).
*
*        This file type may be retrieved from:
*
*             ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SOLAR_RADIO/FLUX/
*
*        as:
*
*             MONTHPLT.ABS
*
* 7.  REFERENCES -                                                                 
*
*     Chapman, S. and J. Bartels, 1940.  Geomagnetism, Clarendon Press, Oxford.
*
*     Langel, R.A., 1987.  Main Field (Chap. 4), in Geomagnetism, J. Jacobs
*          (ed.), pp. 249-512, Academic Press, New York.
*
*     Olsen, N., 1997.  Ionospheric F region currents at middle and low
*          latitudes estimated from Magsat data, J. Geophys. Res., 102,
*          4563-4576.
*
*     Sabaka, T.J., Olsen, N. and R.A. Langel, 2002.  A comprehensive model of
*          the quiet-time, near-Earth magnetic field: phase 3, Geophys. J. Int., 
*          151, 32-68.
*
*     Sabaka, T.J., Olsen, N. and M.E. Purucker, 2004.  Extending comprehensive
*          models of the Earth's magnetic field with Oersted and CHAMP data,
*          Geophys. J. Int., doi: 10.1111/j.1365-246X.2004.02421.x.
*
* 8.  HISTORY -                                                                 
*                                                                               
*     Version    Date          Author             Change Description            
*     -------    ----          ------             ------------------            
*                                                                               
*     1.0       7/26/02   T.J. Sabaka / RITSS  Initial version.
*     2.0      10/22/02   T.J. Sabaka / RITSS  (1) Changed some variable names
*                                                  for readability,
*                                              (2) Input DST and F107 variables
*                                                  from call line or from files,
*                                              (3) Input MUT variable from call
*                                                  line or compute from UT,
*                                              (4) Output model coefficients in
*                                                  GMDL variable.
*                                                                               
* 9.  CONTACTS -                                                                
*
*     Name:     Terence J. Sabaka
*
*     Address:  Goddard Space Flight Center
*               Code 921
*               Greenbelt, MD 20771
*
*     Phone:    (301) 614-6493
*
*     Email:    sabaka@geomag.gsfc.nasa.gov
*                                                                               
********************************************************************************
*
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      LOGICAL CURR,COEF,CORD,GMUT,OMDL
      INTEGER PERR,OERR,CERR,CSYS,PBMG,PEMG,PSMG,PBSQ,PESQ,PSSQ,PBTO
      INTEGER PBTO_MG,PBTO_OR,PETO,PETO_MG,PETO_OR,PSTO,P
      DOUBLE PRECISION MUT
C=======================================================================
C Main Field potential expansion parameters
C=======================================================================
      PARAMETER (NXMF=65,NYMF=8840,NXOR=4,NXKN=19,NXPO=12415)
C=======================================================================
C Magnetospheric and coupled Induction potential expansion parameters
C=======================================================================
      PARAMETER (PBMG=0,PEMG=5,PSMG=2,NXMG=11,MXMG=6,IXMG=226)
C=======================================================================
C Sq and coupled Induction potential expansion parameters
C=======================================================================
      PARAMETER (PBSQ=0,PESQ=4,PSSQ=2,NXSQ=60,MXSQ=12,IXSQ=2736)
C=======================================================================
C Toroidal scalar or stream function expansion parameters
C=======================================================================
      PARAMETER (PBTO_MG=0,PETO_MG=0,PBTO_OR=0,PETO_OR=4)
      PARAMETER (PSTO=2,NXTO=60,MXTO=12,IXTO=2736)
      PARAMETER (NTAY_MG=1,NTAY_OR=1)
C=======================================================================
C Set maximum number of years for index table limits
C=======================================================================
      PARAMETER (MAXYRDST=100,MAXYRF107=100)
C=======================================================================
      PARAMETER (IXMF=NXMF*(NXMF+2))
      PARAMETER (ILEG=(NXMF+1)*(NXMF+2))
      PARAMETER (NSMG=2*PSMG+1)
      PARAMETER (NSSQ=2*PSSQ+1)
      PARAMETER (NSTO_MG=PSTO+1)
      PARAMETER (NSTO_OR=2*PSTO+1)
      PARAMETER (NYMG=(PEMG-PBMG+1)*IXMG)
      PARAMETER (NYSQ=(PESQ-PBSQ+1)*IXSQ)
      PARAMETER (NYTO_MG=(PETO_MG-PBTO_MG+1)*IXTO)
      PARAMETER (NYTO_OR=(PETO_OR-PBTO_OR+1)*IXTO)
C=======================================================================
      CHARACTER*(*) PATH(3)
      CHARACTER*12  BUFF107
      LOGICAL PRED(6),INDX(2),LOAD(3)
      INTEGER UNIT(3)
      INTEGER IDIM(12)
      INTEGER JDST(24)
      INTEGER BORD(IXMF),BKNO(IXMF)
      INTEGER NHMF(2),NLMF(2)
      INTEGER US(IXMF)
      DOUBLE PRECISION JMDL(3,4)
      DIMENSION DSTX(MAXYRDST*366,24)
      DIMENSION F107X(MAXYRF107,12)
      DIMENSION BKPO(NXPO)
      DIMENSION BMDL(3,7)
      DIMENSION BC(28),RLGM(15),RRGT(9),RSE(9)
      DIMENSION TSMG(2*(PSMG+1)),TSSQ(2*(PSSQ+1)),TSTO(2*(PSTO+1))
      DIMENSION TDMG(2*(PEMG+1)),TDSQ(2*(PESQ+1))
      DIMENSION TDTO(2*(PETO_MG+PETO_OR+1))
      DIMENSION GMDL(*)
      DIMENSION GAMF(NYMF)
      DIMENSION GPMG(NYMG,NSMG,2),GSMG(NYMG,NSMG,2)
      DIMENSION GPSQ(NYSQ,NSSQ,2),GSSQ(NYSQ,NSSQ)
      DIMENSION GCTO_MG(NYTO_MG,NSTO_MG,NTAY_MG+1,2)
      DIMENSION GCTO_OR(NYTO_OR,NSTO_OR,NTAY_OR+1)
      DIMENSION EPMG(NYMG),ESMG(NYMG),EPSQ(NYSQ),ESSQ(NYSQ)
      DIMENSION ECTO(NYTO_MG+NYTO_OR)
      DIMENSION HYMG(NYMG,6),HYSQ(NYSQ,6)
      DIMENSION HYTO(3*(NYTO_MG+NYTO_OR))
      DIMENSION PLEG(ILEG),RCUR(2*ILEG+4*NXMF),TRIG(2*(NXMF+1))
      DIMENSION HT(2*NYMF),HQ(6*NYMF),WS(IXMF),WB(2*(2*NXOR+NXKN+2))
C=======================================================================
      DATA DRAD /0.017453292519943D0/, DSTT /0.D0/
C=======================================================================
      IF (CERR.LT.50) THEN                                                      
         IF (LOAD(1)) THEN
            LOAD(1)=.FALSE.
            OPEN(UNIT(1),FILE=PATH(1))
            REWIND UNIT(1)                                                            
            READ(UNIT(1),1000) LSMF,LPOS,LCMF
            READ(UNIT(1),1000) LUM1
            READ(UNIT(1),1001) EPCH,RE,RP,RM
            READ(UNIT(1),1002) (BORD(J),J=1,LSMF)
            READ(UNIT(1),1002) (BKNO(J),J=1,LSMF)
            READ(UNIT(1),1001) (BKPO(J),J=1,LPOS)
            READ(UNIT(1),1001) (GAMF(J),J=1,LCMF)
            READ(UNIT(1),1000) LCMG,LSMG
            READ(UNIT(1),1000) LUM1,LUM2,LUM3,LUM4,LUM5,LUM6
            READ(UNIT(1),1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM
            READ(UNIT(1),1001) (((GPMG(I,J,K),I=1,LCMG),J=1,LSMG),K=1,2)
            READ(UNIT(1),1001) (((GSMG(I,J,K),I=1,LCMG),J=1,LSMG),K=1,2)
            READ(UNIT(1),1000) LCSQ,LSSQ
            READ(UNIT(1),1000) LUM1,LUM2,LUM3,LUM4,LUM5,LUM6
            READ(UNIT(1),1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,HION
            READ(UNIT(1),1001) (((GPSQ(I,J,K),I=1,LCSQ),J=1,LSSQ),K=1,2)
            READ(UNIT(1),1001) ((GSSQ(I,J),I=1,LCSQ),J=1,LSSQ)
            READ(UNIT(1),1000) LCTO,LSTO,LRTO
            READ(UNIT(1),1000) LUM1,LUM2,LUM3,LUM4,LUM5,LUM6,LUM7
            READ(UNIT(1),1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,RTAY_DW,
     1                         RTAY_DK
            READ(UNIT(1),1001) ((((GCTO_MG(I,J,K,L),I=1,LCTO),J=1,LSTO),
     1                         K=1,LRTO),L=1,2)
            READ(UNIT(1),1000) LCTO,LSTO,LRTO
            READ(UNIT(1),1000) LUM1,LUM2,LUM3,LUM4,LUM5,LUM6,LUM7
            READ(UNIT(1),1001) CNMP,ENMP,OMGS,OMGD,RE,RP,RM,RTAY_OR
            READ(UNIT(1),1001) (((GCTO_OR(I,J,K),I=1,LCTO),J=1,LSTO),
     1                         K=1,LRTO)
            CPOL=CNMP*DRAD
            EPOL=ENMP*DRAD
            CTMP=DCOS(CPOL)
            STMP=DSIN(CPOL)
            CEMP=DCOS(EPOL)
            SEMP=DSIN(EPOL)
            RION=RM+HION
         END IF
         IYR=INT(UT)
         FYR=UT-DBLE(IYR)
         DOY=FYR*DBLE(366-MIN(1,MOD(IYR,4)))
         IDOY=INT(DOY)
         FDOY=DOY-DBLE(IDOY)
         IDOY=IDOY+1
         MSEC=NINT(FDOY*86400000.D0)
         CALL YDTOMJDX (IYR,IDOY,MJDY,IMON,IDOM,IDIM)
         IF (GMUT) THEN
            CALL GETMUT2 (CNMP,ENMP,IYR,IDOY,MSEC,MUT)
         END IF
         CSYS=1
         IF (CORD) THEN
            CSYS=0
         END IF
         IF (INDX(1)) THEN
            IF (LOAD(2)) THEN
               LOAD(2)=.FALSE.
               OPEN(UNIT(2),FILE=PATH(2))
               REWIND UNIT(2)                                                            
               JAFT=0
   60          READ(UNIT(2),1003,END=70) JYR,JMON,JDOM,JDST
                  JYR=JYR+1900
                  IF (JYR.LT.1957) THEN
                     JYR=JYR+100
                  END IF
                  CALL YMDTOMJD (JYR,JMON,JDOM,JMJD,JDOY)
                  IF (JAFT.EQ.0) THEN
                     JAFT=1
                     MJDL=JMJD
                  END IF
                  DO J=1,24
                     DSTX(JMJD-MJDL+1,J)=DBLE(JDST(J))
                  END DO
               GO TO 60
   70          MJDH=JMJD
            END IF
            CALL INTDST (MAXYRDST,MJDL,MJDH,MJDY,MSEC,DSTX,DST,PERR,
     1                   OERR,CERR)
            IF (CERR.GT.49) GO TO 50
         END IF
         IF (INDX(2)) THEN
            IF (LOAD(3)) THEN
               LOAD(3)=.FALSE.
               OPEN(UNIT(3),FILE=PATH(3))
               REWIND UNIT(3)                                                            
               JAFT=0
   80          READ(UNIT(3),1004,END=90) BUFF107
                  IF (BUFF107(10:12).NE.'---') THEN
                     READ(BUFF107,1005) JYR,JMON,JF107
                     IF (JAFT.EQ.0) THEN
                        JAFT=1
                        IYRL=JYR
                        IMOL=JMON
                     END IF
                     F107X(JYR-IYRL+1,JMON)=DBLE(JF107)/10.D0
                  END IF
               GO TO 80
   90          IYRH=JYR
               IMOH=JMON
            END IF
            CALL INTF107 (MAXYRF107,IYRL,IMOL,IYRH,IMOH,IYR,IMON,IDOM,
     1                    IDIM,MSEC,F107X,F107,PERR,OERR,CERR)
            IF (CERR.GT.49) GO TO 50
         END IF
         CALL R8VSET (1,21,0.D0,BMDL)
         CALL R8VSET (1,12,0.D0,JMDL)
         CLAT=THETA*DRAD
         ELON=PHI*DRAD
         IF (COEF) THEN
            NOUT=1
            NYGO=0
            IF (PRED(2)) NYGO=MAX(NYGO,IXMG/2)
            IF (PRED(3)) NYGO=MAX(NYGO,IXSQ/2)
            IF (PRED(4)) NYGO=MAX(NYGO,IXTO/2)
         END IF
         IF (PRED(1)) THEN
            IGEN=1
            NMAX=MAX(NHMF(1),NHMF(2))
            NMIN=MIN(NLMF(1),NLMF(2))
            NOBO=NSHX(NMIN-1,1,NMIN-1,0)
            NOPO=I8SSUM(1,NOBO,BKNO)+2*NOBO
            CALL BFIELD (IGEN,NMAX,0,NMIN,1,NMAX,0,0,0,0,CSYS,3,2,0,
     1                   EPCH,RE,RP,RM,UT,CLAT,ELON,ALT,DST,DSTT,RSE,NZ,
     2                   MZ,RO,THETAS,US,US,BORD(NOBO+1),BKNO(NOBO+1),
     3                   BKPO(NOPO+1),US,US,US,US,WS,US,GAMF,BC,GAMF,
     4                   PLEG,RCUR,TRIG,US,WS,HT,HQ,HQ,PERR,OERR,CERR)
            IF (CERR.GT.49) GO TO 50
            NOMN=NSHX(NLMF(1)-1,1,NLMF(1)-1,0)
            NOMX=NSHX(NHMF(1)  ,1,NHMF(1)  ,0)
            NOFF=NOMN-NOBO
            NSM1=DIM(NOMX,NOMN)
            NOGA=I8SSUM(     1,NOMN,BORD)+I8SSUM(     1,NOMN,BKNO)+NOMN
            NOHQ=I8SSUM(NOBO+1,NOFF,BORD)+I8SSUM(NOBO+1,NOFF,BKNO)+NOFF
            NIMF=I8SSUM(NOMN+1,NSM1,BORD)+I8SSUM(NOMN+1,NSM1,BKNO)+NSM1
            CALL BLSGEN (NIMF,NZ,3,BMDL(1,1),GAMF(NOGA+1),HQ(NOHQ+1))
            IF (COEF) THEN
               NOPO=I8SSUM(1,NOMN,BKNO)+2*NOMN
               CALL GETGMF (NXOR,NSM1,EPCH,UT,WB,GAMF(NOGA+1),
     1                      GMDL(NOUT),BKNO(NOMN+1),BORD(NOMN+1),
     2                      BKPO(NOPO+1),PERR,OERR,CERR)
               IF (CERR.GT.49) GO TO 50
            END IF
            NOMN=NSHX(NLMF(2)-1,1,NLMF(2)-1,0)
            NOMX=NSHX(NHMF(2)  ,1,NHMF(2)  ,0)
            NOFF=NOMN-NOBO
            NSM2=DIM(NOMX,NOMN)
            NOGA=I8SSUM(     1,NOMN,BORD)+I8SSUM(     1,NOMN,BKNO)+NOMN
            NOHQ=I8SSUM(NOBO+1,NOFF,BORD)+I8SSUM(NOBO+1,NOFF,BKNO)+NOFF
            NIMF=I8SSUM(NOMN+1,NSM2,BORD)+I8SSUM(NOMN+1,NSM2,BKNO)+NSM2
            CALL BLSGEN (NIMF,NZ,3,BMDL(1,2),GAMF(NOGA+1),HQ(NOHQ+1))
            IF (COEF) THEN
               NYGO=MAX(NYGO,NSM1*(NXOR+1))
               NYGO=MAX(NYGO,NSM2*(NXOR+1))
               NOUT=NOUT+NYGO*MIN(1,NSM1)
               NOPO=I8SSUM(1,NOMN,BKNO)+2*NOMN
               CALL GETGMF (NXOR,NSM2,EPCH,UT,WB,GAMF(NOGA+1),
     1                      GMDL(NOUT),BKNO(NOMN+1),BORD(NOMN+1),
     2                      BKPO(NOPO+1),PERR,OERR,CERR)
               IF (CERR.GT.49) GO TO 50
               NOUT=NOUT+NYGO*MIN(1,NSM2)
            END IF
         END IF
         IF (PRED(2).OR.PRED(3).OR.PRED(4)) THEN
            IF (.NOT.PRED(1)) THEN
               CALL GEOCEN (CSYS,RE,RP,RM,ALT,CLAT,RO,THETAS,STHE,CTHE)
            END IF
            PSIZ=THETAS-CLAT
            CPSI=DCOS(PSIZ)
            SPSI=DSIN(PSIZ)
            RLGM(1)=-CPSI
            RLGM(2)=0.D0
            RLGM(3)=-SPSI
            RLGM(4)=0.D0
            RLGM(5)=1.D0
            RLGM(6)=0.D0
            RLGM(7)=SPSI
            RLGM(8)=0.D0
            RLGM(9)=-CPSI
            CTGO=DCOS(THETAS)
            STGO=DSIN(THETAS)
            CEGO=DCOS(ELON)
            SEGO=DSIN(ELON)
            RRGT(1)=CTGO*CEGO
            RRGT(2)=CTGO*SEGO
            RRGT(3)=-STGO
            RRGT(4)=-SEGO
            RRGT(5)=CEGO
            RRGT(6)=0.D0
            RRGT(7)=STGO*CEGO
            RRGT(8)=STGO*SEGO
            RRGT(9)=CTGO
            CALL RMERGE (RLGM,RRGT)
            RRGT(1)=CEMP*CTMP
            RRGT(2)=-SEMP
            RRGT(3)=CEMP*STMP
            RRGT(4)=SEMP*CTMP
            RRGT(5)=CEMP
            RRGT(6)=SEMP*STMP
            RRGT(7)=-STMP
            RRGT(8)=0.D0
            RRGT(9)=CTMP
            CALL RMERGE (RLGM,RRGT)
            XG=STGO*CEGO
            YG=STGO*SEGO
            ZG=CTGO
            XD=RRGT(1)*XG+RRGT(4)*YG+RRGT(7)*ZG
            YD=RRGT(2)*XG+RRGT(5)*YG+RRGT(8)*ZG
            ZD=RRGT(3)*XG+RRGT(6)*YG+RRGT(9)*ZG
            CDIP=DACOS(ZD)
            EDIP=DATAN2(YD,XD)
            CTMO=ZD
            STMO=DSIN(CDIP)
            CEMO=DCOS(EDIP)
            SEMO=DSIN(EDIP)
            RRGT(1)=-CEMO*CTMO
            RRGT(2)=-SEMO
            RRGT(3)=-CEMO*STMO
            RRGT(4)=-SEMO*CTMO
            RRGT(5)=CEMO
            RRGT(6)=-SEMO*STMO
            RRGT(7)=STMO
            RRGT(8)=0.D0
            RRGT(9)=-CTMO
            CALL RMERGE (RLGM,RRGT)
            TAUS=OMGS*UT
            TAUD=OMGD*MUT
         END IF
         IF (PRED(2)) THEN
            IGEN=1
            CALL BFIELD (IGEN,NXMG,NXMG,1,1,MXMG,MXMG,0,0,0,1,3,0,0,
     1                   EPCH,RE,RP,RM,UT,CDIP,EDIP,ALT,DST,DSTT,RSE,NU,
     2                   MU,RU,THETAS,US,US,US,US,WS,US,US,US,US,WS,US,
     3                   GSMG,BC,GSMG,PLEG,RCUR,TRIG,US,WS,HT,HQ,HQ,
     4                   PERR,OERR,CERR)
            IF (CERR.GT.49) GO TO 50
            JS=NU/2
            JY=1
            CALL TRIGMP (PSMG,TAUS,TSMG)
            CALL TRIGMP (PEMG,TAUD,TDMG)
            DO 10 P=PBMG,PEMG
               COSP=TDMG(1+P)
               SINP=TDMG(2+P+PEMG)
               CALL MPOTENT (NXMG,MXMG,NU,NYMG,COSP,SINP,HQ(   1),
     1                       HYMG(JY,1))
               CALL MPOTENT (NXMG,MXMG,NU,NYMG,COSP,SINP,HQ(JS+1),
     1                       HYMG(JY,4))
   10          JY=JY+IXMG
            BC(1)=0.D0
            BC(2)=0.D0
            BC(3)=0.D0
            CALL MSEASON (PSMG,NSMG,NYMG,DST,TSMG,EPMG,GPMG)
            CALL BLSGEN (NYMG,NYMG,3,BC,EPMG,HYMG(1,4))
            CALL LTRANS (1,1,BC,RLGM,BMDL(1,3))
            BC(1)=0.D0
            BC(2)=0.D0
            BC(3)=0.D0
            CALL MSEASON (PSMG,NSMG,NYMG,DST,TSMG,ESMG,GSMG)
            CALL BLSGEN (NYMG,NYMG,3,BC,ESMG,HYMG(1,1))
            CALL LTRANS (1,1,BC,RLGM,BMDL(1,4))
            IF (CURR) THEN
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL JTBELOW (PBMG,PEMG,NXMG,MXMG,RO,RM,NYMG,HYMG(1,1))
               CALL BLSGEN (NYMG,NYMG,3,BC,ESMG,HYMG(1,1))
               CALL LTRANS (1,1,BC,RLGM,JMDL(1,1))
            END IF
            IF (COEF) THEN
               CALL GETGXF (PBMG,PEMG,NXMG,MXMG,JS,EPMG,GMDL(NOUT),TDMG)
               NOUT=NOUT+NYGO
               CALL GETGXF (PBMG,PEMG,NXMG,MXMG,JS,ESMG,GMDL(NOUT),TDMG)
               NOUT=NOUT+NYGO
            END IF
         END IF
         IF (PRED(3)) THEN
            IGEN=1
            FSRF=1.D0+0.01485D0*F107
            IF (RO.LT.RION) THEN
               CALL BFIELD (IGEN,NXSQ,NXSQ,1,1,MXSQ,MXSQ,0,0,0,1,3,0,0,
     1                      EPCH,RE,RP,RM,UT,CDIP,EDIP,ALT,DST,DSTT,RSE,
     2                      NU,MU,RU,THETAS,US,US,US,US,WS,US,US,US,US,
     3                      WS,US,GSSQ,BC,GSSQ,PLEG,RCUR,TRIG,US,WS,HT,
     4                      HQ,HQ,PERR,OERR,CERR)
               IF (CERR.GT.49) GO TO 50
               JS=NU/2
               JY=1
               CALL TRIGMP (PSSQ,TAUS,TSSQ)
               CALL TRIGMP (PESQ,TAUD,TDSQ)
               DO 20 P=PBSQ,PESQ
                  COSP=TDSQ(1+P)
                  SINP=TDSQ(2+P+PESQ)
                  CALL MPOTENT (NXSQ,MXSQ,NU,NYSQ,COSP,SINP,HQ(   1),
     1                          HYSQ(JY,1))
                  CALL MPOTENT (NXSQ,MXSQ,NU,NYSQ,COSP,SINP,HQ(JS+1),
     1                          HYSQ(JY,4))
   20             JY=JY+IXSQ
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL ISEASON (PSSQ,NSSQ,NYSQ,FSRF,TSSQ,EPSQ,GPSQ(1,1,1))
               CALL BLSGEN (NYSQ,NYSQ,3,BC,EPSQ,HYSQ(1,4))
               CALL LTRANS (1,1,BC,RLGM,BMDL(1,5))
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL ISEASON (PSSQ,NSSQ,NYSQ,FSRF,TSSQ,ESSQ,GSSQ(1,1))
               CALL BLSGEN (NYSQ,NYSQ,3,BC,ESSQ,HYSQ(1,1))
               CALL LTRANS (1,1,BC,RLGM,BMDL(1,6))
               IF (CURR) THEN
                  BC(1)=0.D0
                  BC(2)=0.D0
                  BC(3)=0.D0
                  CALL JTABOVE (PBSQ,PESQ,NXSQ,MXSQ,RO,RION,NYSQ,
     1                          HYSQ(1,4))
                  CALL BLSGEN (NYSQ,NYSQ,3,BC,EPSQ,HYSQ(1,4))
                  CALL LTRANS (1,1,BC,RLGM,JMDL(1,2))
                  BC(1)=0.D0
                  BC(2)=0.D0
                  BC(3)=0.D0
                  CALL JTBELOW (PBSQ,PESQ,NXSQ,MXSQ,RO,RM,NYSQ,
     1                          HYSQ(1,1))
                  CALL BLSGEN (NYSQ,NYSQ,3,BC,ESSQ,HYSQ(1,1))
                  CALL LTRANS (1,1,BC,RLGM,JMDL(1,3))
               END IF
               IF (COEF) THEN
                  CALL GETGXF (PBSQ,PESQ,NXSQ,MXSQ,JS,EPSQ,GMDL(NOUT),
     1                         TDSQ)
                  NOUT=NOUT+NYGO
                  CALL GETGXF (PBSQ,PESQ,NXSQ,MXSQ,JS,ESSQ,GMDL(NOUT),
     1                         TDSQ)
                  NOUT=NOUT+NYGO
               END IF
            ELSE
               CALL BFIELD (IGEN,NXSQ,0,1,1,MXSQ,0,0,0,0,1,3,0,0,EPCH,
     1                      RE,RP,RM,UT,CDIP,EDIP,ALT,DST,DSTT,RSE,NU,
     2                      MU,RU,THETAS,US,US,US,US,WS,US,US,US,US,WS, 
     3                      US,GSSQ,BC,GSSQ,PLEG,RCUR,TRIG,US,WS,HT,HQ,
     4                      HQ,PERR,OERR,CERR)
               IF (CERR.GT.49) GO TO 50
               JY=1
               CALL TRIGMP (PSSQ,TAUS,TSSQ)
               CALL TRIGMP (PESQ,TAUD,TDSQ)
               DO 30 P=PBSQ,PESQ
                  COSP=TDSQ(1+P)
                  SINP=TDSQ(2+P+PESQ)
                  CALL MPOTENT (NXSQ,MXSQ,NU,NYSQ,COSP,SINP,HQ(   1),
     1                          HYSQ(JY,1))
   30             JY=JY+IXSQ
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL ISEASON (PSSQ,NSSQ,NYSQ,FSRF,TSSQ,EPSQ,GPSQ(1,1,2))
               CALL BLSGEN (NYSQ,NYSQ,3,BC,EPSQ,HYSQ(1,1))
               CALL LTRANS (1,1,BC,RLGM,BMDL(1,5))
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL ISEASON (PSSQ,NSSQ,NYSQ,FSRF,TSSQ,ESSQ,GSSQ(1,1))
               CALL BLSGEN (NYSQ,NYSQ,3,BC,ESSQ,HYSQ(1,1))
               CALL LTRANS (1,1,BC,RLGM,BMDL(1,6))
               IF (CURR) THEN
                  BC(1)=0.D0
                  BC(2)=0.D0
                  BC(3)=0.D0
                  CALL JTBELOW (PBSQ,PESQ,NXSQ,MXSQ,RO,RION,NYSQ,
     1                          HYSQ(1,1))
                  CALL BLSGEN (NYSQ,NYSQ,3,BC,EPSQ,HYSQ(1,1))
                  CALL LTRANS (1,1,BC,RLGM,JMDL(1,2))
                  BC(1)=0.D0
                  BC(2)=0.D0
                  BC(3)=0.D0
                  CALL JTBCONT (PBSQ,PESQ,NXSQ,MXSQ,RION,RM,NYSQ,
     1                          HYSQ(1,1))
                  CALL BLSGEN (NYSQ,NYSQ,3,BC,ESSQ,HYSQ(1,1))
                  CALL LTRANS (1,1,BC,RLGM,JMDL(1,3))
               END IF
               IF (COEF) THEN
                  CALL GETGXF (PBSQ,PESQ,NXSQ,MXSQ,NU,EPSQ,GMDL(NOUT),
     1                         TDSQ)
                  NOUT=NOUT+NYGO
                  CALL GETGXF (PBSQ,PESQ,NXSQ,MXSQ,NU,ESSQ,GMDL(NOUT),
     1                         TDSQ)
                  NOUT=NOUT+NYGO
               END IF
            END IF
         END IF
         IF (PRED(4)) THEN
            IF (PRED(5)) THEN
               IF (PRED(6)) THEN
                  PBTO=PBTO_MG
                  PETO=PETO_MG
                  NYTO=NYTO_MG
                  NSTO=NSTO_MG
                  NTAY=NTAY_MG
                  RTAY=RTAY_DW
                  OMDL=.FALSE.
                  MMDL=1
               ELSE
                  PBTO=PBTO_MG
                  PETO=PETO_MG
                  NYTO=NYTO_MG
                  NSTO=NSTO_MG
                  NTAY=NTAY_MG
                  RTAY=RTAY_DK
                  OMDL=.FALSE.
                  MMDL=2
               END IF
            ELSE
               PBTO=PBTO_OR
               PETO=PETO_OR
               NYTO=NYTO_OR
               NSTO=NSTO_OR
               NTAY=NTAY_OR
               RTAY=RTAY_OR
               OMDL=.TRUE.
               MMDL=1
            END IF
            IGEN=1
            CALL BFIELD (IGEN,NXTO,0,1,1,MXTO,0,0,0,0,1,3,0,0,EPCH,RE,
     1                   RP,RM,UT,CDIP,EDIP,0.D0,DST,DSTT,RSE,NT,MT,RT,
     2                   THETAS,US,US,US,US,WS,US,US,US,US,WS,US,
     3                   GCTO_MG,BC,GCTO_MG,PLEG,RCUR,TRIG,US,WS,HT,HQ,
     4                   HQ,PERR,OERR,CERR)
            IF (CERR.GT.49) GO TO 50
            FRTO=RM/RO
            FRHO=(RO-RTAY)/RM
            JY=1
            CALL TRIGMP (PSTO,TAUS,TSTO)
            CALL TRIGMP (PETO,TAUD,TDTO)
            DO 40 P=PBTO,PETO
               COSP=TDTO(1+P)
               SINP=TDTO(2+P+PETO)
               CALL MSTREAM (NXTO,MXTO,NT,NYTO,COSP,SINP,FRTO,HQ(1),
     1                       HYTO(JY))
   40          JY=JY+IXTO
            BC(1)=0.D0
            BC(2)=0.D0
            BC(3)=0.D0
            IF (OMDL) THEN
               CALL TSEARAD (OMDL,PSTO,NTAY,NSTO,NYTO,FRHO,TSTO,ECTO,
     1                       GCTO_OR(1,1,1))
            ELSE
               CALL TSEARAD (OMDL,PSTO,NTAY,NSTO,NYTO,FRHO,TSTO,ECTO,
     1                       GCTO_MG(1,1,1,MMDL))
            END IF
            CALL BLSGEN (NYTO,NYTO,2,BC,ECTO,HYTO(1))
            CALL LTRANS (1,1,BC,RLGM,BMDL(1,7))
            IF (COEF) THEN
               CALL GETGXF (PBTO,PETO,NXTO,MXTO,NT,ECTO,GMDL(NOUT),TDTO)
            END IF
            IF (CURR) THEN
               BC(1)=0.D0
               BC(2)=0.D0
               BC(3)=0.D0
               CALL JPOLOID (PBTO,PETO,NXTO,MXTO,RO,RM,NT,NYTO,TDTO,
     1                       HQ(1),HYTO(1))
               CALL BLSGEN (NYTO,NYTO,1,BC(3),ECTO,HYTO(2*NYTO+1))
               IF (OMDL) THEN
                  CALL TSEARDR (OMDL,PSTO,NTAY,NSTO,NYTO,FRHO,TSTO,ECTO,
     1                          GCTO_OR(1,1,1))
               ELSE
                  CALL TSEARDR (OMDL,PSTO,NTAY,NSTO,NYTO,FRHO,TSTO,ECTO,
     1                          GCTO_MG(1,1,1,MMDL))
               END IF
               CALL BLSGEN (NYTO,NYTO,2,BC(1),ECTO,HYTO(1))
               CALL LTRANS (1,1,BC,RLGM,JMDL(1,4))
            END IF
         END IF
      END IF                                                                    
   50 RETURN                                                                    
 1000 FORMAT(10I8)
 1001 FORMAT(4E22.15)
 1002 FORMAT(22I4)
 1003 FORMAT(3X,2I2,1X,I2,10X,24I4)
 1004 FORMAT(A12)
 1005 FORMAT(I4,1X,I2,1X,I4)
      END                                                                       
      SUBROUTINE YMDTOMJD (YEARAD,MONTH,DAYOFMONTH,MJD,DAYOFYEAR)
      INTEGER YEARAD,DAYOFMONTH,DAYOFYEAR,REMYEAR
      INTEGER DAYSUPTOMONTH(12)
      DATA DAYSUPTOMONTH /0,31,59,90,120,151,181,212,243,273,304,334/
      REMYEAR=YEARAD-1900
      IF (REMYEAR.GT.0) THEN
         REMYEAR=REMYEAR-1
         MJD=1461*(REMYEAR/4)+15384
         REMYEAR=MOD(REMYEAR,4)
         DAYOFYEAR=DAYSUPTOMONTH(MONTH)+DAYOFMONTH
         IF (MONTH.GT.2) DAYOFYEAR=DAYOFYEAR+DIM(REMYEAR,2)
         MJD=MJD+365*REMYEAR+DAYOFYEAR
      ELSE
         DAYOFYEAR=DAYSUPTOMONTH(MONTH)+DAYOFMONTH
         MJD=15019+DAYOFYEAR
      END IF
      RETURN
      END
      SUBROUTINE YDTOMJDX (YEARAD,DAYOFYEAR,MJD,MONTH,DAYOFMONTH,
     1                     DAYSINMONTH)
      INTEGER YEARAD,DAYOFMONTH,DAYOFYEAR,REMYEAR
      INTEGER DAYSUPTOMONTH(12),DAYSINMONTH(12)
      DATA DAYSUPTOMONTH /0,31,59,90,120,151,181,212,243,273,304,334/
      REMYEAR=YEARAD-1900
      IF (REMYEAR.GT.0) THEN
         REMYEAR=REMYEAR-1
         MJD=1461*(REMYEAR/4)+15384
         REMYEAR=MOD(REMYEAR,4)
         MJD=MJD+365*REMYEAR+DAYOFYEAR
         LEAPDAY=DIM(REMYEAR,2)
      ELSE
         MJD=15019+DAYOFYEAR
         LEAPDAY=0
      END IF
      DO J=12,1,-1
         LEAPCUM=MIN(1,DIM(J,2))*LEAPDAY
         IF (DAYSUPTOMONTH(J)+LEAPCUM.LT.DAYOFYEAR) THEN
            MONTH=J
            DAYOFMONTH=DAYOFYEAR-DAYSUPTOMONTH(J)-LEAPCUM
            GO TO 10
         END IF
      END DO
   10 DAYSINMONTH(1 )=31
      DAYSINMONTH(2 )=28+LEAPDAY
      DAYSINMONTH(3 )=31
      DAYSINMONTH(4 )=30
      DAYSINMONTH(5 )=31
      DAYSINMONTH(6 )=30
      DAYSINMONTH(7 )=31
      DAYSINMONTH(8 )=31
      DAYSINMONTH(9 )=30
      DAYSINMONTH(10)=31
      DAYSINMONTH(11)=30
      DAYSINMONTH(12)=31
      RETURN                                                                    
      END                                                                       
      SUBROUTINE INTDST (MXYR,MJDL,MJDH,MJDY,MSEC,DSTX,DST,PERR,OERR,
     1                   CERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER PERR,OERR,CERR
      INTEGER HOUR,HTOP,HBOT
      DIMENSION DSTX(MXYR*366,24)
      HOUR=MSEC/3600000
      MJDT=MJDY+HOUR/24
      HOUR=MOD(HOUR,24)+1
      MOBS=MOD(MSEC,3600000)
      IF (MOBS.LE.1800000) THEN
         TTOP=DBLE(MOBS+1800000)/3600000.D0
         IF (HOUR.GT.1) THEN
            JTOP=MJDT
            JBOT=MJDT
            HTOP=HOUR
            HBOT=HOUR-1
         ELSE
            JTOP=MJDT
            JBOT=MJDT-1
            HTOP=1
            HBOT=24
         END IF
      ELSE
         TTOP=DBLE(MOBS-1800000)/3600000.D0
         IF (HOUR.LT.24) THEN
            JTOP=MJDT
            JBOT=MJDT
            HTOP=HOUR+1
            HBOT=HOUR
         ELSE
            JTOP=MJDT+1
            JBOT=MJDT
            HTOP=1
            HBOT=24
         END IF
      END IF
      IF ((JBOT.LT.MJDL).OR.(JTOP.GT.MJDH)) THEN
         CERR=50
         IF (PERR.NE.0) WRITE(OERR,1050) CERR
         DST=-1.D12
      ELSE
         DST=TTOP       *DSTX(JTOP-MJDL+1,HTOP)+
     1       (1.D0-TTOP)*DSTX(JBOT-MJDL+1,HBOT)
      END IF
      RETURN                                                                    
 1050 FORMAT(' ',/,1X,'SUBROUTINE INTDST -- ERROR CODE ',I2,' -- T LIES 
     1OUTSIDE OF DST TABLE TIME SPAN -- ABORT',/,' ')
      END                                                                       
      SUBROUTINE INTF107 (MXYR,IYRL,IMOL,IYRH,IMOH,IYR,IMON,IDOM,IDIM,
     1                    MSEC,F107X,F107,PERR,OERR,CERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER PERR,OERR,CERR,YTOP,YBOT
      INTEGER IDIM(12)
      DIMENSION F107X(MXYR,12)
      THLF=0.5D0*DBLE(IDIM(IMON))
      TOBS=DBLE(IDOM-1)+DBLE(MSEC)/86400000.D0
      IF (TOBS.LE.THLF) THEN
         IF (IMON.GT.1) THEN
            TADD=0.5D0*DBLE(IDIM(IMON-1))
            TOBS=TOBS+TADD
            TINT=THLF+TADD
            TTOP=TOBS/TINT
            YTOP=IYR
            YBOT=IYR
            MTOP=IMON
            MBOT=IMON-1
         ELSE
            TOBS=TOBS+15.5D0
            TINT=THLF+15.5D0
            TTOP=TOBS/TINT
            YTOP=IYR
            YBOT=IYR-1
            MTOP=1
            MBOT=12
         END IF
      ELSE
         IF (IMON.LT.12) THEN
            TADD=0.5D0*DBLE(IDIM(IMON+1))
            TOBS=TOBS-THLF
            TINT=THLF+TADD
            TTOP=TOBS/TINT
            YTOP=IYR
            YBOT=IYR
            MTOP=IMON+1
            MBOT=IMON
         ELSE
            TOBS=TOBS-15.5D0
            TINT=THLF+15.5D0
            TTOP=TOBS/TINT
            YTOP=IYR+1
            YBOT=IYR
            MTOP=1
            MBOT=12
         END IF
      END IF
      IF ((YBOT.LT.IYRL)                     .OR.
     1    (YTOP.GT.IYRH)                     .OR.
     2    ((YBOT.EQ.IYRL).AND.(MBOT.LT.IMOL)).OR.
     3    ((YTOP.EQ.IYRH).AND.(MTOP.GT.IMOH)))
     4THEN
         CERR=50
         IF (PERR.NE.0) WRITE(OERR,1050) CERR
         F107=-1.D0
      ELSE
         F107=TTOP       *F107X(YTOP-IYRL+1,MTOP)+
     1        (1.D0-TTOP)*F107X(YBOT-IYRL+1,MBOT)
      END IF                                                                    
      RETURN                                                                    
 1050 FORMAT(' ',/,1X,'SUBROUTINE INTF107 -- ERROR CODE ',I2,' -- T LIES
     1 OUTSIDE OF F10.7 TABLE TIME SPAN -- ABORT',/,' ')
      END                                                                       
      SUBROUTINE GETMUT2 (THENMP,PHINMP,IYEAR,IDAY,MSEC,MUT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION MUT
      DATA DR /0.017453292519943D0/
      THE0=THENMP*DR
      PHI0=PHINMP*DR
      CTH0=DCOS(THE0)
      STH0=DSIN(THE0)
      GMTS=DBLE(MSEC)/1000.D0
      CALL SUN2 (IYEAR,IDAY,GMTS,GST,SLONG,SRASN,SDEC)
      THES=(90.D0-SDEC)*DR
      CTHS=DCOS(THES)
      STHS=DSIN(THES)
      PHIS=(SRASN-GST)*DR
      CPHD=DCOS(PHIS-PHI0)
      SPHD=DSIN(PHIS-PHI0)
      EOPP=STHS*SPHD
      EADJ=CTH0*STHS*CPHD-STH0*CTHS
      MUT=12.D0-DATAN2(EOPP,EADJ)/DR/15.D0
      RETURN
      END
      SUBROUTINE SUN2 (IYR,IDAY,SECS,GST,SLONG,SRASN,SDEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA RAD /0.572957795130833D+02/
      IF ((IYR.LT.1901).OR.(IYR.GT.2099)) THEN
         GST=0.D0
         SLONG=0.D0
         SRASN=0.D0
         SDEC=0.D0
      ELSE
         FDAY   =SECS/86400.D0
         DJ     =365.D0*DBLE(IYR-1900)+DBLE((IYR-1901)/4)+DBLE(IDAY)+
     1           FDAY-0.5D0
         T      =DJ/36525.D0
         VL     =DMOD(279.696678D0+0.9856473354D0*DJ,360.D0)
         GST    =DMOD(279.690983D0+0.9856473354D0*DJ+360.D0*FDAY+180.D0,
     1           360.D0)
         G      =DMOD(358.475845D0+0.985600267D0*DJ,360.D0)/RAD
         SLONG  =VL+(1.91946D0-0.004789D0*T)*DSIN(G)+0.020094D0*
     1           DSIN(2.D0*G)
         OBLIQ  =(23.45229D0-0.0130125D0*T)/RAD
         SLP    =(SLONG-0.005686D0)/RAD
         SINED  =DSIN(OBLIQ)*DSIN(SLP)
         COSINED=DSQRT(1.D0-SINED**2)
         SDEC   =RAD*DATAN(SINED/COSINED)
         SRASN  =180.D0-RAD*DATAN2(1.D0/DTAN(OBLIQ)*SINED/COSINED,
     1           -COS(SLP)/COSINED)
      END IF
      RETURN
      END
      SUBROUTINE RMERGE (RMRG,RMLT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION RMRG(*),RMLT(*)
      R1=RMRG(1)
      R2=RMRG(2)
      R3=RMRG(3)
      RMRG(1)=R1*RMLT(1)+R2*RMLT(4)+R3*RMLT(7)
      RMRG(2)=R1*RMLT(2)+R2*RMLT(5)+R3*RMLT(8)
      RMRG(3)=R1*RMLT(3)+R2*RMLT(6)+R3*RMLT(9)
      R1=RMRG(4)
      R2=RMRG(5)
      R3=RMRG(6)
      RMRG(4)=R1*RMLT(1)+R2*RMLT(4)+R3*RMLT(7)
      RMRG(5)=R1*RMLT(2)+R2*RMLT(5)+R3*RMLT(8)
      RMRG(6)=R1*RMLT(3)+R2*RMLT(6)+R3*RMLT(9)
      R1=RMRG(7)
      R2=RMRG(8)
      R3=RMRG(9)
      RMRG(7)=R1*RMLT(1)+R2*RMLT(4)+R3*RMLT(7)
      RMRG(8)=R1*RMLT(2)+R2*RMLT(5)+R3*RMLT(8)
      RMRG(9)=R1*RMLT(3)+R2*RMLT(6)+R3*RMLT(9)
      RETURN
      END
      SUBROUTINE TSEARAD (FULL,KS,KR,NS,NG,F,T,E,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      LOGICAL FULL
      DIMENSION E(NG),G(NG,NS,KR+1),T(*)
      CALL R8VSET (1,NG,0.D0,E)
      J=1
      S=1.D0
      CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
      DO 10 I=1,KS
         J=J+1
         S=T(I+1)
         CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
         IF (FULL) THEN
            J=J+1
            S=T(I+KS+2)
            CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
         END IF
   10 CONTINUE
      Z=1.D0
      DO 20 K=1,KR
         J=1
         Z=Z*F/DBLE(K)
         CALL R8VLINKT (1,1,NG,Z,G(1,J,K+1),E)
         DO 20 I=1,KS
            J=J+1
            S=T(I+1)*Z
            CALL R8VLINKT (1,1,NG,S,G(1,J,K+1),E)
            IF (FULL) THEN
               J=J+1
               S=T(I+KS+2)*Z
               CALL R8VLINKT (1,1,NG,S,G(1,J,K+1),E)
            END IF
   20 CONTINUE
      RETURN
      END
      SUBROUTINE TSEARDR (FULL,KS,KR,NS,NG,F,T,E,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      LOGICAL FULL
      DIMENSION E(NG),G(NG,NS,KR+1),T(*)
      CALL R8VSET (1,NG,0.D0,E)
      Z=1.D0
      DO 10 K=1,KR
         J=1
         CALL R8VLINKT (1,1,NG,Z,G(1,J,K+1),E)
         DO 20 I=1,KS
            J=J+1
            S=T(I+1)*Z
            CALL R8VLINKT (1,1,NG,S,G(1,J,K+1),E)
            IF (FULL) THEN
               J=J+1
               S=T(I+KS+2)*Z
               CALL R8VLINKT (1,1,NG,S,G(1,J,K+1),E)
            END IF
   20    CONTINUE
   10    Z=Z*F/DBLE(K)
      RETURN
      END
      SUBROUTINE MSEASON (KS,NS,NG,D,T,E,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION E(NG),G(NG,NS,2),T(*)
      CALL R8VSET (1,NG,0.D0,E)
      J=1
      S=1.D0
      CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
      CALL R8VLINKT (1,1,NG,D,G(1,J,2),E)
      DO 10 I=1,KS
         J=J+1
         S=T(I+1)
         CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
         S=S*D
         CALL R8VLINKT (1,1,NG,S,G(1,J,2),E)
         J=J+1
         S=T(I+KS+2)
         CALL R8VLINKT (1,1,NG,S,G(1,J,1),E)
         S=S*D
   10    CALL R8VLINKT (1,1,NG,S,G(1,J,2),E)
      RETURN
      END
      SUBROUTINE ISEASON (KS,NS,NG,F,T,E,G)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION E(NG),G(NG,NS),T(*)
      CALL R8VSET (1,NG,0.D0,E)
      J=1
      CALL R8VLINKT (1,1,NG,F,G(1,J),E)
      DO 10 I=1,KS
         J=J+1
         S=F*T(I+1)
         CALL R8VLINKT (1,1,NG,S,G(1,J),E)
         J=J+1
         S=F*T(I+KS+2)
   10    CALL R8VLINKT (1,1,NG,S,G(1,J),E)
      RETURN
      END
      SUBROUTINE MPOTENT (NMAX,MMAX,ND,NZ,CPHI,SPHI,D,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION D(*),Z(NZ,*)
      ND2=ND+ND
      ID=0
      IZ=0
      DO 10 N=1,NMAX
         ID=ID+1
         IZ=IZ+1
         Z(IZ,1)=D(    ID)*CPHI
         Z(IZ,2)=D(ND +ID)*CPHI
         Z(IZ,3)=D(ND2+ID)*CPHI
         IZ=IZ+1
         Z(IZ,1)=D(    ID)*SPHI
         Z(IZ,2)=D(ND +ID)*SPHI
         Z(IZ,3)=D(ND2+ID)*SPHI
         DO 10 M=1,MIN(N,MMAX)
            ID=ID+2
            IZ=IZ+1
            Z(IZ,1)=D(    ID-1)*CPHI+D(    ID)*SPHI
            Z(IZ,2)=D(ND +ID-1)*CPHI+D(ND +ID)*SPHI
            Z(IZ,3)=D(ND2+ID-1)*CPHI+D(ND2+ID)*SPHI
            IZ=IZ+1
            Z(IZ,1)=D(    ID)*CPHI-D(    ID-1)*SPHI
            Z(IZ,2)=D(ND +ID)*CPHI-D(ND +ID-1)*SPHI
            Z(IZ,3)=D(ND2+ID)*CPHI-D(ND2+ID-1)*SPHI
            IZ=IZ+1
            Z(IZ,1)=D(    ID-1)*CPHI-D(    ID)*SPHI
            Z(IZ,2)=D(ND +ID-1)*CPHI-D(ND +ID)*SPHI
            Z(IZ,3)=D(ND2+ID-1)*CPHI-D(ND2+ID)*SPHI
            IZ=IZ+1
            Z(IZ,1)=D(    ID)*CPHI+D(    ID-1)*SPHI
            Z(IZ,2)=D(ND +ID)*CPHI+D(ND +ID-1)*SPHI
   10       Z(IZ,3)=D(ND2+ID)*CPHI+D(ND2+ID-1)*SPHI
      RETURN
      END
      SUBROUTINE JTABOVE (PMIN,PMAX,NMAX,MMAX,R,RREF,NZ,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PMIN,PMAX,P
      DIMENSION Z(NZ,*)
      DATA FGEO /0.795774715459478D-03/
      FRMU=RREF/R
      IZ=0
      DO 10 P=PMIN,PMAX
         FRPW=FGEO
         DO 10 N=1,NMAX
            FFAC= FRPW*DBLE(N+N+1)
            FCUR= FFAC/DBLE(N+1)
            FPSI=-RREF*FFAC/DBLE(N*(N+1))
            IZ=IZ+1
            ZTEMP  = Z(IZ,1)
            Z(IZ,1)=-FCUR*Z(IZ,2)
            Z(IZ,2)= FCUR*ZTEMP
            Z(IZ,3)= FPSI*Z(IZ,3)
            IZ=IZ+1
            ZTEMP  = Z(IZ,1)
            Z(IZ,1)=-FCUR*Z(IZ,2)
            Z(IZ,2)= FCUR*ZTEMP
            Z(IZ,3)= FPSI*Z(IZ,3)
            DO 20 M=1,MIN(N,MMAX)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)=-FCUR*Z(IZ,2)
               Z(IZ,2)= FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)=-FCUR*Z(IZ,2)
               Z(IZ,2)= FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)=-FCUR*Z(IZ,2)
               Z(IZ,2)= FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)=-FCUR*Z(IZ,2)
               Z(IZ,2)= FCUR*ZTEMP
   20          Z(IZ,3)= FPSI*Z(IZ,3)
   10       FRPW=FRPW*FRMU
      RETURN
      END
      SUBROUTINE JTBELOW (PMIN,PMAX,NMAX,MMAX,R,RREF,NZ,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PMIN,PMAX,P
      DIMENSION Z(NZ,*)
      DATA FGEO /0.795774715459478D-03/
      FRMU=R/RREF
      FRBG=FGEO*FRMU**3
      IZ=0
      DO 10 P=PMIN,PMAX
         FRPW=FRBG
         DO 10 N=1,NMAX
            FFAC= FRPW*DBLE(N+N+1)
            FCUR= FFAC/DBLE(N)
            FPSI=-RREF*FFAC/DBLE(N*(N+1))
            IZ=IZ+1
            ZTEMP  =Z(IZ,1)
            Z(IZ,1)= FCUR*Z(IZ,2)
            Z(IZ,2)=-FCUR*ZTEMP
            Z(IZ,3)= FPSI*Z(IZ,3)
            IZ=IZ+1
            ZTEMP  =Z(IZ,1)
            Z(IZ,1)= FCUR*Z(IZ,2)
            Z(IZ,2)=-FCUR*ZTEMP
            Z(IZ,3)= FPSI*Z(IZ,3)
            DO 20 M=1,MIN(N,MMAX)
               IZ=IZ+1
               ZTEMP  =Z(IZ,1)
               Z(IZ,1)= FCUR*Z(IZ,2)
               Z(IZ,2)=-FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  =Z(IZ,1)
               Z(IZ,1)= FCUR*Z(IZ,2)
               Z(IZ,2)=-FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  =Z(IZ,1)
               Z(IZ,1)= FCUR*Z(IZ,2)
               Z(IZ,2)=-FCUR*ZTEMP
               Z(IZ,3)= FPSI*Z(IZ,3)
               IZ=IZ+1
               ZTEMP  =Z(IZ,1)
               Z(IZ,1)= FCUR*Z(IZ,2)
               Z(IZ,2)=-FCUR*ZTEMP
   20          Z(IZ,3)= FPSI*Z(IZ,3)
   10       FRPW=FRPW*FRMU
      RETURN
      END
      SUBROUTINE JTBCONT (PMIN,PMAX,NMAX,MMAX,ROLD,RNEW,NZ,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PMIN,PMAX,P
      DIMENSION Z(NZ,*)
      FRMU=ROLD/RNEW
      FRBG=FRMU**2
      IZ=0
      DO 10 P=PMIN,PMAX
         FPSI=FRBG
         DO 10 N=1,NMAX
            FCUR=FPSI*FRMU
            IZ=IZ+1
            Z(IZ,1)=FCUR*Z(IZ,1)
            Z(IZ,2)=FCUR*Z(IZ,2)
            Z(IZ,3)=FPSI*Z(IZ,3)
            IZ=IZ+1
            Z(IZ,1)=FCUR*Z(IZ,1)
            Z(IZ,2)=FCUR*Z(IZ,2)
            Z(IZ,3)=FPSI*Z(IZ,3)
            DO 20 M=1,MIN(N,MMAX)
               IZ=IZ+1
               Z(IZ,1)=FCUR*Z(IZ,1)
               Z(IZ,2)=FCUR*Z(IZ,2)
               Z(IZ,3)=FPSI*Z(IZ,3)
               IZ=IZ+1
               Z(IZ,1)=FCUR*Z(IZ,1)
               Z(IZ,2)=FCUR*Z(IZ,2)
               Z(IZ,3)=FPSI*Z(IZ,3)
               IZ=IZ+1
               Z(IZ,1)=FCUR*Z(IZ,1)
               Z(IZ,2)=FCUR*Z(IZ,2)
               Z(IZ,3)=FPSI*Z(IZ,3)
               IZ=IZ+1
               Z(IZ,1)=FCUR*Z(IZ,1)
               Z(IZ,2)=FCUR*Z(IZ,2)
   20          Z(IZ,3)=FPSI*Z(IZ,3)
   10       FPSI=FPSI*FRMU
      RETURN
      END
      SUBROUTINE MSTREAM (NMAX,MMAX,ND,NZ,CPHI,SPHI,FAOR,D,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION D(*),Z(NZ,*)
      ID=0
      IZ=0
      DO 10 N=1,NMAX
         ID=ID+1
         IZ=IZ+1
         Z(IZ,1)= FAOR*D(ND+ID)*CPHI
         Z(IZ,2)=-FAOR*D(   ID)*CPHI
         IZ=IZ+1
         Z(IZ,1)= FAOR*D(ND+ID)*SPHI
         Z(IZ,2)=-FAOR*D(   ID)*SPHI
         DO 10 M=1,MIN(N,MMAX)
            ID=ID+2
            IZ=IZ+1
            Z(IZ,1)= FAOR*(D(ND+ID-1)*CPHI+D(ND+ID)*SPHI)
            Z(IZ,2)=-FAOR*(D(   ID-1)*CPHI+D(   ID)*SPHI)
            IZ=IZ+1
            Z(IZ,1)= FAOR*(D(ND+ID)*CPHI-D(ND+ID-1)*SPHI)
            Z(IZ,2)=-FAOR*(D(   ID)*CPHI-D(   ID-1)*SPHI)
            IZ=IZ+1
            Z(IZ,1)= FAOR*(D(ND+ID-1)*CPHI-D(ND+ID)*SPHI)
            Z(IZ,2)=-FAOR*(D(   ID-1)*CPHI-D(   ID)*SPHI)
            IZ=IZ+1
            Z(IZ,1)= FAOR*(D(ND+ID)*CPHI+D(ND+ID-1)*SPHI)
   10       Z(IZ,2)=-FAOR*(D(   ID)*CPHI+D(   ID-1)*SPHI)
      RETURN
      END
      SUBROUTINE JPOLOID (PMIN,PMAX,NMAX,MMAX,R,RM,ND,NZ,T,D,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PMIN,PMAX,P
      DIMENSION D(*),T(*),Z(NZ,*)
      DATA U0 /0.12566370614359158D-02/
      ND2=ND+ND
      FHTJ=1.D0/RM/U0
      FRTJ=RM/R**2/U0
      IZ=0
      DO 10 P=PMIN,PMAX
         ID=0
         COSP=T(1+P)
         SINP=T(2+P+PMAX)
         DO 10 N=1,NMAX
            FDEG=FRTJ*DBLE(N)
            ID=ID+1
            IZ=IZ+1
            ZTEMP  = Z(IZ,1)
            Z(IZ,1)= FHTJ*Z(IZ,2)
            Z(IZ,2)=-FHTJ*ZTEMP
            Z(IZ,3)= FDEG*D(ND2+ID)*COSP
            IZ=IZ+1
            ZTEMP  = Z(IZ,1)
            Z(IZ,1)= FHTJ*Z(IZ,2)
            Z(IZ,2)=-FHTJ*ZTEMP
            Z(IZ,3)= FDEG*D(ND2+ID)*SINP
            DO 10 M=1,MIN(N,MMAX)
               ID=ID+2
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)= FHTJ*Z(IZ,2)
               Z(IZ,2)=-FHTJ*ZTEMP
               Z(IZ,3)= FDEG*(D(ND2+ID-1)*COSP+D(ND2+ID)*SINP)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)= FHTJ*Z(IZ,2)
               Z(IZ,2)=-FHTJ*ZTEMP
               Z(IZ,3)= FDEG*(D(ND2+ID)*COSP-D(ND2+ID-1)*SINP)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)= FHTJ*Z(IZ,2)
               Z(IZ,2)=-FHTJ*ZTEMP
               Z(IZ,3)= FDEG*(D(ND2+ID-1)*COSP-D(ND2+ID)*SINP)
               IZ=IZ+1
               ZTEMP  = Z(IZ,1)
               Z(IZ,1)= FHTJ*Z(IZ,2)
               Z(IZ,2)=-FHTJ*ZTEMP
   10          Z(IZ,3)= FDEG*(D(ND2+ID)*COSP+D(ND2+ID-1)*SINP)
      RETURN
      END
      SUBROUTINE BLSGEN (NC,ND,NI,B,C,DLDC)                                       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      DIMENSION B(*),C(*),DLDC(ND,*)
      DO 10 J=1,NI
   10    B(J)=B(J)+R8SDOT(1,1,NC,DLDC(1,J),C)                                        
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GETGMF (NDER,NS,EP,TM,B,C,G,H,O,P,PERR,OERR,CERR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      INTEGER PERR,OERR,CERR,D,Y
      INTEGER H(*),O(*)
      DIMENSION B(*),C(*),G(*),P(*)
      DATA NULL /0/
      IC=1
      IK=1                                                                      
      DO 10 I=1,NS                                                              
         G(I)=C(IC)
         IG=I
         DO 20 J=1,NDER
            IG=IG+NS
   20       G(IG)=0.D0
         N=O(I)                                                              
         IF (N.GT.0) THEN
            K=H(I)                                                              
            IF ((TM.LT.P(IK)).OR.(TM.GT.P(IK+K+1))) THEN                             
               CERR=56
               IF (PERR.NE.0) WRITE(OERR,1056) CERR
               GO TO 60
            END IF                                                                    
            M=N+1                                                               
            NK=K+2
            NB=N+K
            NI=NB+1
            NO=NI+1                                                               
            NW=NO+NO
            CALL R8SLT (1,NK,EP,P(IK),Y)                                            
            LA=MIN(NK,Y+1)                                                         
            CALL R8SLT (1,NK,TM,P(IK),Y)                                            
            LB=MIN(NK,Y+1)                                                         
            CALL R8VSET (1,NW,0.D0,B)                                          
            CALL DBSPLN (LA,EP,M,NULL,K,P(IK),B(   LA-1),B(NW+1))
            CALL DBSPLN (LB,TM,M,NULL,K,P(IK),B(NO+LB-1),B(NW+1))
            CALL R8VSUB (1,NO+1,1,NO,B,B,B)                               
            BSUM=0.D0
            GM=0.D0
            JC=IC+NB
            DO 30 J=NI,2,-1
               BSUM=BSUM+B(J)
               IM=MIN(J,NK)                                                      
               JM=MAX(J-N,1)                                                    
               GM=GM+(P(IK+IM-1)-P(IK+JM-1))*BSUM*C(JC)
   30          JC=JC-1
            G(I)=G(I)+GM/DBLE(N)
            JS=IC+LB-1
            IG=I
            DO 40 D=0,NDER-1
               IG=IG+NS
               CALL DBSPLN (LB,TM,N,D,K,P(IK),B(1),B(NW+1))
               GD=0.D0
               JC=JS
               DO 50 J=1,N
                  GD=GD+B(J)*C(JC)
   50             JC=JC+1
   40          G(IG)=GD
            IC=IC+NB
            IK=IK+NK                                                             
         END IF
   10    IC=IC+1
   60 RETURN                                                                    
 1056 FORMAT(' ',/,1X,'SUBROUTINE GETGMF -- ERROR CODE ',I2,' -- T LIES         
     1OUTSIDE OF KNOT DOMAIN -- ABORT',/,' ')                                   
      END                                                                       
      SUBROUTINE DBSPLN (L,T,N,D,K,X,B,W)                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      INTEGER POSB,POSX,POSW,D,Q,Y
      DIMENSION B(*),X(*),W(*)                                                       
      Q=N-D                                                                  
      IF (Q.EQ.1) THEN                                                       
         B(1)=1.D0                                                            
      ELSE                                                                   
         M=L                                                                 
         IK=MIN(M,K+2)                                                       
         JK=MAX(M-1,1)                                                       
         DIFI=X(IK)-T                                                        
         DELX=X(IK)-X(JK)                                                    
         IF (DELX.EQ.0.D0) THEN                                               
            B(Q)=0.D0                                                         
         ELSE                                                                
            B(Q)=1.D0/DELX                                                    
         END IF                                                              
         POSB=Q-1                                                            
         DO 10 J=2,Q                                                         
            JK=MAX(M-J,1)                                                    
            TEMP=DIFI*B(POSB+1)                                              
            DELX=X(IK)-X(JK)                                                 
            IF (DELX.EQ.0.D0) THEN                                            
               TEMP=0.D0                                                      
            ELSE                                                             
               IF (J.LT.N) THEN                                              
                  TEMP=TEMP/DELX                                             
               END IF                                                        
            END IF                                                           
            B(POSB)=TEMP                                                     
   10       POSB=POSB-1                                                      
         B(Q+1)=0.D0                                                          
         M=M+1                                                               
         DO 20 I=2,Q                                                         
            IK=MIN(M,K+2)                                                    
            DIFI=X(IK)-T                                                     
            POSB=Q                                                           
            DO 30 J=I,Q                                                      
               JK=MAX(M-J,1)                                                 
               DIFJ=T-X(JK)                                                  
               TEMP=DIFJ*B(POSB)+DIFI*B(POSB+1)                              
               DELX=X(IK)-X(JK)                                              
               IF (DELX.EQ.0.D0) THEN                                         
                  TEMP=0.D0                                                   
               ELSE                                                          
                  IF (J.LT.N) THEN                                           
                     TEMP=TEMP/DELX                                          
                  END IF                                                     
               END IF                                                        
               B(POSB)=TEMP                                                  
   30          POSB=POSB-1                                                   
   20       M=M+1                                                            
      END IF                                                                 
      POSX=L+N-1                                                             
      POSW=D+N                                                               
      DO 40 I=1,N                                                            
         LENB=MIN(Q,POSW-D)                                                  
         CALL R8VSET (1,POSW,0.D0,W)                                          
         CALL R8VGATHP (1,1,D+1,LENB,B,W)                                    
         DO 50 J=1,D                                                         
            IK=POSX                                                          
            JK=POSX-Q-J                                                      
            IW=POSW                                                          
            FN=DBLE(Q+J-1)                                                   
            DO 50 M=J,D                                                      
               IF (J.LT.D) THEN                                              
                  DELX=X(MAX(1,MIN(IK,K+2)))-X(MAX(JK,1))
                  IF (DELX.EQ.0.D0) THEN                                      
                     W(IW)=0.D0                                               
                  ELSE                                                       
                     W(IW)=FN*(W(IW-1)-W(IW))/DELX                           
                  END IF                                                     
               ELSE                                                          
                  W(IW)=FN*(W(IW-1)-W(IW))                                   
               END IF                                                        
               IK=IK-1                                                       
               JK=JK-1                                                       
   50          IW=IW-1                                                       
         POSX=POSX-1                                                         
   40    POSW=POSW-1                                                         
      CALL R8VGATHP (D+1,1,1,N,W,B)                                          
      RETURN                                                                    
      END                                                                       
      SUBROUTINE GETGXF (PMIN,PMAX,NMAX,MMAX,NG,E,G,T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PMIN,PMAX,P
      DIMENSION E(*),G(*),T(*)
      CALL R8VSET (1,NG,0.D0,G)
      IE=0
      DO 10 P=PMIN,PMAX
         IG=1
         COSP=T(1+P)
         SINP=T(2+P+PMAX)
         DO 10 N=1,NMAX
            G(IG)=G(IG)+E(IE+1)*COSP+E(IE+2)*SINP
            IG=IG+1
            IE=IE+2
            DO 10 M=1,MIN(N,MMAX)
               G(IG)=G(IG)+(E(IE+1)+E(IE+3))*COSP+(E(IE+4)-E(IE+2))*SINP
               IG=IG+1
               G(IG)=G(IG)+(E(IE+4)+E(IE+2))*COSP+(E(IE+1)-E(IE+3))*SINP
               IG=IG+1
   10          IE=IE+4
      RETURN
      END
      SUBROUTINE BFIELD (RGEN,NMXI,NMXE,NMNI,NMNE,MMXI,MMXE,MMNI,MMNE,          
     1                   GRAD,CTYP,DTYP,ITYP,ETYP,EP,RE,RP,RM,TM,CLAT,          
     2                   ELON,H,DST,DSTT,RSE,NC,NA,RO,THETA,ATYP,DSTI,          
     3                   BORI,BKNI,BKPI,TDGI,DSTE,BORE,BKNE,BKPE,TDGE,          
     4                   A,B,C,P,R,T,U,W,DSDC,DLDC,DLDA,PERR,OERR,CERR)         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,RGEN,CTYP,DTYP,ETYP,GRAD                           
      INTEGER ATYP(*),BORE(*),BORI(*),BKNE(*),BKNI(*),DSTE(*),DSTI(*)           
      INTEGER TDGE(*),TDGI(*),U(*)                                              
      DIMENSION BKPE(*),BKPI(*),DLDA(*),DLDC(*),DSDC(*),RSE(*),A(*),B(*)
      DIMENSION C(*),P(*),R(*),T(*),W(*)                                                  
      IF (CERR.LE.49) THEN                                                      
         PHI=ELON                                                               
         CALL PREBF (RGEN,ITYP,ETYP,DTYP,GRAD,NMNI,NMXI,NMNE,NMXE,MMNI,         
     1               MMXI,MMNE,MMXE,NMAX,MMIN,MMAX,NS,NSI,NSE,NC,NCI,           
     2               NCE,NA,NP,II,IE,ATYP,DSTI,BORI,BKNI,TDGI,DSTE,BORE,        
     3               BKNE,TDGE,U,PERR,OERR,CERR)                                
         IF (CERR.GE.50) GO TO 10                                               
         IF (NC.GT.0) THEN                                                      
            CALL FDSDC (RGEN,ITYP,ETYP,NSI,NSE,NC,NCI,EP,TM,DST,DSTT,           
     1                  DSTI,BORI,BKNI,BKPI,TDGI,DSTE,BORE,BKNE,BKPE,           
     2                  TDGE,U,W,DSDC,PERR,OERR,CERR)                           
            IF (CERR.GE.50) GO TO 10                                            
            CALL FDLDS (RGEN,GRAD,CTYP,CLAT,PHI,H,RE,RP,RM,RO,NSI,NC,           
     1                  NCI,NP,II,IE,NMNI,NMXI,NMNE,NMXE,NMAX,MMNI,MMXI,        
     2                  MMNE,MMXE,MMIN,MMAX,THETA,P,R,T,U,W,DLDC,PERR,          
     3                  OERR,CERR)                                              
            IF (CERR.GE.50) GO TO 10                                            
            IF (RGEN.GT.0) THEN                                                 
               RGEN=0                                                           
               CALL R8VSET (1,28+36*GRAD,0.D0,B)                                 
               CALL FDLDC (GRAD,NC,DSDC,DLDC)                                   
               CALL BLGEN (GRAD,NC,B,C,DLDC)                                    
               CALL BNGEN (B)                                                   
            END IF                                                              
            IF (DTYP.EQ.2) THEN                                                 
               CALL TEC (GRAD,ATYP(1),NC,THETA,PHI,B,DLDC,W)                    
               CALL TSE (GRAD,ATYP(2),NC,RSE,B,DLDC,W)                          
            END IF                                                              
         END IF                                                                 
         CALL R8VGATHP (1,1,15,14,B,B)                                          
         IF (GRAD.EQ.1) THEN                                                    
            CALL R8VGATHP (29,1,47,18,B,B)                                      
         END IF                                                                 
         IA=0                                                                   
         IF (NA.GT.0) THEN                                                      
            CALL R8VSET (1,6*NA,0.D0,DLDA)                                       
            IF (DTYP.EQ.1) THEN                                                 
               CALL TBI (ATYP(1),NA,IA,A,B,DLDA)                                
            ELSE IF (DTYP.EQ.2) THEN                                            
               CALL TMS (GRAD,ATYP(3),NC,NA,IA,A,B,DLDC,DLDA,W)                 
               CALL TNM (GRAD,ATYP(4),NC,NA,IA,A,B,DLDC,DLDA,W)                 
               CALL TVN (GRAD,ATYP(5),NC,NA,IA,A,B,DLDC,DLDA,W)                 
               CALL TBI (ATYP(6),NA,IA,A,B,DLDA)                                
            END IF                                                              
         END IF                                                                 
      END IF                                                                    
   10 RETURN                                                                    
      END                                                                       
      SUBROUTINE PREBF (RGEN,ITYP,ETYP,DTYP,GRAD,NMNI,NMXI,NMNE,NMXE,           
     1                  MMNI,MMXI,MMNE,MMXE,NMAX,MMIN,MMAX,NS,NSI,NSE,          
     2                  NC,NCI,NCE,NA,NP,II,IE,ATYP,DSTI,BORI,BKNI,TDGI,        
     3                  DSTE,BORE,BKNE,TDGE,U,PERR,OERR,CERR)                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,RGEN,DTYP,ETYP,ESVR,EDST,GRAD                      
      INTEGER ATYP(*),BORE(*),BORI(*),BKNE(*),BKNI(*),DSTE(*),DSTI(*)           
      INTEGER TDGE(*),TDGI(*),U(*)                                              
      DATA NX /0/                                                               
      IF (RGEN.EQ.1) THEN                                                       
         IF (MIN(NMNI,NMXI,NMNE,NMXE).LT.0) THEN                                
            CERR=50                                                             
            IF (PERR.NE.0) WRITE(OERR,1050) CERR                                
            GO TO 10                                                            
         END IF                                                                 
         IF (MIN(MMNI,MMXI,MMNE,MMXE).LT.0) THEN                                
            CERR=51                                                             
            IF (PERR.NE.0) WRITE(OERR,1051) CERR                                
            GO TO 10                                                            
         END IF                                                                 
         IF ((MMNI.GT.MMXI).OR.(MMNE.GT.MMXE)) THEN                             
            CERR=52                                                             
            IF (PERR.NE.0) WRITE(OERR,1052) CERR                                
            GO TO 10                                                            
         END IF                                                                 
         IF ((MMXI.GT.NMXI).OR.(MMXE.GT.NMXE)) THEN                             
            CERR=53                                                             
            IF (PERR.NE.0) WRITE(OERR,1053) CERR                                
            GO TO 10                                                            
         END IF                                                                 
         ISVR=MOD(ITYP,3)                                                       
         IDST=ITYP/3                                                            
         ESVR=MOD(ETYP,3)                                                       
         EDST=ETYP/3                                                            
         NMAX=MAX(NMXI,NMXE)                                                    
         MMIN=MIN(MMNI,MMNE)                                                    
         MMAX=MAX(MMXI,MMXE)                                                    
         NSI =NSHX(NMXI,NMNI,MMXI,MMNI)                                         
         NSE =NSHX(NMXE,NMNE,MMXE,MMNE)                                         
         NS  =NSI+NSE                                                           
         NP  =NLPX(NMAX,MMAX,MMIN)                                              
         II  =NLPX(NMNI-1,MMAX,MMIN)                                            
         IE  =NLPX(NMNE-1,MMAX,MMIN)                                            
         NCI=0
         IF (NSI.GT.0) THEN                                                     
            CALL I8VSET (1,NSI,1,U)                                             
            IF (ISVR.EQ.1) THEN                                                 
               CALL I8VADD (1,1,1,NSI,TDGI,U,U)                                 
            ELSE IF (ISVR.EQ.2) THEN                                            
               CALL I8VADD (1,1,1,NSI,BORI,U,U)                                 
               CALL I8VADD (1,1,1,NSI,BKNI,U,U)                                 
            END IF                                                              
            IF (IDST.EQ.1) THEN                                                 
               CALL I8VADD (1,1,1,NSI,DSTI,U,U)                                 
            END IF                                                              
            NCI=I8SSUM(1,NSI,U)                                                 
         END IF                                                                 
         NCE=0
         IF (NSE.GT.0) THEN                                                     
            CALL I8VSET (NSI+1,NSE,1,U)                                         
            IF (ESVR.EQ.1) THEN                                                 
               CALL I8VADD (1,NSI+1,NSI+1,NSE,TDGE,U,U)                         
            ELSE IF (ESVR.EQ.2) THEN                                            
               CALL I8VADD (1,NSI+1,NSI+1,NSE,BORE,U,U)                         
               CALL I8VADD (1,NSI+1,NSI+1,NSE,BKNE,U,U)                         
            END IF                                                              
            IF (EDST.EQ.1) THEN                                                 
               CALL I8VADD (1,NSI+1,NSI+1,NSE,DSTE,U,U)                         
            END IF                                                              
            NCE=I8SSUM(NSI+1,NSE,U)                                             
         END IF                                                                 
         NC=NCI+NCE                                                             
         RGEN=7                                                                 
      END IF                                                                    
      RGEN=RGEN+NX                                                              
      NA=0                                                                      
      NX=0                                                                      
      IF (DTYP.EQ.1) THEN                                                       
         NA=NA+MIN(1,ATYP(1))*3                                                 
      ELSE IF (DTYP.EQ.2) THEN                                                  
         NX=NX+ATYP(1)                                                          
         NX=NX+ATYP(2)                                                          
         NA=NA+MIN(1,ATYP(3))*3                                                 
         NA=NA+MIN(1,ATYP(4))*3                                                 
         NA=NA+MIN(1,ATYP(5))*3                                                 
         NX=NX+NA                                                               
         NA=NA+MIN(1,ATYP(6))*3                                                 
      END IF                                                                    
      NX=MIN(1,NX)                                                              
   10 RETURN                                                                    
 1050 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- NMNI, N        
     1MXI, NMNE, OR NMXE < 0 -- ABORT',/,' ')                                   
 1051 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- MMNI, M        
     1MXI, MMNE, OR MMXE < 0 -- ABORT',/,' ')                                   
 1052 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- EITHER         
     1MMNI > MMXI OR MMNE > MMXE -- ABORT',/,' ')                               
 1053 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- EITHER         
     1MMXI > NMXI OR MMXE > NMXE -- ABORT',/,' ')                               
      END                                                                       
      SUBROUTINE FDLDS (RGEN,GRAD,CTYP,CLAT,PHI,H,RE,RP,RM,RO,NSI,NC,           
     1                  NCI,NP,II,IE,NMNI,NMXI,NMNE,NMXE,NMAX,MMNI,MMXI,        
     2                  MMNE,MMXE,MMIN,MMAX,THETA,P,R,T,U,W,DLDC,PERR,          
     3                  OERR,CERR)                                              
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,CTYP,PGEN,RGEN,TGEN,GRAD                           
      INTEGER U(*)                                                              
      DIMENSION DLDC(*),P(*),R(*),T(*),W(*)                                          
      DATA ROO /0.D0/, PHIO /0.D0/, THETAO /0.D0/, CLATO /0.D0/                     
      CALL GEOCEN (CTYP,RE,RP,RM,H,CLAT,RO,THETA,SINTHE,COSTHE)                 
      PGEN=RGEN-6                                                               
      TGEN=PGEN                                                                 
      IF (PHIO.NE.PHI) THEN                                                     
         PGEN=1                                                                 
         RGEN=RGEN+1                                                            
         PHIO=PHI                                                               
      END IF                                                                    
      IF (THETAO.NE.THETA) THEN                                                 
         TGEN=1                                                                 
         RGEN=RGEN+1                                                            
         THETAO=THETA                                                           
      END IF                                                                    
      IF (CLATO.NE.CLAT) THEN                                                   
         RGEN=RGEN+1                                                            
         CLATO=CLAT                                                             
      END IF                                                                    
      IF (ROO.NE.RO) THEN                                                       
         RGEN=RGEN+1                                                            
         ROO=RO                                                                 
      END IF                                                                    
      IF (RGEN.GT.0) THEN                                                       
         IF (SINTHE.EQ.0.D0) THEN                                                
            IF (GRAD.EQ.0) THEN                                                 
               CERR=1                                                           
               IF (PERR.NE.0) WRITE(OERR,1001) CERR                             
            ELSE                                                                
               CERR=2                                                           
               IF (PERR.NE.0) WRITE(OERR,1002) CERR                             
            END IF                                                              
            PHI=0.D0                                                             
            CSCTHE=0.D0                                                          
            CSCTH2=0.D0                                                          
            COTTHE=0.D0                                                          
         ELSE                                                                   
            CSCTHE=1.D0/SINTHE                                                   
            CSCTH2=CSCTHE*CSCTHE                                                
            COTTHE=COSTHE*CSCTHE                                                
         END IF                                                                 
         IF (TGEN.GT.0) THEN                                                    
            CALL SCHMIT (GRAD,RGEN,NMAX,MMIN,MMAX,SINTHE,COSTHE,P,R)            
         END IF                                                                 
         IF (PGEN.GT.0) THEN                                                    
            CALL TRIGMP (MMAX,PHI,T)                                            
         END IF                                                                 
         CALL R8VSET (1,(6+18*GRAD)*NC,0.D0,DLDC)                                
         IC=0                                                                   
         ID=1                                                                   
         IP=II                                                                  
         AR=RM/RO                                                               
         FA=AR**(NMNI+1)                                                        
         DO 10 N=NMNI,NMXI                                                      
            FNP1=DBLE(N+1)                                                      
            FNP2=DBLE(N+2)                                                      
            FA  =FA*AR                                                          
            DO 10 M=MMIN,MIN(N,MMAX)                                            
               IP=IP+1                                                          
               IF ((M.GE.MMNI).AND.(M.LE.MMXI)) THEN                            
                  IC=IC+1                                                       
                  FM=DBLE(M)                                                    
                  FP= FA*P(   IP)                                               
                  FD=-FA*P(NP+IP)                                               
                  FC=T(     M+1)                                                
                  FS=T(MMAX+M+2)                                                
                  FNFP=FNP1*FP                                                  
                  FMFPST=FM*FP*CSCTHE                                           
                  LEND=U(IC)                                                    
                  CALL R8VSET (     ID,LEND,    FD*FC,DLDC)                     
                  CALL R8VSET (  NC+ID,LEND,FMFPST*FS,DLDC)                     
                  CALL R8VSET (2*NC+ID,LEND,  FNFP*FC,DLDC)                     
                  IF (GRAD.EQ.1) THEN                                           
                     FPRR =FP/RO                                                
                     FDRR =FD/RO                                                
                     PBTPT=-FA*P(2*NP+IP)/RO                                    
                     PBTPP=-FM*FDRR*CSCTHE                                      
                     PBTPR=-FNP2*FDRR                                           
                     PBPPT=-FM*(FDRR+FPRR*COTTHE)*CSCTHE                        
                     PBPPP=FM*FM*FPRR*CSCTH2                                    
                     PBPPR=-FNP2*FM*FPRR*CSCTHE                                 
                     PBRPT=-FNP1*FDRR                                           
                     PBRPP=-FNP1*FM*FPRR*CSCTHE                                 
                     PBRPR=-FNP1*FNP2*FPRR                                      
                     CALL R8VSET ( 6*NC+ID,LEND,PBTPT*FC,DLDC)                  
                     CALL R8VSET ( 7*NC+ID,LEND,PBTPP*FS,DLDC)                  
                     CALL R8VSET ( 8*NC+ID,LEND,PBTPR*FC,DLDC)                  
                     CALL R8VSET ( 9*NC+ID,LEND,PBPPT*FS,DLDC)                  
                     CALL R8VSET (10*NC+ID,LEND,PBPPP*FC,DLDC)                  
                     CALL R8VSET (11*NC+ID,LEND,PBPPR*FS,DLDC)                  
                     CALL R8VSET (12*NC+ID,LEND,PBRPT*FC,DLDC)                  
                     CALL R8VSET (13*NC+ID,LEND,PBRPP*FS,DLDC)                  
                     CALL R8VSET (14*NC+ID,LEND,PBRPR*FC,DLDC)                  
                  END IF                                                        
                  ID=ID+LEND                                                    
                  IF (M.GT.0) THEN                                              
                     IC=IC+1                                                    
                     LEND=U(IC)                                                 
                     CALL R8VSET (     ID,LEND,     FD*FS,DLDC)                 
                     CALL R8VSET (  NC+ID,LEND,-FMFPST*FC,DLDC)                 
                     CALL R8VSET (2*NC+ID,LEND,   FNFP*FS,DLDC)                 
                     IF (GRAD.EQ.1) THEN                                        
                        CALL R8VSET ( 6*NC+ID,LEND, PBTPT*FS,DLDC)              
                        CALL R8VSET ( 7*NC+ID,LEND,-PBTPP*FC,DLDC)              
                        CALL R8VSET ( 8*NC+ID,LEND, PBTPR*FS,DLDC)              
                        CALL R8VSET ( 9*NC+ID,LEND,-PBPPT*FC,DLDC)              
                        CALL R8VSET (10*NC+ID,LEND, PBPPP*FS,DLDC)              
                        CALL R8VSET (11*NC+ID,LEND,-PBPPR*FC,DLDC)              
                        CALL R8VSET (12*NC+ID,LEND, PBRPT*FS,DLDC)              
                        CALL R8VSET (13*NC+ID,LEND,-PBRPP*FC,DLDC)              
                        CALL R8VSET (14*NC+ID,LEND, PBRPR*FS,DLDC)              
                     END IF                                                     
                     ID=ID+LEND                                                 
                  END IF                                                        
               END IF                                                           
   10    CONTINUE                                                               
         IP=IE                                                                  
         RA=RO/RM                                                               
         FR=RA**(NMNE-2)                                                        
         DO 20 N=NMNE,NMXE                                                      
            FNM1=DBLE(N-1)                                                      
            FN  =DBLE(N)                                                        
            FR  =FR*RA                                                          
            DO 20 M=MMIN,MIN(N,MMAX)                                            
               IP=IP+1                                                          
               IF ((M.GE.MMNE).AND.(M.LE.MMXE)) THEN                            
                  IC=IC+1                                                       
                  FM=DBLE(M)                                                    
                  FP= FR*P(   IP)                                               
                  FD=-FR*P(NP+IP)                                               
                  FC=T(     M+1)                                                
                  FS=T(MMAX+M+2)                                                
                  FNFP=-FN*FP                                                   
                  FMFPST=FM*FP*CSCTHE                                           
                  LEND=U(IC)                                                    
                  CALL R8VSET (     ID,LEND,    FD*FC,DLDC)                     
                  CALL R8VSET (  NC+ID,LEND,FMFPST*FS,DLDC)                     
                  CALL R8VSET (2*NC+ID,LEND,  FNFP*FC,DLDC)                     
                  IF (GRAD.EQ.1) THEN                                           
                     FPRR =FP/RO                                                
                     FDRR =FD/RO                                                
                     PBTPT=-FR*P(2*NP+IP)/RO                                    
                     PBTPP=-FM*FDRR*CSCTHE                                      
                     PBTPR=FNM1*FDRR                                            
                     PBPPT=-FM*(FDRR+FPRR*COTTHE)*CSCTHE                        
                     PBPPP=FM*FM*FPRR*CSCTH2                                    
                     PBPPR=FNM1*FM*FPRR*CSCTHE                                  
                     PBRPT=FN*FDRR                                              
                     PBRPP=FN*FM*FPRR*CSCTHE                                    
                     PBRPR=-FN*FNM1*FPRR                                        
                     CALL R8VSET ( 6*NC+ID,LEND,PBTPT*FC,DLDC)                  
                     CALL R8VSET ( 7*NC+ID,LEND,PBTPP*FS,DLDC)                  
                     CALL R8VSET ( 8*NC+ID,LEND,PBTPR*FC,DLDC)                  
                     CALL R8VSET ( 9*NC+ID,LEND,PBPPT*FS,DLDC)                  
                     CALL R8VSET (10*NC+ID,LEND,PBPPP*FC,DLDC)                  
                     CALL R8VSET (11*NC+ID,LEND,PBPPR*FS,DLDC)                  
                     CALL R8VSET (12*NC+ID,LEND,PBRPT*FC,DLDC)                  
                     CALL R8VSET (13*NC+ID,LEND,PBRPP*FS,DLDC)                  
                     CALL R8VSET (14*NC+ID,LEND,PBRPR*FC,DLDC)                  
                  END IF                                                        
                  ID=ID+LEND                                                    
                  IF (M.GT.0) THEN                                              
                     IC=IC+1                                                    
                     LEND=U(IC)                                                 
                     CALL R8VSET (     ID,LEND,     FD*FS,DLDC)                 
                     CALL R8VSET (  NC+ID,LEND,-FMFPST*FC,DLDC)                 
                     CALL R8VSET (2*NC+ID,LEND,   FNFP*FS,DLDC)                 
                     IF (GRAD.EQ.1) THEN                                        
                        CALL R8VSET ( 6*NC+ID,LEND, PBTPT*FS,DLDC)              
                        CALL R8VSET ( 7*NC+ID,LEND,-PBTPP*FC,DLDC)              
                        CALL R8VSET ( 8*NC+ID,LEND, PBTPR*FS,DLDC)              
                        CALL R8VSET ( 9*NC+ID,LEND,-PBPPT*FC,DLDC)              
                        CALL R8VSET (10*NC+ID,LEND, PBPPP*FS,DLDC)              
                        CALL R8VSET (11*NC+ID,LEND,-PBPPR*FC,DLDC)              
                        CALL R8VSET (12*NC+ID,LEND, PBRPT*FS,DLDC)              
                        CALL R8VSET (13*NC+ID,LEND,-PBRPP*FC,DLDC)              
                        CALL R8VSET (14*NC+ID,LEND, PBRPR*FS,DLDC)              
                     END IF                                                     
                     ID=ID+LEND                                                 
                  END IF                                                        
               END IF                                                           
   20    CONTINUE                                                               
         CALL TDC (GRAD,NC,CLAT,THETA,DLDC,W)                                   
      END IF                                                                    
      RETURN                                                                    
 1001 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- GEOGRAP        
     1HIC POLAR POSITION DETECTED, B-PHI INDETERMINABLE -- WARNING',/,'         
     2')                                                                        
 1002 FORMAT(' ',/,1X,'SUBROUTINE BFIELD -- ERROR CODE ',I2,' -- GEOGRAP        
     1HIC POLAR POSITION DETECTED, B-PHI AND',/,36X,'-- PHI-DERIVATIVE G        
     2RADIENT COMPONENTS INDETERMINABLE -- WARNING',/,' ')                      
      END                                                                       
      SUBROUTINE GEOCEN (CTYP,RE,RP,RM,H,CLAT,R,THETA,SINTHE,COSTHE)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER CTYP                                                              
      DOUBLE PRECISION KAPPA                                                                
      DATA IFIRST /1/                                                           
      THETA=CLAT                                                                
      R=RM+H                                                                    
      SINTHE=DSIN(THETA)                                                         
      COSTHE=DCOS(THETA)                                                         
      IF (CTYP.EQ.0) THEN                                                       
         IF (IFIRST.EQ.1) THEN                                                  
            IFIRST=0                                                            
            RE2=RE*RE                                                           
            RP2=RP*RP                                                           
         END IF                                                                 
         SINTH2=SINTHE*SINTHE                                                   
         COSTH2=COSTHE*COSTHE                                                   
         KAPPA=DSQRT(RE2*SINTH2+RP2*COSTH2)                                      
         RHOEW=(RE2/KAPPA)+H                                                    
         RHONS=(RP2/KAPPA)+H                                                    
         THETA=DATAN2(RHOEW*SINTHE,RHONS*COSTHE)                                 
         R=DSQRT(RHOEW**2*SINTH2+RHONS**2*COSTH2)                                
         SINTHE=DSIN(THETA)                                                      
         COSTHE=DCOS(THETA)                                                      
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SCHMIT (GRAD,RGEN,NMAX,MMIN,MMAX,SINTHE,COSTHE,P,R)            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER RGEN,GRAD                                                         
      DIMENSION P(*),R(*)                                                            
      DATA ROOT3 /1.732050807568877D0/
      IF (RGEN.GT.6) THEN                                                       
         CALL SRECUR (GRAD,NMAX,MMIN,MMAX,KSM2,KTM2,NP,NP1SEC,ND0SEC,           
     1                NP1TES,NP2TES,ND0TES,ND1TES,NQ0NNP,NQ0MSQ,R)              
      END IF                                                                    
      IF (NMAX.GE.1) THEN                                                       
         KP0=1                                                                  
         KP2=KP0                                                                
         IF (MMIN.LE.0) THEN                                                    
            P(   KP0)=COSTHE                                                    
            P(NP+KP0)=-SINTHE                                                   
            IF (GRAD.EQ.1) THEN                                                 
               P(2*NP+KP0)=-COSTHE                                              
            END IF                                                              
            KP0=KP0+1                                                           
         END IF                                                                 
         IF (MMAX.GE.1) THEN                                                    
            P(   KP0)=SINTHE                                                    
            P(NP+KP0)=COSTHE                                                    
            IF (GRAD.EQ.1) THEN                                                 
               P(2*NP+KP0)=-SINTHE                                              
            END IF                                                              
            KP0=KP0+1                                                           
         END IF                                                                 
         IF (NMAX.GE.2) THEN                                                    
            KP1=KP0                                                             
            SINTH2=SINTHE*SINTHE                                                
            COSTH2=COSTHE*COSTHE                                                
            CTHSTH=COSTHE*SINTHE                                                
            IF (MMIN.LE.0) THEN                                                 
               P(   KP0)=(3.D0*COSTH2-1.D0)/2.D0                                   
               P(NP+KP0)=-3.D0*CTHSTH                                            
               IF (GRAD.EQ.1) THEN                                              
                  P(2*NP+KP0)=-3.D0*(2.D0*COSTH2-1.D0)                             
               END IF                                                           
               KP0=KP0+1                                                        
            END IF                                                              
            IF ((MMIN.LE.1).AND.(MMAX.GE.1)) THEN                               
               P(   KP0)=ROOT3*CTHSTH                                           
               P(NP+KP0)=ROOT3*(2.D0*COSTH2-1.D0)                                 
               IF (GRAD.EQ.1) THEN                                              
                  P(2*NP+KP0)=-4.D0*ROOT3*CTHSTH                                 
               END IF                                                           
               KP0=KP0+1                                                        
            END IF                                                              
            IF (MMAX.GE.2) THEN                                                 
               P(   KP0)=ROOT3*SINTH2/2.D0                                       
               P(NP+KP0)=ROOT3*CTHSTH                                           
               IF (GRAD.EQ.1) THEN                                              
                  P(2*NP+KP0)=ROOT3*(2.D0*COSTH2-1.D0)                            
               END IF                                                           
               KP0=KP0+1                                                        
            END IF                                                              
            KD2=NP+KP2                                                          
            KD1=NP+KP1                                                          
            KD0=NP+KP0                                                          
            KQ2=NP+KD2                                                          
            KQ1=NP+KD1                                                          
            KQ0=NP+KD0                                                          
            KSEC=1                                                              
            KTES=1                                                              
            KNNP=1                                                              
            IF (SINTHE.EQ.0.D0) THEN                                             
               DO 10 N=3,NMAX                                                   
                  L=MAX(0,MIN(N-1,MMAX)-MMIN+1)                                 
                  IF (L.GT.0) THEN                                              
                     CALL R8VSET (KP0,L,0.D0,P)                                  
                     CALL R8VLINKQ (KP1,NP1TES+KTES,KP0,L,COSTHE,P,R,P)         
                     CALL R8VLINKQ (KP2,NP2TES+KTES,KP0,L, -1.D0,P,R,P)         
                     CALL R8VSET (KD0,L,0.D0,P)                                  
                     CALL R8VLINKQ (KD1,NP1TES+KTES,KD0,L,COSTHE,P,R,P)         
                     CALL R8VLINKQ (KD2,NP2TES+KTES,KD0,L, -1.D0,P,R,P)         
                     IF (GRAD.EQ.1) THEN                                        
                        CALL R8VSET (KQ0,L,0.D0,P)                               
                        CALL R8VLINKQ (KQ1,NP1TES+KTES,KQ0,L, COSTHE,P,         
     1                                 R,P)                                     
                        CALL R8VLINKQ (KP1,NP1TES+KTES,KQ0,L,-COSTHE,P,         
     1                                 R,P)                                     
                        CALL R8VLINKQ (KQ2,NP2TES+KTES,KQ0,L,  -1.D0,P,         
     1                                 R,P)                                     
                     END IF                                                     
                     KTES=KTES+L                                                
                  END IF                                                        
                  IF (N.LE.MMAX) THEN                                           
                     P(KP0+L)=0.D0                                               
                     P(KD0+L)=0.D0                                               
                     IF (GRAD.EQ.1) THEN                                        
                        P(KQ0+L)=0.D0                                            
                     END IF                                                     
                     L=L+1                                                      
                  END IF                                                        
                  KP2=KP1                                                       
                  KP1=KP0                                                       
                  KP0=KP0+L                                                     
                  KD2=KD1                                                       
                  KD1=KD0                                                       
                  KD0=KD0+L                                                     
                  KQ2=KQ1                                                       
                  KQ1=KQ0                                                       
   10             KQ0=KQ0+L                                                     
            ELSE                                                                
               COTTHE=COSTHE/SINTHE                                             
               CSCTHE=1.D0/SINTHE                                                
               CSCTH2=CSCTHE*CSCTHE                                             
               DO 20 N=3,NMAX                                                   
                  L=MAX(0,MIN(N-1,MMAX)-MMIN+1)                                 
                  IF (L.GT.0) THEN                                              
                     CALL R8VSET (KP0,L,0.D0,P)                                  
                     CALL R8VLINKQ (KP1,NP1TES+KTES,KP0,L,COSTHE,P,R,P)         
                     CALL R8VLINKQ (KP2,NP2TES+KTES,KP0,L, -1.D0,P,R,P)         
                     CALL R8VSET (KD0,L,0.D0,P)                                  
                     CALL R8VLINKQ (KP0,ND0TES+KTES,KD0,L, COTTHE,P,R,P)        
                     CALL R8VLINKQ (KP1,ND1TES+KTES,KD0,L,-CSCTHE,P,R,P)        
                     IF (GRAD.EQ.1) THEN                                        
                        CALL R8VSET (KQ0,L,0.D0,P)                               
                        CALL R8VLINKQ (KP0,NQ0MSQ+KTM2,KQ0,L,CSCTH2,P,R,        
     1                                 P)                                       
                        CALL R8VLINKT (KP0,KQ0,L,-R(NQ0NNP+KNNP),P,P)           
                        CALL R8VLINKT (KD0,KQ0,L,-COTTHE,P,P)                   
                     END IF                                                     
                     KTES=KTES+L                                                
                  END IF                                                        
                  IF (N.LE.MMAX) THEN                                           
                     P(KP0+L)=R(NP1SEC+KSEC)*SINTHE*P(KP0-1)                    
                     P(KD0+L)=R(ND0SEC+KSEC)*COTTHE*P(KP0+L)                    
                     IF (GRAD.EQ.1) THEN                                        
                        P(KQ0+L)=(R(NQ0MSQ-KSM2+N+1)*CSCTH2-                    
     1                           R(NQ0NNP+KNNP))*P(KP0+L)-                      
     2                           COTTHE*P(KD0+L)                                
                     END IF                                                     
                     KSEC=KSEC+1                                                
                     L=L+1                                                      
                  END IF                                                        
                  KNNP=KNNP+1                                                   
                  KP2=KP1                                                       
                  KP1=KP0                                                       
                  KP0=KP0+L                                                     
                  KD0=KD0+L                                                     
   20             KQ0=KQ0+L                                                     
            END IF                                                              
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SRECUR (GRAD,NMAX,MMIN,MMAX,KSM2,KTM2,NPALL,NAD1,NAD2,         
     1                   NAD3,NAD4,NAD5,NAD6,NAD7,NAD8,R)                       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION R(*)                                                                 
      LMAX =MIN(NMAX,2)                                                         
      KMAX =MIN(MMAX,2)                                                         
      KMIN =MIN(MMIN,2)                                                         
      KSM2 =MIN(MMIN,3)                                                         
      KTM2 =DIM(MMIN,3)+1                                                       
      NPALL=NLPX(NMAX,MMAX,MMIN)                                                
      NTESS=NPALL-NLPX(LMAX,KMAX,KMIN)+KMAX-MMAX                                
      NSECT=MAX(0,MMAX-2)                                                       
      NNNP1=MAX(0,NMAX-2)                                                       
      NAD1 =0                                                                   
      NAD2 =NSECT                                                               
      NAD3 =NSECT+NAD2                                                          
      NAD4 =NTESS+NAD3                                                          
      NAD5 =NTESS+NAD4                                                          
      NAD6 =NTESS+NAD5                                                          
      NAD7 =NTESS+NAD6                                                          
      NAD8 =NNNP1+NAD7                                                          
      KSEC =0                                                                   
      KTES =0                                                                   
      KNNP =0                                                                   
      DO 10 N=3,NMAX                                                            
         FNP1  =DBLE(N+1)                                                       
         FN    =DBLE(N)                                                         
         FNL1  =DBLE(N-1)                                                       
         F2N   =2.D0*FN                                                          
         F2NL1 =F2N-1.D0                                                         
         FNSQ  =FN*FN                                                           
         FNL1SQ=FNL1*FNL1                                                       
         IF (N.LE.MMAX) THEN                                                    
            KSEC        =KSEC+1                                                 
            R(NAD1+KSEC)=DSQRT(F2NL1/F2N)                                        
            R(NAD2+KSEC)=FN                                                     
         END IF                                                                 
         IF (GRAD.EQ.1) THEN                                                    
            KNNP        =KNNP+1                                                 
            R(NAD7+KNNP)=FN*FNP1                                                
         END IF                                                                 
         DO 10 M=MMIN,MIN(N-1,MMAX)                                             
            KTES        =KTES+1                                                 
            FM          =DBLE(M)                                                
            FMSQ        =FM*FM                                                  
            FDSQRT      =DSQRT(FNSQ-FMSQ)                                        
            R(NAD3+KTES)=F2NL1/FDSQRT                                           
            R(NAD4+KTES)=DSQRT(FNL1SQ-FMSQ)/FDSQRT                               
            R(NAD5+KTES)=FN                                                     
   10       R(NAD6+KTES)=FDSQRT                                                 
      IF (GRAD.EQ.1) THEN                                                       
         KMSQ=0                                                                 
         DO 20 M=KSM2,MMAX                                                      
            KMSQ        =KMSQ+1                                                 
            FM          =DBLE(M)                                                
   20       R(NAD8+KMSQ)=FM*FM                                                  
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TRIGMP (MMAX,PHI,T)                                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION T(*)                                                                 
      T(     1)=1.D0                                                             
      T(MMAX+2)=0.D0                                                             
      IF (MMAX.GT.0) THEN
         T(     2)=DCOS(PHI)                                                        
         T(MMAX+3)=DSIN(PHI)                                                        
         DO 10 M=2,MMAX                                                            
            T(     M+1)=2.D0*T(2)*T(     M  )-T(     M-1)                           
   10       T(MMAX+M+2)=2.D0*T(2)*T(MMAX+M+1)-T(MMAX+M  )                           
      END IF
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TDC (GRAD,NC,CLAT,THETA,DLDC,R)                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION R(*),DLDC(*)                                                         
      DATA NULL /0/, IONE /1/                                                   
      PSI=THETA-CLAT                                                            
      SPSI=DSIN(PSI)                                                             
      CPSI=DCOS(PSI)                                                             
      R(1)=-CPSI                                                                
      R(2)= 0.D0                                                                 
      R(3)=-SPSI                                                                
      R(4)= 0.D0                                                                 
      R(5)= 1.D0                                                                 
      R(6)= 0.D0                                                                 
      R(7)= SPSI                                                                
      R(8)= 0.D0                                                                 
      R(9)=-CPSI                                                                
      CALL LTRANV (IONE,NC,NC,R,DLDC)                                           
      IF (GRAD.EQ.1) THEN                                                       
         CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(6*NC+1))                            
         CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                               
         CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                               
         CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                               
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDSDC (RGEN,ITYP,ETYP,NSI,NSE,NC,NCI,TA,TB,DST,DSTT,           
     1                  DSTI,BORI,BKNI,BKPI,TDGI,DSTE,BORE,BKNE,BKPE,           
     2                  TDGE,U,W,DSDC,PERR,OERR,CERR)                           
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,ETYP,ESVR,EDST,RGEN,TGEN                           
      INTEGER BORE(*),BORI(*),BKNE(*),BKNI(*),DSTE(*),DSTI(*),TDGE(*)           
      INTEGER TDGI(*),U(*)                                                      
      DIMENSION BKPI(*),BKPE(*),DSDC(*),W(*)                                         
      DATA TBO /0.D0/                                                            
      TGEN=MAX(0,RGEN-6)                                                        
      IF (TBO.NE.TB) THEN                                                       
         TGEN=MIN(1,TGEN+ITYP+ETYP)                                             
         RGEN=RGEN+TGEN                                                         
         TBO=TB                                                                 
      END IF                                                                    
      IF (TGEN.GT.0) THEN                                                       
         CALL R8VSET (1,2*NC,0.D0,DSDC)                                          
         IF (NSI.GT.0) THEN                                                     
            ISVR=MOD(ITYP,3)                                                    
            IDST=ITYP/3                                                         
            CALL I8VCUM (1,1,NSI,U)                                             
            CALL R8VSCATS (1,NSI,1.D0,U,DSDC(   1))                              
            CALL R8VSCATS (1,NSI,0.D0,U,DSDC(NC+1))                              
            CALL I8VADDS (1,1,NSI,1,U,U)                                        
            IF (ISVR.EQ.1) THEN                                                 
               CALL TAYLOR (NC,NSI,TA,TB,TDGI,U(1),W,DSDC(1))                   
            ELSE IF (ISVR.EQ.2) THEN                                            
               CALL BSPLYN (NC,NSI,TA,TB,BORI,BKNI,BKPI,U(1),W,DSDC(1),         
     1                      PERR,OERR,CERR)                                     
               IF (CERR.GE.50) GO TO 10
            END IF                                                              
            IF (IDST.EQ.1) THEN                                                 
               CALL DSTORM (NC,NSI,DST,DSTT,DSTI,U(1),DSDC(1))                  
            END IF                                                              
   10       CALL I8VDEL (1,1,NSI,U)                                             
         END IF                                                                 
         IF (NSE.GT.0) THEN                                                     
            ESVR=MOD(ETYP,3)                                                    
            EDST=ETYP/3                                                         
            CALL I8VCUM (1,NSI+1,NSE,U)                                         
            CALL R8VSCATS (NSI+1,NSE,1.D0,U,DSDC(   NCI+1))                      
            CALL R8VSCATS (NSI+1,NSE,0.D0,U,DSDC(NC+NCI+1))                      
            CALL I8VADDS (NSI+1,NSI+1,NSE,1,U,U)                                
            IF (ESVR.EQ.1) THEN                                                 
               CALL TAYLOR (NC,NSE,TA,TB,TDGE,U(NSI+1),W,DSDC(NCI+1))           
            ELSE IF (ESVR.EQ.2) THEN                                            
               CALL BSPLYN (NC,NSE,TA,TB,BORE,BKNE,BKPE,U(NSI+1),W,             
     1                      DSDC(NCI+1),PERR,OERR,CERR)                         
               IF (CERR.GE.50) GO TO 20
            END IF                                                              
            IF (EDST.EQ.1) THEN                                                 
               CALL DSTORM (NC,NSE,DST,DSTT,DSTE,U(NSI+1),DSDC(NCI+1))          
            END IF                                                              
   20       CALL I8VDEL (1,NSI+1,NSE,U)                                         
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TAYLOR (NC,NS,TA,TB,TDEG,U,DSDT,DSDC)                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER TDEG(*),U(*)                                                      
      DIMENSION DSDC(*),DSDT(*)                                                      
      DT=TB-TA                                                                  
      DO 10 I=1,NS                                                              
         N=TDEG(I)                                                              
         IF (N.GT.0) THEN                                                       
            IU=U(I)                                                             
            DSDT(1)=1.D0                                                         
            DO 20 J=1,N                                                         
   20          DSDT(J+1)=DSDT(J)*DT/DBLE(J)                                     
            CALL R8VGATHP (2,1,   IU,N,DSDT,DSDC)                               
            CALL R8VGATHP (1,1,NC+IU,N,DSDT,DSDC)                               
            U(I)=U(I)+N                                                         
         END IF                                                                 
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BSPLYN (NC,NS,TA,TB,BORD,BKNO,BKPO,U,DTDB,DSDC,PERR,           
     1                   OERR,CERR)                                             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,BORD(*),BKNO(*),U(*)                               
      DIMENSION BKPO(*),DSDC(*),DTDB(*)                                              
      IK=1                                                                      
      DO 10 I=1,NS                                                              
         N=BORD(I)                                                              
         K=BKNO(I)                                                              
         L=N+K                                                                  
         IU=U(I)                                                                
         IF (N.GT.0) THEN                                                       
            M=N+1                                                               
            CALL SBSPLN (TA,TB,M,K,BKPO(IK),DTDB,DSDC(IU),PERR,OERR,            
     1                   CERR)                                                  
            IF (CERR.GE.50) GO TO 20                                            
            CALL R8VSET (1,L+1,0.D0,DTDB)                                        
            CALL TBSPLN (TB,N,K,BKPO(IK),DTDB,PERR,OERR,CERR)                   
            IF (CERR.GE.50) GO TO 20                                            
            CALL R8VGATHP (1,1,NC+IU,L,DTDB,DSDC)                               
         ELSE IF (K.GT.0) THEN                                                  
            CALL R8VSET (   IU,L,0.D0,DSDC)                                      
            CALL R8VSET (NC+IU,L,0.D0,DSDC)                                      
         END IF                                                                 
         U(I)=U(I)+L                                                            
   10    IK=IK+K+2                                                              
   20 RETURN                                                                    
      END                                                                       
      SUBROUTINE SBSPLN (TA,TB,N,K,BKPO,DTDB,DSDC,PERR,OERR,CERR)               
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR                                                    
      DIMENSION BKPO(*),DSDC(*),DTDB(*)                                              
      IF (N.GT.1) THEN                                                          
         NL=N+K+1                                                               
         CALL R8VSET (1,2*NL,0.D0,DTDB)                                          
         CALL TBSPLN (TB,N,K,BKPO,DTDB(1   ),PERR,OERR,CERR)                    
         IF (CERR.GE.50) GO TO 20                                               
         CALL TBSPLN (TA,N,K,BKPO,DTDB(NL+1),PERR,OERR,CERR)                    
         IF (CERR.GE.50) GO TO 20                                               
         CALL R8VSUB (NL+1,1,1,NL,DTDB,DTDB,DTDB)                               
         RN=1.D0/DBLE(N-1)                                                       
         NP=N+K-1                                                               
         NS=NP                                                                  
         DO 10 I=1,NP                                                           
            IP=I+1                                                              
            IK=MIN(IP,K+2)                                                      
            JK=MAX(IP-N+1,1)                                                    
            DSDC(I)=(BKPO(IK)-BKPO(JK))*R8SSUM(IP,NS,DTDB)                      
   10       NS=NS-1                                                             
         IF (NP.GT.0) THEN                                                      
            CALL R8VSCALE (1,NP,RN,DSDC)                                        
         END IF                                                                 
      END IF                                                                    
   20 RETURN                                                                    
      END                                                                       
      SUBROUTINE TBSPLN (T,N,K,BKPO,DTDB,PERR,OERR,CERR)                        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER PERR,OERR,CERR,P,Y,POSB                                           
      DIMENSION BKPO(*),DTDB(*)                                                      
      IF ((T.GE.BKPO(1)).AND.(T.LE.BKPO(K+2))) THEN                             
         CALL R8SLT (1,K+2,T,BKPO,Y)                                            
         L=MIN(K+2,Y+1)                                                         
         P=L+N-2                                                                
         IF (N.EQ.1) THEN                                                       
            DTDB(P)=1.D0                                                         
         ELSE IF (N.GT.1) THEN                                                  
            M=L                                                                 
            IK=MIN(M,K+2)                                                       
            JK=MAX(M-1,1)                                                       
            DIFI=BKPO(IK)-T                                                     
            DELK=BKPO(IK)-BKPO(JK)                                              
            IF (DELK.EQ.0.D0) THEN                                               
               DTDB(P)=0.D0                                                      
            ELSE                                                                
               DTDB(P)=1.D0/DELK                                                 
            END IF                                                              
            POSB=P-1                                                            
            DO 10 J=2,N                                                         
               JK=MAX(M-J,1)                                                    
               TEMP=DIFI*DTDB(POSB+1)                                           
               DELK=BKPO(IK)-BKPO(JK)                                           
               IF (DELK.EQ.0.D0) THEN                                            
                  TEMP=0.D0                                                      
               ELSE                                                             
                  IF (J.LT.N) THEN                                              
                     TEMP=TEMP/DELK                                             
                  END IF                                                        
               END IF                                                           
               DTDB(POSB)=TEMP                                                  
   10          POSB=POSB-1                                                      
            DTDB(P+1)=0.D0                                                       
            M=M+1                                                               
            DO 20 I=2,N                                                         
               IK=MIN(M,K+2)                                                    
               DIFI=BKPO(IK)-T                                                  
               POSB=P                                                           
               DO 30 J=I,N                                                      
                  JK=MAX(M-J,1)                                                 
                  DIFJ=T-BKPO(JK)                                               
                  TEMP=DIFJ*DTDB(POSB)+DIFI*DTDB(POSB+1)                        
                  DELK=BKPO(IK)-BKPO(JK)                                        
                  IF (DELK.EQ.0.D0) THEN                                         
                     TEMP=0.D0                                                   
                  ELSE                                                          
                     IF (J.LT.N) THEN                                           
                        TEMP=TEMP/DELK                                          
                     END IF                                                     
                  END IF                                                        
                  DTDB(POSB)=TEMP                                               
   30             POSB=POSB-1                                                   
   20          M=M+1                                                            
         END IF                                                                 
      ELSE                                                                      
         CERR=54                                                                
         IF (PERR.NE.0) WRITE(OERR,1054) CERR                                   
      END IF                                                                    
      RETURN                                                                    
 1054 FORMAT(' ',/,1X,'SUBROUTINE TBSPLN -- ERROR CODE ',I2,' -- T LIES         
     1OUTSIDE OF KNOT DOMAIN -- ABORT',/,' ')                                   
      END                                                                       
      SUBROUTINE DSTORM (NC,NS,DST,DSTT,DSTM,U,DSDC)                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER DSTM(*),U(*)                                                      
      DIMENSION DSDC(*)                                                              
      DO 10 I=1,NS                                                              
         N=DSTM(I)                                                              
         IF (N.GT.0) THEN                                                       
            IU=U(I)                                                             
            CALL R8VSET (   IU,N,DST ,DSDC)                                     
            CALL R8VSET (NC+IU,N,DSTT,DSDC)                                     
            U(I)=U(I)+N                                                         
         END IF                                                                 
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDLDC (GRAD,NC,DSDC,DLDC)                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DSDC(*),DLDC(*)                                                      
      I=1                                                                       
      DO 10 J=1,3                                                               
         CALL R8VMUL (NC+1,I,3*NC+I,NC,DSDC,DLDC,DLDC)                          
   10    I=I+NC                                                                 
      I=1                                                                       
      DO 20 J=1,3                                                               
         CALL R8VMUL (1,I,I,NC,DSDC,DLDC,DLDC)                                  
   20    I=I+NC                                                                 
      IF (GRAD.EQ.1) THEN                                                       
         I=1                                                                    
         DO 30 J=1,9                                                            
            CALL R8VMUL (NC+1,6*NC+I,15*NC+I,NC,DSDC,DLDC,DLDC)                 
   30       I=I+NC                                                              
         I=1                                                                    
         DO 40 J=1,9                                                            
            CALL R8VMUL (1,6*NC+I,6*NC+I,NC,DSDC,DLDC,DLDC)                     
   40       I=I+NC                                                              
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BLGEN (GRAD,NC,B,C,DLDC)                                       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION B(*),C(*),DLDC(*)                                                    
      I=1                                                                       
      DO 10 J=1,6                                                               
         B(J)=B(J)+R8SDOT(I,1,NC,DLDC,C)                                        
   10    I=I+NC                                                                 
      IF (GRAD.EQ.1) THEN                                                       
         I=1                                                                    
         DO 20 J=29,46                                                          
            B(J)=B(J)+R8SDOT(6*NC+I,1,NC,DLDC,C)                                
   20       I=I+NC                                                              
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE BNGEN (B)                                                      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION B(*)                                                                 
      CX =B(1)                                                                  
      CY =B(2)                                                                  
      CZ =B(3)                                                                  
      CXT=B(4)                                                                  
      CYT=B(5)                                                                  
      CZT=B(6)                                                                  
      CH =DSQRT(CX*CX+CY*CY)                                                     
      CF =DSQRT(CH*CH+CZ*CZ)                                                     
      IF (CH.NE.0.D0) THEN                                                       
         CD =2.D0*DATAN(CY/(CH+CX))                                               
         CDT=(CX*CYT-CY*CXT)/CH/CH                                              
         CHT=(CX*CXT+CY*CYT)/CH                                                 
      ELSE                                                                      
         CD =0.D0                                                                
         CDT=0.D0                                                                
         CHT=0.D0                                                                
      END IF                                                                    
      IF (CF.NE.0.D0) THEN                                                       
         CI =2.D0*DATAN(CZ/(CF+CH))                                               
         CIT=(CH*CZT-CZ*CHT)/CF/CF                                              
         CFT=(CH*CHT+CZ*CZT)/CF                                                 
      ELSE                                                                      
         CI =0.D0                                                                
         CIT=0.D0                                                                
         CFT=0.D0                                                                
      END IF                                                                    
      B(7 )=CD                                                                  
      B(8 )=CI                                                                  
      B(9 )=CH                                                                  
      B(10)=CF                                                                  
      B(11)=CDT                                                                 
      B(12)=CIT                                                                 
      B(13)=CHT                                                                 
      B(14)=CFT                                                                 
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TEC (GRAD,K,NC,THETA,PHI,B,DLDC,R)                             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DLDC(*),B(*),R(*)                                                    
      DATA NULL /0/, IONE /1/                                                   
      IF (K.GE.1) THEN                                                          
         SINTHE=DSIN(THETA)                                                      
         COSTHE=DCOS(THETA)                                                      
         SPHI=DSIN(PHI)                                                          
         CPHI=DCOS(PHI)                                                          
         R(1)=-COSTHE*CPHI                                                      
         R(2)=-SPHI                                                             
         R(3)=-SINTHE*CPHI                                                      
         R(4)=-COSTHE*SPHI                                                      
         R(5)= CPHI                                                             
         R(6)=-SINTHE*SPHI                                                      
         R(7)= SINTHE                                                           
         R(8)= 0.D0                                                              
         R(9)=-COSTHE                                                           
         CALL LTRANS (IONE,IONE,B(1),R,B(1))                                    
         CALL LTRANS (IONE,IONE,B(4),R,B(4))                                    
         CALL LTRANV (IONE,NC,NC,R,DLDC(     1))                                
         CALL LTRANV (NULL,NC,NC,R,DLDC(3*NC+1))                                
         CALL BNGEN (B)                                                         
         IF (GRAD.EQ.1) THEN                                                    
            CALL LTRANV (NULL,3,3,R,B(29))                                      
            CALL LTRANV (NULL,3,3,R,B(38))                                      
            CALL LTRANS (IONE,IONE,B(29),R,B(29))                               
            CALL LTRANS (IONE,IONE,B(32),R,B(32))                               
            CALL LTRANS (IONE,IONE,B(35),R,B(35))                               
            CALL LTRANS (IONE,IONE,B(38),R,B(38))                               
            CALL LTRANS (IONE,IONE,B(41),R,B(41))                               
            CALL LTRANS (IONE,IONE,B(44),R,B(44))                               
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC( 6*NC+1))                        
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(15*NC+1))                        
            CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(15*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(18*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(21*NC+1))                            
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TSE (GRAD,K,NC,RSE,B,DLDC,R)                                   
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DLDC(*),RSE(*),B(*),R(*)                                             
      DATA NULL /0/, IONE /1/                                                   
      IF (K.GE.1) THEN                                                          
         CALL R8VGATHP (1,1,1,9,RSE,R)                                          
         CALL LTRANS (IONE,IONE,B(1),R,B(1))                                    
         CALL LTRANS (IONE,IONE,B(4),R,B(4))                                    
         CALL LTRANV (IONE,NC,NC,R,DLDC(     1))                                
         CALL LTRANV (NULL,NC,NC,R,DLDC(3*NC+1))                                
         CALL BNGEN (B)                                                         
         IF (GRAD.EQ.1) THEN                                                    
            CALL LTRANV (NULL,3,3,R,B(29))                                      
            CALL LTRANV (NULL,3,3,R,B(38))                                      
            CALL LTRANS (IONE,IONE,B(29),R,B(29))                               
            CALL LTRANS (IONE,IONE,B(32),R,B(32))                               
            CALL LTRANS (IONE,IONE,B(35),R,B(35))                               
            CALL LTRANS (IONE,IONE,B(38),R,B(38))                               
            CALL LTRANS (IONE,IONE,B(41),R,B(41))                               
            CALL LTRANS (IONE,IONE,B(44),R,B(44))                               
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC( 6*NC+1))                        
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(15*NC+1))                        
            CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(15*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(18*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(21*NC+1))                            
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TMS (GRAD,K,NC,NA,IA,A,B,DLDC,DLDA,R)                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DLDA(*),DLDC(*),A(*),B(*),R(*)                                       
      DATA NULL /0/, IONE /1/                                                   
      IF (K.GE.1) THEN                                                          
         EULX=A(IA+1)                                                           
         EULY=A(IA+2)                                                           
         EULZ=A(IA+3)                                                           
         SEULX=DSIN(EULX)                                                        
         CEULX=DCOS(EULX)                                                        
         SEULY=DSIN(EULY)                                                        
         CEULY=DCOS(EULY)                                                        
         SEULZ=DSIN(EULZ)                                                        
         CEULZ=DCOS(EULZ)                                                        
         CALL FDLDEU (K,NA,IA,SEULX,CEULX,SEULY,CEULY,SEULZ,CEULZ,R,B,          
     1                DLDA)                                                     
         R(1)= CEULY*CEULZ                                                      
         R(2)=-SEULZ                                                            
         R(3)=-SEULY*CEULZ                                                      
         R(4)= CEULY*CEULX*SEULZ+SEULY*SEULX                                    
         R(5)= CEULX*CEULZ                                                      
         R(6)= CEULY*SEULX-SEULY*SEULZ*CEULX                                    
         R(7)=-CEULY*SEULX*SEULZ+SEULY*CEULX                                    
         R(8)=-SEULX*CEULZ                                                      
         R(9)= CEULY*CEULX+SEULY*SEULX*SEULZ                                    
         CALL LTRANS (IONE,IONE,B(1),R,B(1))                                    
         CALL LTRANS (IONE,IONE,B(4),R,B(4))                                    
         CALL LTRANV (IONE,NC,NC,R,DLDC(     1))                                
         CALL LTRANV (NULL,NC,NC,R,DLDC(3*NC+1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(     1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(3*NA+1))                                
         CALL BNGEN (B)                                                         
         IF (GRAD.EQ.1) THEN                                                    
            CALL LTRANV (NULL,3,3,R,B(29))                                      
            CALL LTRANV (NULL,3,3,R,B(38))                                      
            CALL LTRANS (IONE,IONE,B(29),R,B(29))                               
            CALL LTRANS (IONE,IONE,B(32),R,B(32))                               
            CALL LTRANS (IONE,IONE,B(35),R,B(35))                               
            CALL LTRANS (IONE,IONE,B(38),R,B(38))                               
            CALL LTRANS (IONE,IONE,B(41),R,B(41))                               
            CALL LTRANS (IONE,IONE,B(44),R,B(44))                               
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC( 6*NC+1))                        
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(15*NC+1))                        
            CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(15*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(18*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(21*NC+1))                            
         END IF                                                                 
         IA=IA+3                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDLDEU (K,NA,IA,SEULX,CEULX,SEULY,CEULY,SEULZ,CEULZ,R,         
     1                   B,DLDA)                                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION DLDA(*),B(*),R(*)                                                    
      DATA IONE /1/                                                             
      IF (K.EQ.1) THEN                                                          
         I=IA                                                                   
         DO 10 J=1,6                                                            
            DLDA(I+1)=0.D0                                                       
            DLDA(I+2)=0.D0                                                       
            DLDA(I+3)=0.D0                                                       
   10       I=I+NA                                                              
      ELSE                                                                      
         R(1)= 0.D0                                                              
         R(2)= 0.D0                                                              
         R(3)= 0.D0                                                              
         R(4)=-CEULY*SEULX*SEULZ+SEULY*CEULX                                    
         R(5)=-SEULX*CEULZ                                                      
         R(6)= CEULY*CEULX+SEULY*SEULZ*SEULX                                    
         R(7)=-CEULY*CEULX*SEULZ-SEULY*SEULX                                    
         R(8)=-CEULX*CEULZ                                                      
         R(9)=-CEULY*SEULX+SEULY*CEULX*SEULZ                                    
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+1))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+1))                           
         R(1)=-SEULY*CEULZ                                                      
         R(2)= 0.D0                                                              
         R(3)=-CEULY*CEULZ                                                      
         R(4)=-SEULY*CEULX*SEULZ+CEULY*SEULX                                    
         R(5)= 0.D0                                                              
         R(6)=-SEULY*SEULX-CEULY*SEULZ*CEULX                                    
         R(7)= SEULY*SEULX*SEULZ+CEULY*CEULX                                    
         R(8)= 0.D0                                                              
         R(9)=-SEULY*CEULX+CEULY*SEULX*SEULZ                                    
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+2))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+2))                           
         R(1)=-CEULY*SEULZ                                                      
         R(2)=-CEULZ                                                            
         R(3)= SEULY*SEULZ                                                      
         R(4)= CEULY*CEULX*CEULZ                                                
         R(5)=-CEULX*SEULZ                                                      
         R(6)=-SEULY*CEULZ*CEULX                                                
         R(7)=-CEULY*SEULX*CEULZ                                                
         R(8)= SEULX*SEULZ                                                      
         R(9)= SEULY*SEULX*CEULZ                                                
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+3))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+3))                           
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TNM (GRAD,K,NC,NA,IA,A,B,DLDC,DLDA,R)                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DLDA(*),DLDC(*),A(*),B(*),R(*)                                       
      DATA NULL /0/, IONE /1/                                                   
      IF (K.GE.1) THEN                                                          
         CHIX=A(IA+1)                                                           
         CHIY=A(IA+2)                                                           
         CHIZ=A(IA+3)                                                           
         SCHIX=DSIN(CHIX)                                                        
         CCHIX=DCOS(CHIX)                                                        
         SCHIY=DSIN(CHIY)                                                        
         CCHIY=DCOS(CHIY)                                                        
         SCHIZ=DSIN(CHIZ)                                                        
         CCHIZ=DCOS(CHIZ)                                                        
         CALL FDLDNO (K,NA,IA,SCHIX,CCHIX,SCHIY,CCHIY,SCHIZ,CCHIZ,R,B,          
     1                DLDA)                                                     
         R(1)=1.D0                                                               
         R(2)=0.D0                                                               
         R(3)=0.D0                                                               
         R(4)=SCHIX                                                             
         R(5)=CCHIX                                                             
         R(6)=0.D0                                                               
         R(7)=SCHIY*CCHIZ                                                       
         R(8)=SCHIY*SCHIZ                                                       
         R(9)=CCHIY                                                             
         CALL LTRANS (IONE,IONE,B(1),R,B(1))                                    
         CALL LTRANS (IONE,IONE,B(4),R,B(4))                                    
         CALL LTRANV (IONE,NC,NC,R,DLDC(     1))                                
         CALL LTRANV (NULL,NC,NC,R,DLDC(3*NC+1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(     1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(3*NA+1))                                
         CALL BNGEN (B)                                                         
         IF (GRAD.EQ.1) THEN                                                    
            CALL LTRANV (NULL,3,3,R,B(29))                                      
            CALL LTRANV (NULL,3,3,R,B(38))                                      
            CALL LTRANS (IONE,IONE,B(29),R,B(29))                               
            CALL LTRANS (IONE,IONE,B(32),R,B(32))                               
            CALL LTRANS (IONE,IONE,B(35),R,B(35))                               
            CALL LTRANS (IONE,IONE,B(38),R,B(38))                               
            CALL LTRANS (IONE,IONE,B(41),R,B(41))                               
            CALL LTRANS (IONE,IONE,B(44),R,B(44))                               
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC( 6*NC+1))                        
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(15*NC+1))                        
            CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(15*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(18*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(21*NC+1))                            
         END IF                                                                 
         IA=IA+3                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDLDNO (K,NA,IA,SCHIX,CCHIX,SCHIY,CCHIY,SCHIZ,CCHIZ,R,         
     1                   B,DLDA)                                                
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION DLDA(*),B(*),R(*)                                                    
      DATA IONE /1/                                                             
      IF (K.EQ.1) THEN                                                          
         I=IA                                                                   
         DO 10 J=1,6                                                            
            DLDA(I+1)=0.D0                                                       
            DLDA(I+2)=0.D0                                                       
            DLDA(I+3)=0.D0                                                       
   10       I=I+NA                                                              
      ELSE                                                                      
         R(1)= 0.D0                                                              
         R(2)= 0.D0                                                              
         R(3)= 0.D0                                                              
         R(4)= CCHIX                                                            
         R(5)=-SCHIX                                                            
         R(6)= 0.D0                                                              
         R(7)= 0.D0                                                              
         R(8)= 0.D0                                                              
         R(9)= 0.D0                                                              
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+1))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+1))                           
         R(1)= 0.D0                                                              
         R(2)= 0.D0                                                              
         R(3)= 0.D0                                                              
         R(4)= 0.D0                                                              
         R(5)= 0.D0                                                              
         R(6)= 0.D0                                                              
         R(7)= CCHIY*CCHIZ                                                      
         R(8)= CCHIY*SCHIZ                                                      
         R(9)=-SCHIY                                                            
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+2))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+2))                           
         R(1)= 0.D0                                                              
         R(2)= 0.D0                                                              
         R(3)= 0.D0                                                              
         R(4)= 0.D0                                                              
         R(5)= 0.D0                                                              
         R(6)= 0.D0                                                              
         R(7)=-SCHIY*SCHIZ                                                      
         R(8)= SCHIY*CCHIZ                                                      
         R(9)= 0.D0                                                              
         CALL LTRANS (NA,IONE,B(1),R,DLDA(     IA+3))                           
         CALL LTRANS (NA,IONE,B(4),R,DLDA(3*NA+IA+3))                           
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TVN (GRAD,K,NC,NA,IA,A,B,DLDC,DLDA,R)                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER GRAD                                                              
      DIMENSION DLDA(*),DLDC(*),A(*),B(*),R(*)                                       
      DATA NULL /0/, IONE /1/                                                   
      IF (K.GE.1) THEN                                                          
         SLPX=A(IA+1)                                                           
         SLPY=A(IA+2)                                                           
         SLPZ=A(IA+3)                                                           
         CALL FDLDSL (K,NA,IA,B,DLDA)                                           
         R(1)=SLPX                                                              
         R(2)=0.D0                                                               
         R(3)=0.D0                                                               
         R(4)=0.D0                                                               
         R(5)=SLPY                                                              
         R(6)=0.D0                                                               
         R(7)=0.D0                                                               
         R(8)=0.D0                                                               
         R(9)=SLPZ                                                              
         CALL LTRANS (IONE,IONE,B(1),R,B(1))                                    
         CALL LTRANS (IONE,IONE,B(4),R,B(4))                                    
         CALL LTRANV (IONE,NC,NC,R,DLDC(     1))                                
         CALL LTRANV (NULL,NC,NC,R,DLDC(3*NC+1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(     1))                                
         CALL LTRANV (NULL,NA,IA,R,DLDA(3*NA+1))                                
         CALL BNGEN (B)                                                         
         IF (GRAD.EQ.1) THEN                                                    
            CALL LTRANV (NULL,3,3,R,B(29))                                      
            CALL LTRANV (NULL,3,3,R,B(38))                                      
            CALL LTRANS (IONE,IONE,B(29),R,B(29))                               
            CALL LTRANS (IONE,IONE,B(32),R,B(32))                               
            CALL LTRANS (IONE,IONE,B(35),R,B(35))                               
            CALL LTRANS (IONE,IONE,B(38),R,B(38))                               
            CALL LTRANS (IONE,IONE,B(41),R,B(41))                               
            CALL LTRANS (IONE,IONE,B(44),R,B(44))                               
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC( 6*NC+1))                        
            CALL LTRANV (NULL,3*NC,3*NC,R,DLDC(15*NC+1))                        
            CALL LTRANV (NULL,NC,NC,R,DLDC( 6*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC( 9*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(12*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(15*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(18*NC+1))                            
            CALL LTRANV (NULL,NC,NC,R,DLDC(21*NC+1))                            
         END IF                                                                 
         IA=IA+3                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDLDSL (K,NA,IA,B,DLDA)                                        
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION DLDA(*),B(*)                                                         
      I=IA                                                                      
      DO 10 J=1,6                                                               
         DLDA(I+1)=0.D0                                                          
         DLDA(I+2)=0.D0                                                          
         DLDA(I+3)=0.D0                                                          
   10    I=I+NA                                                                 
      IF (K.GT.1) THEN                                                          
         I=IA                                                                   
         DLDA(I+1)=B(1)                                                         
         I=I+NA                                                                 
         DLDA(I+2)=B(2)                                                         
         I=I+NA                                                                 
         DLDA(I+3)=B(3)                                                         
         I=I+NA                                                                 
         DLDA(I+1)=B(4)                                                         
         I=I+NA                                                                 
         DLDA(I+2)=B(5)                                                         
         I=I+NA                                                                 
         DLDA(I+3)=B(6)                                                         
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TBI (K,NA,IA,A,B,DLDA)                                         
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION DLDA(*),A(*),B(*)                                                    
      IF (K.GE.1) THEN                                                          
         BIAX=A(IA+1)                                                           
         BIAY=A(IA+2)                                                           
         BIAZ=A(IA+3)                                                           
         CALL FDLDBI (K,NA,IA,DLDA)                                             
         B(1)=B(1)+BIAX                                                         
         B(2)=B(2)+BIAY                                                         
         B(3)=B(3)+BIAZ                                                         
         CALL BNGEN (B)                                                         
         IA=IA+3                                                                
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FDLDBI (K,NA,IA,DLDA)                                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION DLDA(*)                                                              
      I=IA                                                                      
      DO 10 J=1,6                                                               
         DLDA(I+1)=0.D0                                                          
         DLDA(I+2)=0.D0                                                          
         DLDA(I+3)=0.D0                                                          
   10    I=I+NA                                                                 
      IF (K.GT.1) THEN                                                          
         I=IA                                                                   
         DLDA(I+1)=1.D0                                                          
         I=I+NA                                                                 
         DLDA(I+2)=1.D0                                                          
         I=I+NA                                                                 
         DLDA(I+3)=1.D0                                                          
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LTRANS (N,M,Q,R,S)                                             
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      DIMENSION Q(*),R(*),S(*)                                                       
      Q1=Q(    1)                                                               
      Q2=Q(  M+1)                                                               
      Q3=Q(M+M+1)                                                               
      S(    1)=Q1*R(1)+Q2*R(2)+Q3*R(3)                                          
      S(  N+1)=Q1*R(4)+Q2*R(5)+Q3*R(6)                                          
      S(N+N+1)=Q1*R(7)+Q2*R(8)+Q3*R(9)                                          
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LTRANV (RFAC,N,M,R,V)                                          
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      SAVE                                                                      
      INTEGER RFAC                                                              
      DIMENSION R(*),V(*)                                                            
      IF (M.GT.0) THEN                                                          
         IF (RFAC.EQ.1) THEN                                                    
            R(10)=R(4)/R(1)                                                     
            R(11)=R(5)-R(2)*R(10)                                               
            R(12)=R(6)-R(3)*R(10)                                               
            R(13)=R(7)/R(1)                                                     
            R(14)=(R(8)-R(2)*R(13))/R(11)                                       
            R(15)=(R(9)-R(3)*R(13))-R(12)*R(14)                                 
            R(13)=R(13)-R(10)*R(14)                                             
         END IF                                                                 
         CALL R8VSCALE (1,M,R(1),V)                                             
         CALL R8VLINKT (N+1,1,M,R(2),V,V)                                       
         CALL R8VLINKT (N+N+1,1,M,R(3),V,V)                                     
         CALL R8VSCALE (N+1,M,R(11),V)                                          
         CALL R8VLINKT (1,N+1,M,R(10),V,V)                                      
         CALL R8VLINKT (N+N+1,N+1,M,R(12),V,V)                                  
         CALL R8VSCALE (N+N+1,M,R(15),V)                                        
         CALL R8VLINKT (1,N+N+1,M,R(13),V,V)                                    
         CALL R8VLINKT (N+1,N+N+1,M,R(14),V,V)                                  
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
      FUNCTION NSHX (NMAX,NMIN,MMAX,MMIN)                                       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      KMAX=MMAX+1                                                               
      NSHX=MAX(0,KMAX**2-                                                       
     1           MMIN**2+                                                       
     2           MIN(NMIN,MMIN)**2-                                             
     3           MIN(NMIN,KMAX)**2+                                             
     4           (NMAX-MMAX-DIM(NMIN,KMAX))*(2*MMAX+1)+                         
     5           (DIM(NMIN,MMIN)-NMAX+MMIN-1)*MAX(0,2*MMIN-1))                  
      RETURN                                                                    
      END                                                                       
      FUNCTION NLPX (NMAX,MMAX,MMIN)                                            
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                                   
      MDIF=MAX(0,MIN(NMAX,MMAX)-MMIN+1)                                         
      NLPX=MDIF*(MDIF+1)/2+MDIF*DIM(NMAX,MMAX)+MMIN-1                           
      RETURN                                                                    
      END                                                                       
      FUNCTION I8SSUM (ABEG,ALEN,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER AADR,ABEG,ALEN
      INTEGER A(*)
      I8SSUM=0
      AADR=ABEG
      DO 10 I=1,ALEN
         I8SSUM=I8SSUM+A(AADR)
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE I8VSET (ABEG,ALEN,S,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN,S
      INTEGER A(*)
      AADR=ABEG
      DO 10 I=1,ALEN
         A(AADR)=S
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE I8VADD (ABEG,BBEG,CBEG,VLEN,A,B,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,CADR,CBEG,VLEN
      INTEGER A(*),B(*),C(*)
      AADR=ABEG
      BADR=BBEG
      CADR=CBEG
      DO 10 I=1,VLEN
         C(CADR)=B(BADR)+A(AADR)
         AADR=AADR+1
         BADR=BADR+1
   10    CADR=CADR+1
      RETURN
      END
      SUBROUTINE I8VADDS (ABEG,BBEG,VLEN,S,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,VLEN,S
      INTEGER A(*),B(*)
      AADR=ABEG
      BADR=BBEG
      DO 10 I=1,VLEN
         B(BADR)=A(AADR)+S
         AADR=AADR+1
   10    BADR=BADR+1
      RETURN
      END
      SUBROUTINE I8VCUM (ABAS,ABEG,ALEN,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN,ABAS,APRV,ACUR
      INTEGER A(*)
      APRV=A(ABEG)
      A(ABEG)=ABAS
      AADR=ABEG+1
      DO 10 I=1,ALEN-1
         ACUR=A(AADR)
         A(AADR)=A(AADR-1)+APRV
         APRV=ACUR
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE I8VDEL (ABAS,ABEG,ALEN,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN,ABAS,APRV,ACUR
      INTEGER A(*)
      APRV=ABAS
      AADR=ABEG
      DO 10 I=1,ALEN
         ACUR=A(AADR)
         A(AADR)=ACUR-APRV
         APRV=ACUR
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE R8VSET (ABEG,ALEN,S,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN
      DIMENSION A(*)
      AADR=ABEG
      DO 10 I=1,ALEN
         A(AADR)=S
   10    AADR=AADR+1
      RETURN
      END
      FUNCTION R8SDOT (ABEG,BBEG,VLEN,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER AADR,ABEG,BADR,BBEG,VLEN
      DIMENSION A(*),B(*)
      R8SDOT=0.D0
      AADR=ABEG
      BADR=BBEG
      DO 10 I=1,VLEN
         R8SDOT=R8SDOT+A(AADR)*B(BADR)
         AADR=AADR+1
   10    BADR=BADR+1
      RETURN
      END
      FUNCTION R8SSUM (ABEG,ALEN,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER AADR,ABEG,ALEN
      DIMENSION A(*)
      R8SSUM=0.D0
      AADR=ABEG
      DO 10 I=1,ALEN
         R8SSUM=R8SSUM+A(AADR)
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE R8SLT (ABEG,ALEN,S,A,J)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN
      DIMENSION A(*)
      AADR=ABEG
      DO 10 I=1,ALEN
         IF (S.LT.A(AADR)) THEN
            J=I-1
            GO TO 20
         END IF
   10    AADR=AADR+1
      J=ALEN
   20 RETURN
      END
      SUBROUTINE R8VSUB (ABEG,BBEG,CBEG,VLEN,A,B,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,CADR,CBEG,VLEN
      DIMENSION A(*),B(*),C(*)
      AADR=ABEG
      BADR=BBEG
      CADR=CBEG
      DO 10 I=1,VLEN
         C(CADR)=B(BADR)-A(AADR)
         AADR=AADR+1
         BADR=BADR+1
   10    CADR=CADR+1
      RETURN
      END
      SUBROUTINE R8VMUL (ABEG,BBEG,CBEG,VLEN,A,B,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,CADR,CBEG,VLEN
      DIMENSION A(*),B(*),C(*)
      AADR=ABEG
      BADR=BBEG
      CADR=CBEG
      DO 10 I=1,VLEN
         C(CADR)=B(BADR)*A(AADR)
         AADR=AADR+1
         BADR=BADR+1
   10    CADR=CADR+1
      RETURN
      END
      SUBROUTINE R8VSCALE (ABEG,ALEN,S,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,ALEN
      DIMENSION A(*)
      AADR=ABEG
      DO 10 I=1,ALEN
         A(AADR)=S*A(AADR)
   10    AADR=AADR+1
      RETURN
      END
      SUBROUTINE R8VSCATS (QBEG,QLEN,S,Q,A)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER QADR,QBEG,QLEN
      INTEGER Q(*)
      DIMENSION A(*)
      QADR=QBEG
      DO 10 I=1,QLEN
         A(Q(QADR))=S
   10    QADR=QADR+1
      RETURN
      END
      SUBROUTINE R8VLINKT (ABEG,BBEG,VLEN,S,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,VLEN
      DIMENSION A(*),B(*)
      AADR=ABEG
      BADR=BBEG
      DO 10 I=1,VLEN
         B(BADR)=B(BADR)+S*A(AADR)
         AADR=AADR+1
   10    BADR=BADR+1
      RETURN
      END
      SUBROUTINE R8VLINKQ (ABEG,BBEG,CBEG,VLEN,S,A,B,C)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,BADR,BBEG,CADR,CBEG,VLEN
      DIMENSION A(*),B(*),C(*)
      AADR=ABEG
      BADR=BBEG
      CADR=CBEG
      DO 10 I=1,VLEN
         C(CADR)=C(CADR)+S*A(AADR)*B(BADR)
         AADR=AADR+1
         BADR=BADR+1
   10    CADR=CADR+1
      RETURN
      END
      SUBROUTINE R8VGATHP (ABEG,AINC,BBEG,BLEN,A,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      INTEGER AADR,ABEG,AINC,BADR,BBEG,BLEN
      DIMENSION A(*),B(*)
      AADR=ABEG
      BADR=BBEG
      DO 10 I=1,BLEN
         B(BADR)=A(AADR)
         AADR=AADR+AINC
   10    BADR=BADR+1
      RETURN
      END
