function c_null_to_f_string(cstr) result(fstr)
  use iso_c_binding
  implicit none
  character(kind=c_char), dimension(:) :: cstr
  character(len=256) :: fstr
  integer :: i

  i = 1
  do while (i <= size(cstr) .and. cstr(i) /= c_null_char)
    fstr(i:i) = cstr(i)
    i = i + 1
  end do
  fstr(i:) = ' '
end function

subroutine call_cm4(UT,thet, phi, alt, dst,f107, &
     pred1,pred2,pred3,pred4,pred5,pred6, &
     CORD, NHMF1,NHMF2, NLMF1,NLMF2, cof_path, bmdl,jmdl) bind(C, name="call_cm4")

      use iso_c_binding

      implicit none

      ! Declare variables
      character(len=256) :: PATH(3)
      integer :: UNIT(3)
      logical :: LOAD(3)
      logical :: INDX(2)
      logical :: GMUT
      real(c_double), intent(in) :: UT
      real(8) :: MUT
      logical(c_bool), intent(in) :: pred1,pred2,pred3,pred4,pred5,pred6
      logical(c_bool), intent(in) :: CORD
      logical :: PRED(6)
      logical :: CURR, COEF
      integer(c_int), intent(in) :: NHMF1,NHMF2, NLMF1,NLMF2
      integer :: NHMF(2), NLMF(2)
      real(c_double), intent(in) :: alt, thet, phi
      real(c_double), intent(in) :: dst, f107
      real(c_double), dimension(3, 7), intent(out) :: bmdl
      real(c_double), dimension(3, 4), intent(out) :: jmdl
      real(8), dimension(3, 7) :: gmdl
      integer(4) :: perr, oerr, cerr
      integer :: i
      character(kind=c_char), intent(in) :: cof_path(*)


      character(len=256) :: fstr
      i = 1
      do while (i <= 256 .and. cof_path(i) /= c_null_char)
            fstr(i:i) = cof_path(i)
            i = i + 1
      end do
      if (i <= 256) fstr(i:) = ' '  ! Pad rest with spaces
      i = 1




      ! Assigning values
      PATH(1) = fstr
      !PATH(2) = "strings are weird"
      !PATH(3) = "weird1"

      UNIT(1) = 11![10, 20, 30]
      UNIT(2) = 22
      UNIT(3) = 33
      LOAD(1) = .true. ![.true., .false., .false.]
      LOAD(2) = .false.
      LOAD(3) = .false.
      !Use arg dst and f10.7 respectively
      INDX(1) = .false.![.false., .false.]
      INDX(2) = .false.!
      GMUT = .true.
      !MUT = 51544.50847222
      !CORD = .true.!.false.!.true.
      !PRED = [.true.,.true.,.true., .true., .true., .true.]



      PRED = [pred1,pred2,pred3,pred4,pred5,pred6]



      CURR = .true.
      COEF = .false.



      !NHMF = [13, 45]
      !NLMF = [1, 14]
      NHMF = [NHMF1, NHMF2]
      NLMF = [NLMF1, NLMF2]



      ! Initialize altitude
      ! Set all elements to this value

      ! Initialize time in year decimal
      !UT = 1964.49738353192675!2000  ! Modified Julian Date 51544.50847222
      !1.96407E+11

      !thet = 44.0152  ! Initialize if needed, e.g. theta = [(i, i=1, pos_len)]
      !phi = - 62.7482    ! Initialize if needed, e.g. phi = [(i, i=-180, 180)]

      !dst = 7
      !f107 = 723!723
      perr = 1
      oerr = 50
      cerr = 0

      ! Initialize bmdl, jmdl, gmdl
      bmdl = 0.0
      jmdl = 0.0
      gmdl = 0.0

      !call hello()


      call CM4FIELD(PATH, UNIT, LOAD, INDX, GMUT, CORD, PRED, &
                     CURR, COEF, NHMF, NLMF, UT, MUT, thet, phi, alt, &
                     dst, f107, bmdl, jmdl, gmdl, perr, oerr, cerr)
      !print *, CHAR(10)
      !print *, jmdl
      !print *, CHAR(10)
      !UT = 8888
      !print *, gmdl
      end subroutine call_cm4

subroutine call_cm4_org(UT,thet, phi, alt, dst,f107, &
     pred1,pred2,pred3,pred4,pred5,pred6, &
     CORD, NHMF1,NHMF2, NLMF1,NLMF2, bmdl,jmdl) bind(C, name="call_cm4_org")

      use iso_c_binding

      implicit none

      ! Declare variables
      character(len=256) :: PATH(3)
      integer :: UNIT(3)
      logical :: LOAD(3)
      logical :: INDX(2)
      logical :: GMUT
      real(c_double), intent(in) :: UT
      real(8) :: MUT
      logical(c_bool), intent(in) :: pred1,pred2,pred3,pred4,pred5,pred6
      logical(c_bool), intent(in) :: CORD
      logical :: PRED(6)
      logical :: CURR, COEF
      integer(c_int), intent(in) :: NHMF1,NHMF2, NLMF1,NLMF2
      integer :: NHMF(2), NLMF(2)
      real(c_double), intent(in) :: alt, thet, phi
      real(c_double), intent(in) :: dst, f107
      real(c_double), dimension(3, 7), intent(out) :: bmdl
      real(c_double), dimension(3, 4), intent(out) :: jmdl
      real(8), dimension(3, 7) :: gmdl
      integer(c_int) :: perr, oerr, cerr
      integer :: i
      !character(kind=c_char), intent(in) :: cof_path(*)


      print *, "Finish to save parameters"
      print *, "print CORD: ", CORD
      print *, "print pred1: ", pred1
      print *, "NLMF1: ", NLMF1
      print *, "NLMF2: ", NLMF2
      print *, "NHMF1: ", NHMF1
      print *, "NHMF2: ", NHMF2



      ! Assigning values
      PATH(1) = "/Users/lily/Projects/CM4/CM4/umdl.CM4"
      !PATH(2) = "strings are weird"
      !PATH(3) = "weird1"

      UNIT(1) = 11![10, 20, 30]
      UNIT(2) = 22
      UNIT(3) = 33
      LOAD(1) = .true. ![.true., .false., .false.]
      LOAD(2) = .false.
      LOAD(3) = .false.

      print *, "print LORD(1): ", LOAD(1)
      !Use arg dst and f10.7 respectively
      INDX(1) = .false.![.false., .false.]
      INDX(2) = .false.!
      GMUT = .true.

      print *, "Finish to save PATH"
      !MUT = 51544.50847222
      !CORD = .true.!.false.!.true.
      !PRED = [.true.,.true.,.true., .true., .true., .true.]


      PRED = [pred1,pred2,pred3,pred4,pred5,pred6]

      print *, "Finish to save PRED"


      CURR = .true.
      COEF = .false.


      !NHMF = [13, 45]
      !NLMF = [1, 14]
      NHMF = [NHMF1, NHMF2]
      NLMF = [NLMF1, NLMF2]

      print *, "Finish to save NHMF"

      ! Initialize altitude
      ! Set all elements to this value

      ! Initialize time in year decimal
      !UT = 1964.49738353192675!2000  ! Modified Julian Date 51544.50847222
      !1.96407E+11

      !thet = 44.0152  ! Initialize if needed, e.g. theta = [(i, i=1, pos_len)]
      !phi = - 62.7482    ! Initialize if needed, e.g. phi = [(i, i=-180, 180)]

      !dst = 7
      !f107 = 723!723
      perr = 1
      oerr = 50
      cerr = 0

      ! Initialize bmdl, jmdl, gmdl
      bmdl = 0.0
      jmdl = 0.0
      gmdl = 0.0

      !call hello()

      print *, "call cm4field"
      call CM4FIELD(PATH, UNIT, LOAD, INDX, GMUT, CORD, PRED, &
                     CURR, COEF, NHMF, NLMF, UT, MUT, thet, phi, alt, &
                     dst, f107, bmdl, jmdl, gmdl, perr, oerr, cerr)
      !print *, CHAR(10)
      !print *, jmdl
      !print *, CHAR(10)
      !UT = 8888
      !print *, gmdl
      end subroutine call_cm4_org

subroutine fortran_add(x, y, result) bind(C, name="fortran_add")
  use iso_c_binding
  implicit none
  real(c_double), intent(in) :: x, y
  real(c_double), intent(out) :: result

  result = x + y
end subroutine fortran_add