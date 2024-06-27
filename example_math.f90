subroutine sum_array(arr, n, total)
  implicit none
  integer, intent(in) :: n
  real, intent(in) :: arr(n)
  real, intent(out) :: total
  integer :: i
  
  total = 0.0
  do i = 1, n
     total = total + arr(i)
  end do
end subroutine sum_array

