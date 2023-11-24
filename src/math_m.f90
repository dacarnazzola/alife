module math_m
use, non_intrinsic :: constants_m, only: sp, dp
implicit none
private

    public :: sum, avg, var, std

    contains


        pure function sum(x) result(sum_x)
            real(sp), intent(in) :: x(:)
            real(sp) :: sum_x
            real(dp) :: work
            integer :: n, i
            n = size(x)
            if (n > 0) then
                work = real(x(1), kind=dp)
                do i=2,n
                    work = work + real(x(i), kind=dp)
                end do
                sum_x = real(work, kind=sp)
            else
                sum_x = 0.0_sp
            end if
        end function sum


        pure function avg(x) result(avg_x)
            real(sp), intent(in) :: x(:)
            real(sp) :: avg_x
            integer :: n
            n = size(x)
            if (n > 0) then
                avg_x = sum(x)/real(n, kind=sp)
            else
                avg_x = 0.0_sp
            end if
        end function avg


        pure function var(x) result(var_x)
            real(sp), intent(in) :: x(:)
            real(sp) :: var_x
            integer :: n
            n = size(x)
            if (n > 1) then
                var_x = sum((x - avg(x))**2)/real(n - 1, kind=sp)
            else
                var_x = 0.0_sp
            end if
        end function var


        pure function std(x) result(std_x)
            real(sp), intent(in) :: x(:)
            real(sp) :: std_x
            std_x = sqrt(var(x))
        end function std


end module math_m
