module random_m
use, non_intrinsic :: constants_m, only: sp, pi_sp
implicit none
private

    public :: random_uniform, random_normal

    contains


        impure subroutine random_uniform(vals, n, val_min, val_max)
            real(sp), intent(out) :: vals(*)
            integer, intent(in) :: n
            real(sp), intent(in) :: val_min, val_max
            call random_number(vals(1:n))
            vals(1:n) = vals(1:n)*(val_max - val_min) + val_min
        end subroutine random_uniform 


        impure subroutine random_normal(vals, n, val_avg, val_std)
            real(sp), intent(out) :: vals(*)
            integer, intent(in) :: n
            real(sp), intent(in) :: val_avg, val_std
            real(sp) :: u((n+1)/2), v((n+1)/2), r((n+1)/2)
            integer :: nu
            call random_number(v)
            call random_number(u)
            u = 1.0_sp - u
            nu = size(u)
            r = val_std*sqrt(-2.0_sp*log(u))
            vals(1:nu) = val_avg + r*sin(2.0_sp*pi_sp*v)
            if (n > (nu + 1)) vals((nu+1):n) = val_avg + r(1:(n-nu))*cos(2.0*pi_sp*v(1:(n-nu)))
        end subroutine random_normal


end module random_m
