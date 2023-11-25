program alife
use, non_intrinsic :: constants_m, only: stdout, sp
use, non_intrinsic :: random_m, only: random_uniform, random_normal
use, non_intrinsic :: math_m, only: vavg, vstd
implicit none

    integer :: i, j, n
    real(sp), allocatable :: x(:)

    do i=1,6
        do j=1,9
            n = j*(10**i)
            if (allocated(x)) deallocate(x)
            allocate(x(n))
            call random_uniform(x, n, 1.0, 6.0)
            write(stdout,'(a,i0,2(a,f0.1))') 'n: ',n,', min: ',minval(x),', max: ',maxval(x)
            call random_normal(x, n, 19.93, 8.31)
            write(stdout,'(a,i0,2(a,f0.1))') 'n: ',n,', avg: ',vavg(x),', std: ',vstd(x)
        end do
    end do 

end program alife
