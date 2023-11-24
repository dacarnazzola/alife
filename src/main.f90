program alife
use, non_intrinsic :: constants_m, only: stdout, stdin, sp
use, non_intrinsic :: math_m, only: sum, avg, var, std
implicit none

    integer :: i_max, i
    real(sp), allocatable :: test_vector(:)

    write(stdout, '(a)', advance='no') 'enter test vector length: '
    
    read(stdin,*) i_max
    if (allocated(test_vector)) deallocate(test_vector)
    allocate(test_vector(i_max))

    do i=1,i_max
        test_vector(i) = real(i, kind=sp)
    end do

    write(stdout,'(a,*(" ",f0.1))') 'test_vector: ',test_vector
    write(stdout,'(a,f0.1)') 'sum: ',sum(test_vector)
    write(stdout,'(a,f0.1)') 'avg: ',avg(test_vector)
    write(stdout,'(a,f0.1)') 'var: ',var(test_vector)
    write(stdout,'(a,f0.1)') 'std: ',std(test_vector)

end program alife
