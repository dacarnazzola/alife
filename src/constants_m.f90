module constants_m
use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit, int8, int16, int32, int64, real32, real64, real128
implicit none
private

    integer, parameter, public :: stdin = input_unit
    integer, parameter, public :: stdout = output_unit
    integer, parameter, public :: stderr = error_unit

    integer, parameter, public :: i8 = int8
    integer, parameter, public :: i16 = int16
    integer, parameter, public :: i32 = int32
    integer, parameter, public :: i64 = int64

    integer, parameter, public :: sp = real32
    integer, parameter, public :: dp = real64
    integer, parameter, public :: qp = real128

    real(sp), parameter, public :: pi_sp = acos(-1.0_sp)
    real(dp), parameter, public :: pi_dp = acos(-1.0_dp)
    real(qp), parameter, public :: pi_qp = acos(-1.0_qp)

end module constants_m
