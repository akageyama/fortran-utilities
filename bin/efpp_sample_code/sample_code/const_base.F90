module const_base_m
  use iso_fortran_env
  implicit none
  private
  public :: SI, DI, SR, DR, NIL, NAN, PI, TWOPI
  public :: const_base__check

  !<< Fortran constants >>!
  integer, parameter :: SI = int32
  integer, parameter :: DI = int64
  integer, parameter :: SR = real32
  integer, parameter :: DR = real64
  integer, parameter :: NIL = -huge(1)
  real(DR), parameter :: NAN = -huge(1.0_DR)

  !<< Mathematical constants >>!
  real(DR), parameter :: PI = atan(1.0_DR)*4
  real(DR), parameter :: TWOPI = PI*2


contains


  subroutine print_DR_or_SI( string, val_real, val_int )
    character(len=*), intent(in) :: string
    real(DR), intent(in), optional :: val_real
    integer(SI), intent(in), optional :: val_int

    integer(SI), parameter :: LENGTH = 60
    character(len=22) :: string_for_val
    character(len=LENGTH) :: line
    integer(SI) :: len_str_for_val

    line = repeat('_',LENGTH)
    line(1:len_trim(string)) = trim(string)

    if ( present(val_real) ) then
      write(string_for_val,'(1pe22.15)') val_real
    else if ( present(val_int) ) then
      write(string_for_val,'(a1,i0)') ' ', val_int ! put a space in front of i
    end if

    len_str_for_val = len_trim(string_for_val)
    line(LENGTH-len_str_for_val:LENGTH) = trim(string_for_val)

    print *, line
  end subroutine print_DR_or_SI


  subroutine const_base__check
    call print_DR_or_SI( 'const_base_m: SI', val_int=SI )
    call print_DR_or_SI( 'const_base_m: DI', val_int=DI )
    call print_DR_or_SI( 'const_base_m: SR', val_int=SR )
    call print_DR_or_SI( 'const_base_m: DR', val_int=DR )
    call print_DR_or_SI( 'const_base_m: PI', val_real=PI )
    call print_DR_or_SI( 'const_base_m: TWOPI', val_real=TWOPI )
    call print_DR_or_SI( 'const_base_m: NIL', val_int=NIL )
    call print_DR_or_SI( 'const_base_m: NAN', val_real=NAN )
  end subroutine const_base__check   

end module const_base_m
