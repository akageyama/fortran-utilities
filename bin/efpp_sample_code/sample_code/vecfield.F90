!!>
!  sample for efpp, ver.230811
!!<

module vecfield_m
  use const_base_m
  implicit none

  private
  public :: VECFIELD__DVOL
  public :: operator( .dot. ),  &
            vecfield__init,  &
            vecfield__is_zero_Bool

  integer(SI), parameter :: VECFIELD__NX = 10
  integer(SI), parameter :: VECFIELD__NY = 12
  integer(SI), parameter :: VECFIELD__NZ = 14
  real(DR), parameter :: VECFIELD__DX = 1.0_DR / VECFIELD__NX
  real(DR), parameter :: VECFIELD__DY = 1.0_DR / VECFIELD__NY
  real(DR), parameter :: VECFIELD__DZ = 1.0_DR / VECFIELD__NZ
  real(DR), parameter :: VECFIELD__DVOL = VECFIELD__DX*VECFIELD__DY*VECFIELD__DZ

  type, public :: vecfield__t
    real(DR) :: x(VECFIELD__NX,VECFIELD__NY,VECFIELD__NZ)
    real(DR) :: y(VECFIELD__NX,VECFIELD__NY,VECFIELD__NZ)
    real(DR) :: z(VECFIELD__NX,VECFIELD__NY,VECFIELD__NZ)
  end type vecfield__t

  interface operator( .dot. )
     module procedure operator_dot_product
  end interface


contains

  function operator_dot_product(a,b)
    type(vecfield__t), intent(in) :: a, b
    real(DR), dimension(VECFIELD__NX,  &
                        VECFIELD__NY,  &
                        VECFIELD__NZ) :: operator_dot_product
    operator_dot_product  = a%x*b%x
    operator_dot_product = operator_dot_product + a%y*b%y
    operator_dot_product = operator_dot_product + a%z*b%z

  end function operator_dot_product


  subroutine vecfield__init(a)
    type(vecfield__t), intent(out) :: a

print *, "\\vecfield_m(51): ", ' hello. I am in a function named vecfield__init'
print *, "\\vecfield_m(52): ", '        which is in vecfield_m'
print *, "\\vecfield_m(53): ", '        this is at line number 53'
print *, "\\vecfield_m(54): ", '        in short, ', 'i am in vecfield_m/vecfield__init'

    a%x(:,:,:) = 1.0_DR
    a%y(:,:,:) = 0.0_DR
    a%z(:,:,:) = 0.0_DR
  end subroutine vecfield__init


  function vecfield__is_zero_Bool(a) result(ans)
    type(vecfield__t), intent(in) :: a
    logical :: ans

print *, "\\vecfield_m(66): ", ' hello. I am in a function named vecfield__is_zero_Bool'
print *, "\\vecfield_m(67): ", '        which is in vecfield_m'
print *, "\\vecfield_m(68): ", '        this is at line number 68'
print *, "\\vecfield_m(69): ", '        in short, ', 'i am in vecfield_m/vecfield__is_zero_Bool'

    ans = ( sum( a .dot. a ) == 0.0_DR )

  end function vecfield__is_zero_Bool

end module vecfield_m
