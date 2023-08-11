!!>
!  sample for efpp, ver.230811
!!<

module time_m
  use const_base_m
  implicit none
  private
  public :: time__t

  type time__t
     integer(SI) :: loop
     real(DR) :: dt
     real(DR) :: t
  end type time__t

end module time_m
