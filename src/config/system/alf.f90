module alf_config_module
  use config_type_module, only: WritableConfigType
  implicit none
  private
  public :: AtomicLocalFrameConfig

  type, extends(WritableConfigType) :: AtomicLocalFrameConfig
    integer :: origin
    integer :: x_axis
    integer :: xy_plane
  contains
    private
    procedure, public, pass(self) :: init_alf_config
    procedure, public, pass(self) :: init_alf_config_array
    procedure, public, pass(self) :: info
    procedure, public, pass(self) :: write
    generic, public :: init => init_alf_config, init_alf_config_array
  end type AtomicLocalFrameConfig

contains

  subroutine init_alf_config(self, origin, x_axis, xy_plane)
    class(AtomicLocalFrameConfig), intent(inout) :: self
    integer, intent(in) :: origin
    integer, intent(in) :: x_axis
    integer, intent(in) :: xy_plane

    self%origin = origin
    self%x_axis = x_axis
    self%xy_plane = xy_plane
  end subroutine init_alf_config

  subroutine init_alf_config_array(self, alf)
    class(AtomicLocalFrameConfig), intent(inout) :: self
    integer, dimension(3), intent(in) :: alf

    self%origin = alf(1)
    self%x_axis = alf(2)
    self%xy_plane = alf(3)
  end subroutine init_alf_config_array

  subroutine info(self, unit)
    class(AtomicLocalFrameConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '("  - ", A, T30, I0, 1X, I0, 1X, I0)') "ALF", self%origin, self%x_axis, self%xy_plane
  end subroutine info

  subroutine write(self, unit)
    class(AtomicLocalFrameConfig), intent(in) :: self
    integer, intent(in) :: unit

    write(unit, '(A, 1X, I0, 1X, I0, 1X, I0)') "ALF", self%origin, self%x_axis, self%xy_plane
  end subroutine write
end module alf_config_module