!> Implementation of a ferebus kernel config
module kernel_config_type_module
  use config_type_module, only : ConfigType
  use config_error, only: error_data
  use tomlf, only: toml_table
  implicit none
  private
  public :: KernelConfig
 
  type, abstract, extends(ConfigType) :: KernelConfig
    character(len=:), allocatable :: name
    integer, allocatable, dimension(:) :: active_dims
    integer :: ndim = -1
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure, public, pass(self) :: set_ndim
  end type KernelConfig

  abstract interface
    subroutine init_interface(self, table, error)
      import KernelConfig
      import toml_table
      import error_data
      class(KernelConfig), intent(inout) :: self
      type(toml_table), intent(inout) :: table
      type(error_data), allocatable, intent(out) :: error
    end subroutine init_interface
  end interface

contains

  subroutine set_ndim(self, ndim)
    class(KernelConfig), intent(inout) :: self
    integer, intent(in) :: ndim
    integer :: i

    if (self%ndim .eq. -1) then
      if (.not.allocated(self%active_dims)) then
        self%ndim = ndim
      else
        self%ndim = size(self%active_dims)
      end if
    end if

    if (.not.allocated(self%active_dims)) then
      allocate(self%active_dims(self%ndim))
      do i = 1, self%ndim
        self%active_dims(i) = i
      end do
    end if
  end subroutine set_ndim
end module kernel_config_type_module
