module mean_type
  use kinds, only: wp
  use writable_module, only: Writable
  implicit none
  private
  public :: MeanType

  type, abstract, extends(Writable) :: MeanType
  contains
    private
    procedure(init_interface), public, deferred, pass(self) :: init
    procedure(value_interface), public, deferred, pass(self) :: value
    procedure(write_interface), public, deferred, pass(self) :: write
    procedure, public, pass(self) :: nparams => nparams_interface
    procedure, public, pass(self) :: get_params => get_params_interface
    procedure, public, pass(self) :: set_params => set_params_interface
  end type MeanType

  abstract interface
    subroutine init_interface(self, x, y)
      import MeanType
      import wp
      class(MeanType), intent(inout) :: self
      real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(in) :: y
    end subroutine init_interface

    subroutine value_interface(self, x, mean)
      import MeanType
      import wp
      class(MeanType), intent(in) :: self
      real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(out) :: mean
    end subroutine value_interface

    subroutine write_interface(self, unit)
      import MeanType
      class(MeanType), intent(in) :: self
      integer, intent(in) :: unit
    end subroutine write_interface
  end interface

  contains

  function nparams_interface(self) result(nparams)
    class(MeanType), intent(in) :: self
    integer :: nparams

    nparams = 0
  end function nparams_interface

  function get_params_interface(self) result(params)
    class(MeanType), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    allocate(params(0))
  end function get_params_interface

  subroutine set_params_interface(self, params)
    class(MeanType), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
  end subroutine set_params_interface
end module mean_type