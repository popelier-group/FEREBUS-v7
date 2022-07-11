module constant_mean_module
  use mean_type, only: MeanType
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: ConstantMean

  type, extends(MeanType) :: ConstantMean
    real(kind=wp) :: mean
  contains
    private
    procedure, public, pass(self) :: init => init_constant_mean
    procedure, public, pass(self) :: value => value_constant_mean
    procedure, public, pass(self) :: write => write_constant_mean
  end type ConstantMean
  
contains

  subroutine init_constant_mean(self, x, y)
    class(ConstantMean), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(in) :: y

    self%mean = mean(y)
  end subroutine init_constant_mean

  subroutine value_constant_mean(self, x, mean)
    class(ConstantMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    mean(:) = self%mean
  end subroutine value_constant_mean

  subroutine write_constant_mean(self, unit)
    class(ConstantMean), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, '(A)') "[mean]"
    write(unit, '(A, 1x, A)') "type", "constant"
    write(unit, '(A, 1x, G0)') "value", self%mean
  end subroutine write_constant_mean

  function nparams_constant_mean(self) result(nparams)
    class(ConstantMean), intent(in) :: self
    integer :: nparams

    nparams = 1
  end function nparams_constant_mean

  function get_params_constant_mean(self) result(params)
    class(ConstantMean), intent(in) :: self
    real(kind=wp), dimension(:), allocatable :: params

    params = (/self%mean/)
  end function get_params_constant_mean

  subroutine set_params_constant_mean(self, params)
    class(ConstantMean), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params

    self%mean = params(1)
  end subroutine set_params_constant_mean
end module constant_mean_module