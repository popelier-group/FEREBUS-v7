module zero_mean_module
  use mean_type, only: MeanType
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: ZeroMean

  type, extends(MeanType) :: ZeroMean
  contains
    private
    procedure, public, pass(self) :: init => init_zero_mean
    procedure, public, pass(self) :: value => value_zero_mean
    procedure, public, pass(self) :: write => write_zero_mean
  end type ZeroMean
  
contains

  subroutine init_zero_mean(self, x, y)
    class(ZeroMean), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(in) :: y
  end subroutine init_zero_mean

  subroutine value_zero_mean(self, x, mean)
    class(ZeroMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    mean(:) = 0.0_wp
  end subroutine value_zero_mean

  subroutine write_zero_mean(self, unit)
    class(ZeroMean), intent(in) :: self
    integer, intent(in) :: unit
    write(unit, '(A)') "[mean]"
    write(unit, '(A, 1x, A)') "type", "zero"
  end subroutine write_zero_mean
end module zero_mean_module