module linear_mean_module
  use least_squares_mean_module, only: LeastSquaresMean
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: LinearMean

  type, extends(LeastSquaresMean) :: LinearMean
  contains
    private
    procedure, public, pass(self) :: value => value_linear_mean
    procedure, public, pass(self) :: write => write_linear_mean
    procedure, public, pass(self) :: order => order_linear
  end type LinearMean
  
contains

  subroutine value_linear_mean(self, x, mean)
    class(LinearMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    real(kind=wp), dimension(:,:), allocatable :: a
    
    allocate(a, source=x)
    a = a - spread(self%xmin, 1, size(x,1))

    mean = matmul(a, self%beta) + self%ymin
  end subroutine value_linear_mean

  subroutine write_linear_mean(self, unit)
    class(LinearMean), intent(in) :: self
    integer, intent(in) :: unit
    character(len=50) :: fmt

    write(fmt,'(a,i0,a)') "(a,1x,(", size(self%beta), "(g0,1x)))"

    write(unit, '(a)') "[mean]"
    write(unit, '(a, 1x, a)') "type", "linear"
    write(unit, trim(fmt)) "beta", self%beta
    write(unit, trim(fmt)) "xmin", self%xmin
    write(unit, '(a, 1x, g0)') "ymin", self%ymin
  end subroutine write_linear_mean

  function order_linear(self) result(order)
    class(LinearMean), intent(in) :: self
    integer :: order

    order = 1
  end function order_linear
end module linear_mean_module