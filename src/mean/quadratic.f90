module quadratic_mean_module
  use least_squares_mean_module, only: LeastSquaresMean
  use kinds, only: wp
  use utils, only: mean
  implicit none
  private
  public :: QuadraticMean

  type, extends(LeastSquaresMean) :: QuadraticMean
  contains
    private
    procedure, public, pass(self) :: value => value_quadratic_mean
    procedure, public, pass(self) :: write => write_quadratic_mean
    procedure, public, pass(self) :: order => order_quadratic
  end type QuadraticMean
  
contains

  subroutine value_quadratic_mean(self, x, mean)
    class(QuadraticMean), intent(in) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:), intent(out) :: mean
    real(kind=wp), dimension(:,:), allocatable :: a

    allocate(a, source=x)
    a = a - spread(self%xmin, 1, size(x,1))
    a = a*a

    mean = matmul(a, self%beta) + self%ymin
  end subroutine value_quadratic_mean

  subroutine write_quadratic_mean(self, unit)
    class(QuadraticMean), intent(in) :: self
    integer, intent(in) :: unit
    character(len=50) :: fmt

    write(fmt,'(a,i0,a)') "(a,1x,(", size(self%beta), "(g0,1x)))"

    write(unit, '(a)') "[mean]"
    write(unit, '(a, 1x, a)') "type", "quadratic"
    write(unit, trim(fmt)) "beta", self%beta
    write(unit, trim(fmt)) "xmin", self%xmin
    write(unit, '(a, 1x, g0)') "ymin", self%ymin
  end subroutine write_quadratic_mean

  function order_quadratic(self) result(order)
    class(QuadraticMean), intent(in) :: self
    integer :: order

    order = 2
  end function order_quadratic
end module quadratic_mean_module