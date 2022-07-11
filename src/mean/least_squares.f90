module least_squares_mean_module
    use mean_type, only: MeanType
    use kinds, only: wp
    use linalg_utils, only: posv
    implicit none
    private
    public :: LeastSquaresMean
  
    type, abstract, extends(MeanType) :: LeastSquaresMean
      real(kind=wp), dimension(:), allocatable :: beta
      real(kind=wp), dimension(:), allocatable :: xmin
      real(kind=wp) :: ymin
    contains
      private
      procedure(order_interface), public, deferred, pass(self) :: order
      procedure, public, pass(self) :: init => init_least_squares_mean
      procedure, public, pass(self) :: nparams => nparams_lsq_mean
      procedure, public, pass(self) :: get_params => get_params_lsq_mean
      procedure, public, pass(self) :: set_params => set_params_lsq_mean
    end type LeastSquaresMean

    abstract interface
      function order_interface(self) result(order)
        import LeastSquaresMean
        class(LeastSquaresMean), intent(in) :: self
        integer :: order
      end function order_interface
    end interface
    
  contains
  
    subroutine init_least_squares_mean(self, x, y)
      class(LeastSquaresMean), intent(inout) :: self
      real(kind=wp), dimension(:,:), intent(in) :: x
      real(kind=wp), dimension(:), intent(in) :: y

      integer :: ntrain, nfeats, i
      integer, dimension(1) :: imin
      real(kind=wp), dimension(:,:), allocatable :: a
      real(kind=wp), dimension(:), allocatable :: b
      real(kind=wp), dimension(:,:), allocatable :: aa, ab

      ntrain = size(x, 1)
      nfeats = size(x, 2)

      imin = minloc(y)

      allocate(self%xmin(nfeats))
      self%xmin = x(imin(1),:)
      self%ymin = y(imin(1))

      allocate(a, source=x)
      allocate(b, source=y)
      a = a - spread(self%xmin, 1, ntrain)
      b = b - self%ymin

      do i = 1, self%order() - 1
        a = a*a
      end do

      allocate(aa(nfeats, nfeats))
      allocate(ab(nfeats, 1))

      aa = matmul(transpose(a), a)
      ab(:,1) = matmul(transpose(a), b)

      call posv(aa, ab)

      allocate(self%beta(nfeats))
      self%beta = ab(:,1)
    end subroutine init_least_squares_mean

    function nparams_lsq_mean(self) result(nparams)
      class(LeastSquaresMean), intent(in) :: self
      integer :: nparams
  
      nparams = size(self%beta)
    end function nparams_lsq_mean
  
    function get_params_lsq_mean(self) result(params)
      class(LeastSquaresMean), intent(in) :: self
      real(kind=wp), dimension(:), allocatable :: params
  
      allocate(params, source=self%beta)
    end function get_params_lsq_mean
  
    subroutine set_params_lsq_mean(self, params)
      class(LeastSquaresMean), intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
  
      self%beta = params
    end subroutine set_params_lsq_mean
  end module least_squares_mean_module