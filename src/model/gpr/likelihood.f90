module likelihood_module
  use kinds, only: wp
  use writable_module, only: Writable
  use linalg_utils, only: potrf, potrs
  use utils_arith, only: log_determinant
  use utils_array, only: ones
  use constants, only: LOG_TWOPI
  use utils, only: random
  implicit none
  private
  public :: Likelihood, ConcentratedLikelihood, MarginalLikelihood

  type, abstract, extends(Writable) :: Likelihood
    real(kind=wp), dimension(:), allocatable :: computed_params !> Change this to computed_kernel (type(Kernel)) at some point
    real(kind=wp), dimension(:,:), allocatable :: covariance_matrix
    real(kind=wp) :: log_determinant
    real(kind=wp), dimension(:), allocatable :: mean
    real(kind=wp), dimension(:), allocatable :: y_minus_mean
    real(kind=wp), dimension(:), allocatable :: alpha
  contains
    private
    procedure(init_likelihood_interface), public, deferred, pass(self) :: init
    procedure, pass(self) :: compute_likelihood
    procedure, pass(self) :: computed
    procedure(ll_interface), public, deferred, pass(self) :: ll
    procedure, public, pass(self) :: write => write_likelihood
  end type Likelihood

  abstract interface
    subroutine init_likelihood_interface(self, nparams, ntrain)
      import Likelihood
      class(Likelihood), intent(inout) :: self
      integer, intent(in) :: nparams
      integer, intent(in) :: ntrain
    end subroutine init_likelihood_interface

    function ll_interface(self, params, mean, x_values, y_values, info) result(ll)
      import Likelihood
      import wp
      class(Likelihood), target, intent(inout) :: self
      real(kind=wp), dimension(:), intent(in) :: params
      real(kind=wp), dimension(:), intent(in) :: mean
      real(kind=wp), dimension(:,:), intent(in) :: x_values
      real(kind=wp), dimension(:), intent(in) :: y_values
      ! real(kind=wp), dimension(:,:), intent(in) :: covariance_matrix
      integer, optional, intent(out) :: info
      real(kind=wp) :: ll
    end function ll_interface
  end interface

  type, extends(Likelihood) :: MarginalLikelihood
  contains
    private
    procedure, public, pass(self) :: init => init_marginal_likelihood
    procedure, public, pass(self) :: ll => ll_marginal
  end type MarginalLikelihood

  type, extends(Likelihood) :: ConcentratedLikelihood
    real(kind=wp) :: sigma_squared
  contains
    private
    procedure, public, pass(self) :: init => init_concentrated_likelihood
    procedure, public, pass(self) :: ll => ll_concentrated
  end type ConcentratedLikelihood

contains

  subroutine init_marginal_likelihood(self, nparams, ntrain)
    class(MarginalLikelihood), intent(inout) :: self
    integer, intent(in) :: nparams
    integer, intent(in) :: ntrain

    allocate(self%computed_params(nparams))
    allocate(self%covariance_matrix(ntrain, ntrain))
    allocate(self%y_minus_mean(ntrain))
    allocate(self%alpha(ntrain))
    allocate(self%mean(ntrain))
  end subroutine init_marginal_likelihood

  subroutine init_concentrated_likelihood(self, nparams, ntrain)
    class(ConcentratedLikelihood), intent(inout) :: self
    integer, intent(in) :: nparams
    integer, intent(in) :: ntrain

    allocate(self%computed_params(nparams))
    allocate(self%covariance_matrix(ntrain, ntrain))
    allocate(self%y_minus_mean(ntrain))
    allocate(self%alpha(ntrain))
    allocate(self%mean(ntrain))
  end subroutine init_concentrated_likelihood

  function computed(self, params)
    class(Likelihood), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    logical :: computed
    integer :: i

    computed = size(params) .eq. size(self%computed_params)
    if (computed) then
      ! computed = any(params .ne. self%computed_params)
      do i = 1, size(params)
        computed = params(i) .eq. self%computed_params(i)
        if (.not.(computed .eqv. .true.)) exit
      end do
    end if
  end function computed

  subroutine compute_likelihood(self, params, x_values, info)
    class(Likelihood), target, intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    real(kind=wp), dimension(:,:), intent(in) :: x_values
    integer, optional, intent(out) :: info

    integer :: info_, ntrain
    real(kind=wp), dimension(:,:), pointer :: R

    if (computed(self, params)) return

    R => self%covariance_matrix

    ntrain = size(self%covariance_matrix, 1)

    call potrf(R, info=info_)

    if (info_ == 0) then
      self%log_determinant = log_determinant(self%covariance_matrix)
      self%computed_params = params
    else if (.not.present(info)) then
      !> TODO Convert this to fatal error
      print*, "Error in cholesky decomposition, non-positive-definite matrix encountered"
      stop
    end if

    if (present(info)) then
      info = info_
    end if
  end subroutine compute_likelihood

  function ll_marginal(self, params, mean, x_values, y_values, info) result(ll)
    class(MarginalLikelihood), target, intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    real(kind=wp), dimension(:), intent(in) :: mean
    real(kind=wp), dimension(:,:), intent(in) :: x_values
    real(kind=wp), dimension(:), intent(in) :: y_values
    ! real(kind=wp), dimension(:,:), intent(in) :: covariance_matrix
    integer, optional, intent(out) :: info
    real(kind=wp), dimension(:,:), allocatable :: tmp
    real(kind=wp) :: ll
    integer :: info_, ntrain

    ntrain = size(self%covariance_matrix, 1)
    allocate(tmp(ntrain,1))

    ! self%mean = gpr%mean%value()
    self%mean = mean
    self%y_minus_mean = y_values - self%mean

    call self%compute_likelihood(params, x_values, info=info_)
    if (info_ .eq. 0) then
      tmp(:,1) = self%y_minus_mean
      call potrs(self%covariance_matrix, tmp, info=info_)

      !> See Rasmussen (p.114 eq(5.9))
      !> alpha == weights
      self%alpha = tmp(:,1)

      ll = -0.5_wp*dot_product(self%y_minus_mean, self%alpha) &
        & -0.5_wp*self%log_determinant &
        & -0.5_wp*real(ntrain, kind=wp)*LOG_TWOPI
    else if (.not. present(info)) then
      !> TODO Convert this to fatal error
      print*, "Error in cholesky decomposition, non-positive-definite matrix encountered"
      stop
    end if

    if (present(info)) then
      info = info_
    end if
  end function ll_marginal

  function ll_concentrated(self, params, mean, x_values, y_values, info) result(ll)
    class(ConcentratedLikelihood), target, intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: params
    real(kind=wp), dimension(:), intent(in) :: mean
    real(kind=wp), dimension(:,:), intent(in) :: x_values
    real(kind=wp), dimension(:), intent(in) :: y_values
    integer, optional, intent(out) :: info
    real(kind=wp), dimension(:), allocatable :: ones_, R_y, R_1
    real(kind=wp), dimension(:,:), allocatable :: tmp
    real(kind=wp) :: ll
    integer :: info_, ntrain

    ntrain = size(self%covariance_matrix, 1)
    allocate(tmp(ntrain,1))
    allocate(R_y(ntrain))
    allocate(R_1(ntrain))

    call self%compute_likelihood(params, x_values, info=info_)
    if (info_ .eq. 0) then
      tmp(:,1) = y_values
      call potrs(self%covariance_matrix, tmp, info=info_)
      R_y = tmp(:,1)

      ones_ = ones(ntrain)
      tmp(:,1) = ones_
      call potrs(self%covariance_matrix, tmp, info=info_)
      R_1 = tmp(:,1)

      self%mean = dot_product(ones_, R_y)/dot_product(ones_, R_1)

      self%y_minus_mean = y_values - self%mean
      tmp(:,1) = self%y_minus_mean
      call potrs(self%covariance_matrix, tmp, info=info_)
      !> See Rasmussen (p.114 eq(5.9))
      !> alpha == weights
      self%alpha = tmp(:,1)

      self%sigma_squared = dot_product(self%y_minus_mean, self%alpha)/real(ntrain, kind=wp)

      ll = -0.5_wp*real(ntrain, kind=wp)*log(self%sigma_squared) &
        & -0.5_wp*self%log_determinant
    else if (.not. present(info)) then
      !> TODO Convert this to fatal error
      print*, "Error in cholesky decomposition, non-positive-definite matrix encountered"
      stop
    end if

    if (present(info)) then
      info = info_
    end if
  end function ll_concentrated

  subroutine write_likelihood(self, unit)
    class(Likelihood), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i

    write(unit, "(A)") "[weights]"
    do i = 1, size(self%alpha)
      write(unit, "(g0)") self%alpha(i)
    end do
  end subroutine write_likelihood
end module likelihood_module
