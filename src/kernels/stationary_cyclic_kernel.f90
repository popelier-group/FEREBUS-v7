module stationary_cyclic_kernel_module
  use kinds, only: wp
  use utils, only: modulo
  use constants, only: PI, MINUS_PI, TWOPI
  use stationary_kernel_module, only: StationaryKernel
  use distance_matrix_module, only: cyclic_distance_matrix
  implicit none
  private
  public :: StationaryCyclicKernel

  type, abstract, extends(StationaryKernel) :: StationaryCyclicKernel
  contains
    private
    procedure, public, pass(self) :: init_cyclic_pi
    procedure, public, pass(self) :: R => R_stationary
    procedure, public, pass(self) :: cleanup => cleanup_stationary
  end type StationaryCyclicKernel

  contains

  subroutine cleanup_stationary(self)
    class(StationaryCyclicKernel), intent(inout) :: self

    !!$acc exit data delete(self, self%lengthscale)
    !$acc exit data delete(cyclic_distance_matrix)
  end subroutine cleanup_stationary

  subroutine init_cyclic_pi(self, x_std)
    class(StationaryCyclicKernel), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: x_std

    if (allocated(self%cyclic_pi)) deallocate(self%cyclic_pi)
    allocate(self%cyclic_pi(size(x_std)))
    self%cyclic_pi = PI/x_std

    if (allocated(self%cyclic_minus_pi)) deallocate(self%cyclic_minus_pi)
    allocate(self%cyclic_minus_pi(size(x_std)))
    self%cyclic_minus_pi = MINUS_PI/x_std

    if (allocated(self%cyclic_twopi)) deallocate(self%cyclic_twopi)
    allocate(self%cyclic_twopi(size(x_std)))
    self%cyclic_twopi = TWOPI/x_std
  end subroutine init_cyclic_pi

#if defined(__PGI__)
  subroutine R_stationary(self, x, R)
    class(StationaryCyclicKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    real(kind=wp), dimension(:,:), allocatable :: tmp
    real(kind=wp), dimension(:), pointer :: lengthscale
    integer :: ntrain, nfeats, i, j, thread

    ntrain = size(x, 1)
    nfeats = size(x, 2)

    lengthscale => self%lengthscale

    if (.not.allocated(R)) allocate(R(ntrain, ntrain))

    thread = omp_get_thread_num() + 2

    !!$acc update device(self, self%lengthscale) async(thread)
    !!$acc data copy(R) async(thread)

    !$acc data copy(R) copyin(lengthscale) async(thread)
    !$acc wait(1)
    !$acc parallel loop collapse(2) async(thread)
    do j = 1, ntrain
      do i = 1, ntrain
        ! R(j, i) = self%k_diff(self%distance_matrix(j, i, :))
        R(i, j) = exp(-0.5_wp*sum(lengthscale * cyclic_distance_matrix(:, i, j)))
        ! R(i,j) = self%distance_matrix(i, i, j)
        ! R(i, j) = exp(tmp(i, j))
      end do
    end do
    !$acc end parallel
    !$acc end data
    !$acc wait(thread)
  end subroutine R_stationary
#else
  subroutine R_stationary(self, x, R)
    class(StationaryCyclicKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    integer :: ntrain, nfeats, i, j

    ntrain = size(x, 1)
    nfeats = size(x, 2)

    do j = 1, ntrain
      R(j,j) = 0.0_wp
      do i = j+1, ntrain
        R(i,j) = sum(self%lengthscale*cyclic_distance_matrix(:,i,j))
        R(j,i) = R(i,j)
      end do
    end do

    ! if (size(self%active_dims) .eq. nfeats) then
    !   call dgemv('t', nfeats, ntrain*ntrain, -0.5_wp, cyclic_distance_matrix, nfeats, self%lengthscale, 1, 0.0_wp, R, 1)
    ! else
    !   call dgemv('t', size(self%active_dims), ntrain*ntrain, -0.5_wp, cyclic_distance_matrix(self%active_dims,:,:), size(self%active_dims), self%lengthscale, 1, 0.0_wp, R, 1)
    ! end if
    R = exp(-0.5_wp*R) ! <- maybe
  end subroutine R_stationary
#endif
end module stationary_cyclic_kernel_module
  
