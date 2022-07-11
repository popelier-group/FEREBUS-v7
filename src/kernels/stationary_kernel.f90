module stationary_kernel_module
  use kinds, only: wp
  use kernels_module, only: Kernel
  use constants, only: PI, TWOPI
  use omp_lib, only: omp_get_thread_num
  use distance_matrix_module, only: distance_matrix

  use utils, only: print_matrix
  implicit none
  private
  public :: StationaryKernel

  type, abstract, extends(Kernel) :: StationaryKernel
    real(kind=wp), dimension(:), allocatable :: lengthscale
  contains
    private
    procedure(k_diff_interface), public, deferred, pass(self) :: k_diff
    ! procedure, public, pass(self) :: compute_distance_matrix
    procedure, public, pass(self) :: R => R_stationary
    procedure, public, pass(self) :: r2
    procedure, public, pass(self) :: cleanup => cleanup_stationary
  end type StationaryKernel

  interface
    function k_diff_interface(self, diff) result(k)
      import StationaryKernel
      import wp
      class(StationaryKernel), intent(in) :: self
      real(kind=wp), dimension(:), intent(in) :: diff
      real(kind=wp) :: k
    end function k_diff_interface
  end interface

contains
  subroutine cleanup_stationary(self)
    class(StationaryKernel), intent(inout) :: self

    !!$acc exit data delete(self, self%lengthscale)
    !$acc exit data delete(distance_matrix)
  end subroutine cleanup_stationary


  function r2(self, diff2)
    class(StationaryKernel), intent(in) :: self
    real(kind=wp), dimension(:), intent(in) :: diff2
    real(kind=wp) :: r2
    r2 = sum(self%lengthscale*diff2)
    ! r2 = sum(1.0_wp/(self%lengthscale*self%lengthscale)*diff2)
  end function r2

#if defined(__PGI__)
  subroutine R_stationary(self, x, R)
    class(StationaryKernel), target, intent(inout) :: self
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
        R(i, j) = exp(-0.5_wp*sum(lengthscale * distance_matrix(:, i, j)))
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
    class(StationaryKernel), target, intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: x
    real(kind=wp), dimension(:,:), intent(inout) :: R
    integer :: ntrain, nfeats, i
    real(kind=wp), dimension(:,:,:), allocatable :: dm

    ntrain = size(x,1)
    nfeats = size(x,2)

    ! print*, distance_matrix(self%active_dims, 2, 1)

    if (size(self%active_dims) .eq. nfeats) then
      call dgemv('t', nfeats, ntrain*ntrain, -0.5_wp, distance_matrix, nfeats, self%lengthscale, 1, 0.0_wp, R, 1)
    else
      allocate(dm(size(self%active_dims), ntrain, ntrain))
      do i = 1, size(self%active_dims)
        dm(i,:,:) = distance_matrix(self%active_dims(i),:,:)
      end do
      call dgemv('t', size(self%active_dims), ntrain*ntrain, -0.5_wp, dm, size(self%active_dims), self%lengthscale, 1, 0.0_wp, R, 1)
      deallocate(dm)
    end if
    R = exp(R) ! <- maybe
  end subroutine R_stationary
#endif
end module stationary_kernel_module
