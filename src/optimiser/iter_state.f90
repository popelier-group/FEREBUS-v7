module optimiser_iter_state
  use kinds, only: wp
  use optimiser_iter_result, only: IterResult
  implicit none
  private
  public :: IterState

  type IterState
    real(kind=wp), dimension(:), allocatable :: param
    real(kind=wp), dimension(:), allocatable :: prev_param
    real(kind=wp), dimension(:), allocatable :: best_param
    real(kind=wp), dimension(:), allocatable :: prev_best_param
    real(kind=wp) :: cost
    real(kind=wp) :: prev_cost
    real(kind=wp) :: best_cost
    real(kind=wp) :: prev_best_cost
    real(kind=wp), dimension(:), allocatable :: gradient
    real(kind=wp), dimension(:), allocatable :: prev_gradient
    real(kind=wp), dimension(:,:), allocatable :: hessian
    real(kind=wp), dimension(:,:), allocatable :: prev_hessian
    integer :: iter
    integer :: best_iter
    integer :: max_iter
    logical :: terminated
  contains
    private
    procedure, public, pass(self) :: update
    procedure, pass(self) :: update_param
    procedure, pass(self) :: update_best_param
    procedure, pass(self) :: update_cost
    procedure, pass(self) :: update_best_cost
    procedure, pass(self) :: update_gradient
    procedure, pass(self) :: update_hessian
    procedure, pass(self) :: update_best
    procedure, public, pass(self) :: increment
  end type IterState

contains

  subroutine update(self, iter_result)
    class(IterState), intent(inout) :: self
    type(IterResult), intent(in) :: iter_result
    
    call self%update_param(iter_result%param)
    call self%update_cost(iter_result%cost)

    if (self%cost .le. self%best_cost) then
      call self%update_best_param(iter_result%param)
      call self%update_best_cost(iter_result%cost)
      call self%update_best()
    end if

    if (allocated(self%gradient)) then
      call self%update_gradient(iter_result%gradient)
    end if
    if (allocated(self%hessian)) then
      call self%update_hessian(iter_result%hessian)
    end if
    ! call self%increment()
  end subroutine update

  subroutine update_param(self, param)
    class(IterState), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    self%prev_param = self%param
    self%param = param
  end subroutine update_param

  subroutine update_best_param(self, param)
    class(IterState), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    self%prev_best_param = self%best_param
    self%best_param = param
  end subroutine update_best_param

  subroutine update_cost(self, cost)
    class(IterState), intent(inout) :: self
    real(kind=wp), intent(in) :: cost
    self%prev_cost = self%cost
    self%cost = cost
  end subroutine update_cost

  subroutine update_best_cost(self, cost)
    class(IterState), intent(inout) :: self
    real(kind=wp), intent(in) :: cost
    self%prev_best_cost = self%best_cost
    self%best_cost = cost
  end subroutine update_best_cost

  subroutine update_gradient(self, gradient)
    class(IterState), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: gradient
    self%prev_gradient = self%gradient
    self%gradient = gradient
  end subroutine update_gradient

  subroutine update_hessian(self, hessian)
    class(IterState), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: hessian
    self%prev_hessian = self%hessian
    self%hessian = hessian
  end subroutine update_hessian

  subroutine update_best(self)
    class(IterState), intent(inout) :: self
    self%best_iter = self%iter
  end subroutine

  subroutine increment(self)
    class(IterState), intent(inout) :: self
    self%iter = self%iter + 1
  end subroutine

  subroutine destructor(self)
    type(IterState), intent(inout) :: self

    if(allocated(self%param)) deallocate(self%param)
    if(allocated(self%prev_param)) deallocate(self%prev_param)
    if(allocated(self%best_param)) deallocate(self%best_param)
    if(allocated(self%prev_best_param)) deallocate(self%prev_best_param)
    if(allocated(self%gradient)) deallocate(self%gradient)
    if(allocated(self%prev_gradient)) deallocate(self%prev_gradient)
    if(allocated(self%hessian)) deallocate(self%hessian)
    if(allocated(self%prev_hessian)) deallocate(self%prev_hessian)
  end subroutine destructor
end module optimiser_iter_state
