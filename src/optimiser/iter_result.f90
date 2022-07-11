! MIT License
!
! Copyright (c) 2022 Popelier Group
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module optimiser_iter_result
  use kinds, only: wp
  implicit none
  private
  public :: IterResult

  type IterResult
    real(kind=wp), dimension(:), allocatable :: param
    real(kind=wp) :: cost
    real(kind=wp), dimension(:), allocatable :: gradient
    real(kind=wp), dimension(:,:), allocatable :: hessian
    logical :: success
  contains
    private
    procedure, public, pass(self) :: new_iter_result_param_cost
    procedure, public, pass(self) :: new_iter_result_param_cost_gradient
    procedure, public, pass(self) :: new_iter_result_param_cost_gradient_hessian
    procedure, public, pass(self) :: new_iter_result_param_cost_success
    procedure, public, pass(self) :: new_iter_result_param_cost_gradient_success
    procedure, public, pass(self) :: new_iter_result_param_cost_gradient_hessian_success
    procedure, pass(self) :: set_param
    procedure, pass(self) :: set_gradient
    procedure, pass(self) :: set_hessian
    generic, public :: init => new_iter_result_param_cost,                  &
                               new_iter_result_param_cost_gradient,         &
                               new_iter_result_param_cost_gradient_hessian, &
                               new_iter_result_param_cost_success,          &
                               new_iter_result_param_cost_gradient_success, &
                               new_iter_result_param_cost_gradient_hessian_success
  end type IterResult

contains

  subroutine new_iter_result_param_cost(self, param, cost)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost

    call self%set_param(param)
    self%cost = cost
    self%success = .true.
  end subroutine new_iter_result_param_cost

  subroutine new_iter_result_param_cost_gradient(self, param, cost, gradient)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost
    real(kind=wp), dimension(:), intent(in) :: gradient

    call self%set_param(param)
    self%cost = cost
    call self%set_gradient(gradient)
    self%success = .true.
  end subroutine new_iter_result_param_cost_gradient

  subroutine new_iter_result_param_cost_gradient_hessian(self, param, cost, gradient, hessian)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost
    real(kind=wp), dimension(:), intent(in) :: gradient
    real(kind=wp), dimension(:,:), intent(in) :: hessian

    call self%set_param(param)
    self%cost = cost
    call self%set_gradient(gradient)
    call self%set_hessian(hessian)
    self%success = .true.
  end subroutine new_iter_result_param_cost_gradient_hessian

  subroutine new_iter_result_param_cost_success(self, param, cost, info)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost
    integer, intent(in) :: info

    call self%set_param(param)
    self%cost = cost
    self%success = info .eq. 0
  end subroutine new_iter_result_param_cost_success

  subroutine new_iter_result_param_cost_gradient_success(self, param, cost, gradient, info)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost
    real(kind=wp), dimension(:), intent(in) :: gradient
    integer, intent(in) :: info

    call self%set_param(param)
    self%cost = cost
    call self%set_gradient(gradient)
    self%success = info .eq. 0
  end subroutine new_iter_result_param_cost_gradient_success

  subroutine new_iter_result_param_cost_gradient_hessian_success(self, param, cost, gradient, hessian, info)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param
    real(kind=wp), intent(in) :: cost
    real(kind=wp), dimension(:), intent(in) :: gradient
    real(kind=wp), dimension(:,:), intent(in) :: hessian
    integer, intent(in) :: info

    call self%set_param(param)
    self%cost = cost
    call self%set_gradient(gradient)
    call self%set_hessian(hessian)
    self%success = info .eq. 0
  end subroutine new_iter_result_param_cost_gradient_hessian_success

  subroutine set_param(self, param)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: param

    allocate(self%param, source=param)
    self%param = param
  end subroutine set_param

  subroutine set_gradient(self, gradient)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:), intent(in) :: gradient

    allocate(self%gradient, source=gradient)
    self%gradient = gradient
  end subroutine set_gradient

  subroutine set_hessian(self, hessian)
    class(IterResult), intent(inout) :: self
    real(kind=wp), dimension(:,:), intent(in) :: hessian

    allocate(self%hessian, source=hessian)
    self%hessian = hessian
  end subroutine set_hessian

  subroutine destructor(self)
    type(IterResult), intent(inout) :: self

    if (allocated(self%param)) deallocate(self%param)
    if (allocated(self%gradient)) deallocate(self%gradient)
    if (allocated(self%hessian)) deallocate(self%hessian)
  end subroutine destructor

end module optimiser_iter_result
