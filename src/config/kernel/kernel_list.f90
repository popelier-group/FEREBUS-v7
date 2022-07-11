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

module kernel_config_list_module
  use kernel_config_type_module, only: KernelConfig
  use config_type_module, only : ConfigType
  use utils, only: sanitise
  use constants, only: output_unit
  implicit none
  private
  public :: KernelConfigList

  type, extends(ConfigType) :: KernelConfigNode
    class(KernelConfig), allocatable :: kernel
  contains
    private
    procedure, public, pass(self) :: init => init_node
    procedure, public, pass(self) :: info => info_node
    procedure, public, pass(self) :: set_ndim => set_ndim_kernel
  end type KernelConfigNode

  type, extends(ConfigType) :: KernelConfigList
    type(KernelConfigNode), allocatable, dimension(:) :: kernels
    integer :: i = 1
  contains
    private
    procedure, public, pass(self) :: init => init_kernel_config_list
    procedure, public, pass(self) :: info => info_list
    procedure, public, pass(self) :: add
    procedure, public, pass(self) :: get_index
    procedure, public, pass(self) :: get_name
    generic, public :: get => get_index, get_name
    procedure, public, pass(self) :: set_ndim => set_ndim_list
  end type KernelConfigList

contains
  
  subroutine init_node(self, kernel)
    class(KernelConfigNode), intent(inout) :: self
    class(KernelConfig), intent(in) :: kernel
    
    allocate(self%kernel, mold=kernel)
    self%kernel = kernel
  end subroutine init_node

  subroutine init_kernel_config_list(self, n)
    class(KernelConfigList), intent(inout) :: self
    integer, intent(in) :: n
    allocate(self%kernels(n))
  end subroutine init_kernel_config_list

  subroutine add(self, kernel_config)
    class(KernelConfigList), intent(inout) :: self
    class(KernelConfig), intent(in) :: kernel_config
    type(KernelConfigNode), allocatable :: node

    allocate(node)
    call node%init(kernel_config)
    self%kernels(self%i) = node
    self%i = self%i + 1
  end subroutine add

  function get_index(self, i) result(kernel_config)
    class(KernelConfigList), intent(in) :: self
    integer, intent(in) :: i

    class(KernelConfig), allocatable :: kernel_config
    kernel_config = self%kernels(i)%kernel
  end function get_index

  function get_name(self, name) result(kernel_config)
    class(KernelConfigList), intent(in) :: self
    character(len=*), intent(in) :: name
    class(KernelConfig), allocatable :: kernel_config

    character(len=len(name)) :: sanitised_name
    type(KernelConfigNode), pointer :: node
    integer :: i

    sanitised_name = sanitise(name)

    do i = 1, self%i-1
      if (sanitised_name .eq. self%kernels(i)%kernel%name) then
        kernel_config = self%kernels(i)%kernel
        exit
      end if
    end do
  end function get_name

  subroutine info_node(self, unit)
    class(KernelConfigNode), intent(in) :: self
    integer, intent(in) :: unit

    call self%kernel%info(unit)
  end subroutine info_node

  subroutine info_list(self, unit)
    class(KernelConfigList), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i

    do i = 1, size(self%kernels)
      call self%kernels(i)%info(unit)
    end do
  end subroutine info_list

  subroutine set_ndim_kernel(self, ndim)
    class(KernelConfigNode), intent(inout) :: self
    integer :: ndim

    call self%kernel%set_ndim(ndim)
  end subroutine set_ndim_kernel

  subroutine set_ndim_list(self, ndim)
    class(KernelConfigList), intent(inout) :: self
    integer, intent(in) :: ndim
    integer :: i

    do i = 1, self%i-1
      call self%kernels(i)%set_ndim(ndim)
    end do
  end subroutine set_ndim_list
end module kernel_config_list_module
