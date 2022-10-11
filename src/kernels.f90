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

module kernels
  use kernels_module, only: add_kernels,mult_kernels,  Kernel, KernelSum, KernelProd, operator(.eq.), CompositeKernel
  use constant_kernel_module, only: Constant
  use rbf_kernel_module, only: RBF
  use rbf_cyclic_kernel_module, only: RBFCyclic
  use periodic_kernel_module, only: PeriodicKernel
  use stationary_kernel_module, only: StationaryKernel
  use stationary_cyclic_kernel_module, only: StationaryCyclicKernel
  use linear_kernel_module, only: LinearKernel
  use distance_matrix_module, only: compute_distance_matrix
  implicit none
  private

  public :: add_kernels, mult_kernels, Kernel, KernelSum, KernelProd, RBF, RBFCyclic, &
    StationaryKernel, operator(.eq.), CompositeKernel, Constant, compute_distance_matrix, &
    PeriodicKernel, LinearKernel

end module kernels
