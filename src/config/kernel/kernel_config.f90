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

module kernel_config_module
  use kernel_config_type_module, only: KernelConfig
  use kernel_config_list_module, only: KernelConfigList
  use stationary_kernel_config_module, only: StationaryKernelConfig
  use rbf_config_module, only: RBFConfig
  use rbf_cyclic_config_module, only: RBFCyclicConfig
  use periodic_config_module, only: PeriodicConfig
  use linear_config_module, only: LinearConfig

  private
  public :: KernelConfig, RBFConfig, RBFCyclicConfig, KernelConfigList, PeriodicConfig, LinearConfig
end module kernel_config_module