module stationary_kernel_config_module
  use kinds, only: wp
  use kernel_config_type_module, only: KernelConfig

  implicit none
  private
  public :: StationaryKernelConfig

  type, abstract, extends(KernelConfig) :: StationaryKernelConfig
    real(kind=wp), allocatable, dimension(:) :: lengthscale
  end type StationaryKernelConfig
end module stationary_kernel_config_module