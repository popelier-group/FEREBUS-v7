module kernel_config_module
  use kernel_config_type_module, only: KernelConfig
  use kernel_config_list_module, only: KernelConfigList
  use stationary_kernel_config_module, only: StationaryKernelConfig
  use rbf_config_module, only: RBFConfig
  use rbf_cyclic_config_module, only: RBFCyclicConfig
  use periodic_config_module, only: PeriodicConfig

  private
  public :: KernelConfig, RBFConfig, RBFCyclicConfig, KernelConfigList, PeriodicConfig
end module kernel_config_module