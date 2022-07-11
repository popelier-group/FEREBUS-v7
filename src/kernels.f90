module kernels
  use kernels_module, only: add_kernels,mult_kernels,  Kernel, KernelSum, KernelProd, operator(.eq.), CompositeKernel
  use constant_kernel_module, only: Constant
  use rbf_kernel_module, only: RBF
  use rbf_cyclic_kernel_module, only: RBFCyclic
  use periodic_kernel_module, only: PeriodicKernel
  use stationary_kernel_module, only: StationaryKernel
  use stationary_cyclic_kernel_module, only: StationaryCyclicKernel
  use distance_matrix_module, only: compute_distance_matrix
  implicit none
  private

  public :: add_kernels, mult_kernels, Kernel, KernelSum, KernelProd, RBF, RBFCyclic, &
    StationaryKernel, operator(.eq.), CompositeKernel, Constant, compute_distance_matrix, &
    PeriodicKernel

end module kernels
