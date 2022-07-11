module optimiser_module
  use optimiser_type, only: Optimiser
  use optimiser_iter_result, only: IterResult
  use optimiser_pso_module, only: ParticleSwarm
  implicit none
  private
  public :: Optimiser, IterResult, ParticleSwarm
end module optimiser_module
