module mean_module
  use kinds, only: wp
  use mean_type, only: MeanType
  use constant_mean_module, only: ConstantMean
  use zero_mean_module, only: ZeroMean
  use least_squares_mean_module, only: LeastSquaresMean
  use linear_mean_module, only: LinearMean
  use quadratic_mean_module, only: QuadraticMean
  implicit none
  private
  public :: MeanType, ConstantMean, ZeroMean, LeastSquaresMean, LinearMean, QuadraticMean
end module mean_module