module utils
  use utils_str, only: to_lower, to_upper, starts_with, is_feature_header, strip, replace, sanitise, contains_substr, &
                       is_space, is_digit, is_alpha, is_alphanum, is_num, concat, fmt_str
  use utils_array, only: stack, identity, ones, zeros, print_matrix, euclidean_distance
  use utils_arith, only: mean, std, standardise, log_determinant, modulo
  use utils_rand, only: random, init_rand
  use utils_toml, only: get_value_converted, get_table, is_empty_table
  use utils_time, only: wtime
!  use mpi_utils, only: setup_blacs
  implicit none
  private

  public :: to_lower, to_upper, starts_with, is_feature_header, strip, replace, sanitise, contains_substr
  public :: is_space, is_digit, is_alpha, is_alphanum, is_num, concat
  public :: stack, identity, ones, zeros, print_matrix, euclidean_distance
  public :: mean, std, standardise, log_determinant, modulo
  public :: random, init_rand
  public :: get_value_converted, get_table, is_empty_table
  public :: wtime
  public :: fmt_str
!  public :: setup_blacs
end module utils
