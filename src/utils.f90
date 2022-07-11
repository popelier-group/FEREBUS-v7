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
