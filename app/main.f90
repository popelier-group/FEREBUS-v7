program main
  use config_module, only: Config, error_data, SlicedConfig
  use command_line, only: command_line_arguments, parse_command_line_arguments
  use csv, only: read_training_set
  use data_set_module, only: DataSets, DataSet
  use kinds, only: wp, qp
  use model, only: GaussianProcessRegressor
  use utils, only: wtime, init_rand
  use optimiser_module, only: ParticleSwarm
  use kernels, only: compute_distance_matrix
  use constants, only: OUTPUT_UNIT, ERROR_UNIT

  implicit none

  type(Config), allocatable :: cfg
  type(SlicedConfig), allocatable, target :: sliced_cfg
  type(command_line_arguments) :: cmd_args
  type(error_data), allocatable :: error
  type(DataSets), allocatable :: training_sets
  type(DataSet), allocatable, target :: training_set 
  character(len=:), allocatable :: training_set_file_name
  type(GaussianProcessRegressor), allocatable :: gpr
  integer :: isys, iatm, iout, nout
  real(wp) :: start, finish, istart, ifinish
  type(ParticleSwarm), allocatable :: opt
  logical :: config_exists, training_set_exists

  !$acc init

  call init_rand()

  print*, "Starting ferebus"
  print*, "Reading command line arguments"
  !> Read Command Line Arguments
  call parse_command_line_arguments(cmd_args)

  print*, "Reading config"
  !> Read and Print Out Config Data
  inquire(file=cmd_args%config_file, exist=config_exists)
  if (.not.config_exists) then
    write(ERROR_UNIT, *) "Error: Config file '", cmd_args%config_file ,"' does not exist."
    stop
  end if

  allocate(cfg)
  call cfg%init(cmd_args%config_file, error)

  if (allocated(error)) then
    write(output_unit, *) error%message
    stop
  end if
  call cfg%info(output_unit)

  if (cmd_args%read_config_only) stop

  do isys = 1, size(cfg%systems)
    do iatm = 1, size(cfg%systems(isys)%atoms)
      training_set_file_name = cfg%systems(isys)%name // "_" // &
                             & cfg%systems(isys)%atoms(iatm)%name // "_TRAINING_SET.csv"

      inquire(file=training_set_file_name, exist=training_set_exists)
      if (.not.training_set_exists) then
        write(ERROR_UNIT, *) "Error: Training Set file '", training_set_file_name ,"' does not exist."
        stop
      end if
      print*, "Reading training set file: ", training_set_file_name
      training_sets = read_training_set(               &
                        training_set_file_name,        &
                        cfg%systems(isys)%atoms(iatm), &
                        OUTPUT_UNIT,                   &
                        cfg%model%standardise          &
                      )
      do iout = 1, training_sets%nout()
        ! call cpu_time(start)
        start = wtime()
        print*, "initialising model"

        allocate(sliced_cfg)
        call sliced_cfg%slice(cfg, isys, iatm, iout)

        allocate(training_set, source=training_sets%data_sets(iout)%element)
        call sliced_cfg%kernels%set_ndim(size(training_set%x%data, 2))

        allocate(gpr)
        call gpr%init(sliced_cfg, training_set, verbose=.true.)
        call compute_distance_matrix(training_set%x%data)
        
        print*, "initialising pso"
        allocate(opt)
        call opt%init(gpr, cfg%model%optimiser)

        istart = wtime()
        call opt%optimise()
        ifinish = wtime()
        print *, "Opt Time Taken = ", ifinish-istart," seconds."

        ! call gpr%optimise()
        call opt%op%write(cfg%systems(isys)%name, cfg%systems(isys)%atoms(iatm)%name)
        ! call cpu_time(finish)
        finish = wtime()
        print *, "Time Taken = ", finish-start, " seconds."
        deallocate(gpr)
        deallocate(opt)
        deallocate(training_set)
        deallocate(sliced_cfg)
      end do
    end do
  end do

end program main
