  program MonteCarlo
  use omp_lib
  !compile: gfortran -fopenmp -ffree-form .\pi-parallel.f -opip
  implicit none
  ! -----------------------------------------------Declare
  real*4 pointX, pointY, pi, tmp, ran0
  integer*4 iterations, count, i, count_local, tid
  integer*4, parameter :: numthreads = 4

  ! -----------------------------------------------Input
  !print*, "Enter the number of iterations to compute Pi ..."
  !read*, iterations
  ! -----------------------------------------------Compute
  iterations = 1E6
  count=0
  !$omp parallel private(count_local,tid,i) num_threads(numthreads)
    tid = omp_get_thread_num()
    count_local = 0
    do i=0, iterations/numthreads
      pointX = rand(0)
      pointY = rand(0)
      !print*, "Point: (", pointX, ", ", pointY, ")."
      if (sqrt(pointX**2 + pointY**2) <= 1) then
        count_local = count_local + 1
      end if
    end do
    !$omp critical
      count = count + count_local
    !$omp end critical
  !$omp end parallel
  pi = (real(count) / iterations) * 4
  ! -----------------------------------------------Output
  print*, "An approximation of pi after "
  print*, iterations, " iterations is "
  print*, pi
  end 