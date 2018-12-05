  function ran0(seed)
    ! Park & Miller Random Generator
    integer seed,ia,im,iq,ir,mask,k
    real ran0,am
    parameter (ia=16807,im=2147483647,am=1./im,iq=127773,ir=2836,mask=123459876)
    seed=ieor(seed,mask)
    k=seed/iq
    seed=ia*(seed-k*iq)-ir*k
    if (seed.lt.0) seed=seed+im
      ran0=am*seed
      seed=ieor(seed,mask)
    return
  end
  program MonteCarlo
  use omp_lib
  !compile: gfortran -fopenmp -ffree-form .\pi-parallel-ran0.f -opipr
  implicit none
  ! -----------------------------------------------Declare
  real*4 pointX, pointY, pi, tmp, ran0
  integer*4 iterations, count, i, count_local, tid
  integer*4, parameter :: numthreads = 4


  ! -----------------------------------------------Input
  print*, "Enter the number of iterations to compute Pi ..."
  read*, iterations
  ! -----------------------------------------------Compute
  !iterations = 5E7
  count=0
  !$omp parallel private(count_local,tid,i) num_threads(numthreads)
    tid = omp_get_thread_num()
    count_local = 0
    do i=0, iterations/numthreads
      pointX = ran0(tid)
      pointY = ran0(tid)
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

