  program MonteCarlo
  !!compile: gfortran -ffree-form .\pi.f -opi
  implicit none
  ! -----------------------------------------------Declare
  real*4 pointX, pointY, pi, tmp
  integer*4 iterations, count, i
  ! -----------------------------------------------Input
  !print*, "Enter the number of iterations to compute Pi ..."
  !read*, iterations
  ! -----------------------------------------------Compute
  iterations = 5E7
  count = 0
  do i=0, iterations
    pointX = rand(0)
    pointY = rand(0)
    !print*, "Point: (", pointX, ", ", pointY, ")."
    if (sqrt(pointX**2 + pointY**2) <= 1) then
      count = count + 1
    end if
  end do
  pi = (real(count) / iterations) * 4
  ! -----------------------------------------------Output
  print*, "An approximation of pi after "
  print*, iterations, " iterations is "
  print*, pi
  end 