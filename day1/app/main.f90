program main
  use day1, only: process_data
  implicit none

  character(len=1000) :: filename
  integer :: count

  print *, 'Enter input filename:'
  read *, filename
  count = process_data(filename)
  
end program main
