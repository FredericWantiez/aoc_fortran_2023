module day1
  implicit none

  private
  public :: process_data

contains

  subroutine read_line(funit, line, iostat)
    integer, intent(in) :: funit
    integer, intent(in out) :: iostat
    character(:), allocatable, intent(out) :: line

    integer, parameter :: buffer_len = 80
    character(len=buffer_len) :: buffer
    integer :: size_read

    line = ''
    do
      read(unit=funit, fmt='(a)', iostat=iostat, advance='no', size=size_read) buffer
      if (is_iostat_eor(iostat)) then
        line = line // buffer(:size_read)
        iostat = 0
        exit
      else if (iostat == 0) then
        line = line // buffer
      else
        exit
      end if
    end do
    
  end subroutine read_line

  subroutine process_line(buffer, count)
    character(*), intent(in) :: buffer
    integer, intent(in out) :: count
    integer :: char, subcount, i, iostat
    integer :: size_buffer
    character :: left, right
    character(len=2) :: number

    left = ''
    right = ''

    size_buffer = len(buffer)
    if (size_buffer == 0) return

    do i=1, size_buffer
      read(buffer(i:i), '(I1)', iostat=iostat) char
      if (iostat == 0) then
        if (left == '') left = buffer(i:i)
      end if
    end do

    write(right, '(I1)') char
    number = left // right
    read(number(1:2), '(I2)') subcount
    count = count + subcount
  end subroutine process_line
    
  integer function process_data(filename) result(count)
    character(*), intent(in) :: filename

    character(:), allocatable :: buffer
    integer :: fileunit
    integer :: iostat

    count = 0
    open(newunit=fileunit, file=trim(filename), iostat=iostat)
    do 
      call read_line(fileunit, buffer, iostat)
      call process_line(buffer, count)
      if (is_iostat_end(iostat)) exit
    end do
    close(unit=fileunit, iostat=iostat)
    if (iostat /= 0) print *, "Error closing file"
  end function process_data

end module day1
