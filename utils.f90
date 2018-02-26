module utils

    implicit none

    !foreground colours
    character(len=2), parameter :: black   = '30', &
                                   red     = '31', &
                                   green   = '32', &
                                   yellow  = '33', &
                                   blue    = '34', &
                                   magenta = '35', &
                                   cyan    = '36', &
                                   white   = '37'

    !background colours
    character(len=2), parameter :: black_b   = '40', &
                                   red_b     = '41', &
                                   green_b   = '42', &
                                   yellow_b  = '43', &
                                   blue_b    = '44', &
                                   magenta_b = '45', &
                                   cyan_b    = '46', &
                                   white_b   = '47'

    !styles
    character(len=2), parameter :: bold          = '01', &
                                   italic        = '03', &
                                   underline     = '04', &
                                   inverse       = '07', &
                                   strikethrough = '09'

    character(len=2), parameter :: start = achar(27)//'['
    character(len=3), parameter :: end = '[0m'


    !functions to add colour to output via ANSI colour codes
    interface colour
        module procedure colour_char
        module procedure colour_int
        module procedure colour_real4
        module procedure colour_real8
    end interface

    !functions to turn number variables into strings
    interface str
        module procedure str_I
        module procedure str_R4
        module procedure str_R8
    end interface str

    !subroutines to swap variables
    interface swap
        module procedure swap_I
        module procedure swap_R4
        module procedure swap_R8
    end interface swap

    private
    public :: str, swap, colour
    public :: bold, italic, underline, strikethrough, black, red, green, yellow, blue, magenta, cyan, white
    public :: black_b, red_b, green_b, yellow_b, blue_b, magenta_b, cyan_b, white_b

    contains

        subroutine swap_I(a, b)

            implicit none

            integer, intent(INOUT) :: a, b
            integer :: tmp

            tmp = a
            a = b
            b = tmp
        end subroutine swap_I


        subroutine swap_R4(a, b)

            implicit none

            real, intent(INOUT) :: a, b
            real :: tmp

            tmp = a
            a = b
            b = tmp
        end subroutine swap_R4


        subroutine swap_R8(a, b)

            implicit none

            double precision, intent(INOUT) :: a, b
            double precision :: tmp

            tmp = a
            a = b
            b = tmp
        end subroutine swap_R8


        function str_I(i)

            implicit none

            integer, intent(IN) :: i

            character(len=:), allocatable :: str_i
            character(len=100) :: string

            write(string,'(I100.2)') I

            str_i = trim(adjustl(string))
        end function str_I


        function str_R4(i)

            implicit none

            real, intent(IN) :: i

            character(len=:), allocatable :: str_R4
            character(len=100) :: string

            write(string,'(f100.8)') I

            str_R4 = trim(adjustl(string))
        end function str_r4


        function str_R8(i)

            implicit none

            double precision, intent(IN) :: i

            character(len=:), allocatable :: str_R8
            character(len=100) :: string

            write(string,'(f100.16)') I

            str_R8 = trim(adjustl(string))
        end function str_r8


        function colour_char(string, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            character(*),           intent(IN) :: string
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5
            character(len=:), allocatable      :: colourised

            colourised = string

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start//fmt1//';'//fmt2//'m'//string//achar(27)//end
            elseif(present(fmt1))then
                colourised = start//fmt1//'m'//string//achar(27)//end
            end if
        end function colour_char


        function colour_int(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            integer,                intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(I50.1)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start//fmt1//';'//fmt2//'m'//string//achar(27)//end
            elseif(present(fmt1))then
                colourised = start//fmt1//'m'//string//achar(27)//end
            end if
        end function colour_int


        function colour_real4(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            real,                   intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(F50.8)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start//fmt1//';'//fmt2//'m'//string//achar(27)//end
            elseif(present(fmt1))then
                colourised = start//fmt1//'m'//string//achar(27)//end
            end if
        end function colour_real4


        function colour_real8(inte, fmt1, fmt2, fmt3, fmt4, fmt5) result(colourised)

            implicit none

            double precision,       intent(IN) :: inte
            character(*), optional, intent(IN) :: fmt1, fmt2, fmt3, fmt4, fmt5

            character(len=:), allocatable :: colourised, string
            character(len=50)             :: tmp

            write(tmp,'(F50.16)') inte
            string = trim(adjustl(tmp))
            colourised = trim(adjustl(string))

            if(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4) .and. present(fmt5))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//';'//fmt5//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3) .and. present(fmt4))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//';'//fmt4//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2) .and. present(fmt3))then
                colourised = start//fmt1//';'//fmt2//';'//fmt3//'m'//string//achar(27)//end
            elseif(present(fmt1) .and. present(fmt2))then
                colourised = start//fmt1//';'//fmt2//'m'//string//achar(27)//end
            elseif(present(fmt1))then
                colourised = start//fmt1//'m'//string//achar(27)//end
            end if
        end function colour_real8
end module utils