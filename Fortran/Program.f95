module mod_simulation
    implicit none
    !mod level constants
    integer             :: GA_POPSIZE = 2048;
    real                :: GA_ELITRATE = 0.10;
    real                :: GA_MUTATIONRATE = 0.25;
    character(len = 12) :: GA_TARGET = "Hello world!";

    type, public :: ga_type
        integer             :: fitness
        character(len = 12) :: str
    end type ga_type

contains
    integer function RandNum()
        !num between 33 and 125 see http://infohost.nmt.edu/tcc/help/lang/fortran/scaling.html
        RandNum = int(rand(0)*(125+1-33))+33
    end function RandNum

    function GenerateRandomString() result(s)
        character(len = 12) :: s
        integer :: i
        do i = 1, 12
            s = s(1:i) // char(RandNum())
        end do
    end function GenerateRandomString

    subroutine init
!        character(len = 12) :: s
!        s = GenerateRandomString()
        write(*,*) GenerateRandomString()
    end subroutine init

end module mod_simulation


program test_char
    use mod_simulation

    do j = 1, 100
        call init()
    end do

end program test_char
