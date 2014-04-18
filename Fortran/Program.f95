module mod_simulation
    implicit none
    integer, parameter             :: GA_POPSIZE = 2048;
    real, parameter                :: GA_ELITRATE = 0.10;
    real, parameter                :: GA_MUTATIONRATE = 0.25;
    character(len = 12), parameter :: GA_TARGET = "Hello world!";
    type, public :: typ_ga
        integer                            :: fitness
        character(len = len(GA_TARGET))    :: str
        contains
            procedure :: GetFitness => calc_fitness
        end type typ_ga
    type(typ_ga), dimension(GA_POPSIZE) :: population
    type(typ_ga), dimension(GA_POPSIZE) :: buffer
contains
    subroutine init_random_seed
        INTEGER :: i, n, clock
        INTEGER, DIMENSION(:), ALLOCATABLE :: seed
        CALL RANDOM_SEED(size = n)
        ALLOCATE(seed(n))
        CALL SYSTEM_CLOCK(COUNT=clock)
        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        CALL RANDOM_SEED(PUT = seed)
        DEALLOCATE(seed)
        end subroutine init_random_seed
    integer function RandNum(x, min, max)
        integer :: min, max
        real :: x
        RandNum = int( x *(max + 1 - min)) + min !see http://infohost.nmt.edu/tcc/help/lang/fortran/scaling.html
        end function RandNum
    subroutine GenerateString
        real :: x
        integer :: i, j = 1, k = 12
        integer, dimension(12*2048) :: arr1, arr2
        character (len = 12*2048) :: s1, s2
        call init_random_seed
        do i =1, 12*2048
            call random_number(x)
            arr1(i) = RandNum(x, 33, 125)
            s1 = s1(1:i) // char(arr1(i))
            end do
        do i =1, 12*2048
            call random_number(x)
            arr2(i) = RandNum(x, 33, 125)
            s2 = s2(1:i) // char(arr2(i))
            end do
        i = 1
        do while (.TRUE.)
            population(i)%str = s1(j : k)
            buffer(i)%str = s2(j : k)
            j = j + 12
            k = k + 12
            i = i + 1
            if (k > 12 * 2048) then
                exit
                end if
            end do
        end subroutine GenerateString
    subroutine calc_fitness(this)
        class(typ_ga) :: this
        integer :: i, temp
        do i = 1, len(this%str)
            temp = temp + abs( ichar(this%str(i:i)) - ichar(GA_TARGET(i:i)))
            end do
        this%fitness = temp
        temp = 0
        end subroutine calc_fitness
    subroutine sort
        type(typ_ga) :: temp1
        integer i,j,k
        do j = 1, GA_POPSIZE - 1
            do k = j+1, GA_POPSIZE
                if (population(j)%fitness > population(k)%fitness) then
                    temp1 = population(k)
                    population(k) = population(j)
                    population(j) = temp1
                    end if
                end do
            end do
        end subroutine sort
    subroutine mate
        integer, parameter :: esize = 1500 ! = int(GA_POPSIZE * GA_ELITRATE)
        integer :: i, j, i1, i2, spos, pos
        real :: x
        buffer(1:esize) = population(1:esize)
        do i = esize, GA_POPSIZE  !TODO: check here for problems
            i1 = int(rand(0)*( (GA_POPSIZE / 2) + 1 - 0 ) ) + 0
            i2 = int(rand(0)*( (GA_POPSIZE / 2) + 1 - 0 ) ) + 0
            spos = int(rand(0)*( (len(GA_TARGET) + 1 - 0 ) ) ) + 0
            buffer(i)%str = population(i1)%str(1:spos) // population(i2)%str(spos:len(GA_TARGET))
            call random_number(x)
            if (x < GA_MUTATIONRATE) then
                pos = int(rand(0)*( (len(GA_TARGET) -1 + 1 - 0 ) ) ) + 0
                call random_number(x)
                buffer(i)%str = buffer(i)%str(1:pos) // char(RandNum(x, 33, 125)) &
                    // buffer(i)%str(pos + 1 : len(buffer(i)%str) - pos -1)
                end if
            end do
        end subroutine mate
    subroutine swap
        type(typ_ga), dimension(:), allocatable :: temp
        allocate(temp(GA_POPSIZE))
        temp = population
        population = buffer
        buffer = temp
        deallocate(temp)
        end subroutine swap
    subroutine init
        integer :: i
        call GenerateString
        do while (.true.)
            do i = 1, 2048
                call population(i)%GetFitness
                end do
            call sort
            write(*,*) 'Fitness:', population(1)%fitness, 'str:', population(1)%str
            if (population(1)%fitness == 0) then
                exit
                end if
            call mate
            call swap
            end do
        end subroutine init
end module mod_simulation

program main
    use mod_simulation
    call init
end program main
