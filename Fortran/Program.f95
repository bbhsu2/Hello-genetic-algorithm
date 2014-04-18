module mod_random
    implicit none
    private
    public init_random_seed, randreal, randint
 
contains
 
    ! From http://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
    subroutine init_random_seed()
        use iso_fortran_env, only: int64
        implicit none
        integer, allocatable :: seed(:)
        integer :: i, n, un, istat, dt(8), pid
        integer(int64) :: t
        
        call random_seed(size=n)
        allocate(seed(n))
        ! First try if the OS provides a random number generator
        open(newunit=un, file="/dev/urandom", access="stream", &
            form="unformatted", action="read", status="old", iostat=istat)
        if (istat == 0) then
            read(un) seed
            close(un)
        else
            ! Fallback to XOR:ing the current time and pid. The PID is
            ! useful in case one launches multiple instances of the same
            ! program in parallel.
            call system_clock(t)
            if (t == 0) then
                call date_and_time(values=dt)
                t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                    + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                    + dt(3) * 24_int64 * 60 * 60 * 1000 &
                    + dt(5) * 60 * 60 * 1000 &
                    + dt(6) * 60 * 1000 + dt(7) * 1000 &
                    + dt(8)
            end if
            pid = getpid()
            t = ieor(t, int(pid, kind(t)))
            do i = 1, n
                seed(i) = lcg(t)
            end do
        end if
        call random_seed(put=seed)
    contains
        ! This simple PRNG might not be good enough for real work, but is
        ! sufficient for seeding a better PRNG.
        function lcg(s)
            integer :: lcg
            integer(int64) :: s
            if (s == 0) then
                s = 104729
            else
                s = mod(s, 4294967296_int64)
            end if
            s = mod(s * 279470273_int64, 4294967291_int64)
            lcg = int(mod(s, int(huge(0), int64)), kind(0))
        end function
    end subroutine
 
    ! Random real (0.0 <= r < 1.0)
    real function randreal()
        call random_number(randreal)
    end function
 
    ! Random int (a <= r <= b)
    integer function randint(a, b)
        integer, intent(in) :: a, b
        if (a > b) stop "a must be less than or equal to b"
        randint = a + int(randreal() * (b - a + 1))
    end function
 
end module
 
module mod_genetic
    use mod_random
    implicit none
    private
    public genetic_helloworld
 
    integer, parameter          :: GA_POPSIZE = 2048
    real, parameter             :: GA_ELITERATE = 0.10
    real, parameter             :: GA_MUTATIONRATE = 0.25
    integer, parameter          :: GA_ELITISTS = int(GA_POPSIZE * GA_ELITERATE)
    character(len=*), parameter :: GA_TARGET = "Hello world!"
 
    type :: typ_ga
        character(len=len(GA_TARGET)) :: str
        integer :: fitness
    end type
 
    type(typ_ga), pointer :: population(:)
    type(typ_ga), pointer :: buffer(:)
    type(typ_ga), target :: pop_alpha(GA_POPSIZE)
    type(typ_ga), target :: pop_beta(GA_POPSIZE)
 
contains
    
    type(typ_ga) function new_citizen()
        integer :: i
        do i = 1, len(GA_TARGET)
            new_citizen%str(i:i) = achar(randint(32, 122))
        end do
        new_citizen%fitness = fitness(new_citizen%str)
    end function
    
    integer function fitness(str)
        character(len=*), intent(in) :: str
        integer :: i
        fitness = sum([(abs(iachar(str(i:i)) - iachar(GA_TARGET(i:i))), i = 1, len(GA_TARGET))])
    end function
 
    ! Uses counting sort for speeeeeeed :)
    subroutine sort_by_fitness()
        type(typ_ga) :: sorted(GA_POPSIZE)
        integer, parameter :: char_range = 122 - 32
        integer, parameter :: largest_fitness = char_range * len(GA_TARGET)
        integer :: sort_count(0:largest_fitness)
        integer :: i, cur_count, total
        sort_count = 0
        do i = 1, GA_POPSIZE
            sort_count(population(i)%fitness) = sort_count(population(i)%fitness) + 1
        end do
        total = 0
        do i = 0, size(sort_count)
            cur_count = sort_count(i) 
            sort_count(i) = total
            total = total + cur_count
        end do
        do i = 1, GA_POPSIZE
            sorted(sort_count(population(i)%fitness)) = population(i)
            sort_count(population(i)%fitness) = sort_count(population(i)%fitness) + 1
        end do
        population = sorted
    end subroutine
	
    subroutine elitism()
        buffer(:GA_ELITISTS) = population(:GA_ELITISTS)
    end subroutine
 
    subroutine mutate(citizen)
        type(typ_ga), intent(inout) :: citizen
        integer :: ipos
        ipos = randint(1, len(GA_TARGET))
        citizen%str(ipos:ipos) = achar(randint(32, 122))
    end subroutine
    
    subroutine mate()
        integer :: i, i1, i2, spos
        call elitism()
        do i = GA_ELITISTS + 1, GA_POPSIZE
            i1 = randint(1, GA_POPSIZE / 2)
            i2 = randint(1, GA_POPSIZE / 2)
            spos = randint(0, len(GA_TARGET))
            buffer(i)%str = population(i1)%str(:spos) // population(i2)%str(spos+1:)
            if (randreal() < GA_MUTATIONRATE) then
                call mutate(buffer(i))
            end if
            buffer(i)%fitness = fitness(buffer(i)%str)
        end do
    end subroutine
    
    subroutine swap()
        if (associated(population, target=pop_alpha)) then
            population => pop_beta
            buffer => pop_alpha
        else
            population => pop_alpha
            buffer => pop_beta
        end if
    end subroutine
    
    subroutine genetic_helloworld()
        integer :: i
        call init_random_seed()
        pop_alpha = [(new_citizen(), i = 1, GA_POPSIZE)]
        population => pop_alpha
        buffer => pop_beta
        do while (.true.)
            call sort_by_fitness()
            print *, 'Best: ', population(1)
            if (population(1)%fitness == 0) then
                exit
            endif
            call mate()
            call swap()
        end do
    end subroutine
 
end module
 
program main
    use mod_random
    use mod_genetic, only: genetic_helloworld
    implicit none
    
    call genetic_helloworld()    
end program