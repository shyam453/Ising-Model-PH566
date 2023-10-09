program ising
    implicit none
    integer size, n
    CHARACTER(len=*), PARAMETER :: PLT_FILE = 'plot.plt'
    open(2, file="OUT.dat")

    size = 20
    call compute(size)

    call execute_command_line('gnuplot -p ' // PLT_FILE)
    close(2)
contains

subroutine compute(size)
    implicit none
    integer i, j, k, m, iterations, it, jt, size
    integer, dimension(size, size) :: lattice
    real temp, x, y, z, egy, egy_new, w, mag_val, v

    iterations = 810000

    do m = 1, 25
        temp = 1.0*m/5

        ! Initialising the 20x20 matrix with random values

        do i = 1, 4
            do j = 1, 4
                call random_number(v)
                if(v.ge.0.5) then
                    lattice(i, j) = 1
                else
                    lattice(i, j) = -1
                end if
            end do
        end do

        ! ! Initialising the 20x20 matrix with random values

        ! do i = 1, 20
        !     do j = 1, 20
        !         lattice(i, j) = 1
        !     end do
        ! end do

        mag_val = 0

        do k = 1, iterations
            call random_number(x)
            x = 1+floor(4*x)
            it = int(x)
            call random_number(y)
            y = 1+floor(4*y)
            jt = int(y)
            egy = energy(lattice, size)
            lattice(it, jt) = -1*lattice(it, jt)
            egy_new = energy(lattice, size)
            if((egy_new - egy).ge.0) then
                w = exp(-1.0*(egy_new - egy)/temp)
                call random_number(z)
                if(z.gt.w) then
                    lattice(it, jt) = -1*lattice(it, jt)
                end if
            end if

            if(k>iterations-10000) then
                mag_val = mag_val + (abs(mag(lattice, size))/16)
            end if
        end do

        mag_val = mag_val/10000

        write(2,*)temp, mag_val
    end do

end subroutine

function energy(lattice, size)
    implicit none
    integer, dimension(size, size) :: lattice
    integer i, j, size
    real energy
    energy = 0.0
    do i = 1, 4
        do j = 1, 4
            if(j.ne.4) then
                energy = energy - lattice(i,j)*lattice(i,j+1)
            end if
            if(i.ne.4) then
                energy = energy - lattice(i,j)*lattice(i+1,j)
            end if
        end do
    end do
    return
end function energy

function mag(lattice, size)
    implicit none
    integer, dimension(size, size) :: lattice
    integer i, j, mag, size
    mag = 0
    do i = 1, 4
        do j = 1, 4
            mag = mag + lattice(i, j)
        end do
    end do
    return
end function mag

end program