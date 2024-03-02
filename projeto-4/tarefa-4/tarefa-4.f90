program tarefa_4 !Efeito corona, basicamente o mesmo código da tarefa 2
    integer, parameter :: L = 800, N = 50000
    integer, dimension(-L:L,0:4000) :: grade = 0
    integer, dimension(0:3) :: px = (/1,-1, 0, 0/), py = (/0,0,1,-1/)
    real(8) :: Y_inic, Y_max, soma
    logical :: condition

    call srand(423)

    open(1, file='data.out')

    do i = -L, L
        grade(i,0) = 1
        write(1,*) i, 0
    end do

    Y_inic = 5
    Y_max = 1.5 * Y_inic
    do i = 1, N
        condition = .true.

        ix = (- L) + (2 * L * rand())
        iy = Y_inic


        do while (condition .eqv. .true.)

            ia = 4 * rand()
            ix = ix + px(ia)
            iy = iy + py(ia)

            do j = -1, 1
                do k = 0, 1
                soma = soma + grade(ix + j, iy - k)
                end do
            end do

            if (ix >= L .or. ix < -L .or. iy > Y_max) then
                condition = .false.
            else if (soma > 0) then
                grade(ix, iy) = 1
                write(1,*) ix, iy
                condition = .false.
                soma = 0
                if (iy == Y_inic) then
                    Y_inic = Y_inic + 5
                    Y_max = Y_inic * 1.5d0
                end if
            end if
        end do
    end do

end program tarefa_4