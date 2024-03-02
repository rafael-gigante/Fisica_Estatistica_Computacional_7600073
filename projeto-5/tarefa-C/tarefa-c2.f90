program tarefa_C2
    integer, parameter :: L = 80
    integer(2), dimension(1:L) :: plus, minus
    real(8), dimension(-4:4) :: fb
    real :: beta = 0.30d0, dbeta = 0.01d0
    real(8) :: erg
    byte grade(1:L,1:L)
    byte grade_aux(1:L,1:L)
    logical :: condition = .false.

    open(2,file='erg-spin.out')

    call srand(234)

    !Definindo a peridiocidade da grade
    do i = 2, L-1
        plus(i) = i + 1
        minus(i) = i - 1
    end do
    plus(1) = 2
    plus(L) = 1
    minus(1) = L
    minus(L) = L -1

    !Definindo a configuração inicial do sistema (Metade ordenado e metade aleatória)
    grade = 1
    do i = 1, L
        do j = 1, (L/2)
            if (rand() <= 0.50) then
                grade(i,j) = 1
            else
                grade(i,j) = -1
            end if
        end do
    end do

    grade_aux = grade

    icount = 0
    iaux = 0
    !Dinâmica de Monte Carlo
    do while (condition .eqv. .false.)

        erg = 0

        grade = grade_aux

        do i = -4, 4    
            fb(i) = exp(beta * i)
        end do

        do while (icount < 25)

            !Cálculo da Energia
            do j = 1, L
                do n = 1, L
                    erg = erg + grade(j,n) * (grade(minus(j),n) + grade(plus(j),n) + grade(j,minus(n)) + grade(j,plus(n)))
                end do
            end do
            erg = (-erg/2) / (L ** 2)

            write(2,*) icount, erg, beta

            !Iteração de Monte Carlo
            do i = 1, (L ** 2)
                !Escolha aleatória de um sítio
                ix = (L * rand()) + 1
                iy = (L * rand()) + 1

                !Probabilidades de o sítio mudar de spin
                M = grade(minus(ix),iy) + grade(plus(ix),iy) + grade (ix,minus(iy)) + grade(ix,plus(iy))

                P_s = fb(grade(ix, iy) * M) / (fb(-(grade(ix, iy) * M)) + fb(grade(ix, iy) * M))

                if (rand() <= P_s) then
                    grade(ix, iy) = grade(ix, iy)
                else 
                    grade(ix, iy) = - grade(ix, iy)
                end if
            end do

            icount = icount + 1
        end do 
        write(2,*)
        write(2,*)
        icount = 0

        beta = beta + dbeta

        if (beta > 0.6d0) then
            condition = .true.
        end if
    end do
            
end program tarefa_C2