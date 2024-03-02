program tarefa_C
    integer, parameter :: L = 100
    integer(2), dimension(1:L) :: plus, minus
    real(8), dimension(-4:4) :: fb
    real :: beta = 0.0d0, dbeta = 0.0001d0
    real(8) :: erg
    byte grade(1:L,1:L)
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

    !Definindo a configuração inicial do sistema (Cada spin com p = 1/2)
    do i = 1, L
        do j = 1, L
            if (rand() <= 0.50) then
                grade(i,j) = 1
            else
                grade(i,j) = -1
            end if
        end do
    end do

    icount = 0
    iaux = 0
    !Dinâmica de Monte Carlo
    do while (condition .eqv. .false.)
        erg = 0
        do i = -4, 4    
            fb(i) = exp(beta * i)
        end do
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
    
        icount = icount + (-1) ** iaux
        beta = beta + dbeta

        if (beta >= 1.75) then
            dbeta = (-1) * dbeta
            iaux = 1
        end if

        !Cálculo da Energia
        do j = 1, L
            do n = 1, L
                erg = erg + grade(j,n) * (grade(minus(j),n) + grade(plus(j),n) + grade(j,minus(n)) + grade(j,plus(n)))
            end do
        end do
        erg = (-erg/2) / (L ** 2)

        if (beta <= 0.d0) then
            condition = .true.
        else
            write(2,*) beta, erg
        end if
    end do
            
end program tarefa_C