program tarefa_D
    integer, parameter :: L = 10
    integer(2), dimension(1:L) :: plus, minus
    real(8), dimension(-4:4) :: fb
    real, parameter :: beta = 0.5d0
    real(8) :: mag, aux
    byte grade(L,L)

    call srand(35)

    open(1, file = "10")
    !Definindo os possíveis valores de exponenciais
    do i = -4, 4    
        fb(i) = exp(beta * i)
    end do

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
    icount2 = 0
    aux = 0
    iaux = 0
    !Dinâmica de Monte Carlo
    do k = 1, 100000
        mag = 0
        !1 iteração de Monte Carlo
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

        !Magnetização
        do j = 1, L
            do n = 1, L
                mag = mag + (grade(j,n))
            end do
        end do
        mag = mag / (L ** 2)

        if (((mag < 0 .and. aux > 0) .or. (mag > 0 .and. aux < 0)) .and. icount > 2000) then 
            iT = iT + (icount - iaux)
            iaux = icount
            icount2 = icount2 + 1
        end if

        aux = mag
    end do

    !Média dos intervalos 
    T = iT / icount2

    write(1,*) L, T

end program tarefa_D