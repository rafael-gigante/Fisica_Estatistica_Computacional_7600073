program tarefa_A
    integer, parameter :: L = 100
    integer(2), dimension(1:L) :: plus, minus
    real(8), dimension(-4:4) :: fb
    real, parameter :: beta = 0.1d0
    real(8) :: mag, aux, med, erro = 0.001d0
    byte grade(L,L)
    logical :: condition = .false.
    character*1 isimb(-1:1)
    
    isimb(1) = '1'
    isimb(-1) = '0'

    open(1,file='config.out')

    call srand(124)

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

    !Definindo a configuração inicial do sistema (Nesse caso totalmente ordenado)
    grade = 1

    icount = 0
    aux = 0
    med = 0
    !Dinâmica de Monte Carlo
    do while (condition .eqv. .false.)
        mag = 0
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

        do j = 1, L
            do n = 1, L
                mag = mag + (grade(j,n))
            end do
        end do
        mag = mag / (L ** 2)

        !Condição de saída do loop
        if (abs((mag - med)) <= erro) then 
            condition = .true.
        else
            aux = aux + mag
            med = aux / icount
        end if
    end do
            
    do ix=1,L
        write(1,"(200a2)")(isimb(grade(ix,iy)),iy=1,L)
    end do



end program tarefa_A