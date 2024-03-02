program tarefa_2 !Modelo de crescimento DLA 2d
    integer, parameter :: L = 400, N = 15000
    integer, dimension(-L:L,-L:L) :: grade
    integer, dimension(0:3) :: px = (/1,-1,0,0/), py = (/0,0,1,-1/)
    real(8), parameter :: pi = acos(-1.d0)
    real(8) :: Rinic, Rfin, theta, soma
    logical :: cond = .true.

    call srand(265)

    !Definição dos raios inicial e final
    Rinic = 5.0d0
    Rfin = 1.5d0 * Rinic

    open(1, file='seed.out')
    open(2, file='fractal.out')

    grade(0,0) = 1
    write(1,*) 0, 0

    do i = 1, N
        cond = .true.
        soma = 0

        !Posição que as partículas são iniciadas
        theta = 2 * pi * rand()
        ix = Rinic * cos(theta)
        iy = Rinic * sin(theta)

        do while (cond .eqv. .true.)

            !random walk para a partícula iniciada
            ia = 4 * rand()
            ix = ix + px(ia)
            iy = iy + py(ia)

            !Checagem no entorno da partícula iniciada
            do j = -1, 1
                do k = -1, 1
                    soma = soma + grade(ix + j, iy + k)
                end do
            end do

            !Distância da partícula iniciada em relação à origem
            d = sqrt(real(ix ** 2 + iy ** 2))

            !Condição para caso a partícula se afaste demais do agregado
            if (d >= Rfin) then
                cond = .false.
            !Condição para a partícula se juntar ao agregado
            else if (soma >= 1) then
                grade(ix, iy) = 1
                write(1,*) ix, iy
                cond = .false.
                !Redefinição do raio inicial para iniciar novas partículas
                if (d > Rinic) then
                    Rinic = d + 5
                    Rfin = Rinic * 1.5
                end if
            end if
        end do
    end do

    !Trecho de código nada otimizado para realizar a contagem de partículas para
    !determinados valores de raio
    id = 5
    icount = 0
    do while (id <= Rinic)
        do i = -id, id
            do j = -id, id
                if (sqrt(real(i ** 2 + j ** 2)) <= real(id)) then
                    if (grade(i,j) == 1) then
                        icount = icount + 1
                    end if
                end if
            end do
        end do

        write(2,*) log(real(id)), log(real(icount))
        icount = 0
        id = id + 5
    end do 
end program tarefa_2