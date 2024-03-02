program tarefa_5 !Modelo de crescimento de revoluções populares
    integer, parameter :: L = 1000
    real, parameter :: p = 0.1d0
    integer, dimension(-L:L,-L:L) :: grade_estac,grade_movel, grade_aux
    integer, dimension(0:3) :: px = (/1,-1,0,0/), py = (/0,0,1,-1/)
    logical :: condition = .true.

    call srand(324)

    open(1, file='grade_movel.out')
    open(2, file='grade_estac.out')
    open(3, file='fractal.out')

    grade_estac(0,0) = 0
    grade_movel(0,0) = 1

    !Definindo os sítios que serão ocupados inicialmente
    do i = -L, L
        do j = -L, L
            if (rand() <= 0.10) then
                grade_estac(i,j) = 1
            end if
        end do
    end do
    
    !Aqui definimos as coordenadas ix_count e iy_count que basicamente são a localização
    !da partícula inicial, utilizamos uma distância id como referência para observar a grade.
    !Então, basicamente ao invés de olhar as grades por completo para verificar a posição das
    !partículas nós simplesmente observamos uma fração da grade com escala comparável ao tamanho
    !do reticulado.
    id = 5
    ix_count = 0
    iy_count = 0
    do m = 1, 6000
        do i = - id, id
            do j = - id, id
                if (grade_movel(ix_count + i, iy_count + j) == 1) then
                    do k = -1, 1
                        do n = -1, 1
                            if (grade_estac(i+k,j+n) == 1) then
                                grade_movel(i+k,j+n) = 1
                                grade_estac(i+k,j+n) = 0
                                !Esse trecho é para verificar se a distância da nova partícula agregada
                                !em relação à partícula inicial é maior do que a distância id definida previamente.
                                s = sqrt(real(((i+k) - ix_count) ** 2 + ((j+n) - iy_count) ** 2)) 
                                if (s >= id) then
                                    id = id + 5
                                end if
                            end if
                        end do
                    end do
                end if
            end do
        end do

        ia = 4 * rand()
        do i =  ix_count - id,  ix_count + id
            do j = iy_count - id, iy_count + id
                if (grade_movel(i,j) == 1) then
                    !Nova posição das partículas após se movimentarem
                    grade_aux(i+px(ia),j +py(ia)) = grade_movel(i, j)
                end if
            end do
        end do

        ix_count = px(ia)
        iy_count = py(ia)

        grade_movel = grade_aux
        grade_aux = 0
    end do                          

    do i = -(ix_count + id), (ix_count + id)
        do j = -(iy_count + id), (ix_count + id)
            if (grade_movel(i,j) == 1) then
                !Usamos a grade auxiliar para transladar o agregado de volta para a origem
                grade_aux(i - ix_count, j - iy_count) = 1
                write(1, *) i, j
            end if

            if (grade_estac(i,j) == 1) then
                write(2,*) i, j
            end if
        end do
    end do 
        
    !Trecho de código nada otimizado para realizar a contagem de partículas para
    !determinados valores de raio
    id = 5
    icount = 0
    iaux = 0
    do while (id <  L)
        do i = -id, id
            do j = -id, id
                if (sqrt(real(i ** 2 + j ** 2)) <= real(id)) then
                    if (grade_aux(i,j) == 1) then
                        icount = icount + 1
                    end if
                end if
            end do
        end do

        if (iaux /= icount) then 
            write(3,*) log(real(id)), log(real(icount))
            icount = 0
            id = id + 5
        else
            id = L
        end if 
    end do 

end program tarefa_5