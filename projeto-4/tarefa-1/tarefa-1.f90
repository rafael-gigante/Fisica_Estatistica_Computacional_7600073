program tarefa_1 !autômato celular determínistico
    integer, parameter :: L = 100
    integer :: valor_regra
    integer, dimension(1:L) :: Cc, Cn, mais, menos !C current e C next
    character(8) :: string_regra

    !Inputs do usuário para definir a regra e a cadeia inicial
    write(*,*) 'Digite o número da regra (0 - 256): '
    read(*,*) valor_regra !Inteiro de 0 até 256
    write(string_regra,'(B8.8)') valor_regra !Transforma o valor da regra em binário e o guarda em uma string
    write(*,*) 'Escolha a cadeia inicial (0, 1 ou aleatório (2)):'
    read(*,*) j !Definindo a cadeia inicial |0, 1|

    open(1, file='data.out')

    !Definição da periodicidade da cadeia
    do i = 2, L-1
        mais(i) = i + 1
        menos(i) = i - 1
    end do
    mais(L) = 1
    mais(1) = 2
    menos(1) = L
    menos(L) = L-1

    !Configurações iniciais
    if (j == 0) then
        Cc = 0
    else if (j == 1) then
        Cc = 1
    else
        iseed = 25
        call srand(iseed)
        do i = 1, L
            a = rand()
            if (a < 0.5d0) then
                Cc(i) = 0
            else
                Cc(i) = 1
            end if
        end do
    end if

    write(1,*) Cc

    !Iterações da cadeia
    do j = 1, 100    
        do i = 1, L
            Cn(i) = func_regra(Cc(menos(i)), Cc(i), Cc(mais(i)), string_regra)
        end do

        Cc = Cn
        write(1,*) Cc

    end do 

    contains

    !Função que recebe os valores de 3 sítios da cadeia e a string contendo o número da regra
    !e então a aplica para o valor dos sítios a, b e c
    integer function func_regra(a,b,c,string)
        integer :: a, b, c, valor
        character(8) :: string

        if (a+b+c == 0) then
            read(string(8:8), '(I10)') valor
            func_regra = valor

        else if (a+b+c == 1) then
            if (a == 0 .and. b == 0) then
                read(string(7:7), '(I10)') valor
                func_regra = valor
            else if (a == 0 .and. c == 0) then
                read(string(6:6), '(I10)') valor
                func_regra = valor
            else 
                read(string(4:4), '(I10)') valor
                func_regra = valor
            end if
        else if (a+b+c == 2) then
            if (b == 1 .and. c == 1) then
                read(string(5:5), '(I10)') valor
                func_regra = valor
            else if (a == 1 .and. c == 1) then
                read(string(3:3), '(I10)') valor
                func_regra = valor
            else 
                read(string(2:2), '(I10)') valor
                func_regra = valor
            end if
        else
            read(string(1:1), '(I10)') valor
            func_regra = valor
        end if
    end function


end program tarefa_1