program corda_extremidade_livre
    implicit none
    integer :: i, j, x, xo
    real(8), parameter :: dx = 0.01, c = 300.d0, L = 1.d0
    real(8) :: dt, r
    real(8), dimension(0:int(L/dx)) :: yp, yc, yn, Yo !previous, current and next

    open(1, file='sinal.out')

    dt = (dx / c)
    r = c / (dx / dt)

    write(*,*) "Posição na corda para pegar o sinal (0 a 100) // Posição do pacote gaussiano (0 a 100)"
    read(*,*) x, xo


    !Pacote gaussiano
    do i = 0, int(L / dx)
        if (i == 0) then
            Yo(i) = 0
        else if (i == int(L/dx) ) then
            Yo(i) = Yo(i - 1)
        else
            Yo(i) = exp(- ((i * dx - real(xo/100.d0)) ** 2) / (L / 30) ** 2)
        end if
    end do

    write(1,*) 0, Yo(x) 

    yp = Yo
    yc = Yo

    do j = 1, 1000
        do i = 0, int(L / dx)
        if (i == 0) then
            yn(i) = 0
        else if (i == int(L/dx) ) then
            yn(i) = yn(i-1)
        else
            yn(i) = (2 * (1 - (r ** 2)) * yc(i)) + ((r ** 2) * (yc(i + 1) + yc(i - 1))) - yp(i)
        end if
        end do
        yp = yc
        yc = yn
        write(1,*) (j * dt), yn(x)
    end do 
end program corda_extremidade_livre