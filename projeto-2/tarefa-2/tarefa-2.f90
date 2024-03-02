program tarefa2
    use ogpf
    implicit none
    TYPE(gpf):: gp
    integer :: i, j
    real(8) :: dx = 0.01, dt, c = 300.d0, L = 1.d0, r
    real(8), dimension(0:int(1.d0/0.01)) :: yp, yc, yn, Yo !previous, current and next
    real(8), dimension(0:int(1.d0/0.01)) :: x, y !previous, current and next

    open(1, file='data.out')

    dt = dx / c
    r = c / (dx / dt)

    call gp%animation_start(0.1) ! start animation, set delay is one second between frames
    call gp%axis([0.d0, 1.d0, -0.5d0, 0.5d0])
    call gp%xlabel('x (m)')
    call gp%ylabel('Amplitude')
    call gp%title('Propagação de Ondas em uma Corda', textcolor='#000000')

    !Onda inicial
    do i = 0, int(L / dx)
        if (i == 0 .or. i == int(L / dx)) then
            Yo(i) = 0
        else if (i * dx <= (L / 4) ) then
            Yo(i) = i * dx
        else
            Yo(i) = -(1.d0/3.d0)*i*dx + (1.d0/3.d0)
        end if

        x(i) = (i*dx)
        y(i) = (Yo(i))

        write(1,*) (i * dx), Yo(i)

    end do 

    call gp%plot((x(0:int(1.d0/0.01))), y(0:int(1.d0/0.01)),'w lp lc "black" pt 7 ps 0.5 lw 1.5')

    yp = Yo
    yc = Yo

    do j = 1, 200
        do i = 0, int(L / dx)
        if (i == 0 .or. i == int(L / dx)) then
            yn(i) = 0
        else
            yn(i) = (2 * (1 - (r ** 2)) * yc(i)) + ((r ** 2) * (yc(i + 1) + yc(i - 1))) - yp(i)
        end if

        x(i) = (i*dx)
        y(i) = (yn(i))

        !write(1,*) (i*dx), (yn(i) + j * 1)

        end do
        yp = yc
        yc = yn
        call gp%plot((x(0:int(1.d0/0.01))), y(0:int(1.d0/0.01)),'w lp lc "black" pt 7 ps 0.5 lw 1.5')
    end do 

    call gp%animation_show()

end program tarefa2