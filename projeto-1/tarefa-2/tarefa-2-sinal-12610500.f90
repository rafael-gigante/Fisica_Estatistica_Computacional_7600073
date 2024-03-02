program signal
    implicit none
    integer :: N, a1, a2, i
    real(8), parameter :: pi=acos(-1.d0)
    real(8) :: yi, w1, w2, dt

    open(1, file='data.in')
    !open(2, file='sinal_real.in')

    read(*,*) N, dt, a1, a2, w1, w2

    do i=0, N-1
        yi = a1*cos(w1 * pi * i * dt) + a2*sin(w2 * pi* i * dt)
        write(1,*) (i * dt), complex(yi,0.0d0)
        !write(2,*) (i * dt), yi
    end do 

    close(1)
end program signal