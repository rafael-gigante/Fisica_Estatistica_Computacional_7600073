program fourier_transform
    implicit none
    integer :: N, j, k
    real(8) :: pi = acos(-1.0d0), dt, aux
    complex(8) :: i = (0.0d0, 1.0d0), yy
    complex(8), dimension(0:400) :: y_j, Y_k

    read(*,*) N, dt

    open(1, file='data.in', status='old')
    open(2, file='tarefa-2-freq-real-12610500.out')
    open(3, file='tarefa-2-freq-imag-12610500.out')
    open(4, file='data.out')

    read(1,*) (aux, y_j(j), j=0, (N-1))
    
    !Transformada de Fourier
    do k=0, (N/2 - 1)
       yy = 0.d0
        do j=0, (N-1)
            yy = yy + y_j(j) * exp((2.0d0 * pi * i * j * k) / N)        
        end do 
        Y_k(k) = yy * 2.d0/N
        write(2,*) (k / (N * dt)), real(Y_k(k))
        write(3,*) (k / (N * dt)), aimag(Y_k(k))
        write(4,*) (k / (N * dt)), (Y_k(k))
    end do

    !Transformada de Fourier Inversa
    do j=0, (N - 1)
        y_j(j) = 0
        do k=0, (N/2 -1)
           y_j(j) = y_j(j) + Y_k(k) * exp(-(2.0d0 * pi * i * j * k) / N)
        end do
    end do

end program fourier_transform