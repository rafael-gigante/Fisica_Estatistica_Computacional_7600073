program transformada_de_fourier
    implicit none
    integer :: N, j, k
    real(8) :: pi = acos(-1.0d0), dt, aux
    complex(8) :: i = (0.0d0, 1.0d0), yy
    complex(8), dimension(0:800) :: Y_k
    real(8), dimension(0:800) :: y_j

    N = 800
    dt = 0.01d0 / 300.d0

    open(1, file='sinal.out', status='old')
    open(4, file='espectro.out')

    read(1,*) (aux, y_j(j), j=0, (N-1))
    
    !Espectro de potências
    do k=0, (N/2 -1)
       yy = 0.d0
        do j=0, (N-1)
            yy = yy + y_j(j) * exp((2.0d0 * pi * i * j * k) / N)        
        end do 
        Y_k(k) = yy * 2.d0/N
        write(4,*) (k / (N * dt)), ((real(Y_k(k)) ** 2) + (aimag(Y_k(k)) ** 2))
    end do
end program transformada_de_fourier