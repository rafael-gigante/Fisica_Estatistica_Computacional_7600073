program tarefa_c
    integer, parameter :: lenght = 10, n_particles = 20
    real(8), parameter :: pi = acos(-1.0d0)
    real(8) :: v_0 = 1.0d0, dt = 0.02, fx, fy, f, dx, dy
    real(8), dimension(0:2,n_particles) :: x, y
    real(8), dimension(n_particles) :: v_x, v_y

    call srand(69)
    
    open(1,file='v.out')
    open(2,file='vx.out')
    open(3,file='vy.out')

    !Definindo o espaçamento das partículas ao iniciar a grade
    sqrt_n = sqrt(real(n_particles))
    if (sqrt_n == int(sqrt_n)) then
        grid = lenght / sqrt_n
    else
        grid = lenght / int(sqrt_n + 1)
    end if
    
   !Iniciando as partículas 
   dmax = grid / 4 
   n = 0
   i = 0
   do while (i < lenght)   
      j = 0			
      do while (j < lenght)
         n = n + 1
         if (n <= n_particles) then 
            theta = 2* pi * rand()
            x(1,n) = i + 0.5 * grid + dmax * rand() * cos(theta)
            y(1,n) = j + 0.5 * grid + dmax * rand() * sin(theta)

            if (n >= n_particles/2) then
                v_x(n) = v_0
                v_y(n) = 0
            else 
                v_x(n) = 0
                v_y(n) = v_0
            end if

            x(0,n) = x(1,n) - v_x(n) * dt
            y(0,n) = y(1,n) - v_y(n) * dt

         end if
         j = j + grid
      end do
      i = i + grid
   end do

    do i = 1, n_particles
        write(1,*) sqrt((v_x(i) ** 2) + (v_y(i) ** 2))
    end do 

    !Movimento das partículas
    do it = 0, 50000
        do n = 1, n_particles
            call force(n,x,y,n_particles,lenght,fx,fy)

            x(2,n) = 2 * x(1,n) - x(0, n) + fx * (dt ** 2)
            y(2,n) = 2 * y(1,n) - y(0, n) + fy * (dt ** 2)

            ix2 = int((x(2,n) + lenght) / lenght) - 1 

            iy2 = int((y(2,n) + lenght) / lenght) - 1

            if (ix2 /= 0) then
                x(2,n) = x(2,n) - lenght * ix2
                x(1,n) = x(1,n) - sign(lenght,int(ix2))
                x(0,n) = x(0,n) - sign(lenght,int(ix2))
            end if 
            if (iy2 /= 0) then
                y(2,n) = y(2,n) - lenght * iy2
                y(1,n) = y(1,n) - sign(lenght,int(iy2))
                y(0,n) = y(0,n) - sign(lenght,int(iy2))
            end if

            v_x(n) = (x(2,n) - x(0,n)) / (2 * dt)
            v_y(n) = (y(2,n) - y(0,n)) / (2 * dt)
        end do

        do n = 1, n_particles
            x(0,n) = x(1,n)
            x(1,n) = x(2,n)
            y(0,n) = y(1,n)
            y(1,n) = y(2,n)
        end do

        do i = 1, n_particles
        end do
        if (mod(it,10) == 0) then
            if (it > 10000) then
                do n = 1, n_particles
                    write(1,*) sqrt((v_x(n) ** 2) + (v_y(n) ** 2))
                    write(2,*) v_x(n)
                    write(3,*) v_y(n)
                    write(4,*) x(1,n), y(1,n)
                end do
                    write(4,*)
                    write(4,*)
            end if 
        end if
    end do

    contains
    
    subroutine force(n,x,y,n_particles,lenght,fx,fy)
        integer, intent(in) :: lenght, n_particles, n
        real(8), dimension(0:2,n_particles), intent(in) :: x, y
        real(8) :: dx, dy, r 
        real(8), intent(out) :: fx, fy

        fx = 0
        fy = 0

        do i = 1, n_particles
            if (i /= n) then
                dx = x(1,n) - x(1,i)
                dy = y(1,n) - y(1,i)

                if (abs(dx) > (lenght / 2)) then
                    dx = dx - sign(lenght,int(dx))
                end if

                if (abs(dy) > (lenght / 2)) then
                    dy = dy - sign(lenght,int(dy))
                end if
                r = sqrt(dx ** 2 + dy ** 2)               

                if (r < 3.0d0) then
                    f = 24.0d0 * ((2.0d0 / (r ** 13)) - (1.0d0 / (r ** 7)))
                    fx = fx + f * (dx / r)
                    fy = fy + f * (dy / r)
                end if
            end if
        end do
        return
    end subroutine force

end program tarefa_c