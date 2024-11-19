module calculo_area
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
contains 
    subroutine area_quadrado(lado, np, area_estimada)
        real :: lado
        integer  :: np
        real:: area_estimada

        integer :: pontos_dentro, i
        real :: x, y

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Módulo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        pontos_dentro = 0
        
        call random_seed()

        do i = 1, np
            call random_number(x) 
            x = x * lado

            call random_number(y)
            y = y* lado

            if (x >= 0.0 .and. x <= lado .and. y >= 0.0 .and. y >= lado) then
                pontos_dentro = pontos_dentro + 1 
            end if
        end do 

        area_estimada = (pontos_dentro/np)/np * lado**2

    end subroutine


end module 