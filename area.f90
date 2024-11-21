program programa_principal
    
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    use formas
    use rndgen_mod
    implicit none

    real(kind = dp) :: lado, x_min, x_max, y_min, y_max, area_retangulo
    real(kind = dp) :: x_centro, y_centro
    integer(kind = i8) :: n_pontos, pontos_dentro, i
    type(rndgen) :: gerador
    real(kind = dp) :: x, y

    type(ResultadoArea) :: resultado  

    lado = 2.0_dp

    x_centro = 0.0_dp
    y_centro = 0.0_dp

    x_min = x_centro - lado/2.0_dp
    x_max = x_centro + lado/2.0_dp
    y_min = y_centro - lado/2.0_dp
    y_max = y_centro + lado/2.0_dp

    area_retangulo = (x_max - x_min)*(y_max-y_min)

    n_pontos = 10000_i8

    call gerador%init(12345_i4)

    pontos_dentro = 0

    do i = 1_i8, n_pontos
        x = gerador%real(x_min, x_max)
        y = gerador%real(y_min, y_max)

        if (ponto_dentro_quadrado(x, y, lado, x_centro, y_centro)) then
            pontos_dentro = pontos_dentro + 1_i8
        end if
    end do

    resultado%lado = lado  
    resultado%area_estimada = area_retangulo * (real(pontos_dentro, kind=dp) / real(n_pontos, kind=dp))

    print *, "Lado do quadrado:", resultado%lado
    print *, "Área estimada usando Monte Carlo:", resultado%area_estimada
    print *, "Área real do quadrado:", resultado%lado**2

end program