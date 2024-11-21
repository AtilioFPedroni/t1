module formas
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none 

    type :: ResultadoArea
        real(kind=dp) :: lado
        real(kind=dp) :: area_estimada
    end type ResultadoArea

contains

    function ponto_dentro_quadrado(x, y, lado, x_centro, y_centro) result(dentro)
        real (kind = dp) :: x, y, lado, x_centro, y_centro
        logical :: dentro
        real (kind = dp) :: x_min, x_max, y_min, y_max

        x_min = x_centro - lado/2.0_dp
        x_max = x_centro + lado/2.0_dp
        y_min = y_centro - lado/2.0_dp
        y_max = y_centro + lado/2.0_dp

        dentro = (x>= x_min .and. x <= x_max .and. y >= y_min .and. y <= y_max)
    end function

end module

