program programa_principal
    use calculo_area
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none 

    real :: area_real, lado, area_estimada
    integer :: np

    lado = 10
    area_real = lado**2
    np = 100000

    call area_quadrado(lado, np, area_estimada)

    print *, "Lado do quadrado:", lado
    print *, "Área real do quadrado:", area_real
    print *, "Área estimada usando Monte Carlo:", area_estimada


end program 

