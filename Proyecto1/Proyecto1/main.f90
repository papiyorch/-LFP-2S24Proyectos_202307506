program main
    use moduloAnalizador
    use moduloInformacion

    implicit none
  
    character(len=:), allocatable :: entrada
    character(len=10000) :: buffer
    integer :: io_status
    type(Analizador) :: mi_analizador
    type(Informacion) :: mi_informacion

    entrada = ''
    do
        read(*, '(A)', iostat=io_status) buffer
        if (io_status /= 0) exit
        entrada = entrada // trim(buffer) // new_line('a')
       ! print*, buffer
    end do

    ! Inicializar el estado del autómata en 0
    call mi_analizador%inicializarEstado()

    ! Analizar la cadena de entrada
    call mi_analizador%analizar(entrada)

    !Guardar la info de los tokens
    call mi_informacion%crearInfo(mi_analizador%tokens)

    !interar tokens
    call mi_informacion%iteracionTK()

end program main