module errormodulo
    implicit none

    type :: Error
        character(len= 200) :: ultimoToken
        character(len= 200) :: tokenEsperado
        integer fila, columna

    end type Error 

    type (Error), allocatable :: arregloError(:)
    
contains

    subroutine agregar_error(ultimoToken, tokenEsperado, fila, columna)
        character(len=*), intent(in) :: ultimoToken
        character(len=*), intent(in) :: tokenEsperado
        integer :: fila, columna, i
        type(Error) :: nuevoError
        type(Error), allocatable :: tempArray(:)
        
        nuevoError%ultimoToken = ultimoToken
        nuevoError%tokenEsperado = tokenEsperado
        nuevoError%fila = fila
        nuevoError%columna = columna

        if(.not. allocated(arregloError))then
            allocate(arregloError(1))
        else
            i = size(arregloError)
            allocate(tempArray(i+1))
            tempArray(:i) = arregloError
            tempArray(i+1) = nuevoError
            deallocate(arregloError)
            allocate(arregloError(i+1))
            arregloError = tempArray
        end if
        
    end subroutine agregar_error

    subroutine imprimir_errores()
        integer :: i 
        character(len=20) :: str_fila, str_columna
        
        if (.not. allocated(arregloError)) then
                print *, "No hay errores"
            else
                do i = 1, size(arregloError)

                    write(str_fila, '(I0)') arregloError(i)%fila
                    write(str_columna, '(I0)') arregloError(i)%columna
                    print *, 'Error Sintactico: '
                    print *, 'Ultimo Token: ', trim(arregloError(i)%ultimoToken)
                    print *, 'Token Esperado: ', trim(arregloError(i)%tokenEsperado)
                    print *, 'Fila: ', trim(str_fila)
                    print *, 'Columna: ', trim(str_columna)
                end do 
        end if

    end subroutine imprimir_errores
    
end module errormodulo

