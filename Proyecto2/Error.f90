module errormodulo
    implicit none


    type :: Error
        CHARACTER(LEN = 100) :: ultimoToken
        CHARACTER(LEN = 100) :: tokenEsperado 
        character(len=30) :: tipoError
        integer :: fila, columna
    End type Error

    type(Error), ALLOCATABLE ::  arregloError(:)

contains 

    ! Subrutina para agregar errores a la lista de error
    subroutine agregar_error(tipoError, ultimoToken, tokenEsperado, fila, columna)
        character(len=*), intent(in) :: tipoError
        character(len=*), intent(in) :: ultimoToken
        character(len=*), intent(in) :: tokenEsperado
        integer :: fila, columna, n
        type(Error) :: nuevo_error
        type(Error), allocatable ::  arregloTemp(:)
        
        nuevo_error%tipoError = tipoError
        nuevo_error%ultimoToken = ultimoToken
        nuevo_error%tokenEsperado = tokenEsperado
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        if (.not. allocated(arregloError)) then 
            allocate(arregloError(1)) 
            arregloError(1) =  nuevo_error 
        else
            n = size(arregloError)
            allocate(arregloTemp(n+1))
            arregloTemp(:n) = arregloError 
            arregloTemp(n+1) = nuevo_error
            deallocate(arregloError) 
            allocate(arregloError(n+1)) 
            arregloError = arregloTemp
        end if
    end subroutine agregar_error

    subroutine agregar_errorLex(tipoError, ultimoToken,tokenEsperado, fila, columna)
        character(len=*), intent(in) :: tipoError
        character(len=*), intent(in) :: ultimoToken
        character(len=*), intent(in) :: tokenEsperado
        integer :: fila, columna
        type(Error) :: nuevo_error
        
        nuevo_error%tipoError = tipoError
        nuevo_error%ultimoToken = ultimoToken
        nuevo_error%tokenEsperado = tokenEsperado
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        if(.not. allocated(arregloError))then
            allocate(arregloError(1))
            arregloError(1) = nuevo_error
        else
            call agregar_error(nuevo_error%tipoError, nuevo_error%ultimoToken, nuevo_error%tokenEsperado, nuevo_error%fila, nuevo_error%columna)
        end if 
        
    end subroutine agregar_errorLex

    
    !subroutine imprimir_errores()
     !   integer :: i 
      !  character(len=20) :: str_fila, str_columna
        
       ! if (.not. allocated(arregloError)) then
        !        print *, "No hay errores"
         !   else
          !      do i = 1, size(arregloError)

           !         write(str_fila, '(I0)') arregloError(i)%fila
            !        write(str_columna, '(I0)') arregloError(i)%columna
             !       print *, 'Error Sintactico: '
              !      print *, 'Ultimo Token: ', trim(arregloError(i)%ultimoToken)
               !     print *, 'Token Esperado: ', trim(arregloError(i)%tokenEsperado)
                !    print *, 'Fila: ', trim(str_fila)
                 !   print *, 'Columna: ', trim(str_columna)
                !end do 
        !end if

    !end subroutine imprimir_errores

    subroutine imprimir_errores()
        integer :: i
        character(len=30) :: str_fila, str_columna
    
        if (.not. allocated(arregloError)) then
            print *, "No hay errores"
        else
            do i = 1, size(arregloError)
                write(str_fila, '(I0)') arregloError(i)%fila
                write(str_columna, '(I0)') arregloError(i)%columna
    
                ! Imprime el error en una sola línea con el formato especificado
                print *, trim(arregloError(i)%tipoError), ",", &
                         trim(arregloError(i)%ultimoToken), ",", &
                         trim(arregloError(i)%tokenEsperado), ",", &
                         trim(str_fila), ",", &
                         trim(str_columna)
            end do
        end if
    end subroutine imprimir_errores

    subroutine archivoErrores()

        integer :: i
        character(len=30) :: str_fila, str_columna
        character(len=100) :: nombreArchivo
        integer :: unidad
        
        nombreArchivo = './Proyecto2/errores.txt'  ! Archivo donde se almacenarán los errores
        unidad = 20  ! Número de unidad para el archivo
    
        ! Abre el archivo para escritura
        open(unit=unidad, file=nombreArchivo, status='replace', action='write')
    
        if (.not. allocated(arregloError)) then
           write(unidad, '(A)') 'No hay errores'
        else
            do i = 1, size(arregloError)
                write(str_fila, '(I0)') arregloError(i)%fila
                write(str_columna, '(I0)') arregloError(i)%columna
    
                ! Escribe el error en el archivo en una sola línea
                write(unidad, '(A)') trim(arregloError(i)%tipoError) // "," // &
                                     trim(arregloError(i)%ultimoToken) // "," // &
                                     trim(arregloError(i)%tokenEsperado) // "," // &
                                     trim(str_fila) // "," // &
                                     trim(str_columna)
            end do
        end if
    
        ! Cierra el archivo
        close(unidad)
        
    end subroutine archivoErrores
    
end module errormodulo

