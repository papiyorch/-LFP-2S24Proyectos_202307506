module moduloClave
    implicit none

    type :: Clave

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 150) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY
    end type Clave
    
    type (Clave), allocatable :: arregloClave(:)

    contains

    subroutine agregarClave(id)
        character(len=*), intent(in) :: id
        type(Clave) :: nuevaClave
        integer :: n
        type(Clave), allocatable :: tempArray(:)

        nuevaClave%id = id
        nuevaClave%tipo = 'Texto'
        nuevaClave%texto = ""
        nuevaClave%posX = ""
        nuevaClave%posY = ""

        if(.not. allocated(arregloClave)) then
            allocate(arregloClave(1))
            arregloClave(1) = nuevaClave
        else
            n = size(arregloClave)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloClave
            tempArray(n+1) = nuevaClave
            deallocate(arregloClave)
            allocate(arregloClave(n+1))
            arregloClave = tempArray
        end if
        
    end subroutine agregarClave

    subroutine claveTexto(id, texto)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: texto
        integer :: i
        
        if (.not. allocated(arregloClave)) then
            print *, "No hay claves"
        else
            do i = 1, size(arregloClave)
                if (trim(arregloClave(i)%id) == id) then
                    arregloClave(i)%texto = texto
                end if
            end do 
        end if
    
    end subroutine claveTexto

    subroutine clavePos(id, posX, posY)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: posX
        character(len=*), intent(in) :: posY
        integer :: i
        
        if (.not. allocated(arregloClave)) then
            print *, "No hay claves"
        else
            do i = 1, size(arregloClave)
                if (trim(arregloClave(i)%id) == id) then
                    arregloClave(i)%posX = posX
                    arregloClave(i)%posY = posY
                end if
            end do
        end if
    
    end subroutine clavePos

    subroutine claves_html()
        implicit none
        integer :: i, unidad, ios
        character(len=1000) :: html_line
    
        unidad = 30
        open(unit=unidad, file='./Proyecto2/claves.html', status='replace', action='write', iostat=ios)
    
        if (ios /= 0) then
            print *, 'Error al abrir el archivo HTML'
            stop
        end if
    
        if (.not. allocated(arregloClave)) then
            print *, "No hay claves"    
        else
            do i = 1, size(arregloClave)
                html_line = '<input type="password" id="' // trim(arregloClave(i)%id) // '" value="' // trim(arregloClave(i)%texto) // '"/>'
                write(unidad, '(A)') trim(html_line)
            end do 
        end if
    
        close(unidad)
    end subroutine claves_html

    subroutine claves_css()

        integer :: i, unidad, ios
        character(len=1000) :: css_line
    
        unidad = 31
            open(unit=unidad, file='./Proyecto2/estilos.css', status='old', action='write', position='append', iostat=ios)
    
            if (ios /= 0) then
                print *, 'Error al abrir el archivo CSS'
                stop
            end if
    
            if(.not. allocated(arregloClave))then
                print *, "No hay claves"
            else
                do i = 1, size(arregloClave)
                    css_line ='#' // trim(arregloClave(i)%id) // '{'
                    write(unidad, '(A)')trim(css_line)
    
                    ! Posición
                    if (trim(arregloClave(i)%posX) /= "" .and. trim(arregloClave(i)%posY) /= "") then
                        css_line = '    position: absolute;'
                        write(unidad, '(A)') trim(css_line)
                        css_line = '    left: ' // trim(arregloClave(i)%posX) // 'px;'
                        write(unidad, '(A)') trim(css_line)
                        css_line = '    top: ' // trim(arregloClave(i)%posY) // 'px;'
                        write(unidad, '(A)') trim(css_line)
                    end if
    
                css_line = '}'
                write(unidad, '(A)') trim(css_line)
                end do
            end if
        
    end subroutine claves_css


end module moduloClave