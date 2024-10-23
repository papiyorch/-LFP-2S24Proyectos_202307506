module moduloTexto
    implicit none
    type :: texto

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY
    end type Texto
    
    type (Texto), allocatable :: arregloTexto(:)

    contains

    subroutine agregarTexto(id)
        character(len=*), intent(in) :: id
        type(Texto) :: nuevoTexto
        integer :: n
        type(Texto), allocatable :: tempArray(:)

        nuevoTexto%id = id
        nuevoTexto%tipo = 'Texto'
        nuevoTexto%texto = ""
        nuevoTexto%posX = ""
        nuevoTexto%posY = ""

        if(.not. allocated(arregloTexto)) then
            allocate(arregloTexto(1))
            arregloTexto(1) = nuevoTexto
        else
            n = size(arregloTexto)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloTexto
            tempArray(n+1) = nuevoTexto
            deallocate(arregloTexto)
            allocate(arregloTexto(n+1))
            arregloTexto = tempArray
        end if
        
    end subroutine agregarTexto

    subroutine textoTexto(id, texto)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: texto
        integer :: i
        
        if (.not. allocated(arregloTexto)) then
            print *, "No hay texto"
        else
            do i = 1, size(arregloTexto)
                if (trim(arregloTexto(i)%id) == id) then
                    arregloTexto(i)%texto = texto
                end if
            end do 
        end if
    
    end subroutine textoTexto

    subroutine textoPos(id, posX, posY)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: posX
        character(len=*), intent(in) :: posY
        integer :: i
        
        if (.not. allocated(arregloTexto)) then
            print *, "No hay texto"
        else
            do i = 1, size(arregloTexto)
                if (trim(arregloTexto(i)%id) == id) then
                    arregloTexto(i)%posX = posX
                    arregloTexto(i)%posY = posY
                end if
            end do
        end if
    
    end subroutine textoPos

    subroutine textos_html()
        implicit none
        integer :: i, unidad, ios
        character(len=1000) :: html_line
    
        unidad = 30
        open(unit=unidad, file='./Proyecto2/textos.html', status='replace', action='write', iostat=ios)
    
        if (ios /= 0) then
            print *, 'Error al abrir el archivo HTML'
            stop
        end if
    
        if (.not. allocated(arregloTexto)) then
            print *, "No hay etiquetas"
        else
            do i = 1, size(arregloTexto)
                html_line = '<input type="text" id="' // trim(arregloTexto(i)%id) // '" value="'// trim(arregloTexto(i)%texto) // '"/>' 
                write(unidad, '(A)') trim(html_line)
            end do 
        end if
    
        close(unidad)
    end subroutine textos_html

    subroutine textos_css()
        integer :: i, unidad, ios
        character(len=1000) :: css_line
        
        unidad = 31
        open(unit=unidad, file='./Proyecto2/estilos.css', status='old', action='write', position='append', iostat=ios)
    
        if (ios /= 0) then
            print *, 'Error al abrir el archivo CSS'
            stop
        end if
    
        if (.not. allocated(arregloTexto)) then
            print *, "No hay textos"
        else
            do i = 1, size(arregloTexto)
                css_line = '#' // trim(arregloTexto(i)%id) // ' {'
                write(unidad, '(A)') trim(css_line)
    
                ! Posición
                if (trim(arregloTexto(i)%posX) /= "" .and. trim(arregloTexto(i)%posY) /= "") then
                    css_line = '    position: absolute;'
                    write(unidad, '(A)') trim(css_line)
                    css_line = '    left: ' // trim(arregloTexto(i)%posX) // 'px;'
                    write(unidad, '(A)') trim(css_line)
                    css_line = '    top: ' // trim(arregloTexto(i)%posY) // 'px;'
                    write(unidad, '(A)') trim(css_line)
                end if
                
                css_line = '}'
                write(unidad, '(A)') trim(css_line)
        end do
        end if
    
        close(unidad)
        
    end subroutine textos_css

end module moduloTexto