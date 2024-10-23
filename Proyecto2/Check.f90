module moduloCheck
    implicit none
    type :: Check

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY
    logical :: marcado = .false.

    end type Check
    
    type (Check), allocatable :: arregloCheck(:)

    contains

    subroutine agregarCheck(id)
        character(len=*), intent(in) :: id
        type(Check) :: nuevoCheck
        integer :: n
        type(Check), allocatable :: tempArray(:)

        nuevoCheck%id = id
        nuevoCheck%tipo = 'Check'
        nuevoCheck%texto = ""
        nuevoCheck%posX = ""
        nuevoCheck%posY = ""
        nuevoCheck%marcado = .false.

        if(.not. allocated(arregloCheck)) then
            allocate(arregloCheck(1))
            arregloCheck(1) = nuevoCheck
        else
            n = size(arregloCheck)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloCheck
            tempArray(n+1) = nuevoCheck
            deallocate(arregloCheck)
            allocate(arregloCheck(n+1))
            arregloCheck = tempArray
        end if
        
    end subroutine agregarCheck

    subroutine checkTexto(id, texto)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: texto
        integer :: i
        
        if (.not. allocated(arregloCheck)) then
            print *, "No hay checks"
        else
            do i = 1, size(arregloCheck)
                if (trim(arregloCheck(i)%id) == id) then
                    arregloCheck(i)%texto = texto
                end if
            end do 
        end if
    
    end subroutine checkTexto

    subroutine checkPos(id, posX, posY)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: posX
        character(len=*), intent(in) :: posY
        integer :: i
        
        if (.not. allocated(arregloCheck)) then
            print *, "No hay texto"
        else
            do i = 1, size(arregloCheck)
                if (trim(arregloCheck(i)%id) == id) then
                    arregloCheck(i)%posX = posX
                    arregloCheck(i)%posY = posY
                end if
            end do
        end if
    
    end subroutine checkPos

    subroutine marcarCheck(id,  estado)
        character(len=*), intent(in) :: id
        logical, intent(in) :: estado
        integer :: i
    
        if(.not. allocated(arregloCheck)) then
            print *, 'No hay check'
        else
            do i = 1, size(arregloCheck)
                if (trim(arregloCheck(i)%id) == id)then
                    arregloCheck(i)%marcado = estado
                end if
            end do
        end if
        
    end subroutine marcarCheck
    
    subroutine verificarCheckMarcado(id, marcado)
        character(len=*), intent(in) :: id
        logical, intent(out) :: marcado  
        integer :: i
        logical :: encontrado = .false.
    
        if (.not. allocated(arregloCheck)) then
            print *, "No hay botones radio"
            marcado = .false.
        else
            do i = 1, size(arregloCheck)
                if (trim(arregloCheck(i)%id) == id) then
                    marcado = arregloCheck(i)%marcado
                    encontrado = .true.
                end if
            end do
            if (.not. encontrado) marcado = .false.
        end if
    
    end subroutine verificarCheckMarcado

    subroutine checks_html()
        implicit none
        integer :: i, unidad, ios
        character(len=1000) :: html_line
    
        unidad = 30
        open(unit=unidad, file='./Proyecto2/checks.html', status='replace', action='write', iostat=ios)
    
        if (ios /= 0) then
            print *, 'Error al abrir el archivo HTML'
            stop
        end if
    
        if (.not. allocated(arregloCheck)) then
            print *, "No hay etiquetas"
        else
            do i = 1, size(arregloCheck)
                html_line = '<inpuy type="checkbox" id="' // trim(arregloCheck(i)%id) // '"'// trim(adjustl(merge('checked', '       ', arregloCheck(i)%marcado))) //'/>' 
                write(unidad, '(A)') trim(html_line)
            end do 
        end if

        close(unidad)
    end subroutine checks_html

    subroutine checks_css()

        integer :: i, unidad, ios
        character(len=1000) :: css_line
    
        unidad = 31
            open(unit=unidad, file='./Proyecto2/estilos.css', status='old', action='write', position='append', iostat=ios)
    
            if (ios /= 0) then
                print *, 'Error al abrir el archivo CSS'
                stop
            end if
    
            if(.not. allocated(arregloCheck))then
                print *, "No hay checks"
            else
                do i = 1, size(arregloCheck)
                    css_line ='#' // trim(arregloCheck(i)%id) // '{'
                    write(unidad, '(A)')trim(css_line)
    
                    ! Posición
                    if (trim(arregloCheck(i)%posX) /= "" .and. trim(arregloCheck(i)%posY) /= "") then
                        css_line = '    position: absolute;'
                        write(unidad, '(A)') trim(css_line)
                        css_line = '    left: ' // trim(arregloCheck(i)%posX) // 'px;'
                        write(unidad, '(A)') trim(css_line)
                        css_line = '    top: ' // trim(arregloCheck(i)%posY) // 'px;'
                        write(unidad, '(A)') trim(css_line)
                    end if
    
                css_line = '}'
                write(unidad, '(A)') trim(css_line)
                end do
            end if
        
    end subroutine checks_css
end module moduloCheck