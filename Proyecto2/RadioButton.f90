module moduloRadioB
    implicit none

    type :: Radio
    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY
    logical :: marcado = .false.

    end type Radio

    type(Radio), allocatable :: arregloRadio(:)
    
contains

subroutine agregarRadio(id)
    character(len=*), intent(in) :: id
    type(Radio) :: nuevoRadio
    integer :: n
    type(Radio), allocatable :: tempArray(:)

    nuevoRadio%id = id
    nuevoRadio%tipo = 'Radio'
    nuevoRadio%texto = ""
    nuevoRadio%posX = ""
    nuevoRadio%posY = ""
    nuevoRadio%marcado = .false.

    if(.not. allocated(arregloRadio)) then
        allocate(arregloRadio(1))
        arregloRadio(1) = nuevoRadio
    else
        n = size(arregloRadio)
        allocate(tempArray(n+1))
        tempArray(:n) = arregloRadio
        tempArray(n+1) = nuevoRadio
        deallocate(arregloRadio)
        allocate(arregloRadio(n+1))
        arregloRadio = tempArray
    end if
    
end subroutine agregarRadio

subroutine radioTexto(id, texto)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: texto
    integer :: i
    
    if (.not. allocated(arregloRadio)) then
        print *, "No hay botones radio"
    else
        do i = 1, size(arregloRadio)
            if (trim(arregloRadio(i)%id) == id) then
                arregloRadio(i)%texto = texto
            end if
        end do 
    end if

end subroutine radioTexto

subroutine radioPos(id, posX, posY)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: posX
    character(len=*), intent(in) :: posY
    integer :: i
    
    if (.not. allocated(arregloRadio)) then
        print *, "No hay botones radio"
    else
        do i = 1, size(arregloRadio)
            if (trim(arregloRadio(i)%id) == id) then
                arregloRadio(i)%posX = posX
                arregloRadio(i)%posY = posY
            end if
        end do
    end if

end subroutine radioPos

subroutine marcarRadio(id,  estado)
    character(len=*), intent(in) :: id
    logical, intent(in) :: estado
    integer :: i

    if(.not. allocated(arregloRadio)) then
        print *, 'No hay botones radio'
    else
        do i = 1, size(arregloRadio)
            if (trim(arregloRadio(i)%id) == id)then
                arregloRadio(i)%marcado = estado
            end if
        end do
    end if
    
end subroutine marcarRadio

subroutine verificarRadioMarcado(id, marcado)
    character(len=*), intent(in) :: id
    logical, intent(out) :: marcado  
    integer :: i
    logical :: encontrado = .false.

    if (.not. allocated(arregloRadio)) then
        print *, "No hay botones radio"
        marcado = .false.
    else
        do i = 1, size(arregloRadio)
            if (trim(arregloRadio(i)%id) == id) then
                marcado = arregloRadio(i)%marcado
                encontrado = .true.
            end if
        end do
        if (.not. encontrado) marcado = .false.
    end if

end subroutine verificarRadioMarcado

subroutine radios_html()
    implicit none
    integer :: i, unidad, ios
    character(len=1000) :: html_line

    unidad = 30
    open(unit=unidad, file='./Proyecto2/radios.html', status='replace', action='write', iostat=ios)

    if (ios /= 0) then
        print *, 'Error al abrir el archivo HTML'
        stop
    end if

    if (.not. allocated(arregloRadio)) then
        print *, "No hay botones radio"
    else
        do i = 1, size(arregloRadio)
            html_line = '<input type="radio" id="' // trim(arregloRadio(i)%id) // '" ' //  trim(adjustl(merge('checked', '       ', arregloRadio(i)%marcado))) // '/>' // &
            trim(arregloRadio(i)%texto)
            write(unidad, '(A)') trim(html_line)
        end do 
    end if

    close(unidad)
end subroutine radios_html

subroutine radios_css()
    integer :: i, unidad, ios
    character(len=1000) :: css_line
    
    unidad = 31
    open(unit=unidad, file='./Proyecto2/estilos.css', status='old', action='write', position='append', iostat=ios)

    if (ios /= 0) then
        print *, 'Error al abrir el archivo CSS'
        stop
    end if

    if (.not. allocated(arregloRadio)) then
        print *, "No hay radio botones"
    else
        do i = 1, size(arregloRadio)
            css_line = '#' // trim(arregloRadio(i)%id) // ' {'
            write(unidad, '(A)') trim(css_line)

            ! Posición
            if (trim(arregloRadio(i)%posX) /= "" .and. trim(arregloRadio(i)%posY) /= "") then
                css_line = '    position: absolute;'
                write(unidad, '(A)') trim(css_line)
                css_line = '    left: ' // trim(arregloRadio(i)%posX) // 'px;'
                write(unidad, '(A)') trim(css_line)
                css_line = '    top: ' // trim(arregloRadio(i)%posY) // 'px;'
                write(unidad, '(A)') trim(css_line)
            end if
            
            css_line = '}'
            write(unidad, '(A)') trim(css_line)
    end do
    end if

    close(unidad)
    
end subroutine radios_css
    
end module moduloRadioB