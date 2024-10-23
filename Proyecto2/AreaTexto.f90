module moduloAreaT

    implicit none
    type :: AreaT 
    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    
    end type AreaT
    type (AreaT), allocatable :: arregloArea(:)

contains
    
subroutine agregarAreaT(id)
    character(len=*), intent(in) :: id
    type(AreaT) :: nuevaArea
    integer :: n
    type(AreaT), allocatable :: tempArray(:)

    nuevaArea%id = id
    nuevaArea%tipo = 'Area'
    nuevaArea%texto = ""
    

    if(.not. allocated(arregloArea)) then
        allocate(arregloArea(1))
        arregloArea(1) = nuevaArea
    else
        n = size(arregloArea)
        allocate(tempArray(n+1))
        tempArray(:n) = arregloArea
        tempArray(n+1) = nuevaArea
        deallocate(arregloArea)
        allocate(arregloArea(n+1))
        arregloArea = tempArray
    end if
    
end subroutine agregarAreaT

subroutine areaTexto(id, texto)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: texto
    integer :: i
    
    if (.not. allocated(arregloArea)) then
        print *, "No hay areas"
    else
        do i = 1, size(arregloArea)
            if (trim(arregloArea(i)%id) == id) then
                arregloArea(i)%texto = texto
            end if
        end do 
    end if

end subroutine areaTexto

subroutine areas_html()
    implicit none
    integer :: i, unidad, ios
    character(len=1000) :: html_line

    unidad = 30
    open(unit=unidad, file='./Proyecto2/areas.html', status='replace', action='write', iostat=ios)

    if (ios /= 0) then
        print *, 'Error al abrir el archivo HTML'
        stop
    end if

    if (.not. allocated(arregloArea)) then
        print *, "No hay contenedores"
    else
        do i = 1, size(arregloArea)
            html_line = '<TEXTAREA id="' // trim(arregloArea(i)%id) // '">' // trim(arregloArea(i)%texto) // '</TEXTAREA>'
            write(unidad, '(A)') trim(html_line)
        end do 
    end if

    close(unidad)
end subroutine areas_html

end module moduloAreaT
