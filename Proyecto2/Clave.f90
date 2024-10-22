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

end module moduloClave