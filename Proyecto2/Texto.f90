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

end module moduloTexto