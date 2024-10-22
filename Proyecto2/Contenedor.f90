module moduloContenedor
    implicit none

    type :: Contenedor

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 50) :: alto
    character(len = 50) :: ancho
    character(len = 50) :: color_FondoR
    character(len = 50) :: color_FondoG
    character(len = 50) :: color_FondoB
    character(len = 50) :: posX
    character(len = 50) :: posY

    end type Contenedor 

    type(Contenedor), allocatable :: arregloContenedor(:)

    contains 

    subroutine agregarContenedor(id)
        character(len=*), intent(in) :: id
        type(Contenedor) :: nuevoContenedor
        integer :: n
        type(Contenedor) , allocatable :: tempArray(:)
    
        nuevoContenedor%id = id
        nuevoContenedor%tipo = 'Contenedor'
        nuevoContenedor%alto = ""
        nuevoContenedor%ancho = ""
        nuevoContenedor%color_FondoR = ""
        nuevoContenedor%color_FondoG = ""
        nuevoContenedor%color_FondoB = ""
        nuevoContenedor%posX = ""
        nuevoContenedor%posY = ""
    
        if(.not. allocated(arregloContenedor)) then
            allocate(arregloContenedor(1))
            arregloContenedor(1) = nuevoContenedor
        else
            n = size(arregloContenedor)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloContenedor
            tempArray(n+1) = nuevoContenedor
            deallocate(arregloContenedor)
            allocate(arregloContenedor(n+1))
            arregloContenedor = tempArray
        end if
    end subroutine agregarContenedor

    subroutine contenedorAlto(id, alto)

        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: alto
        integer :: i
    
        if(.not. allocated(arregloContenedor)) then
            print*, "No hay contenedores"
        else
            do i = 1, size(arregloContenedor)
                if(trim(arregloContenedor(i)%id) == id) then
                    arregloContenedor(i)%alto = alto
                end if  
            end do
        end if
        
    end subroutine contenedorAlto
    
    subroutine contenedorAncho(id, ancho)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: ancho
        integer :: i
        
        if (.NOT. allocated(arregloContenedor)) then
            print *, "No hay contenedores"
        else
            DO i = 1, size(arregloContenedor)
                if (trim(arregloContenedor(i)%id) == id) then
                    arregloContenedor(i)%ancho = ancho
                end if
            END DO
        end if
    
    end subroutine contenedorAncho
    
    
subroutine contenedorColorFondo(id, color_FondoR, color_FondoG, color_FondoB)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: color_FondoR
    character(len=*), intent(in) :: color_FondoG
    character(len=*), intent(in) :: color_FondoB
    integer :: i

    if (.not. allocated(arregloContenedor)) then
        print *, "No hay contenedores"
    else
        do i = 1, size(arregloContenedor)
            if (trim(arregloContenedor(i)%id) == id) then
                arregloContenedor(i)%color_FondoR = color_FondoR
                arregloContenedor(i)%color_FondoG = color_FondoG
                arregloContenedor(i)%color_FondoB = color_FondoB
            end if
        end do
    end if
    
end subroutine contenedorColorFondo

subroutine contenedorPos(id, posX, posY)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: posX
    character(len=*), intent(in) :: posY
    integer :: i
    
    if (.not. allocated(arregloContenedor)) then
        print *, "No hay contenedores"
    else
        do i = 1, size(arregloContenedor)
            if (trim(arregloContenedor(i)%id) == id) then
                arregloContenedor(i)%posX = posX
                arregloContenedor(i)%posY = posY
            end if
        end do
    end if

end subroutine contenedorPos

end module moduloContenedor