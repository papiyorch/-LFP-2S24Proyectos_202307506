module moduloBoton
    implicit none

    type Boton
    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 150) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY

    end type Boton

    type(Boton), allocatable :: arregloBoton(:)
    
contains

    subroutine agregarBoton(id)
        character(len=*), intent(in) :: id
        type(Boton) :: nuevoBoton
        integer :: n
        type(Boton) , allocatable :: tempArray(:)

        nuevoBoton%id = id
        nuevoBoton%tipo = 'Boton'
        nuevoBoton%texto = ""
        nuevoBoton%posX = ""
        nuevoBoton%posY = ""

        if(.not. allocated(arregloBoton)) then
            allocate(arregloBoton(1))
            arregloBoton(1) = nuevoBoton
        else
            n = size(arregloBoton)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloBoton
            tempArray(n+1) = nuevoBoton
            deallocate(arregloBoton)
            allocate(arregloBoton(n+1))
            arregloBoton = tempArray
        end if
        
    end subroutine agregarBoton

    subroutine botonTexto(id, texto)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: texto
        integer :: i
        
        if (.not. allocated(arregloBoton)) then
            print *, "No hay botones"
        else
            do i = 1, size(arregloBoton)
                if (trim(arregloBoton(i)%id) == id) then
                    arregloBoton(i)%texto = texto
                end if
            end do 
        end if
    
    end subroutine botonTexto

    subroutine botonPos(id, posX, posY)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: posX
        character(len=*), intent(in) :: posY
        integer :: i
        
        if (.not. allocated(arregloBoton)) then
            print *, "No hay botones"
        else
            do i = 1, size(arregloBoton)
                if (trim(arregloBoton(i)%id) == id) then
                    arregloBoton(i)%posX = posX
                    arregloBoton(i)%posY = posY
                end if
            end do
        end if
    
    end subroutine botonPos

end module moduloBoton