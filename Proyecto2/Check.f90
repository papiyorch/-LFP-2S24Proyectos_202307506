module moduloCheck
    implicit none
    type :: Check

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY
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

end module moduloCheck