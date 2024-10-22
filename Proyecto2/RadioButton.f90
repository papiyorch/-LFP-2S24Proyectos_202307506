module moduloRadioB
    implicit none

    type :: Radio
    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 250) :: texto
    character(len = 50) :: posX
    character(len = 50) :: posY

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
    
end module moduloRadioB