module moduloEtiqueta
    implicit none
    
    type :: Etiqueta

    character(len = 50) :: id
    character(len = 50) :: tipo
    character(len = 50) :: alto
    character(len = 50) :: ancho
    character(len = 250) :: texto
    character(len = 50) :: color_textoR
    character(len = 50) :: color_textoG
    character(len = 50) :: color_textoB
    character(len = 50) :: posX
    character(len = 50) :: posY

    end type Etiqueta

    type(Etiqueta), allocatable :: arregloEtiqueta(:)
    
contains

subroutine agregarEtiqueta(id)
    character(len=*), intent(in) :: id
    type(Etiqueta) :: nuevaEtiqueta
    integer :: n
    type(Etiqueta) , allocatable :: tempArray(:)

    nuevaEtiqueta%id = id
    nuevaEtiqueta%tipo = 'Etiqueta'
    nuevaEtiqueta%alto = ""
    nuevaEtiqueta%ancho = ""
    nuevaEtiqueta%texto = ""
    nuevaEtiqueta%color_textoR = ""
    nuevaEtiqueta%color_textoG = ""
    nuevaEtiqueta%color_textoB = ""
    nuevaEtiqueta%posX = ""
    nuevaEtiqueta%posY = ""

    if(.not. allocated(arregloEtiqueta)) then
        allocate(arregloEtiqueta(1))
        arregloEtiqueta(1) = nuevaEtiqueta
    else
        n = size(arregloEtiqueta)
        allocate(tempArray(n+1))
        tempArray(:n) = arregloEtiqueta
        tempArray(n+1) = nuevaEtiqueta
        deallocate(arregloEtiqueta)
        allocate(arregloEtiqueta(n+1))
        arregloEtiqueta = tempArray
    end if
    
end subroutine agregarEtiqueta
    
subroutine etiquetaAlto(id, alto)

    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: alto
    integer :: i

    if(.not. allocated(arregloEtiqueta)) then
        print*, "No hay etiquetas"
    else
        do i = 1, size(arregloEtiqueta)
            if(trim(arregloEtiqueta(i)%id) == id) then
                arregloEtiqueta(i)%alto = alto
            end if  
        end do
    end if
    
end subroutine etiquetaAlto

subroutine etiquetaAncho(id, ancho)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: ancho
    integer :: i
    
    if (.NOT. allocated(arregloEtiqueta)) then
        print *, "No hay etiquetas"
    else
        DO i = 1, size(arregloEtiqueta)
            if (trim(arregloEtiqueta(i)%id) == id) then
                arregloEtiqueta(i)%ancho = ancho
            end if
        END DO
    end if

end subroutine etiquetaAncho

subroutine etiquetaTexto(id, texto)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: texto
    integer :: i
    
    if (.not. allocated(arregloEtiqueta)) then
        print *, "No hay etiquetas"
    else
        do i = 1, size(arregloEtiqueta)
            if (trim(arregloEtiqueta(i)%id) == id) then
                arregloEtiqueta(i)%texto = texto
            end if
        end do 
    end if

end subroutine etiquetaTexto

subroutine etiquetaColorTexto(id, color_textoR, color_textoG, color_textoB)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: color_textoR
    character(len=*), intent(in) :: color_textoG
    character(len=*), intent(in) :: color_textoB
    integer :: i
    
    if (.not. allocated(arregloEtiqueta)) then
        print *, "No hay etiquetas"
    else
        do i = 1, size(arregloEtiqueta)
            if (trim(arregloEtiqueta(i)%id) == id) then
                arregloEtiqueta(i)%color_textoR = color_textoR
                arregloEtiqueta(i)%color_textoG = color_textoG
                arregloEtiqueta(i)%color_textoB = color_textoB
            end if
        end do
    end if

end subroutine etiquetaColorTexto

subroutine etiquetaPos(id, posX, posY)
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: posX
    character(len=*), intent(in) :: posY
    integer :: i
    
    if (.not. allocated(arregloEtiqueta)) then
        print *, "No hay etiquetas"
    else
        do i = 1, size(arregloEtiqueta)
            if (trim(arregloEtiqueta(i)%id) == id) then
                arregloEtiqueta(i)%posX = posX
                arregloEtiqueta(i)%posY = posY
            end if
        end do
    end if

end subroutine etiquetaPos

function buscarEtiqueta(id) result(encontrado)
    character(len=*), intent(in) :: id
    logical :: encontrado
    integer :: i

    encontrado = .false.

    if(.not. allocated(arregloEtiqueta)) then
        print*, "No hay etiquetas"
    else
        do  i = 1, size(arregloEtiqueta)
            if (trim(arregloEtiqueta(i)%id) == trim(id)) then
                encontrado = .true.
                return
            end if
        end do
    end if
    
end function buscarEtiqueta

subroutine imprimir_etiquetas()

    integer :: i

        if (.not. allocated(arregloEtiqueta)) then
            print *, "No hay etiquetas"
        else
            print *, "etiquetas encontrados: ", size(arregloEtiqueta)
            do i = 1, size(arregloEtiqueta)
                print *, 'id: ', trim(arregloEtiqueta(i)%id)
                print *, 'alto: ', trim(arregloEtiqueta(i)%alto)
                print *, 'ancho: ', trim(arregloEtiqueta(i)%ancho)
                print *, 'texto: ', trim(arregloEtiqueta(i)%texto)
                print *, 'color_textoR: ', trim(arregloEtiqueta(i)%color_textoR)
                print *, 'color_textoG: ', trim(arregloEtiqueta(i)%color_textoG)
                print *, 'color_textoB: ', trim(arregloEtiqueta(i)%color_textoB)
                print *, 'posX: ', trim(arregloEtiqueta(i)%posX)
                print *, 'posY: ', trim(arregloEtiqueta(i)%posY)
                print *, '---------------------------------'
            end do 
        end if

    end subroutine imprimir_etiquetas

end module moduloEtiqueta
