module moduloContinente
    use moduloPais
    implicit none

    type :: Continente
        character(len=:), allocatable :: nombre
        type(Pais) :: paises(50)
        integer :: iPaises = 1
     contains
        procedure :: crearContinente

    end type Continente

    contains

    subroutine crearContinente(this, nombre)
       class(Continente), intent(inout) :: this
       character(len=*), intent(in) :: nombre

       this%nombre = nombre
        
    end subroutine crearContinente

end module moduloContinente 

