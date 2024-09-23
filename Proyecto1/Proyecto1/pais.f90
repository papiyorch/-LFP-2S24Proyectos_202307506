module moduloPais
    implicit none 

    type :: Pais
        character(len=:), allocatable :: nombre, bandera
        integer :: poblacion, saturacion

    contains
        procedure :: crearPais

    end type Pais

    contains

    subroutine crearPais(this, nombre, bandera, poblacion, saturacion)
       class(Pais), intent(inout) :: this
       character(len=*), intent(in) :: nombre, bandera
       integer, intent(in) :: poblacion, saturacion
        
       this%nombre = nombre
       this%bandera = bandera
       this%poblacion = poblacion
       this%saturacion = saturacion
        
    end subroutine crearPais

end module moduloPais   
