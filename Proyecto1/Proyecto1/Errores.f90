module moduloError
    implicit none
    type :: Error
        character(len=:), allocatable :: caracter
        character(len=:), allocatable :: descripcion
        integer :: linea, columna

        contains
        procedure :: crearError
        procedure :: tieneError 
    end type Error

    contains

    subroutine crearError(this, caracter, descripcion, linea, columna) 
        class(Error), intent(inout) :: this
        character(len=*), intent(in) :: caracter
        character(len=*), intent(in) :: descripcion
        integer, intent(in) :: linea
        integer, intent(in) :: columna

        this%caracter = caracter
        this%descripcion = descripcion
        this%linea = linea
        this%columna = columna
    end subroutine crearError

    logical function tieneError(this)
        class(Error), intent(in) :: this
        tieneError = allocated(this%caracter) .and. allocated(this%descripcion) &
        .or. this%caracter == "" .and.  this%descripcion == ""
    end function tieneError
end module moduloError