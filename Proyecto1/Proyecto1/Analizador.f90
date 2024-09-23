module moduloAnalizador
    use moduloError
    use moduloToken
    implicit none
    

    type :: Analizador


        integer :: estado
        
        type(Token) :: tokens(500)
        type(Error) :: errores(500)

       
        integer :: linea = 1
        integer :: columna = 1

      
        integer :: iTokens = 1
        integer :: iErrores = 1

       
        integer :: i = 1

        logical :: hayError = .false.

        character(len=:), allocatable :: buffer 

        contains
        procedure :: analizar
        procedure :: inicializarEstado
        procedure :: agregarToken
        procedure :: agregarError
        procedure :: estado0
        procedure :: estado1
        procedure :: estado2
        procedure :: estado3
        procedure :: inicializarBuffer
        procedure :: generarReporteTokens
        procedure :: generarReporteErrores

    end type Analizador

    contains

        subroutine analizar(this, entrada)
            class (Analizador), intent(inout) :: this
            character(len=*), intent(in) :: entrada

            integer :: longitud 
            this%i = 1 

           
            this%iTokens = 1
            this%iErrores = 1


            longitud = len_trim(entrada)

            do while(this%i <= longitud)
                select case(this%estado)
                    case(0)
                        call this%estado0(entrada(this%i:this%i))
                    case(1)
                        call this%estado1(entrada(this%i:this%i))
                    case(2)
                        call this%estado2(entrada(this%i:this%i))
                    case(3)
                        call this%estado3(entrada(this%i:this%i))
                end select
                this%i = this%i + 1
            end do

            if (.not. this%errores(1)%tieneError()) then
                call this%generarReporteTokens('tokens.html')
            else
                call this%generarReporteErrores('errores.html')
                call this%generarReporteTokens('tokens.html')
            end if

        end subroutine analizar

        subroutine inicializarEstado(this)
            class(Analizador), intent(inout) :: this
            this%estado = 0
        end subroutine inicializarEstado

        subroutine inicializarBuffer(this)
            class(Analizador), intent(inout) :: this
            if (.not. allocated(this%buffer)) allocate(character(len=0) :: this%buffer)
            this%buffer = ''
        end subroutine inicializarBuffer

        subroutine agregarToken(this, nombre, lexema, linea, columna)
            class(Analizador), intent(inout) :: this
            character(len=*), intent(in) :: nombre
            character(len=*), intent(in) :: lexema
            integer, intent(in) :: linea
            integer, intent(in) :: columna

            
            type(Token) :: token
            call token%crearToken(nombre, lexema, linea, columna)
            this%tokens(this%iTokens) = token
            this%iTokens = this%iTokens + 1
        end subroutine agregarToken

        subroutine agregarError(this, caracter, descripcion, linea, columna)
            class(Analizador), intent(inout) :: this
            character(len=*), intent(in) :: caracter
            character(len=*), intent(in) :: descripcion
            integer, intent(in) :: linea
            integer, intent(in) :: columna
            
            type(Error) :: error
            
            call error%crearError(caracter, descripcion, linea, columna)
            this%errores(this%iErrores) = error
            this%iErrores = this%iErrores + 1

          

        end subroutine agregarError

        subroutine estado0(this, caracter)
            class(Analizador), intent(inout) :: this
            character(len=*), intent(in):: caracter
        
            if ((iachar(caracter) >= iachar('A') .and. iachar(caracter) <= iachar('Z')) .or. &
                (iachar(caracter) >= iachar('a') .and. iachar(caracter) <= iachar('z'))) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                this%estado = 1

            else if (caracter == '"') then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                this%estado = 2

            else if (iachar(caracter) >= iachar('0') .and. iachar(caracter) <= iachar('9')) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                this%estado = 3

            else if (caracter == ':') then
                call this%agregarToken('dos_puntos', caracter, this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1

            else if (caracter == '{') then
                call this%agregarToken('llave_abre', caracter, this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1

            else if (caracter == '}') then
                call this%agregarToken('llave_cierra', caracter, this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1

            else if (caracter == '%') then
                call this%agregarToken('porcentaje', caracter, this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1

            else if (caracter == ';') then
                call this%agregarToken('punto_y_coma', caracter, this%linea, this%columna)
                call this%inicializarBuffer()
                this%columna = this%columna + 1

            else if (caracter == new_line('A')) then
                this%linea = this%linea + 1
                this%columna = 1

            else if (caracter == ' ') then
                this%columna = this%columna + 1

            else if (caracter == char(9)) then  
                this%columna = this%columna + 1  
            else
               
                call this%agregarError(caracter, 'Caracter no valido', this%linea, this%columna)
                this%columna = this%columna + 1
                this%hayError = .true.
            end if
        end subroutine estado0

        
        subroutine estado1(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            character(len=:), allocatable :: tempBuffer 

            if ((iachar(caracter) >= iachar('A') .and. iachar(caracter) <= iachar('Z')) .or. &
            (iachar(caracter) >= iachar('a') .and. iachar(caracter) <= iachar('z'))) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1

            else
                
                tempBuffer = trim(this%buffer)

                if (tempBuffer=="grafica") then
                    
                    call this%agregarToken("Palabra reservada [grafica]",tempBuffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                   
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer=="nombre") then
                   
                    call this%agregarToken("Palabra reservada [nombre]",tempBuffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer == 'continente') then
                    call this%agregarToken('palabra_reservada [continente]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer == 'pais') then
                    call this%agregarToken('palabra_reservada [pais]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                   
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer == 'poblacion') then
                    call this%agregarToken('palabra_reservada [poblacion]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer == 'saturacion') then
                    call this%agregarToken('palabra_reservada [saturacion]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    
                    this%i=this%i-1
                    this%estado = 0

                else if (tempBuffer == 'bandera') then
                    call this%agregarToken('palabra_reservada [bandera]', this%buffer, this%linea, this%columna)
                    call this%inicializarBuffer()
                    
                    this%i=this%i-1
                    this%estado = 0
                else
                    call this%agregarError(tempBuffer, 'Palabra reservada no valida', this%linea, this%columna)
                    call this%inicializarBuffer()
                    this%columna = this%columna + 1
                    this%estado = 0
                end if
            end if

        end subroutine estado1

        subroutine estado2(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            if(caracter=='"') then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
                call this%agregarToken('cadena',this%buffer,this%linea, this%columna)
                call this%inicializarBuffer()
                this%estado = 0
            else
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
            end if

        end subroutine estado2

        subroutine estado3(this, caracter)
            class(Analizador), intent(inout) ::this
            character(len=*), intent(in)::caracter
            
            if(iachar(caracter) >= iachar('0') .and. iachar(caracter) <= iachar('9')) then
                this%buffer = this%buffer // caracter
                this%columna = this%columna + 1
            else

                call this%agregarToken('entero',trim(this%buffer),this%linea, this%columna)
                call this%inicializarBuffer()
                
                this%estado = 0
                this%i = this%i - 1
            end if

        end subroutine estado3

        subroutine generarReporteTokens(this, archivo)
            class(Analizador), intent(in) :: this
            character(len=*), intent(in) :: archivo
            integer :: i
            integer :: unit = 12
        
           
            open(unit=unit, file=archivo, status='replace', action='write')
        
           
            write(unit, '(A)') "<html><body><table border='1'>"
          
            write(unit, '(A)') "<tr><th>Nombre</th><th>Lexema</th><th>Linea</th><th>Columna</th></tr>"
        
            
            do i = 1, this%iTokens-1
                write(unit, '(A, A, A, A, A, I0, A, I0, A)', advance="no") &
                    "<tr><td>", trim(this%tokens(i)%nombre), "</td><td>", &
                    trim(this%tokens(i)%lexema), "</td><td>", &
                    this%tokens(i)%linea, "</td><td>", &
                    this%tokens(i)%columna, "</td></tr>"
            end do
        
           
            write(unit, '(A)') "</table></body></html>"
        
            close(unit)
        end subroutine generarReporteTokens
        
    
        subroutine generarReporteErrores(this, archivo)
            class(Analizador), intent(in) :: this
            character(len=*), intent(in) :: archivo
            integer :: i
            integer :: unit =11
        
            
            open(unit=unit, file=archivo, status='replace', action='write')
        
            
            write(unit, '(A)') "<html><body><table border='1'>"
            write(unit, '(A)') "<tr><th>Caracter</th><th>Descripcion</th><th>Linea</th><th>Columna</th></tr>"
        
            
            do i = 1, this%iErrores-1
                write(unit, '(A, A, A, A, A, I0, A, I0, A)', advance="no") &
                    "<tr><td>", trim(this%errores(i)%caracter), "</td><td>", &
                    trim(this%errores(i)%descripcion), "</td><td>", &
                    this%errores(i)%linea, "</td><td>", &
                    this%errores(i)%columna, "</td></tr>"
            end do
        
           
            write(unit, '(A)') "</table></body></html>"
        
            
            close(unit)
        end subroutine generarReporteErrores
            
        function int2str(i) result(res)
            integer, intent(in) :: i
            character(:), allocatable :: res
            character(range(i)+2) :: tmp
            write(tmp,'(i0)') i
            res = trim(tmp)
        end function int2str



end module moduloAnalizador

