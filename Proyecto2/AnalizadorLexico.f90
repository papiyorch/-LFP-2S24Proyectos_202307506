program analizador_lexico
    use moduloToken
    use errormodulo
    use moduloEtiqueta
    use moduloBoton
    use moduloClave
    use moduloContenedor
    use moduloTexto
    use moduloArea
    use moduloCheck
    use moduloRadioB
    implicit none

    integer :: len, fila, columna, estado, puntero
    integer :: ios 
    character(len=100000) :: contenido, buffer
    character(len=1) :: caracter
    character(len=100) ::  tokenAuxiliar

    estado = 0
    puntero = 1
    columna = 0
    fila = 1
    tokenAuxiliar = ""
    
    contenido = ""

    !unidad = 10
    !open(unit = unidad, file='Entrada.LFP', status = 'old', action ='read', iostat=ios)
   
    !if(ios /= 0) then 
     !   print *, 'Error al abrir el erchivo'
      !  stop
    !end if

    do
        read(*, '(A)', iostat = ios) buffer
        if(ios /=0) exit
        contenido = trim(contenido)// trim(buffer)//new_line('a')
    end do 

    !close(unidad)

    len = len_trim(contenido)

    do while(puntero <= len)
        caracter = contenido(puntero:puntero)
        select case (estado)

        case(0)

             !Comentario linea
            if(caracter == '/' .and. contenido(puntero+1:puntero+1) == '/')then
                estado = 7
                puntero = puntero + 2
                columna = columna + 2

            !comentario bloque
            else if(caracter == '/' .and. contenido(puntero+1:puntero+1) == '*')then
                estado = 8
                puntero = puntero + 2
                columna = columna + 2

            else if(caracter == ';' .or. caracter == '-' .or. caracter == '.' .or. caracter == '(' .or. caracter == ')' .or. caracter == ',' .or. caracter == '<' .or. caracter == '>' .or. caracter == '!')then
            estado = 1
            columna = columna + 1

            else if(caracter >= 'A'.and. caracter <= 'Z' .or. caracter >= 'a' .and. caracter <= 'z')then
            estado = 2
            
            else if(caracter >='0' .and. caracter <= '9')then
                estado = 3

            else if(caracter == '"')then
                tokenAuxiliar = trim(tokenAuxiliar) // caracter
                columna = columna + 1
                puntero = puntero + 1
                estado = 4
            
                !Salto linea
            else if(ichar(caracter) ==10)then
                columna = 0
                fila = fila + 1
                puntero = puntero + 1

                !Tab
            elseif (ichar(caracter) == 9) then
                columna = columna + 4
                puntero = puntero + 1

                !Espacio en blanco
            elseif (ichar(caracter) == 32) then
                columna = columna + 1
                puntero = puntero + 1  
            
            else
                call agregar_errorLex('Lexico',caracter, 'Error Lexico', fila, columna)
                columna = columna + 1
                puntero = puntero + 1 

        end if

    case(1)
        if ( caracter == ';' ) then
            call crearToken(caracter, 'tk_puntoycoma', fila, columna)
            
        elseif ( caracter == '.' ) then
            call crearToken(caracter, 'tk_punto', fila, columna)

        elseif ( caracter == ',' ) then
            call crearToken(caracter, 'tk_coma', fila, columna)

        elseif ( caracter == '>') then
            call crearToken(caracter, 'tk_mayor', fila, columna)

        elseif ( caracter == '<') then
            call crearToken(caracter, 'tk_menor', fila, columna)

        elseif ( caracter == '(') then
            call crearToken(caracter, 'tk_parentesisI', fila, columna)

        elseif ( caracter == ')') then
            call crearToken(caracter, 'tk_parentesisD', fila, columna)         
        
        elseif ( caracter == '-') then
            call crearToken(caracter, 'tk_guion', fila, columna)
        
        elseif ( caracter == '!') then
            call crearToken(caracter, 'tk_admiracion', fila, columna)
            
        else
            call agregar_errorLex('Lexico', caracter, 'Error Lexico', fila, columna)

        end if
        puntero = puntero + 1
        estado = 0

    case(2)
        if ( (caracter >= 'A' .and. caracter <= 'Z') .or. (caracter >= 'a' .and. caracter <= 'z') .or. (caracter >= '0' .and. caracter <= '9' ) ) then
            tokenAuxiliar = trim(tokenAuxiliar) // caracter
            columna = columna + 1
            puntero = puntero + 1
            
        else
            if ((tokenAuxiliar == 'Contenedor')) then
                call crearToken(tokenAuxiliar, 'tk_contenedor', fila, columna)

            elseif ((tokenAuxiliar == 'Etiqueta')) then
                call crearToken(tokenAuxiliar, 'tk_etiqueta', fila, columna)
            
            elseif ((tokenAuxiliar == 'Boton')) then
                call crearToken(tokenAuxiliar, 'tk_boton', fila, columna)

            elseif((tokenAuxiliar == 'Texto'))then
                call crearToken(tokenAuxiliar, 'tk_texto', fila, columna)

            elseif((tokenAuxiliar == 'Clave'))then
                call crearToken(tokenAuxiliar, 'tk_clave', fila, columna)

            elseif((tokenAuxiliar == 'Check'))then
                call crearToken(tokenAuxiliar, 'tk_check', fila, columna)

            elseif((tokenAuxiliar == 'RadioBoton'))then
                call crearToken(tokenAuxiliar, 'tk_radioB', fila, columna)

            elseif((tokenAuxiliar == 'AreaTexto'))then
                call crearToken(tokenAuxiliar, 'tk_area', fila, columna)
            
            elseif ((tokenAuxiliar == 'setAncho')) then
                call crearToken(tokenAuxiliar, 'tk_setAncho', fila, columna)
            
            elseif ((tokenAuxiliar == 'setAlto')) then
                call crearToken(tokenAuxiliar, 'tk_setAlto', fila, columna)
            
            elseif ((tokenAuxiliar == 'setColorFondo')) then
                call crearToken(tokenAuxiliar, 'tk_setColorFondo', fila, columna)

            elseif ((tokenAuxiliar == 'setColorLetra')) then
                call crearToken(tokenAuxiliar, 'tk_setColorLetra', fila, columna)
            
            elseif ((tokenAuxiliar == 'setTexto')) then
                call crearToken(tokenAuxiliar, 'tk_setTexto', fila, columna)

            elseif ((tokenAuxiliar == 'setPosicion')) then
                call crearToken(tokenAuxiliar, 'tk_setPosicion', fila, columna)
            
            elseif (tokenAuxiliar == 'this') then
                call crearToken(tokenAuxiliar, 'tk_this', fila, columna)
            
            elseif (tokenAuxiliar == 'add') then
                call crearToken(tokenAuxiliar, 'tk_add', fila, columna)

            else 
                call crearToken(tokenAuxiliar, 'tk_id', fila, columna)

            end if

            tokenAuxiliar = ""
            estado = 0      
                
        end if

    case(3)
        if(caracter >= '0' .and. caracter <= '9') then
            tokenAuxiliar = trim(tokenAuxiliar) //caracter
            columna = columna + 1 
            puntero = puntero + 1
        else
            call crearToken(tokenAuxiliar, 'tk_numero', fila, columna)
            tokenAuxiliar = ""
            estado = 0
        end if

    case(4) 
        if(ichar(caracter)>= 0 .and. ichar(caracter) <= 255 .and. caracter .ne. '"')then
            tokenAuxiliar = trim(tokenAuxiliar) // caracter
            columna = columna + 1
            puntero = puntero + 1
            estado = 6

        else if(caracter == '"') then
            estado = 5
        else 
            call agregar_errorLex('Lexico', tokenAuxiliar, 'Error Lexico', fila, columna)

            tokenAuxiliar = ""
            estado = 0
        end if

    case(5)
        tokenAuxiliar =  trim(tokenAuxiliar) // caracter
        columna = columna + 1
        puntero = puntero + 1

        call crearToken(tokenAuxiliar, 'tk_literal', fila, columna)
        tokenAuxiliar = ""
        estado = 0

    case(6)
        if(ichar(caracter) >= 0 .and. ichar(caracter) <= 255 .and. caracter .ne. '"')then
            tokenAuxiliar = trim(tokenAuxiliar) //caracter
            columna = columna + 1
            puntero = puntero + 1

        else if(caracter == '"') then
            estado = 5
        else 
            call agregar_errorLex('Lexico', tokenAuxiliar, 'Error Lexico', fila, columna)
   
            tokenAuxiliar = ""
            estado = 0
        end if

    case(7)
        !Estado comentario linea
        if(ichar(caracter)==10)then
            estado = 0
            columna = 0
            fila = fila + 1

        else
            !ignorar caracteres dentro 
            puntero = puntero + 1
            columna = columna + 1
        end if

    case(8)
        !Estado comentario bloque
        if(caracter == '*' .and. contenido(puntero+1:puntero+1) == '/')then
            estado = 0
            puntero = puntero + 2
            columna = columna + 2
        else
            !Ignora caracteres dentro
            if(ichar(caracter) == 10)then
                fila = fila + 1
                columna = 0

            end if
            puntero = puntero + 1
            columna = columna + 1
        end if

    end select
end do

call parser
    
call imprimir_errores

call imprimir_tokens

call imprimir_etiquetas

call archivoErrores

call etiquetas_html

call contenedores_html

call areas_html

call radios_html

call checks_html

call botones_html

call textos_html

call claves_html

call etiquetas_css

call contenedores_css

call radios_css

call checks_css

call botones_css

call textos_css

call claves_css

call generar_html

contains

subroutine generar_html()
    use errormodulo
    implicit none
    integer :: unidad_html, ios_html
    logical :: hay_errores_lexicos, hay_errores_sintacticos
    character(len=20) :: errorMsg

    ! Inicializar las variables de error
    hay_errores_lexicos = .FALSE.
    hay_errores_sintacticos = .FALSE.

    ! Llamar a las subrutinas que verifican errores léxicos y sintácticos
    if (hay_errores_lexicos) then
        errorMsg = 'Lexico error'
        call agregar_error(errorMsg, '', '', 0, 0)
    endif

    if (hay_errores_sintacticos) then
        errorMsg = 'Sintactico error'
        call agregar_error(errorMsg, '', '', 0, 0)
    endif

    ! Si hay errores, no generar el archivo HTML ni la tabla de tokens
    if (hay_errores_lexicos .OR. hay_errores_sintacticos) then
        print *, 'Errores encontrados, no se puede crear el html.'
        ! Asegúrate de que el archivo HTML esté en blanco
        unidad_html = 30
        open(unit=unidad_html, file='./Proyecto2/resultado.html', status='replace', action='write', iostat=ios_html)
        if (ios_html /= 0) then
            print *, 'Error al abrir el archivo HTML'
            stop
        end if
        close(unidad_html)  ! Cierra el archivo sin escribir nada
        return
    end if

    ! Si no hay errores, procede a generar el archivo HTML
    unidad_html = 30
    open(unit=unidad_html, file='./Proyecto2/resultado.html', status='replace', action='write', iostat=ios_html)
    
    if (ios_html /= 0) then
        print *, 'Error al abrir el archivo HTML'
        stop
    end if
    
    write(unidad_html, '(A)') '<!DOCTYPE html>'
    write(unidad_html, '(A)') '<html>'
    write(unidad_html, '(A)') '<head>'
    write(unidad_html, '(A)') '<title>Resultado</title>'
    write(unidad_html, '(A)') '<link rel="stylesheet" type="text/css" href="./Proyecto2/estilos.css">'
    write(unidad_html, '(A)') '</head>'
    write(unidad_html, '(A)') '<body>'
    
    ! Llamar a las subrutinas para agregar contenido HTML
    call agregar_contenedor(unidad_html)
    call agregar_etiquetas(unidad_html)
    call agregar_area(unidad_html)
    call agregar_radio(unidad_html)
    call agregar_check(unidad_html)
    call agregar_boton(unidad_html) 
    call agregar_texto(unidad_html)
    call agregar_clave(unidad_html)
    
    write(unidad_html, '(A)') '</body>'
    write(unidad_html, '(A)') '</html>'
    
    close(unidad_html)
end subroutine generar_html

subroutine agregar_etiquetas(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_etiquetas, unidad_lectura_etiquetas
    character(len=1000) :: html_line_etiqueta

    unidad_lectura_etiquetas = 31
    open(unit=unidad_lectura_etiquetas, file='./Proyecto2/etiquetas.html', status='old', action='read', iostat=ios_etiquetas)

    if (ios_etiquetas /= 0) then
        print *, 'Error al abrir el archivo etiquetas.html'
        stop
    end if

    do
        read(unidad_lectura_etiquetas, '(A)', iostat=ios_etiquetas) html_line_etiqueta
        if (ios_etiquetas /= 0) exit
        write(unidad_html, '(A)') trim(html_line_etiqueta)
    end do

    close(unidad_lectura_etiquetas)
end subroutine agregar_etiquetas

subroutine agregar_contenedor(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_contenedores, unidad_lectura_contenedores
    character(len=1000) :: html_line_contenedor

    unidad_lectura_contenedores = 31
    open(unit=unidad_lectura_contenedores, file='./Proyecto2/contenedores.html', status='old', action='read', iostat=ios_contenedores)

    if (ios_contenedores /= 0) then
        print *, 'Error al abrir el archivo contenedores.html'
        stop
    end if

    do
        read(unidad_lectura_contenedores, '(A)', iostat=ios_contenedores) html_line_contenedor
        if (ios_contenedores /= 0) exit 
        write(unidad_html, '(A)') trim(html_line_contenedor)
    end do

    close(unidad_lectura_contenedores)
end subroutine agregar_contenedor

subroutine agregar_area(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_areas, unidad_lectura_areas
    character(len=1000) :: html_line_area

    unidad_lectura_areas = 31
    open(unit=unidad_lectura_areas, file='./Proyecto2/areas.html', status='old', action='read', iostat=ios_areas)

    if (ios_areas /= 0) then
        print *, 'Error al abrir el archivo areas.html'
        stop
    end if

    do
        read(unidad_lectura_areas, '(A)', iostat=ios_areas) html_line_area
        if (ios_areas /= 0) exit  
        write(unidad_html, '(A)') trim(html_line_area)
    end do

    close(unidad_lectura_areas)
end subroutine agregar_area

subroutine agregar_radio(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_radios, unidad_lectura_radios
    character(len=1000) :: html_line_radio

    unidad_lectura_radios = 31
    open(unit=unidad_lectura_radios, file='./Proyecto2/radios.html', status='old', action='read', iostat=ios_radios)

    if (ios_radios /= 0) then
        print *, 'Error al abrir el archivo radios.html'
        stop
    end if

    do
        read(unidad_lectura_radios, '(A)', iostat=ios_radios) html_line_radio
        if (ios_radios /= 0) exit 
        write(unidad_html, '(A)') trim(html_line_radio)
    end do

    close(unidad_lectura_radios)
end subroutine agregar_radio

subroutine agregar_check(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_checks, unidad_lectura_checks
    character(len=1000) :: html_line_check

    unidad_lectura_checks = 31
    open(unit=unidad_lectura_checks, file='./Proyecto2/checks.html', status='old', action='read', iostat=ios_checks)

    if (ios_checks /= 0) then
        print *, 'Error al abrir el archivo checks.html'
        stop
    end if

    do
        read(unidad_lectura_checks, '(A)', iostat=ios_checks) html_line_check
        if (ios_checks /= 0) exit 
        write(unidad_html, '(A)') trim(html_line_check)
    end do

    close(unidad_lectura_checks)
end subroutine agregar_check

subroutine agregar_boton(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_botones, unidad_lectura_botones
    character(len=1000) :: html_line_boton

    unidad_lectura_botones = 31
    open(unit=unidad_lectura_botones, file='./Proyecto2/botones.html', status='old', action='read', iostat=ios_botones)

    if (ios_botones /= 0) then
        print *, 'Error al abrir el archivo botones.html'
        stop
    end if

    do
        read(unidad_lectura_botones, '(A)', iostat=ios_botones) html_line_boton
        if (ios_botones /= 0) exit  
        write(unidad_html, '(A)') trim(html_line_boton)
    end do

    close(unidad_lectura_botones)
end subroutine agregar_boton

subroutine agregar_texto(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_textos, unidad_lectura_textos
    character(len=1000) :: html_line_texto

    unidad_lectura_textos = 31
    open(unit=unidad_lectura_textos, file='./Proyecto2/textos.html', status='old', action='read', iostat=ios_textos)

    if (ios_textos /= 0) then
        print *, 'Error al abrir el archivo textos.html'
        stop
    end if

    do
        read(unidad_lectura_textos, '(A)', iostat=ios_textos) html_line_texto
        if (ios_textos /= 0) exit 
        write(unidad_html, '(A)') trim(html_line_texto)
    end do

    close(unidad_lectura_textos)
end subroutine agregar_texto

subroutine agregar_clave(unidad_html)
    implicit none
    integer, intent(in) :: unidad_html
    integer :: ios_claves, unidad_lectura_claves
    character(len=1000) :: html_line_clave

    unidad_lectura_claves = 31
    open(unit=unidad_lectura_claves, file='./Proyecto2/claves.html', status='old', action='read', iostat=ios_claves)

    if (ios_claves /= 0) then
        print *, 'Error al abrir el archivo claves.html'
        stop
    end if

    do
        read(unidad_lectura_claves, '(A)', iostat=ios_claves) html_line_clave
        if (ios_claves /= 0) exit 
        write(unidad_html, '(A)') trim(html_line_clave)
    end do

    close(unidad_lectura_claves)
end subroutine agregar_clave


end program analizador_lexico