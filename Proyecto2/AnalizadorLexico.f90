program analizador_lexico
    use moduloToken
    use errormodulo
    use moduloEtiqueta

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
            if(caracter == ';' .or. caracter == '-' .or. caracter == '.' .or. caracter == '(' .or. caracter == ')' .or. caracter == ',' .or. caracter == '<' .or. caracter == '>' .or. caracter == '!')then
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

            else if(ichar(caracter) ==10)then
                columna = 0
                fila = fila + 1
                puntero = puntero + 1

            elseif (ichar(caracter) == 9) then
                columna = columna + 4
                puntero = puntero + 1

            elseif (ichar(caracter) == 32) then
                columna = columna + 1
                puntero = puntero + 1  
            
            else
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
            call crearToken(tokenAuxiliar, 'tk_num', fila, columna)
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
            tokenAuxiliar = ""
            estado = 0
        end if

    end select
end do

call parser
    
call imprimir_errores

call imprimir_tokens

call imprimir_etiquetas

end program analizador_lexico