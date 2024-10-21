module moduloToken
    use errormodulo
    use moduloEtiqueta
    implicit none

    type :: Token
        character(len =100) :: lexema
        character(len =200):: tipo
        integer :: fila, columna
        
    end type Token

    type(Token), allocatable :: arregloToken(:)
contains
subroutine crearToken(lexema, tipo, fila, columna)
    
        character(len =*), intent(in):: lexema
        character(len =*), intent(in):: tipo
        integer :: fila, columna, n
        type(Token) :: nuevoToken
        type(Token), allocatable :: tempArray(:)

        nuevoToken%lexema = lexema
        nuevoToken%tipo = tipo
        nuevoToken%fila  = fila
        nuevoToken%columna = columna

        if(.not. allocated(arregloToken)) then
            allocate(arregloToken(1))
            arregloToken(1) = nuevoToken
        else
            n = size(arregloToken)
            allocate(tempArray(n+1))
            tempArray(:n) = arregloToken
            tempArray(n+1) = nuevoToken
            deallocate(arregloToken)
            allocate(arregloToken(n+1))
            arregloToken = tempArray
        end if  
end subroutine crearToken

subroutine parser()

    integer :: i

   if (.not. allocated(arregloToken)) then
        print *, "No hay tokens"
   else
        do i = 1, size(arregloToken)
            if(arregloToken(i)%tipo == 'tk_etiqueta') then
                if(arregloToken(i+1)%tipo == 'tk_id') then
                else
                    call agregar_error(arregloToken(i+1)%lexema, 'tk_id', arregloToken(i+1)%fila, arregloToken(i+1)%columna)
                end if
            end if

            if(arregloToken(i)%tipo =='tk_id' .and. arregloToken(i+1)%tipo == 'tk_punto') then
                if(arregloToken(i+2)%tipo == 'tk_setAncho') then
                    if(arregloToken(i+3)%tipo .ne. 'tk_parentesisI') then
                    call agregar_error(arregloToken(i+3)%lexema, 'tk_parentesisI', arregloToken(i+3)%fila, arregloToken(i+3)%columna)

                    else if(arregloToken(i+4)%tipo .ne. 'tk_numero') then
                        call agregar_error(arregloToken(i+4)%lexema, 'tk_numero', arregloToken(i+4)%fila, arregloToken(i+4)%columna)

                    elseif(arregloToken(i+5)%tipo .ne. 'tk_parentesisD') then
                        call agregar_error(arregloToken(i+5)%lexema, 'tk_parentesisD', arregloToken(i+5)%fila, arregloToken(i+5)%columna)

                    else if(arregloToken(i+6)%tipo .ne. 'tk_puntoycoma') then
                        call agregar_error(arregloToken(i+6)%lexema, 'tk_puntoycoma', arregloToken(i+6)%fila, arregloToken(i+6)%columna)
                        
                    else
                        call etiquetaAncho(arregloToken(i)%lexema, arregloToken(i+4)%lexema)

                    end if
                end if

                if(arregloToken(i+2)%tipo == 'tk_setAlto')then
                    if(arregloToken(i+3)%tipo .ne. 'tk_parentesisI') then
                        call agregar_error(arregloToken(i+3)%lexema, 'tk_parentesisI', arregloToken(i+3)%fila, arregloToken(i+3)%columna)

                    else if(arregloToken(i+4)%tipo .ne. 'tk_numero') then
                        call agregar_error(arregloToken(i+4)%lexema, 'tk_numero', arregloToken(i+4)%fila, arregloToken(i+4)%columna)

                    elseif(arregloToken(i+5)%tipo .ne. 'tk_parentesisD') then
                        call agregar_error(arregloToken(i+5)%lexema, 'tk_parentesisD', arregloToken(i+5)%fila, arregloToken(i+5)%columna)

                    else if(arregloToken(i+6)%tipo .ne. 'tk_puntoycoma') then
                        call agregar_error(arregloToken(i+6)%lexema, 'tk_puntoycoma', arregloToken(i+6)%fila, arregloToken(i+6)%columna)
                        
                    else
                        call etiquetaAlto(arregloToken(i)%lexema, arregloToken(i+4)%lexema)

                    end if
                end if

                if(arregloToken(i+2)%tipo == 'tk_setTexto')then
                    if(arregloToken(i+3)%tipo .ne. 'tk_parentesisI') then
                        call agregar_error(arregloToken(i+3)%lexema, 'tk_parentesisI', arregloToken(i+3)%fila, arregloToken(i+3)%columna)

                    else if(arregloToken(i+4)%tipo .ne. 'tk_numero') then
                        call agregar_error(arregloToken(i+4)%lexema, 'tk_numero', arregloToken(i+4)%fila, arregloToken(i+4)%columna)

                    elseif(arregloToken(i+5)%tipo .ne. 'tk_parentesisD') then
                        call agregar_error(arregloToken(i+5)%lexema, 'tk_parentesisD', arregloToken(i+5)%fila, arregloToken(i+5)%columna)

                    else if(arregloToken(i+6)%tipo .ne. 'tk_puntoycoma') then
                        call agregar_error(arregloToken(i+6)%lexema, 'tk_puntoycoma', arregloToken(i+6)%fila, arregloToken(i+6)%columna)
                        
                    else
                        call etiquetaTexto(arregloToken(i)%lexema, arregloToken(i+4)%lexema)

                    end if
                end if

                if(arregloToken(i+2)%tipo == 'tk_setColorLetra')then
                    if(arregloToken(i+3)%tipo .ne. 'tk_parentesisI') then
                        call agregar_error(arregloToken(i+3)%lexema, 'tk_parentesisI', arregloToken(i+3)%fila, arregloToken(i+3)%columna )

                        elseif (arregloToken(i+4)%tipo .ne. 'tk_numero') then
                            call agregar_error(arregloToken(i+4)%lexema, 'tk_numero', arregloToken(i+4)%fila, arregloToken(i+4)%columna )
                        
                        elseif (arregloToken(i+5)%tipo .ne. 'tk_coma') then
                            call agregar_error(arregloToken(i+5)%lexema, 'tk_coma', arregloToken(i+5)%fila, arregloToken(i+5)%columna )

                        elseif (arregloToken(i+6)%tipo .ne. 'tk_numero') then
                            call agregar_error(arregloToken(i+6)%lexema, 'tk_numero', arregloToken(i+6)%fila, arregloToken(i+6)%columna )

                        elseif (arregloToken(i+7)%tipo .ne. 'tk_coma') then
                            call agregar_error(arregloToken(i+7)%lexema, 'tk_coma', arregloToken(i+7)%fila, arregloToken(i+7)%columna )

                        elseif (arregloToken(i+8)%tipo .ne. 'tk_numero') then
                            call agregar_error(arregloToken(i+8)%lexema, 'tk_numero', arregloToken(i+8)%fila, arregloToken(i+8)%columna )

                        elseif (arregloToken(i+9)%tipo .ne. 'tk_parentesisD') then
                            call agregar_error(arregloToken(i+9)%lexema, 'tk_parentesisD', arregloToken(i+9)%fila, arregloToken(i+9)%columna )

                        elseif (arregloToken(i+10)%tipo .ne. 'tk_puntoycoma') then
                            call agregar_error(arregloToken(i+10)%lexema, 'tk_puntoycoma', arregloToken(i+10)%fila, arregloToken(i+10)%columna )
                        
                        else
                            call etiquetaColorTexto(arregloToken(i)%lexema, arregloToken(i+4)%lexema, arregloToken(i+6)%lexema, arregloToken(i+8)%lexema )
                            
                        end if
                    end if

                    if(arregloToken(i+2)%tipo == 'tk_setPosicion') then
                        if(arregloToken(i+3)%tipo .ne. 'tk_parentesisI')then
                            call agregar_error(arregloToken(i+3)%lexema, 'tk_parentesisI', arregloToken(i+3)%fila, arregloToken(i+3)%columna )  
                            
                            elseif (arregloToken(i+4)%tipo .ne. 'tk_numero') then
                                call agregar_error(arregloToken(i+4)%lexema, 'tk_numero', arregloToken(i+4)%fila, arregloToken(i+4)%columna )
                            
                            elseif (arregloToken(i+5)%tipo .ne. 'tk_coma') then
                                call agregar_error(arregloToken(i+5)%lexema, 'tk_coma', arregloToken(i+5)%fila, arregloToken(i+5)%columna )
    
                            elseif (arregloToken(i+6)%tipo .ne. 'tk_numero') then
                                call agregar_error(arregloToken(i+6)%lexema, 'tk_numero', arregloToken(i+6)%fila, arregloToken(i+6)%columna )
    
                                elseif (arregloToken(i+7)%tipo .ne. 'tk_parentesisD') then
                                    call agregar_error(arregloToken(i+7)%lexema, 'tk_parentesisD', arregloToken(i+7)%fila, arregloToken(i+7)%columna )
        
                                elseif (arregloToken(i+8)%tipo .ne. 'tk_puntoycoma') then
                                    call agregar_error(arregloToken(i+8)%lexema, 'tk_puntoycoma', arregloToken(i+8)%fila, arregloToken(i+8)%columna )
                                
                                else
                                    call etiquetaPos(arregloToken(i)%lexema, arregloToken(i+4)%lexema, arregloToken(i+6)%lexema )
                                    
                            end if
                        end if

                    end if 
        end do
        
   end if

    
end subroutine parser

subroutine imprimir_tokens()
    integer :: i
    if (.not. allocated(arregloToken)) then
        print *, "No hay tokens"
    else
        do i = 1, size(arregloToken)
            print *, trim(arregloToken(i)%lexema), ',', trim(arregloToken(i)%tipo), ',', arregloToken(i)%fila, ',', arregloToken(i)%columna
        end do
    end if
end subroutine imprimir_tokens
end module moduloToken