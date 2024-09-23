module moduloInformacion
    use moduloAnalizador
    use moduloPais
    use moduloContinente

    implicit none
    
    type :: Informacion
        type(Token) :: tokens(200)
        character(len=:), allocatable :: grafico
        type(Continente) :: continentes(100)
        integer :: iContinente = 1
    
        contains
            procedure :: crearInfo
            procedure :: iteracionTK
            procedure :: graficar

     end type Informacion

    contains

    subroutine crearInfo(this, tokens)
        class(Informacion), intent(inout) :: this
        type(Token), intent(in) :: tokens(200)

        this%tokens = tokens
        
    end subroutine crearInfo

    subroutine iteracionTK(this)
        class(Informacion), intent(inout) :: this
        integer :: i
        type(Continente) :: continente_actual
        type(Pais) :: pais_actual
        logical :: dentroContinente = .false.
        logical  :: dentroPais = .false.
        character(len=:), allocatable :: nombrePais, bandera
        integer :: poblacion, saturacion

        do i =1, 200
            if(this%tokens(i)%lexema=="grafica") then
                this%grafico = this%tokens(i+5)%lexema

            else if(this%tokens(i)%lexema == "continente") then
                continente_actual = Continente(this%tokens(i+5)%lexema)
                dentroContinente = .true.

            else if (this%tokens(i)%lexema=="pais") then
                dentroPais = .true.

            else if(this%tokens(i)%lexema=="nombre" .and. dentroPais)then
                nombrePais = this%tokens(i+2)%lexema

            else if(this%tokens(i)%lexema=="bandera" .and. dentroPais)then
                bandera = this%tokens(i+2)%lexema

            else if(this%tokens(i)%lexema =="poblacion" .and. dentroPais)then
                poblacion = this%tokens(i+2)%linea

            else if(this%tokens(i)%lexema =="saturacion" .and. dentroPais)then
                saturacion =  this%tokens(i+2)%linea

            else if(this%tokens(i)%lexema=="}" .and. dentroPais) then
                pais_actual = Pais(nombrePais, bandera, poblacion, saturacion)
                dentroPais = .false.

                continente_actual%paises(continente_actual%iPaises) = pais_actual
                continente_actual%iPaises = continente_actual%iPaises + 1

            else if(this%tokens(i)%lexema=="}" .and. dentroContinente)then
                this%continentes(this%iContinente) = continente_actual
                this%iContinente = this%iContinente + 1
                dentroContinente = .false.
            end if
        end do 
        
        call this%graficar(this%continentes, this%grafico)
    end subroutine iteracionTK

    subroutine graficar(this, continentes, grafico)
        class(Informacion), intent(inout) :: this
        type(Continente), intent(in) :: continentes(100)
        character(len=*),  intent(in) :: grafico
        integer :: unit = 15
        integer :: i,j
        character(len=:), allocatable :: nombreSinComillas
        character(len=:), allocatable :: color
        integer :: promedio = 0
        integer :: sumaPaises = 0
        integer :: numeroPaises = 0

        open(unit = unit, file="grafica.dot", status='replace', action='write')
        write(unit, '(A)') 'diagraph G {'
        write(unit, '(A)') grafico // '[shape=Mdiamond]'
        
        do i = 1, this%iContinente -1
            numeroPaises = 0
            sumaPaises = 0
            promedio = 0
            write(unit, '(A)') grafico// '->'// continentes(i)%nombre

            do j = 1, continentes(i)%iPaises - 1
                numeroPaises = numeroPaises + 1
                sumaPaises = sumaPaises + continentes(i)%paises(j)%saturacion

                nombreSinComillas = continentes(i)%paises(j)%nombre(2:len_trim(continentes(i)%paises(j)%nombre)-1)
                if(continentes(i)%paises(j)%saturacion > 20) then
                    color = "green"
                else
                    color = "red"
                end if

                write(unit, '(A, A, A, A, I0, A, A, A, A)', advance="no") & 
                continentes(i)%paises(j)%nombre , '[style=filled shape=box label="' , &
                nombreSinComillas , ' \n ' , &
                continentes(i)%paises(j)%saturacion , &
                '%"]', '[fillcolor=' , color , ']'

                write(unit, '(A)') continentes(i)%nombre 
                write(unit, '(A)') '->'
                write(unit, '(A)') continentes(i)%paises(j)%nombre
            end do

            promedio = sumaPaises / numeroPaises
            nombreSinComillas = continentes(i)%nombre(2:len_trim(continentes(i)%nombre)-1)
            if(promedio > 20) then
                color = "green"
            else
                color = "red"
            end if

            write(unit, '(A, A, A, A, I0, A, A, A, A)', advance="no") & 
            continentes(i)%nombre , '[style=filled shape=box label="' , &
            nombreSinComillas , ' \n ' , &
            promedio , &
            '%"]', '[fillcolor=' , color , ']'
            
        end do

        write(unit, '(A)') '}'
        close(unit)

        call system('dot -Tpng grafica.dot -o grafica.png')
    end subroutine graficar

    end module moduloInformacion