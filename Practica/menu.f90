module Definiciones
    implicit none
    type Equipo
        character(len=100) :: nombre      ! Nombre del equipo
        integer :: cantidad               ! Cantidad de unidades en inventario
        real :: precio_unitario           ! Precio unitario del equipo
        character(len=100) :: ubicacion  ! Ubicación del equipo en el inventario
    end type Equipo
end module Definiciones

module LecturaArchivo
    use Definiciones, only: Equipo
    implicit none

    contains

    subroutine leer_archivo_inv(archivo, inventario, num_items)
        implicit none
        character(len=100), intent(in) :: archivo
        type(Equipo), allocatable, intent(out) :: inventario(:)
        integer, intent(out) :: num_items
        character(len=200) :: linea
        integer :: iostat, i
        character(len=100) :: nombre, ubicacion
        integer :: cantidad
        real :: precio_unitario

        num_items = 0

        open(unit=20, file=archivo, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo o el archivo no existe: ", archivo
            return
        end if

        ! Contar el número de líneas válidas y mostrar el contenido
        do
            read(20, '(A)', iostat=iostat) linea
            if (iostat /= 0) exit
            print *, trim(linea)  ! Imprimir cada línea del archivo
            if (index(linea, "crear_equipo") /= 0) num_items = num_items + 1
        end do

        ! Volver al principio del archivo
        rewind(20)

        ! Alocar el inventario
        if (num_items > 0) then
            allocate(inventario(num_items))
        else
            print *, "No se encontraron equipos en el archivo."
            close(20)
            return
        end if

        ! Leer y guardar los datos
        i = 0
        do
            read(20, '(A)', iostat=iostat) linea
            if (iostat /= 0) exit
            if (index(linea, "crear_equipo") /= 0) then
                i = i + 1
                ! Eliminar "crear_equipo" y los punto y coma
                linea = trim(adjustl(linea(index(linea, "crear_equipo") + len("crear_equipo"):)))
                linea = trim(adjustl(replace_semicolon(linea)))
                read(linea, *, iostat=iostat) nombre, cantidad, precio_unitario, ubicacion
                ! print *, trim(linea) 
                if (iostat /= 0) then
                    cycle
                end if
                inventario(i)%nombre = trim(adjustl(nombre))
                inventario(i)%cantidad = cantidad
                inventario(i)%precio_unitario = precio_unitario
                inventario(i)%ubicacion = trim(adjustl(ubicacion))
            end if
        end do

        close(20)
        num_items = i
    end subroutine leer_archivo_inv

    function replace_semicolon(str) result(new_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: new_str
        integer :: i, j

        j = 1
        do i = 1, len_trim(str)
            if (str(i:i) /= ';') then
                new_str(j:j) = str(i:i)
                j = j + 1
            else
                new_str(j:j) = ' '
                j = j + 1
            end if
        end do
        new_str = trim(new_str)
    end function replace_semicolon

end module LecturaArchivo

module ProcesarInstrucciones
    use Definiciones, only: Equipo
    implicit none

    contains

    function limpiar_cadena(cadena) result(cadena_limpia)
        implicit none
        character(len=*), intent(in) :: cadena
        character(len=7) :: cadena_limpia
    
        ! Copiar los primeros 7 caracteres de la cadena original
        if (len_trim(cadena) >= 7) then
            cadena_limpia = cadena(1:7)
        else
            cadena_limpia = cadena(1:len_trim(cadena))
        end if
    end function limpiar_cadena

    subroutine leer_instrucciones(archivo, inventario, num_items)
        implicit none
        character(len=100), intent(in) :: archivo
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        integer, intent(inout) :: num_items
        character(len=256) :: linea, comando, nombre, ubicacion
        integer :: iostat, pos, cantidad

        open(unit=22, file=archivo, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo o el archivo no existe: ", archivo
            return
        end if

        do
            read(22, '(A)', iostat=iostat) linea
            if (iostat /= 0) exit
            linea = trim(linea)

            ! Encuentra el primer espacio para extraer el comando
            pos = index(linea, ' ')
            if (pos > 0) then
                comando = linea(1:pos-1)
                linea = trim(linea(pos+1:))

                ! Separar por ';'
                pos = index(linea, ';')
                if (pos > 0) then
                    nombre = linea(1:pos-1)
                    linea = trim(linea(pos+1:))

                    pos = index(linea, ';')
                    if (pos > 0) then
                        read(linea(1:pos-1), *, iostat=iostat) cantidad
                        ubicacion = trim(linea(pos+1:))

                        if (comando == "agregar_stock") then
                            call agregar_stock(inventario, num_items, nombre, cantidad, ubicacion)
                        else if (comando == "eliminar_equipo") then
                            call eliminar_equipo(inventario, num_items, nombre, cantidad, ubicacion)
                        end if
                    end if
                end if
            end if
        end do

        close(22)
    end subroutine leer_instrucciones

    subroutine agregar_stock(inventario, num_items, nombre, cantidad, ubicacion)
        implicit none
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        integer, intent(inout) :: num_items
        character(len=100), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        integer :: i, encontrado

        encontrado = 0
        do i = 1, num_items
            if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(limpiar_cadena(inventario(i)%ubicacion)) == trim(ubicacion)) then
                inventario(i)%cantidad = inventario(i)%cantidad + cantidad
                encontrado = 1
                print *, "Stock actualizado"
                print *, ""
                exit
            end if
        end do

        if (encontrado == 0) then
            print *, "Error: No se encontro el equipo: ", trim(nombre), " en la ubicacion: ", trim(ubicacion)
        end if
    end subroutine agregar_stock

    subroutine eliminar_equipo(inventario, num_items, nombre, cantidad, ubicacion)
        implicit none
        type(Equipo), allocatable, intent(inout) :: inventario(:)
        integer, intent(inout) :: num_items
        character(len=100), intent(in) :: nombre, ubicacion
        integer, intent(in) :: cantidad
        integer :: i, encontrado

        encontrado = 0
        do i = 1, num_items
            if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(limpiar_cadena(inventario(i)%ubicacion)) == trim(ubicacion)) then
                if (inventario(i)%cantidad >= cantidad) then
                    inventario(i)%cantidad = inventario(i)%cantidad - cantidad
                    encontrado = 1
                    print *, "Cantidad eliminada"
                    if (inventario(i)%cantidad == 0) then
                        print *, "El equipo se ha agotado"
                    end if
                else
                    print *, "Error: La cantidad a eliminar es mayor que la cantidad disponible en el equipo: ", trim(nombre), " en la ubicacion: ", trim(ubicacion)
                end if
                print *, ""
                exit
            end if
        end do

        if (encontrado == 0) then
            print *, "Error: No se encontro el equipo: ", trim(nombre), " en la ubicacion: ", trim(ubicacion)
        end if
    end subroutine eliminar_equipo

end module ProcesarInstrucciones

module CrearInforme
    use Definiciones, only: Equipo
    implicit none

    contains

    subroutine crear_informe(inventario, num_items)
        implicit none
        type(Equipo), intent(in) :: inventario(:)
        integer, intent(in) :: num_items
        integer :: i
        real :: valor_total
        integer :: unidad_archivo, iostat
        character(len=100) :: archivo = 'informe.txt'
    
        ! Imprimir el encabezado del informe en pantalla
        print *, "Informe de Inventario:"
        print *, ""
        print *, "Equipo         Cantidad  Precio Unitario    Valor Total  Ubicacion"
        print *, "------------------------------------------------------------------"
    
        ! Abrir el archivo para escribir el informe
        open(unit=unidad_archivo, file=archivo, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo para escribir el informe."
            return
        end if
    
        ! Imprimir el encabezado del informe en el archivo
        write(unidad_archivo, '(A22)') "Informe de Inventario:"
        write(unidad_archivo, *) ""
        write(unidad_archivo, '(A15, A11, A18, A15, A14)') "Equipo", "Cantidad", "Precio Unitario", "Valor Total", "Ubicación"
        write(unidad_archivo, *) "--------------------------------------------------------------------------"
    
        ! Imprimir cada item del inventario en pantalla y en el archivo
        do i = 1, num_items
            valor_total = inventario(i)%cantidad * inventario(i)%precio_unitario
            print '(A13, 3X, I7, 4X, A1, F11.2, 4X, A1, F10.2, 4X, A7)', &
                  trim(adjustl(inventario(i)%nombre)), &
                  inventario(i)%cantidad, &
                  'Q', inventario(i)%precio_unitario, &
                  'Q', valor_total, &
                  trim(adjustl(inventario(i)%ubicacion))
            write(unidad_archivo, '(A15, 4X, I7, 5X, A1, F12.2, 4X, A1, F10.2, 7X, A7)') &
                  trim(adjustl(inventario(i)%nombre)), &
                  inventario(i)%cantidad, &
                  'Q', inventario(i)%precio_unitario, &
                  'Q', valor_total, &
                  trim(adjustl(inventario(i)%ubicacion))
        end do
    
        ! Cerrar el archivo
        close(unidad_archivo)
    
        print *, "Informe de inventario creado y exportado a ", archivo, "...."
        print *, ""
    end subroutine crear_informe

end module CrearInforme

program Menu
    use Definiciones, only: Equipo
    use LecturaArchivo, only: leer_archivo_inv
    use CrearInforme, only: crear_informe
    use ProcesarInstrucciones, only: leer_instrucciones
    implicit none
    integer :: opcion
    character(len=100) :: archivo
    character(len=100) :: Instrucciones
    type(Equipo), allocatable :: inventario(:)
    integer :: num_items

    archivo = 'inventario.inv'
    Instrucciones = 'instrucciones.mov'

    print *, "------------------------------------------------"
    print *, "Practica 1 - Lenguajes formales y de programacion"
    print *, "------------------------------------------------"

    do 
        print *, "# Sistema de inventario:"
        print *, ""
        print *, "1. Cargar Inventario inicial"
        print *, "2. Cargar Instrucciones de movimientos"
        print *, "3. Cargar Informe de inventario"
        print *, "4. Salir"
        print *, ""
        print *, "Ingresa una opcion: "
        read (*,*) opcion

        if (opcion == 4) then
            print *, "Ha salido del sistema de inventario"
            exit 
        else if (opcion == 1) then
            print *, ""
            call leer_archivo_inv(archivo, inventario, num_items)
            print *, "Inventario cargado con exito."
            print *, ""
        else if (opcion == 2) then
            print *, ""
            call leer_instrucciones(Instrucciones, inventario, num_items)
            print *, ""
        else if (opcion == 3) then
            print *, ""
            if (allocated(inventario)) then  ! Verifica si el inventario está cargado
                call crear_informe(inventario, num_items)
                print *, "Informe de inventario creado con exito."
                print *,""
            else
                print *, "Error: No se ha cargado el inventario."
            end if
            print *, ""
        else
            print *, "Opción no válida, por favor elige una opción entre 1 y 4."
        end if
    end do
end program Menu