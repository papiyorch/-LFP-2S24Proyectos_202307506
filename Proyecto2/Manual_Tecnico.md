# Manual Técnico

**Descripción Breve:** Este proyecto consiste en la implemntación de un analizador léxico y sintáctico, el cual se encargará de analizar una entrada de texto, con extensión LFP. Este archivo será analizado a travez de una interfaz gráfica hecha con Tkinter de Python y mostrará en tablas separadas, los tokens y los posibles errores que recolecte, a su vez, generará un archivo html y uno css para levantar una página. 

### Analizar
Dentro del programa existe una subrutina llamada Parse, la cual, se encargará de verificar si la entrada de texto viene en un orden especifico:

```Fortran
subroutine Parser()

contlogin.setAncho(190); 
```

Ese es un ejemplo de una propiedad que viene en el archivo de entrada, si por alguna razón esa propiedad no se encuentre en ese orden se marcaría como un error sintáctico.

De igual forma en el programa principal, se crea el analizador léxico, el cual nos permite verificar si cada token pertenece a nuestro lenguaje, en caos no pertenezca, este será considerado como un error léxico. A continuación se muestra un ejemplo de un error léxico:


```Fortran
Controles ¡--> // Error lexico: '¡' no pertenece al lenguaje
```
### Generación Archivo HTML
Una vez, se haya logrado analizar nuestra entrada de texto, nuestro programa llamará a una subrutina llamada `generar_html`, la cual verificará si existe un archivo html para nuestro programa y en caso no exista, esta creará uno donde irán todas las etiquetas transformadas desde nuestra entrada de texto. 

De igual forma, cada modulo tiene su propia subrutina que le permite transformar las etiquetas, contenedores, botones, etc., en formato html para posteriormente juntar todos los archivos html en la subrutina `generar_html`.


Aqui se pueden apreciar dos subrutinas, una es la que se encargará de juntar todos los archivos html creados en cada modulo la cual seria `generar_html` y la otra es la subrutina del módulo `moduloEtiqueta` que nos creará un archivo html con las etiquetas ya transformadas. 

```Fortran
subroutine generar_html()
subroutine etiquetas_html()
```

### Generación Archivo CSS
De igual forma que el punto anterio, el programa crea un archivo css con el nombre `estilos_css`, el cual contendrá todas las propiedades y colocaciones de nuestras etiquetas, botones, areas de texto, etc.

Ejemplo de una propiedad: 

```Fortran
contFondo.setColorFondo(64,64,64);
```
Ejemplo de una propiedad en formato css, en este caso de la Etiqueta passw:

```Fortran
#passw{
    position: absolute;
    left: 11px;
    top: 62px;
    color: rgb(128,128,128);
}
```
### Conexión Fortran - Python
Para realizar la conexión, se utilizó la librería `subprocess` de Python, el cual permite ejecutar un subproceso. Lo que se hizo es ejecutar el .exe del Main de Fortran y enviarle todo lo capturando en el input de la GUI de Tkinter. El método responsable de este proceso es el `def ejecutar_fortran(contenido)`, a continuación se detalla la parte importante de esa funcion:

```Python
proceso = subprocess.Popen(["./Proyecto2/main.exe"], stdin =subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stoudt, stderr = proceso.communicate(input=contenido)
```
### AFD para el proyecto 
![AFD](./Proyecto2/AFD/AFD1.png)
![AFD](./Proyecto2/AFD/AFD2.png)