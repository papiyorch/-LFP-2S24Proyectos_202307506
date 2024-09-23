import tkinter as tk
from tkinter import filedialog, messagebox
from PIL import Image, ImageTk
import webbrowser
import subprocess
import os

def abrir_archivo():
    archivo = filedialog.askopenfilename(filetypes=[("Archivos ORG","*.ORG")])
    if archivo:
        with open(archivo, 'r') as file:
            texto.delete(1.0, tk.END)
            texto.insert(tk.END, file.read())
        ruta.set(archivo)

def guardar_archivo():
    archivo = ruta.get()
    if archivo:
        with open(archivo,'w')as file:
            file.write(texto.get(1.0, tk.END))
    ruta.set(archivo)

def guardar_como():
    archivo = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos ORG","*.ORG")])
    if archivo:
        with open(archivo, 'w') as file:
            file.write(texto.get(1.0, tk.END))
        ruta.set(archivo)

def ejecutar_fortran(contenido):
    proceso = subprocess.Popen(["./Proyecto1/main.exe"], stdin =subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stoudt, stderr = proceso.communicate(input=contenido)

    nombre = ''
    poblacion = ''
    bandera = ''
    error = ''

    salida = stoudt.strip().split('\n')
    #print(stoudt)

    if proceso.returncode !=0:
        print(f"Error: {stderr}")
    else:
        print(stoudt)

    for linea in salida:
        if "pais:" in linea:
            nombre = linea.split(":")[1].strip()
        elif "poblacion:" in linea:
            poblacion = linea.split(":")[1].strip()
        elif "bandera:" in linea:
            bandera = linea.split(":")[1].strip()
        elif "error:" in linea:
            error = linea.split(":")[1].strip()

    actualizar_interfaz(nombre, poblacion, bandera, error)
    print(contenido)
    print(salida)

"""
def enviar_datos():
    texto.delete(1.0, tk.END)
    try: 
        subprocess.run(["gfortran", "-o","main.exe","main.f90"], check=True)
    except subprocess.CalledProcessError as e:
        print(f"Error en la compilacion: {e}")
        return
    try:
        resultado = subprocess.run(["main.exe"], input=data, stdout=subprocess.PIPE, text=True, check=True)
        texto.insert(tk.END, resultado.stdout)
    except subprocess.CalledProcessError as e:
        print(f"Errror al ejecutar main.exe {e}")
        return
        """

def actualizar_interfaz(nombre, poblacion, bandera, error):
    if error == 'no':
        if bandera != '':
            labelImagen = tk.Label(ventana)
            labelImagen.pack()
            cargar_bandera(bandera.replace('"', '').strip(), labelImagen)

        if poblacion != '':
            labelPoblacion.config(text="Poblacion:" + poblacion)

        if nombre !='':
            labelPais.config(text="Pais:"+ nombre)

            cargar_grafico("grafica.png")
            abrir_html('C:/Users/PC/Documents/LENGUAJES/LAB/fortran/tokens.html')
        else:
            abrir_html('C:/Users/PC/Documents/LENGUAJES/LAB/fortran/errores.html')
            labelPais.config(text="Pais: ")
            labelPoblacion.config(text="Poblacion: ")
            imagenGrafico.delete("all")

def abrir_html(archivo_html):
    archivo_html_link = 'file://' + archivo_html
    webbrowser.open(archivo_html_link)

def analizar():
    contenido = texto.get(1.0, tk.END)
    if contenido.strip():
        ejecutar_fortran(contenido)

def cargar_bandera(ruta_bandera, labelImagen):
    try:
        img = Image.open(ruta_bandera)
        img = img.resize((150, 100), Image.Resampling.LANCZOS)
        img_tk = ImageTk.PhotoImage(img)
        labelImagen.config(image=img_tk)
        labelImagen.image = img_tk  
        labelImagen.place(x=700, y=400)
    except FileNotFoundError:
        messagebox.showerror("Error", "No se encontró la imagen del país.")

def informacion():
    messagebox.showinfo("Acerca de", "Proyecto 1 \n Desarrollado por: Jorge Ivan Samayoa Sian \n Carnet: 202307506")

def cargar_grafico(archivo):
    try:
        imagen = Image.open(archivo)
        imagen = imagen.resize((700,300), Image.Resampling.LANCZOS)
        imagen_tk = ImageTk.PhotoImage(imagen)
        imagenGrafico.delete("all")
        imagenGrafico.create_image(0,0, anchor="nw", image=imagen_tk)
        imagenGrafico.image = imagen_tk
    except FileNotFoundError:
        messagebox.showerror("Error, no se encontró el archivo grafica")


#Ventana Principal
ventana = tk.Tk()
ventana.title("Analizador de Archivos")
ventana.geometry("1200x600")
ruta = tk.StringVar()

menu = tk.Menu(ventana)
ventana.config(menu=menu)

archivo_menu = tk.Menu(menu, tearoff=0)
menu.add_cascade(label="Archivo", menu=archivo_menu)
archivo_menu.add_command(label="Abrir", command=abrir_archivo)
archivo_menu.add_command(label="Guardar", command=guardar_archivo)
archivo_menu.add_command(label="Guardar como", command=guardar_como)
archivo_menu.add_separator()
archivo_menu.add_command(label="Salir", command=ventana.quit)

acercaDe = tk.Menu(menu, tearoff=0)
menu.add_cascade(label="Acerca de", menu=acercaDe)
acercaDe.add_command(label="Acerda de...", command=informacion)

texto = tk.Text(ventana, wrap='word', height=20, width=40)
texto.place(x=50, y=50)

btnAnalizar = tk.Button(ventana, text="Analizar", command=analizar)
btnAnalizar.place(x=50, y=450)

labelPais = tk.Label(ventana, text="País: ")
labelPais.place(x=450, y=350)

labelPoblacion = tk.Label(ventana, text="Población: ")
labelPoblacion.place(x=450, y=400)

imagenGrafico = tk.Canvas(ventana, width=700, height=300, bg="white")
imagenGrafico.place(x=450, y=50)

ventana.mainloop()
