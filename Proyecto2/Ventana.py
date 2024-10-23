import tkinter as tk
from tkinter import filedialog, messagebox
from tkinter import ttk
import subprocess

def abrir_archivo():
    archivo = filedialog.askopenfilename(filetypes=[("Archivos LFP","*.LFP")])
    if archivo:
        with open(archivo, 'r') as file:
            texto.delete(1.0, tk.END)
            texto.insert(tk.END, file.read())
        ruta.set(archivo)

def analizar():
    contenido = texto.get(1.0, tk.END)
    if contenido.strip():
        ejecutar_fortran(contenido)

def ejecutar_fortran(contenido):
    proceso = subprocess.Popen(["./Proyecto2/main.exe"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stoudt, stderr = proceso.communicate(input=contenido)
    
    salida = stoudt.strip().split('\n')

    if proceso.returncode !=0:
        print(f"Error: {stderr}")
    else:
        actualizar_TablaT(stoudt)
        actualizar_TablaE()

    #print(salida)

def actualizar_TablaT(salida):
    tokens = salida.strip().split('\n')
    tablaTokens.delete(*tablaTokens.get_children())

    for token in tokens:
        datos_token = token.split(',')
        if len(datos_token) == 4:
            tablaTokens.insert('', 'end', values=datos_token)

def actualizar_TablaE():
    tablaErrores.delete(*tablaErrores.get_children())
    with open("./Proyecto2/errores.txt", "r") as file:
     for error in file:
        datos_error = error.strip().split(',')
        if len(datos_error) == 5:
            tablaErrores.insert('','end',values=datos_error)

def informacion():
    messagebox.showinfo("Acerca de", "Proyecto 2 \n Desarrollado por: Jorge Ivan Samayoa Sian \n Carnet: 202307506")

def guardar_archivo():
    archivo = ruta.get()
    if archivo:
        with open(archivo,'w')as file:
            file.write(texto.get(1.0, tk.END))
    ruta.set(archivo)

def guardar_como():
    archivo = filedialog.asksaveasfilename(defaultextension=".LFP", filetypes=[("Archivos LFP","*.LFP")])
    if archivo:
        with open(archivo, 'w') as file:
            file.write(texto.get(1.0, tk.END))
        ruta.set(archivo)

#Diseño de ventana
ventana = tk.Tk()
ventana.title("Analizador")
ventana.geometry("1200x800")
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

texto = tk.Text(ventana, wrap='word', height=20, width=100)
texto.place(x=50, y=40)

btnAnalizar = tk.Button(ventana, text="Analizar", command=analizar, height=3, width=15)
btnAnalizar.place(x=55,y=380)

tab_control = ttk.Notebook(ventana)
tab_control.place(x=80, y=450, relwidth=0.9, relheight=0.4)

tab1 = ttk.Frame(tab_control)
tab2 = ttk.Frame(tab_control)

tab_control.add(tab1, text="Tokens")
tab_control.add(tab2, text="Errores")

columnasT1 = ("Lexema", "Tipo", "Fila", "Columna")
tablaTokens = ttk.Treeview(tab1, columns=columnasT1, show='headings', height=5)
for col in columnasT1:
    tablaTokens.heading(col, text=col)
    tablaTokens.column(col, width=120)
tablaTokens.pack(fill="both", expand=True)

columnasT2 = ("Tipo", "Descripcion", "Token Esperado", "Fila", "Columna")
tablaErrores = ttk.Treeview(tab2, columns=columnasT2, show='headings', height=5)
for col in columnasT2:
    tablaErrores.heading(col, text=col)
    tablaErrores.column(col, width=100)
tablaErrores.pack(fill="both", expand=True)



ventana.mainloop()