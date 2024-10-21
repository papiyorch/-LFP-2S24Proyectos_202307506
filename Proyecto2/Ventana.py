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
        actualizar_Tablas(stoudt)

    #print(salida)

def actualizar_Tablas(salida):
    tokens = salida.strip().split('\n')
    tablaTokens.delete(*tablaTokens.get_children())

    for token in tokens:
        datos_token = token.split(',')
        if len(datos_token) == 4:
            tablaTokens.insert('', 'end', values=datos_token)

    errores = salida.strip().split('\n')
    tablaErrores.delete(*tablaErrores.get_children())

    for error in errores:
        datos_error = error.split(',')
        if len(datos_token) == 5:
            tablaErrores.insert('','end',values=datos_error)

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
archivo_menu.add_command(label="Guardar", command="guardar_archivo")
archivo_menu.add_command(label="Guardar como", command="guardar_como")
archivo_menu.add_separator()
archivo_menu.add_command(label="Salir", command=ventana.quit)

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

columnasT2 = ("Lexema", "Tipo", "Fila", "Columna", "Descripcion")
tablaErrores = ttk.Treeview(tab2, columns=columnasT2, show='headings', height=5)
for col in columnasT2:
    tablaErrores.heading(col, text=col)
    tablaErrores.column(col, width=100)
tablaErrores.pack(fill="both", expand=True)

#Diseño de tabla para tokens y errores
#frame_tabla = tk.Frame(ventana)
#frame_tabla.place(x=80, y=450, relwidth=0.9, relheight=0.4)

#scroll_tablaX = tk.Scrollbar(frame_tabla, orient=tk.HORIZONTAL)
#scroll_tablaX.pack(side=tk.BOTTOM, fill=tk.X)
#scroll_tablaY = tk.Scrollbar(frame_tabla, orient=tk.HORIZONTAL)
#scroll_tablaY.pack(side=tk.RIGHT, fill=tk.Y)

#tablaTokens =ttk.Treeview(frame_tabla, columns=("Lexema", "Tipo", "Fila", "Columna"), show="headings", height=5,
#                          xscrollcommand=scroll_tablaX.set, yscrollcommand=scroll_tablaY.set)
#tablaTokens.pack(fill=tk.BOTH, expand=True)

#scroll_tablaX.config(command=tablaTokens.xview)
#scroll_tablaY.config(command=tablaTokens.yview)

#tablaTokens.heading("Lexema", text="Lexema")
#tablaTokens.heading("Tipo", text="Tipo")
#tablaTokens.heading("Fila", text="Fila")
#tablaTokens.heading("Columna", text="Columna")

#tablaTokens.column("Lexema", width=200)
#tablaTokens.column("Tipo", width=120)
#tablaTokens.column("Fila", width=120)
#tablaTokens.column("Columna", width=120)


ventana.mainloop()