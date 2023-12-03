'#Include "GL/glut.bi"
#Include "window9.bi"


enum 
	bt_convertir=0,
	bt_radios,
	bt_puntos,
	bt_zonas,
	bt_leernc,
	bt_turbo,
	bt_planta,
	bt_isomet,
	bt_lateral,
	bt_frente,
	bt_compensar,
	barraestado,
	barraprogreso,
	string1,
	string2,
	string3,
	string4,
	toolbar1, ' abrir fichero
	toolbar2, ' cortar (tijeras)
	toolbar3, ' copiar
	toolbar4, ' boton de chequeo
	toolbar5, ' menu desplegable de valores
	pantalla2d,
	pantalla3d,
	grp_ficheros,
	grp_vistas,
	grp_info,
	grp_mostrar,
	grp_cotas,
	grp_varios,
	grp_logo
end enum


Sub apaga3D()
	' libera la ventana 3D
	freegadget(pantalla3d)
	settimer -1,-1,-1,-1
	' y abre una ventana para salida de datos
	ScreenRes 640,480,8
End Sub
Sub enciende3D()
	' borro la pantalla actual de datos
	Screen 0
	' y vuelvo a crear una grafica nueva
	wingraf=ImageGadget(pantalla3d,3,3,anchograf, altograf)
	GetClientRect(wingraf , @e_wingraf)
	lpTimerFunc=@evento_grafico() ' puntero a la rutina que controla el timer
	SetTimer(wingraf,pantalla3d,5,lpTimerFunc) ' esto es el autorefresco
	' la refresco
	anchopanold+=1 ' es una trampa para que el autorefresco funcione
	cambiaposiciones(0)	
End Sub


' si es 1 solo pone redibujando y sale
' si es 0 primero borra, y luego, dependiendo de valores RADIO y ZOOM pone o no
Sub advertencias(mensaje As integer)
	WindowStartDraw(hwnd,670,altopan-90,305,63,0)  ' el ultimo param=1, es fondo transparente, lo necesito  
	   FontDraw(fuente_grande)
	   If mensaje=1 Then 
	   	BoxDraw (0,0,305,63,&h00ffff,&h00ffff) ' fondo amarillo
	   	TextDraw(3,22,"     RECALCULANDO VISTA",-1,&hFF00FF) 
	   	stopdraw
	   	Exit sub
	   EndIf
	   If mensaje=2 Then 
	   	BoxDraw (0,0,305,63,&hf0f0f0,&hf0f0f0) ' fondo amarillo
	   	TextDraw(3,22,"    BOTON DERECHO SALIR",-1,&h0000FF) 
	   	stopdraw
	   	Exit sub
	   EndIf
		BoxDraw (0,0,305,63,&hf0f0f0,&hf0f0f0) 'color de texto y fondo, en teoria, como la ventana windows abierta
	   If RADIOS Then TextDraw(0,0 ,"    MEDIR RADIOS ACTIVO",-1,&h0000FF) 
	   If ZOOM   Then TextDraw(0,30,"            ZOOM ACTIVO",-1,&h0000FF)
	StopDraw 
End Sub


Sub pontextogrande()
	WindowStartDraw(hwnd,(anchopan-204)+3,3+3,212-6,(altopan-300)-6,1)  ' el ultimo param=1, es fondo transparente, lo necesito
	   BoxDraw(3,12,212-3,(altopan-200)-6,&hf0f0f0,&hf0f0f0) 'color de texto y fondo, en teoria, como la ventana windows abierta
	   FontDraw(fuente_grande) 
	   
	   ' coordenadas reales
	   TextDraw(3,14,Xtext,-1,&h010101) 
	   TextDraw(3,34,Ytext,-1,&h010101)
	   TextDraw(3,54,Ztext,-1,&h010101)
		' avance y RPM
	   TextDraw(3,100,Ftext,-1,&h010101)
	   TextDraw(3,120,Stext,-1,&h010101)
		' MAXMIN X
	   TextDraw(3,162,Xmintxt,-1,&h0000c0) ' rojos
	   TextDraw(3,182,Xmaxtxt,-1,&h0000c0)
		' MAXMIN Y
	   TextDraw(3,212,Ymintxt,-1,&h00c000) ' verdes
	   TextDraw(3,232,Ymaxtxt,-1,&h00c000)
	   ' MAXMIN Z
	   TextDraw(3,262,Zmintxt,-1,&hc00000) ' azules
	   TextDraw(3,282,Zmaxtxt,-1,&hc00000)
	   ' TIEMPO
	   TextDraw(3,340,Ttext,-1,&h010101)
	StopDraw 
	
	' TEXTOS DE ADVERTENCIA
	advertencias(0) ' borra advertencias y pone "redibujando"
End Sub

' si "quehacer" es 1, solo refrescamos, si es "0", cambiamos tamaño (si es que hemos cambiado, sino, sale sin hacer nada)
Sub cambiaposiciones(redibuja As Integer)
		anchopan= WindowClientWidth(hwnd)
		altopan = WindowClientHeight(hwnd)

		' si solo queremos redibujar
		If redibuja=1 Then GoTo redibujar
		
		' redimensiona la parte grafica, solo si cambia la medida de la ventana (y la primera vez, cuando se inicia)
		If (anchopan<>anchopanold Or altopan<>altopanold) Then 
			' lo primero, si nos pasamos de pequeño, no lo admitimos y salimos
			' el minimo aceptable es de 1000x730, para que se vea en 1024x768
			If (anchopan<980) Or (altopan<670) Then ' las medidas interiores no son como las exteriores
				resizewindow(HWND,fb_ignore, fb_ignore,1000,730) ' exteriores
				anchopan=980 ' interiores
				altopan=670
			EndIf
			anchopanold= anchopan  ' las nuevas medidas de ventana
			altopanold = altopan
			
			anchograf  = anchopan-212  ' las nuevas medidas graficas
	 		altograf   = altopan-178
		
			' cambiamos tamaño de la parte grafica
			 ResizeGadget(pantalla3d,3,3,anchograf, altograf) ' 3,3 es el inicio dentro de la ventna windows
			 GetClientRect(wingraf , @e_wingraf )
		    With bmi ' esto, no se que es, pero sirve para coger los datos graficos guardados y usarlo en el screenres
			    .bV4Size = len(BITMAPV4HEADER)
			    .bv4width = e_wingraf.right+1
			    .bv4height = -(e_wingraf.bottom+1)
			    .bv4planes = 1
			    .bv4bitcount = 32
			    .bv4v4compression = 0
			    .bv4sizeimage = (e_wingraf.right+1) * (e_wingraf.bottom+1) * 4
			    .bV4RedMask = &h0f00
			    .bV4GreenMask = &h00f0
			    .bV4BlueMask = &h000f
			    .bV4AlphaMask = &hf000
		    End with
			ScreenRes e_wingraf.right+1, e_wingraf.bottom+1, 32, 1, FB.GFX_NULL	

		Else
			' si no, no hacemos nada
			Exit Sub
		End If
		
redibujar:		
		GroupGadget(grp_mostrar,3,altopan-108,215,85,"Mostrar")
		ButtonGadget(bt_zonas, 10,altopan-90,100,30,"Ver Zonas",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_puntos,10,altopan-60,100,30,"Ver Puntos",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_radios,110,altopan-90,100,30,"Medir Radios",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_turbo,110,altopan-60,100,30,"Modo Lento",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		'
		GroupGadget(grp_ficheros,233,altopan-108,205,85,"Ficheros")
		ButtonGadget(bt_convertir,240,altopan-90,100,30,"Convertir") 
		ButtonGadget(bt_compensar,240,altopan-60,100,30,"Compensar") 
		'
		ButtonGadget(bt_leernc,340,altopan-90,90,60,"Leer   Fichero", BS_MULTILINE) ' multiline para que el nombre del boton baje de linea
		'
		GroupGadget(grp_vistas,454,altopan-108,194,85,"Vistas")
		ButtonGadget(bt_planta,460,altopan-60,90,30,"Planta",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_isomet,550,altopan-60,90,30,"Isometrico",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_lateral,460,altopan-90,90,30,"Lateral",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		ButtonGadget(bt_frente,550,altopan-90,90,30,"Frente",BS_PUSHLIKE Or BS_AUTOCHECKBOX) 
		'
		GroupGadget(grp_info,3,altopan-170,anchopan-7,60,"Informacion")
		ProgressBarGadget(barraprogreso,anchopan-415,altopan-150,400,30,0,100,1)
		'
		GroupGadget(grp_cotas,anchopan-204,3,200,altopan-300,"Datos del Programa")
		GroupGadget(grp_varios,665,altopan-108,315,85,"Varios")
		
		' logo empresa y mi nombre
		marcos()

		
		'StringGadget(string2,anchopan-275,40,150,20,"cadena 2",ES_RIGHT Or ES_NUMBER)
		
		' barra de estado inferior
		StatusBarGadget(barraestado,"Tamaño Ventana: "+Str(anchopan)+"x"+Str(altopan))
		
		'dibuja2d()
		pontextogrande()
End Sub
