	#Include "fbgfx.bi"
	#if __FB_LANG__ = "fb"
	Using FB '' Scan code constants are stored in the FB namespace in lang FB
	#EndIf
	

	
	' para el FILEDIALOG y el MSGBOX
	#Include "windows.bi"
	#Include "win\commdlg.bi"
	Declare Function GetOFN( byval hwndOwner as HWND, byval pszFile as zstring ptr, byval nMaxFile as integer) as Integer
	dim Shared As zstring * MAX_PATH file



	' ficheros
	Dim Shared As String nombrenc
	Dim Shared As String rutanc
	Dim As Integer tempf, sin_fichero=0
	
	nombrenc=Command
	'If nombrenc="" Then nombrenc="ncpruebas\18993ps043.nc" ' peque
	'If nombrenc="" Then nombrenc="ncpruebas\cc.nc" ' intermedio
	'If nombrenc="" Then nombrenc="ncpruebas\error_z.h"
	'If nombrenc="" Then nombrenc="ncpruebas\18104ci001.h" 'grande
	
	' SI ENTRAMOS SIN INDICAR FICHERO, PODEMOS PEDIRLO, O MEJOR AUN, MOSTRAR EL NC-JOSEBA-EPALZA
	If nombrenc="" Then 
		'GetOFN( 0, @file, MAX_PATH ):nombrenc=file
		sin_fichero=1
	EndIf
	
	
	' del nombre cogemos la ruta suelta, para luego las conversiones, hacerlas en la misam ruta
	For tempf=Len(nombrenc) To 1 Step -1
		If Mid(nombrenc,tempf,1)="\" Then Exit For
	Next
	rutanc="" ' si no aparece "\", no hay ruta, simple como la vida misma
	If tempf>0 Then rutanc=Left(nombrenc,tempf)


 	'MessageBox(NULL, "     Cargando Librerias", "SimulaCNC", MB_APPLMODAL)
	





	#Include "variables.bas"
	#Include "guindous.bas"

	#Include "rutinasCOMUN.bas"
	#Include "rutinas3D.bas"
	#Include "rutinasNC.bas"
	#Include "rutinasCONV.bas"
	
	' logotipos, en este caso, solo mi nombre
	#Include "joseba_epalza.bi"



	'If ayuda=1 Then ver_ayuda()



 	anchopan=1000 ' ancho resolucion, iniciamos con una adecuada a 1024x768
 	altopan =730  ' alto resolucion
	'hwnd2=OpenWindow("Conversor NC",100,100,640,480)
	' abrimos ventana guindous
	hwnd=OpenWindow("Simulador y Editor de programas CNC (Jepalza-2001-23)",500,100,anchopan,altopan)
	UseGadgetList(hwnd)

	'hidewindow(hwnd2)
	CenterWindow(hwnd) ' centrar ventana
	
		anchopan		= WindowClientWidth(hwnd)
		altopan 		= WindowClientHeight(hwnd)
		anchograf  	= anchopan-212
	 	altograf   	= altopan-178

	' esto es el cristo que lo pario.
	' creamos una ventana y le asignamos una serie de eventos, para que se autorefresque y se muestre
	wingraf=ImageGadget(pantalla3d,3,3,anchograf, altograf)
	GetClientRect(wingraf , @e_wingraf)
	
	''' esto es necesario hacerlo tal cual esta. lo de "any ptr" es para evitar un warning que da si pongo directo "@evento_grafico()"
	lpTimerFunc=@evento_grafico() ' puntero a la rutina que controla el timer
	SetTimer(wingraf,pantalla3d,5,lpTimerFunc) ' esto es el autorefresco
	'''

	
	fuente_grande=LoadFont("Arial",16)',,1,1) ' ultimo param. es italica, anterior a este, bold
	fuente_normal=LoadFont("Arial",9)



' pos eso, desde cero, para empezar de nuevo desde aqui, si cargo otro NC
desdecero:



'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	
	' primer dibujado
	'marcos()
	
	' chorranda inventada para que se muestre el logo de empresa en formato NC si no indicamos fichero al entrar
	If sin_fichero=0 Then 
		Open nombrenc For Input As 1
		i=Lof(1)
	Else
		i=22222 ' indicativo de logo de empresa activo. lo uso para entrar en vista ISO o en planta
	End If
	
	''''''''''''''''''''
	' tengo un problema: no se cuanto reservar para coordenadas.
	' podria leer todo entero primero, y segun eso, ya tengo datos, pero es muy lento tener que leer dos veces
	' una para tener datos, y otra para trabajar. (un NC de 130megas tarda 1min en cargar, si debo hacerlo dos veces, son 2min., casi na)
	' por eso, hago un calculo por encima: longitud del NC (en caracteres) dividido entre 30, que es mas o menos, lo que ocupara cada
	' linea de coordenadas, y es mejor tirar para abajo, que da mas espacio (1000/100=10 datos, 1000/10=100 datos)
	f=i/30 ' 30 seria la media entre "N99999 X+9999.999 Y-99999.999 Z+99999.999 F2000 G00"(52c.) y "N100 X100.20 Y200.2 Z1234.0"(28c.)
	' en un NC de 135megas, salen unos 450mil puntos a reservar. si cada COORD(x,4)  tienen 5*4 bytes (0,1,2,3,4 de 4 bytes cada)
	' se reservan, solo para coord, unos 90megas, mas otros 55 megas para el 3D, o sea, unos 150megas de RAM para 130megas de NC
	''''''''''''''''''
	
	If f<20000 Then f=20000 ' por si acaso, para programas peques, reservo un estandar de 20mil XYZ
	ReDim coord(f,4) ' datos reales del NC (XYZ y F+G)
	ReDim coord3D(f,2) ' datos girados, Ssolo XYZ
	
	h=0 ' para el progreso
	es_hnc=0 ' por defecto, pensamos en formato NC
	
	' valores por defecto al comienzo
	' si estos valores no cambian, es que NO se leen coordenadas X o Y o Z, y actuamos en consecuencia
	XOLD=111111
	YOLD=111111
	ZOLD=111111

	' por defecto, al entrar, si no se ha cargado un fichero "arrastrando", muestra mi nombre, "Joseba Epalza"
	If sin_fichero Then 
		sin_fichero=0
	 	Restore NC_JOSEBA_EPALZA
		For f=1 To 1000 ' no hacen falta tantas lineas, pero por si acaso
			Read sa
			If sa="fin" Then Exit For
			lee_coordenadas(UCase(sa))
		next
		nlin=m-1
		GoTo si_entramos_sin_fichero_ponemos_mi_nombre ' al llegar alli, hace un close 1
	EndIf


	' primero, miro si es un HNC
	m=0
	While Not Eof (1)
		Line Input #1,sa
		If InStr(UCase(sa),"BEGIN PGM") Then es_hnc=1
		m+=1
		If m>50 Then Exit While ' basta con mirar 50 lineas
	Wend
	Seek 1,1 ' vuelvo al principio del fichero


	' a cero los comentarios, por si acaso se lee un nuevo NC, que no queden los viejos
	comentario_fresa=""
	comentario_espesor=""
	comentario_zona=0
	
	' pero antes pongo a "0" variables importantes (G,F y S)
	GG=0
	FF=0
	SS=0

	' ahora, saco datos del NC o HNC
	m=0
	bm=0 'por algun Error que desconozco, debo poner a cero los botones del raton, sino, guarda la posicion Y activa menus a voleo
	WindowStartDraw(hwnd,6,altopan-150,anchopan-430,30,0)
		BoxDraw (0,0,anchopan-430,30,&hf0f0f0,&hf0f0f0) ' fondo gris
		FontDraw(fuente_normal)
		TextDraw(4,0,"Leyendo",-1,0)
		TextDraw(4,14,UCase(nombrenc),-1,0) ' letras negras
	stopdraw
	
	' refrescamos los dibujado hasta ahora
	g=f ' guardo estado de f (por que cambiaposiciones() lo toca)
	REDIBUJAR=1:cambiaposiciones(0)
	f=g ' recupera estado de f
	
	While Not Eof (1)
		Line Input #1,sa
		lee_coordenadas(UCase(sa))
			Locate 1,1:Print f
		If m+1>f Then msgbox "Error leyendo fichero", "Demasiado grande o fallo en coordenadas":End ' si lo leido es mayor a "f" (de la matriz "ReDim coord3D(f,2)")
		h=h+Len(sa)+2 ' el +2 es por el CR/LF
		SetGadgetState(barraprogreso,(h/i)*100)
		If mira_ESC() Then Exit While
	Wend

	'DestroyWindow(hwnd2)
	nlin=m-1 ' lineas CON coordenadas leidas del fichero NC o HNC
	
	
si_entramos_sin_fichero_ponemos_mi_nombre:	
	Close 1 ' este close es para cerrar el fichero de entrada NC, tanto si es el logo de empresa como si es arrastrado o leido
	''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	' miro unas pocas coordenadas al inicio, y si hay indicativo 111111 es que alguna de ellas "falta"
	' o sea, que el NC o HNC no la lleva (linea tipo 100LY100,12Z100,12 donde "falta" la X)
	' esto es SOLO importante al incio, por que luego, segun avanza, y encuentra datos, va completando los que faltan por defecto
	' o sea, si falta X en la linea 1, pero aparece en la 2, esta es la X por defecto de la linea 1, o sea, repito coordenada
	' pero al principio, si faltan alguna de las XYZ, no tienen una por defecto, y si pongo "0", puedo liarla parda (imagina un programa girado)
	' por eso es mejor, indicarlo con dato falso (111111 en concreto) y luego, aqui, busco la primera que NO sea 111111, y la asigno
	' a la anterior a ella, que seria la 111111
	' ejemplo:
	' N1 Z100  --> faltan XY, o sea que X=111111 e Y=111111, pero Z=100
	' N2 X100 Z100 ---> falta Y, o sea, que Y=111111, con esta X, puedo dejar la N1 en X=100 , pero la Y sigue sin salir
	' N3 X110 Y100 Z120 --> ya puedo hacer la Y, pero debo hacerlo en las N1 y N2.
	' de eso se encarga esta rutina. lo hago solo en las primeras 100, que deberia valer.

	For f=0 To IIf(nlin<100,nlin,100)
		' reviso X
		'Print coord(f,0),coord(f,1),coord(f,2)
		If coord(f,0)=111111 Then 
			For g=f To IIf(nlin<100,nlin,100)
				If coord(g,0)<>111111 Then coord(f,0)=coord(g,0):Exit for
			Next
		EndIf
		' reviso Y
		If coord(f,1)=111111 Then 
			' reviso Y
			For g=f To IIf(nlin<100,nlin,100)
				If coord(g,1)<>111111 Then coord(f,1)=coord(g,1):Exit for
			Next
		EndIf
		' reviso Z
		If coord(f,2)=111111 Then
			' reviso Z
			For g=f To IIf(nlin<100,nlin,100)
				If coord(g,2)<>111111 Then coord(f,2)=coord(g,2):Exit for
			Next
		EndIf
	Next
	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	' ahora viene otro problema:
	' si llegados al final del NC (ULTIMA coordenada), hay alguna que vale 111111, es señal, de que esa coordenada NO existe
	' ni desde el principio, por ejemplo, tipico de los contornos 2D planos SIN "Z", que solo llevan X e Y
	' revisamos ese punto, y actuamos. si ocurre, asignamos "0" a esa coordenada de principio a fin.
	If coord(nlin,0)=111111 Or coord(nlin,1)=111111 Or coord(nlin,2)=111111  Then ' nlin=ultima coordenada leida
		If coord(nlin,0)=111111 Then
			For f=0 To nlin:coord(f,0)=0:Next ' si falta la X
		EndIf
		If coord(nlin,1)=111111 Then
			For f=0 To nlin:coord(f,1)=0:Next ' si falta la Y
		EndIf
		If coord(nlin,2)=111111 Then
			For f=0 To nlin:coord(f,2)=0:next ' si falta la Z
		EndIf
	End If	
	' IMPORTANTE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	' PODRIA OCURRIR QUE UN PROGRAMA SIN Z, DEBA IR SIN Z, Y CON ESTE METODO MIO, LA METE SI O SI, CON VALOR 0
	' ESO PODRIA SER MALO PARA PROGRAMAS "Z LOCA", QUE VAN SIN Z's.
	' SI ESO ES IMPORTANTE, SE PUEDE PASAR DE ESTA RUTINA ANTERIOR, Y METER UNA QUE DETECTE SI AUN QUEDAN 111111 EN ALGUNA COORD.
	' Y A LA HORA DE CONVERTIR ENTRE FORMATOS, USARLO PARA "NO" GUARDAR ESA COORDENADA..........................................
	''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	
	
	
	
	
	
	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' 
	' EMPIEZA EL DIBUJADO


	' TODO A CERO!!!!!!!!!!!!
	resaltarzonas=0
	verpuntos=0
	modoturbo=0
	' borro botones para que se "suelten" (para que suban)
	SetGadgetstate(bt_radios,0) 
	SetGadgetstate(bt_puntos,0) 
	SetGadgetstate(bt_zonas,0) 
	SetGadgetstate(bt_turbo,0) 
	SetGadgetstate(bt_planta,0) 
	SetGadgetstate(bt_isomet,0) 
	SetGadgetstate(bt_lateral,0) 
	SetGadgetstate(bt_frente,0) 
	
	
	' solo la primera vez
	zoom=0
	RADIOS=0
	ANGX = 0 : ANGY = 0: ANGZ=0 
	VISTA=1 ' vista en planta inicial
	VISTAOLD=9999
	AXONO=0 ' axono inicial
	
	' truco tonto para mostrar mi logo al inicio, en vista 3D
	If i=22222 Then 
		resaltarzonas=1 
		ANGX = 45
		ANGY = 0
		ANGZ=  45
		VISTA=2
		RADIOS=0
	EndIf
	
	gira_y_saca_limites()
	dibuja_3d()
	DIBUJA_EJE() 


	
	' escribe tiempo total
   Dim As integer horas,minutos,segundos
   horas=int(tiempo / 3600)
   minutos=int((tiempo - horas * 3600) / 60)
   segundos=tiempo - (horas * 3600 + minutos * 60)
	tiempo_final=LTrim(RTrim(Str(horas)))+"h. "+LTrim(RTrim(Str(minutos)))+"m. "+LTrim(RTrim(Str(segundos)))+"s."

	Ttext= tiempo_final
	'textoturbo()
	
	' textos limites
	Xmintxt= "XMIN :"+Str(XMIN)	
	Xmaxtxt= "XMAX :"+Str(XMAX)
	'
	Ymintxt= "YMIN :"+Str(YMIN)	
	Ymaxtxt= "YMAX :"+Str(YMAX)	
	'
	Zmintxt= "ZMIN :"+Str(ZMIN)
	Zmaxtxt= "ZMAX :"+Str(ZMAX)	
	
	' los textos de cotas reales, a cero la primera vez
	Xtext="X ***"
	Ytext="Y ***"
	Ztext="Z ***"

	
	
	' pone la "F"
	sa="F "+LTrim(RTrim(Str(FF)))
	Ftext= sa

	' pone la "S" (sin hacer aun, por que tebis no lo incluye, no se lee)
	If SS=0 Then sa="S ****" Else sa="S "+LTrim(RTrim(Str(SS)))
	Stext= sa





	freeconsole()

	
	'''''''''''''' bucle principal
	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Do
	 Var evento = WindowEvent() 
	 
	 '''''''''''
	 Sleep 1 ' IMPORTANTE!!! OBLIGATORIO si se usa WINDOWEVENT, o se sobrecarga la CPU (prueba a quitarlo y veras en "Admn. de tareas de windows")
	 '''''''''''

	 Select Case evento
	
	 	Case eventclose
			Select Case EventHwnd
				Case hwnd
               salir()
			End Select
	   	
	 	Case EventSize ' si cambiamos de tamaño la ventana
	 		If EventHwnd=hwnd	Then 
	 			REDIBUJAR=1
	 			cambiaposiciones(0)
	 		EndIf
	
	 	Case EventMenu   ' menus superiores
		  Select case EventNumber
		   Case 1
		    MessBox("","elegido menu 1")
		   Case 2
		    MessBox("","elegido menu 2")
		   Case 3
		    MessBox("","elegido menu 3 con submenu") 
		  	case else 
		  	 MessBox("","Menu desconocido")
		  End Select
		    
	
		' control de botones
	 	Case EventGadget 'EventLBDown
	 		menu=0
	 		Select Case EventNumber
	 			Case bt_convertir
	 				menu=1
	 			Case bt_turbo
	 				menu=2
	 			Case bt_radios
	 				If VISTA<>1 Then
	 					SetGadgetText(bt_radios,"")  	
						SetGadgetstate(bt_radios,0) 
						Exit select
	 				EndIf
	 				menu=3
	 			Case bt_puntos
	 				menu=4
	 			Case bt_zonas
	 				menu=5
	 			Case bt_planta
	 				menu=6
	 			Case bt_isomet
	 				menu=7
	 			Case bt_lateral
	 				menu=8
	 			Case bt_frente
	 				menu=9
	 			Case bt_leernc
	 				menu=10
	 			Case bt_compensar
	 				menu=11
	 		End Select
	 		
	 	Case EventKeyUp 
	 		 	If Eventkey=VK_C Then menu=1 ' convertir
	 		 	If Eventkey=VK_T Or Eventkey=VK_F2 Then menu=2 ' turbo
	 		 	If Eventkey=VK_R Then menu=3 ' radios
	 		 	If Eventkey=VK_P Then menu=4 ' puntos
	 		 	If Eventkey=VK_Z Then menu=5 ' zoom
	 		 	' vistas
	 		 	If Eventkey=VK_1 Then menu=6 ' planta
	 		 	If Eventkey=VK_2 Then menu=7 ' axono
	 		 	If Eventkey=VK_3 Then menu=8 ' lateral
	 		 	If Eventkey=VK_4 Then menu=9 ' frente
	 		 	'
	 		 	If Eventkey=VK_F Then menu=10 ' nuevo fichero
	 		 	If Eventkey=VK_X Then menu=11 ' menu compensacion, sin asignar aun

	 		 
			 	If Eventkey=VK_F1 Then
				 	'If ayuda=1 Then 
						'ayuda=0
						'FreeConsole()
						'Sleep 200,1
				 	'Else
						'ayuda=1
						'AllocConsole()
						'ver_ayuda()
						'Sleep 200,1
				 	'EndIf
			 	End If
			 	
			 	
		' boton izquierdo
	 	Case EventLBDown
	 		' si pulsamos dos veces, la segunda vale 11, es para el zoom sobre todo
	 		If bm=1 Then bm=11 Else bm=1

	 	' boton derecho
	 	Case EventRBDown
	 		bm=2
	 		
	    
	 End Select
	
	
	 ' si movemos la ventana, redibujamos
	  xpan=WindowX(HWND)
	  ypan=WindowY(HWND)
	  If xpan<>xpanold Or ypan<>ypanold Then
	  	xpanold=xpan
	  	ypanold=ypan
	  	' redibujamos
	  	cambiaposiciones(1)
	  	REDIBUJAR=1
	  EndIf

		
		
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'==========================================================================
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		
		
		' raton
		'GetMouse(mx,my,m,bm,1)
		' debo compensar X y Y del raton, respecto a las reales, con -12 y -34 (o -54)
	 	mx=GlobalMouseX-WindowX(hwnd)-12
		my=GlobalMouseY-WindowY(hwnd)-34 ' nota, si añadimos menus desplegables arriba, hay que poner -54, en vez de -34 !!!!!!!!!!!!!!!!!
		'?mx,my',WindowX(hwnd),WindowY(hwnd)

		
		' si hacemos zoom, pero al soltar boton, el raton no se mueve, no hace el zoom, aqui, le obligo con trampas
		If bm=0 Or bm=11 And xzoomi Then mx=mx+1 ' una trampa, si soltamos el boton derecho o izquierdo (11=soltar boton zoom)
		
		
		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		' seguido de salir de zoom, salimos de medir radios
		' con boton derecho salimos de medicion de radios, en caso de estar dentro
		If bm=2 And RADIOS=1 Then
			SetGadgetstate(bt_radios,0) ' apaga el boton
			RADIOS=0
			REDIBUJAR=1
			bm=0
		EndIf
		 ' con el boton derecho, salimos del zoom, en caso de estar dentro
		If bm=2 And zoom=1 Then
			REDIBUJAR=1
			bm=0 ' botones de raton a cero
			xzoomi=0:yzoomi=0
			anchozoom=0
			zoom=0
			advertencias(0)
		EndIf
		'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		
		
		'====================================================================================
		' detectamos cambios en el raton al moverlo
		If mx<>mxold Or my<>myold Then
			mxold=mx:myold=my
				
				
			''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''	
			' cuadro visible solo	
			If (mx>0 And mx<resx) And (my>0 And my<resy) Then
				
				'''''''''''''''''''''''''''''''''''''''''''''''
				' zoom si hacemos un marco, picando con el sagu
				If zoom=0 And RADIOS=0 Then ' solo permito un zoom, si ya hay uno, aqui no entra (y mientras no mida radios)
					
					' al soltar el boton tras hacer un marco
					If bm=11 And zoom=0 Then 
						If xzoomi>0 Then
							bm=0
							' segun el tipo de forma, elegimos factor x o y
							anchozoom=my-yzoomi ' prevalece la altura
							factorzoom=(resy/anchozoom)*factor ' simple regla de tres inversa--> si RESY es a FACTOR, ANCHOZOOM es a "X"
							If (mx-xzoomi)>anchozoom Then  ' sino, cambiamos por anchura
								anchozoom=mx-xzoomi
								factorzoom=(resx/anchozoom)*factor
							EndIf
							'
							If anchozoom<10 Then 
								xzoomi=0:yzoomi=0
								anchozoom=0	
								'''Put (0,0), imagenzoom, pset
							Else
								X=xzoomi-DIFLIMX
								Y=yzoomi+DIFLIMY
								X=(       ((X -(BORDE/2))/factor)+XMIN )
								Y=( (((resy-Y)-(BORDE/2))/factor)+YMIN )
								zoom=1
								haz_zoom()
							EndIf
						Else
							xzoomi=0:yzoomi=0
							anchozoom=0	
							zoom=0
						EndIf
					EndIf
					' al pulsar el boton la primera vez
					If bm=1 And zoom=0 Then
						If xzoomi=0 Then 
							If imagenzoom Then ImageDestroy (imagenzoom)
							imagenzoom = ImageCreate(resx+10, resy+10)
							Get (0,0)-(resx,resy), imagenzoom
							xzoomi=mx
							yzoomi=my
						EndIf
						If (mx-xzoomi>10 and my-yzoomi>10) Then
							If ((xzoomi+(mx-xzoomi))<resx) and ((yzoomi+(my-yzoomi))<resy) Then 
								Put (0,0), imagenzoom, PSet
								Line (xzoomi,yzoomi)-step(mx-xzoomi,my-yzoomi),amarillo,b
							End If
						End if
					EndIf
				End If
				'''''''''''''''''''''''''''''''''''''''''''''''''''''''
				
				
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				' coordenadas
				If zoom=0 Then
					'Locate 1,1:Print mx,my,XMIN,YMIN,factor
					' busco coordenadas
					X=mx-DIFLIMX
					Y=my+DIFLIMY
					X=(       ((X -(BORDE/2))/factor)+XMIN )
					Y=( (((resy-Y)-(BORDE/2))/factor)+YMIN )
					'Print "REALES:";X,Y
					i=localiza_Z(X,Y)
				Else
					'Locate 1,1:Print mx,my,XMINZOOM,YMINZOOM,factorzoom
					X=XMINZOOM+(mx/factorzoom)
					Y=YMINZOOM-(my/factorzoom)
					'Print "REALES:";X,Y
					i=localiza_Z(X,Y)
				End If
					
				X=0:Y=0 ' quitar si queremos ver SIEMPRE XY, pero es peligroso, por que si "no" hay coordenada real, muestra la de pantalla
				Z=0
				If i>-1 Then 
					X=coord(i,0):Y=coord(i,1):Z=coord(i,2)
					' ponemos un circulo en el punto mas cercano del raton localizado en "localiza_Z"
					' no funciona bien, anulado
					'If zoom=0 Then a= ((X-XMIN)*factor)+(BORDE/2)+DIFLIMX :b=resy-( ((Y-YMIN)*factor)+(BORDE/2)+DIFLIMY ) 
					'If zoom=1 Then a= (X-XMINZOOM)*factorzoom : b= -( (Y-YMINZOOM)*factorzoom )
					'Circle (a,b),5
				EndIf
	
				'X=-9999.999:Y=9999.999:Z=-0.0001 ' depuracion
				signox=Sgn(X):X=Abs(X)
				signoy=Sgn(Y):Y=Abs(Y)
				signoz=Sgn(Z):Z=Abs(Z)
				
				XS=LTrim(RTrim(Str(Int(X*1000))))
				XS=Left(XS,Len(XS)-3)+"."+Right(XS,3)
				If Left(XS,1)="." Then XS="0"+XS
				XS="X"+IIf(signox=-1, "-", "+")+XS
				
				YS=LTrim(RTrim(Str(Int(Y*1000))))
				YS=Left(YS,Len(YS)-3)+"."+Right(YS,3)
				If Left(YS,1)="." Then YS="0"+YS
				YS="Y"+IIf(signoy=-1, "-", "+")+YS
				
				ZS=LTrim(RTrim(Str(Int(Z*1000))))
				ZS=Left(ZS,Len(ZS)-3)+"."+Right(ZS,3)
				If Left(ZS,1)="." Then ZS="0"+ZS
				ZS="Z"+IIf(signoz=-1, "-", "+")+ZS
				
				'Locate 10,10:Color textonegro,negro:Print X,Y,Z
				Xtext= XS':PSet (resx+ 7,160), 0: Draw col1+vtext
				Ytext= YS':PSet (resx+10,200), 0: Draw col1+vtext
				Ztext= ZS':PSet (resx+10,240), 0: Draw col1+vtext
				pontextogrande()
				
			End If ' fin de busqueda de coordenadas en cuadro visible
			''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
			
			''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
			' solo si VISTA=1(planta) hacemos calculo de radios al pasar sobre ellos
			If VISTA=1 And RADIOS=1 Then
				' busco coordenadas
				If zoom=0 Then
					X=mx-DIFLIMX
					Y=my+DIFLIMY
					X=(       ((X -(BORDE/2))/factor)+XMIN )
					Y=( (((resy-Y)-(BORDE/2))/factor)+YMIN )
					f=localiza_Z(X,Y)
				Else
	 				X=XMINZOOM+(mx/factorzoom)
					Y=YMINZOOM-(my/factorzoom)
					f=localiza_Z(X,Y)
				End If
				Put (0,0), imagenradios, PSet
				View (0,0)-(resx,resy)
				If f>-1 Then calcula_radio(X,Y,f)
				View
				'Put (0,0), imagen, PSet
			EndIf
			''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
			
			
			
		End If ' fin de busqueda de coordenadas en posicion de raton
		' =================================================================================================
		
		
		
		
					
			
		
		''''''''''''''''''''''''''''''''  MMMM EEEE NNNN UUUU SSSS '''''''''''''''''''''''''''''''''''''				
		
		' conversion entre formatos
		IF menu=1 Then ' 111111111111111111111111111111111
			SetGadgetstate(bt_convertir,1)
			convertir_NC()
			SetGadgetstate(bt_convertir,0)
			REDIBUJAR=1
		EndIf


		IF menu=2 Then ' 22222222222222222222222222222222
			If modoturbo>0 Then
				modoturbo=0
			Else
				' complicado de explicar: si 'nlin' tarda en dibujarse 'tardanza'
				' entonces, dividiendo tiempo/lineas tenemos cuanto tarda cada linea (punto) en dibujarse el solo
				' ahora, si queremos tardar 10s. para las mismas lineas (nlin), lo que hacemos es "frenar"
				' la diferencia entre ambos "Mundos" (entre modo rapido y lento)
				modoturbo=tardanza/nlin ' con esto, sabemos lo que tarda cada linea (punto) en dibujarse
				modoturbo=(frenartiempo/nlin)-modoturbo ' y con esto, sabemos la dif. de tiempo entre tardar "nada" (rapido) y tardar 10s. (lento)
				' y esa diferencia, la frenamos en "dibuja_3d()"
			EndIf
			SetGadgetstate(bt_turbo,IIf(modoturbo>0,1,0))
			REDIBUJAR=1 ' para que entre en el modo de dibujado, sino, no lo hace
			'Sleep 200,1 ' lo freno, que sino, repite varias veces al pulsar F2
		EndIf
		
		
		' para medir radios SOLO en planta y SOLO si pulsamos R. Si cambiamos vista, se quita
		IF menu=3 And VISTA=1 Then ' 33333333333333333333333333333333
			RADIOS=IIf(RADIOS,0,1)
			SetGadgetstate(bt_radios,RADIOS)
			' si entro a mirar radios, copio la vista 
			If RADIOS=1 Then
				ImageDestroy (imagenradios)
				imagenradios = ImageCreate(resx+10, resy+10)
				Get (0,0)-(resx,resy), imagenradios
				advertencias(0)
			Else
				advertencias(0)
			End If
			menu=0 ' por si acaso, apago
		End If
		
		
		' mostrar puntos NC en cada coord. solo se activa si NO estamos en ZOOM, una vez activo, el zoom tambien los ve
		If menu=4 Then ' 4444444444444444444444444444444444444444444
			verpuntos=iif (verpuntos=1, 0 , 1)	
			SetGadgetstate(bt_puntos,verpuntos)
			REDIBUJAR=1
			'Sleep 200,1
		EndIf


		' resaltar zonas con colores diferentes segun los saltos (P) que mete el tebis
		If menu=5 Then ' 5555555555555555555555555555555555555555
			resaltarzonas=iif (resaltarzonas=1, 0 , 1)	
			SetGadgetstate(bt_zonas,resaltarzonas)
			REDIBUJAR=1
			'Sleep 200,1
		EndIf
		
		
		' planta				
		IF menu=6 Then ' 6666666666666666666666666666666666666666
			'lee_boton() ' para activar boton si se pulsa tecla unicamente
			SetGadgetText(bt_radios,"Medir Radios")  
			' apago los demas botones de vistas
			SetGadgetstate(bt_planta,1) 
			SetGadgetstate(bt_isomet,0) 
			SetGadgetstate(bt_frente,0) 
			SetGadgetstate(bt_lateral,0) 
			'
			ANGX = 0 : ANGY = 0: ANGZ=0 : VISTA=1 : RADIOS=0 : AXONO=0 ' planta
		EndIf
		

		' el axono, va rotando sobre el eje Z a medida que se pulsa la tecla
		If menu=7 Then ' 777777777777777777777777777777777777777777
			' apago el boton medir radios
			SetGadgetText(bt_radios,"") 
			SetGadgetstate(bt_radios,0) 
			' apago los demas botones de vistas
			SetGadgetstate(bt_planta,0) 
			SetGadgetstate(bt_isomet,1) 
			SetGadgetstate(bt_frente,0) 
			SetGadgetstate(bt_lateral,0) 
			'
			If AXONO=4 Then AXONO=0
			If AXONO=0 Then ANGX = 45: ANGY = 0: ANGZ=  45: VISTA=2 : RADIOS=0  ' axonometrico 1
			If AXONO=1 Then ANGX = 45: ANGY = 0: ANGZ= -45: VISTA=2 : RADIOS=0  ' axonometrico 2
			If AXONO=2 Then ANGX = 45: ANGY = 0: ANGZ=-135: VISTA=2 : RADIOS=0  ' axonometrico 3
			If AXONO=3 Then ANGX = 45: ANGY = 0: ANGZ= 135: VISTA=2 : RADIOS=0  ' axonometrico 4
			AXONO+=1: 
			REDIBUJAR=1
			SetGadgetstate(bt_isomet,1) ' como se puede pulsar varias veces, lo activo cada vez que lo pulsa
			'Sleep 150,1
		EndIf


		' vista LATERAL (desde X)
		IF menu=8 Then ' 88888888888888888888888888888888888888
			' apago el boton medir radios
			SetGadgetText(bt_radios,"") 
			SetGadgetstate(bt_radios,0) 
			' apago los demas botones de vistas
			SetGadgetstate(bt_planta,0) 
			SetGadgetstate(bt_isomet,0) 
			SetGadgetstate(bt_frente,0) 
			SetGadgetstate(bt_lateral,1) 
			'
			ANGX = 90: ANGY = 0: ANGZ=90: VISTA=4 : RADIOS=0 : AXONO=0 ' lateral desde X
		EndIf
		
		
		' vista FRENTE desde Y
		IF menu=9 Then ' 999999999999999999999999999999999999999999
			' apago el boton medir radios
			SetGadgetText(bt_radios,"") 
			SetGadgetstate(bt_radios,0) 
			' apago los demas botones de vistas
			SetGadgetstate(bt_planta,0) 
			SetGadgetstate(bt_isomet,0) 
			SetGadgetstate(bt_frente,1) 
			SetGadgetstate(bt_lateral,0) 
			'
			ANGX = 90: ANGY = 0: ANGZ=0 : VISTA=3 : RADIOS=0 : AXONO=0 ' frente desde Y
		EndIf


		' empezar desde cero con otro NC
		If menu=10 Then ' 10 10 10 10 10 10 10 10 10 10 10 10 10
			menu=0 ' es necesario esto, por que se mete en un bucle y se autorepite
			tiempo=0
			tiempo_final=""
			'ANGX = 0: ANGY = 0: ANGZ = 0 ' planta
			es_hnc=0
			VISTA=1:VISTAOLD=9999
			REDIBUJAR=0
			GetOFN( 0, @file, MAX_PATH ):nombrenc=file
			GoTo desdecero
		EndIf
		
		
		
		' COMPENSAR NC, programa externo
		If menu=11 Then ' 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 
			menu=0
			' aqui habia un modulo de compensacion de contornos "L0" que he tenido que borrar
			' dado que es propiedad intelectual "mia", vendido a terceras partes
		EndIf

		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''		
		
		
		
	
	
		If VISTA<>VISTAOLD Or REDIBUJAR Then
			REDIBUJAR=0
			bm=0 ' botones de raton a cero
			
			' si cambio de vista, reseteo el zoom
			'If VISTA<>VISTAOLD Then
				xzoomi=0:yzoomi=0
				If zoom Then
					zoom=0
					Put (0,0),imagenzoom,PSet
				EndIf
			'End If
			
			'textoturbo()			
			
			VISTAOLD=VISTA
	
			' recalculando vista
			advertencias(1) ' 1= borra todo y pone solo "redibujando"
				gira_y_saca_limites()
				dibuja_3d() ' dibuja vista
			advertencias(0) ' borra mensajes
			
			' nombre del NC cargado en la barra INFORMACION
			WindowStartDraw(hwnd,6,altopan-150,anchopan-430,30,0)
				BoxDraw (0,0,anchopan-430,30,&hf0f0f0,&hf0f0f0) ' fondo gris
				FontDraw(fuente_normal)
				TextDraw(4,0,"Fichero:",-1,0)
				TextDraw(4,14,UCase(nombrenc),-1,0) ' letras negras
			stopdraw
			
			' activa o desactiva botones segun su estado
			If VISTA=1 Then 
				SetGadgetText(bt_radios,"Medir Radios") ' reactivo el texto, solo visible en planta
				SetGadgetstate(bt_planta,1) ' solo si estamos en planta. es para el estado inicial, al empezar de cero solo
			EndIf
			
			SetGadgetstate(bt_puntos,verpuntos) ' puntos
			SetGadgetstate(bt_radios,RADIOS) ' radios
			SetGadgetstate(bt_zonas,resaltarzonas) ' zonas
			SetGadgetstate(bt_turbo,IIf(modoturbo>0,1,0)) ' turbo
		End If
			
			
			
			
			
		' parpadeos de atencion cada segundo. Para medio segundo poner el timer*2
		If Int(Timer) And 1 Then 
			If zoom Or RADIOS Then advertencias(2) 
		Else
			If zoom Or RADIOS Then advertencias(0) 
		EndIf
		













  ' tecla ESC para salir 
  If mira_ESC() Then salir()

  
  ' apago cualquier menu activo, con esto evitamos un bucle infinito
  menu=0

Loop