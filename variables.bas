

	' unos colorines	
	Dim Shared As Integer negro=rgb(0,0,0)
	Dim Shared As Integer textonegro=rgb(0,0,0)
	Dim Shared As Integer amarillo=rgb(255,255,0)
	Dim Shared As Integer azul=rgb(0,100,255)
	Dim Shared As Integer cian=rgb(0,160,227)
	Dim Shared As Integer marron=rgb(128,100,0)
	Dim Shared As Integer naranja=rgb(239,127,26)
	Dim Shared As Integer verde=rgb(100,255,0)
	Dim Shared As Integer verdeoscuro=rgb(0,190,0)
	Dim Shared As Integer rojo=rgb(255,0,0)
	Dim Shared As Integer rojotebis=rgb(255,0,100)
	Dim Shared As Integer rosa=rgb(247,191,190)
	Dim Shared As Integer magenta=rgb(229,9,127)
	Dim Shared As Integer gris1=rgb(84,84,84)
	Dim Shared As Integer gris2=rgb(168,168,168)
	Dim Shared As Integer grisfondo=rgb(200,200,200)
	Dim Shared As Integer blanco=rgb(255,255,255)
	Dim Shared As Integer colorsaltos=rgb(216,160,0) ' marron TEBIS
	
	' colores grisaceos para los botones
	Dim Shared As Integer cg1=rgb(50,50,50)
	Dim Shared As Integer cg2=rgb(75,75,75)
	Dim Shared As Integer cg3=rgb(125,125,125)
	Dim Shared As Integer cg4=rgb(160,160,160)


	' colores para destacar las zonas distintas de un NC tipo tebis, con (P) de separacion
	Dim Shared As Integer otrocolor=0
	Dim Shared As Integer mascolores(13)
	mascolores(0)=rojotebis
	mascolores(1)=azul
	mascolores(2)=verde
	mascolores(3)=marron
	mascolores(4)=cian
	mascolores(5)=amarillo
	mascolores(6)=rosa
	mascolores(7)=rojo
	mascolores(8)=magenta
	mascolores(9)=grisfondo
	mascolores(10)=blanco
	mascolores(11)=verdeoscuro
	mascolores(12)=naranja
	mascolores(13)=gris1






	' cadenas
	Dim Shared As String sa, sb, sc, sd, se
	
	' enteros
	Dim Shared As Integer a, b, c, d, e, f, g, h, i, m, s, reg

	' raton
	Dim Shared As Integer mx,my,bm, mxold, myold, mbold ', ruleta, ruletaold
	
	' dobles
	Dim Shared As Single fa,fd, tiempo
	
	
	
	Dim Shared As Integer nlin, colorlinea

	ReDim Shared coord(10,4) As Single ' XYZ,F,G
	ReDim Shared coord3D(10,2) As Single ' XYZ girados
	
	Dim Shared lon As Single ' longitud total del programa (en datos numericos, o sea la distancia entre coordenadas)
	
	Dim Shared As Single X,Y,Z,XOLD,YOLD,ZOLD
	Dim Shared As Single X1,Y1,Z1, X2,Y2,Z2
	
	Dim Shared As String XS,YS,ZS ' para meter como cadenas las coordenadas e imprimir en pantalla
	
	Dim Shared As Integer RADIOS=0 ' para medir radios automatico, solo en planta
	Dim Shared As Integer signox,signoy,signoz ' solo para formatear la salida de datos, conociendo el signo
	
	' avances y G00 G01, con valores por defecto, para calculo de tiempos sobre todo.
	Dim Shared As Integer SS, FF, GG, FFOLD, GGOLD
	Dim Shared As Integer FF_en_G00=8000 ' avance por defecto en rapido (G00)
	Dim Shared As integer FF_en_G01=2000 ' avance por defecto en trabajo (G01) si no hay programado

	Dim Shared As Integer MOVER3D ' 1 si se mueve la vista, 0 si no se hace nada
	Dim Shared As Double  modoturbo=0 ' velocidad de representacion, 0=normal, sin pausas
	Dim Shared As Integer frenartiempo=8 ' segundos que queremos que tarde al dibujar en modo lento
	Dim Shared As Integer VISTA=0, VISTAOLD=0, REDIBUJAR=0, AXONO=0 ' VISTA ACTUAL: 1 PLANTA, 2 AXO, 3 LATERAL, 4 FRENTE
	Dim Shared As Integer verpuntos=0 ' si es 1, dibujo un circulo en cada punto del NC
	Dim Shared As Integer pausa1s ' para un contador de 1s. de parpadeos de textos
	Dim Shared As Integer resaltarzonas ' para ver las sendas en colores en cada salto G00 (si es posible) 
	
	' comentarios del NC o HNC para luego insertar en la conversion
	Dim Shared As String comentario_fresa ' el que lleva (FRESA...
	Dim Shared As String comentario_espesor ' el que lleva (ESPESOR....
	Dim Shared As integer comentario_zona  ' el de los saltos (P), pero este numerico, para engañar al G00
	
	' limites y factores en ejes
	Dim Shared As Single factorx,factory,factorz, factor
	Dim Shared As Single XMAX,XMIN, YMAX,YMIN, ZMAX,ZMIN
	Dim Shared DIFLIMX As Integer
	Dim Shared DIFLIMY As Integer
	
	' para el ZOOM
	Dim Shared As Single factorzoom
	Dim Shared As Single XMINZOOM
	Dim Shared As Single YMINZOOM

	' ayudas con F1
	'Declare Sub ver_ayuda()
	'Dim Shared As Integer ayuda=0
	
	
	Dim Shared As Integer menu=0 
	' menus:
		 			'menu=1 bt_convertir
	 				'menu=2 bt_turbo
	 				'menu=3 bt_radios
	 				'menu=4 bt_puntos
	 				'menu=5 bt_zonas
	 				'menu=6 bt_planta
	 				'menu=7 bt_isomet
	 				'menu=8 bt_lateral
	 				'menu=9 bt_frente
	 				'menu=10 bt_leernc
	 				'menu=11 bt_compensar

	
	Dim Shared es_hnc As Integer=0
	
	Dim Shared As String tiempo_final
	Dim Shared As Double tardanza ' tiempo que tarda en dibujar la pantalla. para luego hacer pausas con el modo turbo <F2>

	'DIM SHARED AS INTEGER A, F
	DIM SHARED AS Integer ANGX,ANGY,ANGZ ' angulo de giro iniciales
	ANGX = 0: ANGY = 0: ANGZ = 0 ' planta

	' para zooms
	Dim Shared As Integer xzoomi, yzoomi, anchozoom
	Dim Shared As Integer zoom=0
	Dim As fb.Image Ptr imagenzoom ' para guardar la pantalla y recuperar tras el zoom
	Dim As fb.Image Ptr imagenradios ' para guardar la pantalla y recuperar tras medir radios
	

	'''''''''''''''''''''''''''''''''''''''''''''''''''''
	Dim Shared As Single ZX,ZY,ZZ ' comunes a los modulos
	'''''''''''''''''''''''''''''''''''''''''''''''''''''
	
	
	Declare Function texto_vector (Text As String, size As Byte =4, spacing As Byte =1, angle As Byte =0)As String
	Declare Sub linea(x0 As integer, y0 As integer, x1 As integer, y1 As Integer, clr As Integer) 
	Declare SUB CALC3D(ByVal ANGX2 As integer, ByVal ANGY2 As Integer, ByVal ANGZ2 As Integer)
	Declare Sub haz_zoom()
	Declare Sub dibuja_3d()
	Declare Function localiza_Z(X As Single, Y As Single) As Integer
	Declare Sub gira_y_saca_limites()
	Declare Sub lee_coord(ByVal sa As String)
	Declare function quitacomas(ByVal sa As String) As String
	Declare Sub DIBUJA_EJE()
	Declare SUB PONEJES3D(xcent As Integer, ycent As integer)
	Declare function mira_ESC() As Integer ' si sale con 1, interrupe lo que este haciendo, lo deja a medias.
	Declare Sub PON_LOGO(X2 As Integer, Y2 As Integer)
	'Declare Sub textoturbo()
	Declare Sub marcos()
	Declare Sub gcls()
	Declare Sub convertir_NC()
	Declare Sub limpia_teclado()
	Declare Sub calcula_radio(X as Single, Y As Single, f As Integer)	
	Declare function boton(X As Integer, Y As Integer, ancho As Integer, texto As String, estado As integer) As integer
	Declare Sub cambiaposiciones(redibuja As Integer)
	Declare Sub evento_grafico()

	' textos para Tiempo, F, S, XYZ reales, y MAX-MIN
	Dim Shared As string Ttext,Ftext,Stext,Xtext,Ytext,Ztext,Xmintxt,Xmaxtxt,Ymintxt,Ymaxtxt,Zmintxt,Zmaxtxt
	
	
	' para temas de resoluciones y factores correctores
	Dim Shared As Integer anchopan,altopan
	Dim Shared As Integer anchopanold,altopanold
	Dim Shared As Integer BORDE=100 ' borde alrededor para dejar espacio al grafico y que no se acerque a los limites de la ventana grafica
	' creacion ventana principal
	Dim Shared As any Ptr hwnd,hwnd2
	Dim Shared As integer evento
	Dim Shared lpTimerFunc As Any Ptr ' refresco de pantalla
	' coordenadas de la ventana principal
	Dim Shared As Integer xpan,ypan
	Dim Shared As Integer xpanold,ypanold
	' medidas ventana grafica
	Dim Shared As Integer anchograf,altograf ' para calcular las medidas del marco grafico, restando los menus y bordes de "anchopan y altopan"
	Dim Shared As Integer resx, resy ' medidas de la resolucion grafica, donde dibujamos (en realidad, una simple copia de anchograf y altograf)
   Dim shared as Any ptr wingraf ' manejador de ventana grafica
   Dim shared as RECT e_wingraf ' evento de reenvio desde la ventana WINDOW9 a la ventana creada en FB con SCREENRES
   Dim shared as BITMAPV4HEADER bmi ' caracteristicas de la ventana grafica creada
	' fuente de texto grande
	Dim Shared As Integer fuente_grande
	' y normalita
	Dim Shared As Integer fuente_normal



