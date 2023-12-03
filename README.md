# FB_SIMULANC
FreeBasic Visualizador de programas CNC

Visualizador de programas CNC (formatos varios, como ISO,NC,HNC,DNC, PIM, etc)

Sirve para ver los programas realizados para máquinas CNC y poder medir los radios de los contornos (tanto planos como 3D)

Se necesita la labreria Windows9 de Freebasic ( https://sourceforge.net/projects/guiwindow9/files/ )

Este es un programa que llevo desarrollando los últimos 25 años. Sus inicios datan de 1998. Lo realicé para la primera empresa en la que trabajé, y al principio era solo MSDOS. Con el tiempo, lo fui ampliando y moviendo a diferentes entornos, como Windows. En 2016, lo cambie todo para emplear las librerias "Windows9" de FreeBasic ( https://www.freebasic.net/forum/viewtopic.php?t=17058&hilit=window9 ) y ahora, en 2023, lo dejo para su uso público.

En todos estos años es un programa que vendía a empresas para las que trabajé (como autónomo), y tenía unas rutinas de compensación de herramienta para contornos que permitía mecanizar "lineas cero" tanto 2D como 3D y meter la fresa que se quisiera, y ademas, reducir los radios a fresas menores, pero es programa aún es empleado por antiguos clientes mios, y no puedo incluirlo, por derechos de copia, por lo que he eliminado toda esa parte, dejando restos visibles, como el menú que lo llama.

El programa tiene varias funcionalidades (excepto la ya mencionada de compensación, que he tenido que eliminar), como ver el NC en colores segun los saltos de zonas que lleva dentro, ver los puntos "reales" (coordenadas XYZ) que lo conforman, medir coordenadas (solo las propias del NC, no mide intermedias), medir radios en contornos (2D o 3D) en tiempo real (saca el valor redondeado mas cercano a la posición del ratón), puede mostrar tiempos de mecanizado en CNC si el programa lleva avances tipo "F", podemos hacer lupas (solo una a la vez) para acercarnos a esquinas pequeñas. Permite ver desde difenrentes angulos (pulsando varias veces el mismo icono). Ademas, tiene un modo que permite ir en modo "lento" para ver como se desarrollaria en la vida real, y poder apreciar saltos y cambios de trayectoria.
Por último, lleva un conversor entre sistemas, que convierte de ISO estandar a PIM,HNC,NC,DNC....

(NOTA: NO ME HAGO RESPONSABLE DE POSIBLES ERRORES EN LA CONVERSION. Es OBLIGACION del usuario final, comprobar visualmente las conversiones, por si acaso el conversor ha hecho algo mal. Hay que pensar en la inmensa cantidad de formatos que hay para máquinas CNC. Esta probado en las máquinas de mis clientes, pero no significa que funcione al 100% en otras)

![Imagen simulanc.jpg](https://github.com/jepalza/FB_SIMULANC/blob/main/pantallazo/simulanc.jpg)
