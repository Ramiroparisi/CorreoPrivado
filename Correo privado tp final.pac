| package |
package := Package name: 'Correo privado tp final'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Certificada;
	add: #Correo;
	add: #Destinatario;
	add: #Destino;
	add: #Encomienda;
	add: #Envio;
	add: #Simple;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Correo
	instanceVariableNames: 'envios destinos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Destinatario
	instanceVariableNames: 'nombre direccion destino'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Destino
	instanceVariableNames: 'codPostal pais localidad importeDestino'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Envio
	instanceVariableNames: 'num fecha destinatario importe'
	classVariableNames: 'Num'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Envio subclass: #Certificada
	instanceVariableNames: ''
	classVariableNames: 'Monto'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Envio subclass: #Encomienda
	instanceVariableNames: 'peso'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Envio subclass: #Simple
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Correo guid: (GUID fromString: '{9ed1f0a2-0910-41b5-b1f4-95817fa1f1f1}')!
Correo comment: ''!
!Correo categoriesForClass!Kernel-Objects! !
!Correo methodsFor!

agregaDestino: destino
destinos add: destino.!

inicializaDes
destinos := OrderedCollection new.


!

inicializaEnvios

envios := OrderedCollection new.
!

listado
|fecha listaFecha listaOrdenada tot|
tot := 0.
fecha := (Prompter prompt: 'Ingrese fecha dd/mm/aaaa') asDate.
listaFecha := envios select: [:i | fecha = (i oFecha) & true = (i esEncomienda)].
listaFecha isEmpty
  ifTrue: [Transcript show: 'No hay encomiendas.']
  ifFalse: [
    listaOrdenada := listaFecha asSortedCollection: [:i :j | i oImporte > j oImporte].
    listaOrdenada do: [:i | i muestraParaListado. tot := tot + (i oImporte).].
    Transcript show: 'El total de las encomiendas es: ', tot printString.
  ].!

nuevoEnvio
| op envio resp destinatario localidad dest codPostal |
resp := true.
[ resp ] whileTrue: [
	localidad := Prompter prompt: 'Ingrese localidad del destinatario'.
	codPostal := Prompter prompt: 'Ingrese localidad del destinatario'.
	dest := destinos detect: [:x | (x oLocalidad = localidad) & (x oCodPostal = codPostal) ] ifNone: [nil].
	(dest isNil) ifTrue: [Transcript show: 'Destino no valido';cr]
			       ifFalse: [
		op := (Prompter prompt:'Ingrese tipo: 1- Cerificada 2- Simple  3-Encomienda') asNumber asInteger.
                [(op < 3) | (op > 1)] whileFalse: [Transcript show: 'Opcion incorrecta';cr.
		op := (Prompter prompt:'Ingrese tipo: 1- Cerificada 2- Simple  3-Encomienda') asNumber asInteger.]. 
		( op = 1 ) ifTrue:[ envio := Certificada new. ].
		( op = 2 ) ifTrue:[ envio := Simple new.].
		(op = 3 ) ifTrue:[ envio := Encomienda new.].
		destinatario := Destinatario new.
		destinatario cargaDatos: dest.
		envio cargaDatos: dest and: destinatario.
		envios add: envio.
		resp := (Prompter prompt: 'Cargar otro envio (s/n)?') asUppercase = 'S'. ]].! !
!Correo categoriesForMethods!
agregaDestino:!public! !
inicializaDes!public! !
inicializaEnvios!public! !
listado!public! !
nuevoEnvio!public! !
!

Destinatario guid: (GUID fromString: '{e3142074-5b2d-488c-9d3d-6c9aea63e368}')!
Destinatario comment: ''!
!Destinatario categoriesForClass!Kernel-Objects! !
!Destinatario methodsFor!

cargaDatos: des

nombre := Prompter prompt: 'Ingrese nombre del destinatario'.
direccion := Prompter prompt: 'Ingrese direccion del destinatario'.
destino := des.! !
!Destinatario categoriesForMethods!
cargaDatos:!public! !
!

Destino guid: (GUID fromString: '{b68b062a-b98b-47ad-8564-382a8086afd6}')!
Destino comment: ''!
!Destino categoriesForClass!Kernel-Objects! !
!Destino methodsFor!

cargaDatos
codPostal := Prompter prompt: 'Ingrese codigo postal del destino:'.
pais := Prompter prompt: 'Ingrese pais de destino:'.
localidad := Prompter prompt: 'Ingrese localidad de destino:'.
importeDestino := (Prompter prompt: 'Ingrese importe del destino:') asNumber.
!

codPostal: unString
codPostal:= unString.!

importeDestino: unNumero
importeDestino:=unNumero.
!

localidad: unString
localidad:=unString.!

oCodPostal

^codPostal.!

oImporteDestino

^importeDestino.!

oLocalidad
^localidad!

oPais

^pais.!

pais: unString
pais:=unString.! !
!Destino categoriesForMethods!
cargaDatos!public! !
codPostal:!public! !
importeDestino:!public! !
localidad:!public! !
oCodPostal!public! !
oImporteDestino!public! !
oLocalidad!public! !
oPais!public! !
pais:!public! !
!

Envio guid: (GUID fromString: '{492ac10b-9046-4ebd-826a-86bd6af1bbdc}')!
Envio comment: ''!
!Envio categoriesForClass!Kernel-Objects! !
!Envio methodsFor!

cargaDatos: destino and: destin

fecha := Date today.
Num := Num + 1.
num := Num.
destinatario := destin.
importe := destino oImporteDestino.
 

!

esEncomienda

^false!

muestraParaListado
Transcript show: num printString; tab;
show: importe printString; cr.!

oFecha
	^fecha!

oImporte
	^importe! !
!Envio categoriesForMethods!
cargaDatos:and:!public! !
esEncomienda!public! !
muestraParaListado!public! !
oFecha!public! !
oImporte!public! !
!

!Envio class methodsFor!

inicializaNum

Num := 1.! !
!Envio class categoriesForMethods!
inicializaNum!public! !
!

Certificada guid: (GUID fromString: '{016e9cd4-4a3e-4e90-99d7-1662b6c2df03}')!
Certificada comment: ''!
!Certificada categoriesForClass!Kernel-Objects! !
!Certificada methodsFor!

cargaDatos: desti

super cargaDatos: desti.
importe := importe + Monto.
! !
!Certificada categoriesForMethods!
cargaDatos:!public! !
!

!Certificada class methodsFor!

Monto
 ^Monto.!

Monto: unNumber
	Monto := unNumber! !
!Certificada class categoriesForMethods!
Monto!public! !
Monto:!public! !
!

Encomienda guid: (GUID fromString: '{8dd06950-db67-4d3d-a301-57ac6211b20d}')!
Encomienda comment: ''!
!Encomienda categoriesForClass!Kernel-Objects! !
!Encomienda methodsFor!

cargaDatos: desti
super cargaDatos: desti. 
peso := (Prompter prompt: 'Ingrese peso de la encomienda') asNumber asFloat.
                [(peso <= 15) | (peso >= 0)] whileFalse: [Transcript show: 'Peso no valido';cr.
		peso := (Prompter prompt: 'Ingrese peso de la encomienda') asNumber asFloat.]. 
		peso <= 1 ifTrue: [importe = importe * 1.02.]
		  ifFalse: [peso <= 5 ifTrue: [importe = importe *1.05.]
		            ifFalse: [peso <= 10 ifTrue: [importe = importe *1.07.]
                                 ifFalse: [importe = importe *1.1.]. 
					].
				] .!

esEncomienda

^true! !
!Encomienda categoriesForMethods!
cargaDatos:!public! !
esEncomienda!public! !
!

Simple guid: (GUID fromString: '{c9fa60c3-901a-41b3-b0c5-80dba7a9a519}')!
Simple comment: ''!
!Simple categoriesForClass!Kernel-Objects! !
!Simple methodsFor!

cargaDatos: desti

super cargaDatos: desti.
! !
!Simple categoriesForMethods!
cargaDatos:!public! !
!

"Binary Globals"!

