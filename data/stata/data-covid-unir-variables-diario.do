********************************************************************************
**************************** GERESA Cusco **************************************
********************** Dirección de Epidemiología ******************************
***************** Generar datos para la sala situacional V2 ********************
********************************************************************************

********************************************************************************
* 0. Pasos previos para juntar las bases de datos: Notificación y SISCOVID

* Definir el directorio de trabajo actual

*global path "/Users/bran/Documents/GitHub/dashboard-covid-19/data"
global path "D:\Documents\GitHub\dashboard-covid-19\data"
	global main "$path"
	global stata "$path/stata"
set more off, permanent

********************************************************************************

*** 1. Pasos previos

use "${stata}/data_dashboard.dta", clear
drop ubigeo

** Quitar acentos a nombres de distritos
replace distrito = ustrregexra( ustrnormalize( distrito, "nfd" ) , "\p{Mark}", "" )

** Combinar distritos y ubigeo
merge m:1 distrito using "${stata}/ubigeo.dta"

** Generar diagnosticados en otras regionaes
gen dis_temp = distrito if ubigeo !=""
drop distrito
rename dis_temp distrito
replace distrito = "OTRO" if distrito == ""
replace provincia = "OTRO" if provincia == ""
replace departamento = "OTRO" if departamento == ""

replace ubigeo = "999999" if ubigeo == ""
replace provincia_ubigeo = "9999" if provincia_ubigeo == ""
replace departamento_ubigeo = "99" if departamento_ubigeo == ""
format fecha_resultado fecha_inicio fecha_recuperado %tdCCYY-NN-DD

** Generar variable identificadora/de conteo
*gen var_id = _n
gen var_count = 1

********************************************************************************

*** 2. Generar indicadores de interés ***

*** Indicadores con fecha_resultado
drop if fecha_resultado < 21986

* 2.1 Casos positivos totales
gen positivo = 1 if  positivo_molecular == 1 | positivo_rapida == 1 | positivo_antigenica == 1

* 2.2 Casos positivos prueba rapida
replace positivo_rapida =. if positivo_rapida==0

* 2.2 Casos positivos prueba molecular
replace positivo_molecular =. if positivo_molecular==0

* 2.3 Casos positivos prueba antigenica
replace positivo_antigenica =. if positivo_antigenica==0

* 2.4 Muestras totales
gen muestra = var_count

* 2.5 Muestras molecular por día
gen muestra_molecular = 1 if tipo_prueba == 1

* 2.6 Muestras rápida por día
gen muestra_rapida = 1 if tipo_prueba == 2

* 2.7 Muestras antigenicas por día
gen muestra_antigenica = 1 if tipo_prueba == 3

* 2.8 Recuperados por día
gen recuperado = 1 if fecha_recuperado !=.

* 2.9 Sintomáticos por día 
gen sintomaticos = 1 if sintomatico == 1

* 2.10 Defunciones 
gen defunciones = 1 if defuncion == 1

tempfile ind
save "`ind'" // Guardar indicadores si fecha_resultado < 21986

*** Indicadores con fecha inicio
keep if fecha_inicio >= 21980

* 2.11 Inicio de síntoma
gen inicio = 1 if positivo == 1 & fecha_inicio !=.

* 2.12 Inicio de síntoma molecular
gen inicio_molecular = 1 if positivo_molecular == 1 & fecha_inicio !=.

* 2.13 Inicio de síntoma prueba rápida
gen inicio_rapida = 1 if positivo_molecular == 1 & fecha_inicio !=.

* 2.13 Inicio de síntoma prueba antigenica
gen inicio_antigenica = 1 if positivo_antigenica == 1 & fecha_inicio !=.

tempfile ind2
save "`ind2'" // Guardar indicadores si fecha_inicio >= 21980

********************************************************************************

*** 3. Exportar ***

** 3.1 Exportar a nivel distrital
use "`ind'"
* Colapsar fecha resultado
collapse (first) provincia_ubigeo departamento_ubigeo distrito provincia departamento (count) positivo positivo_rapida positivo_molecular positivo_antigenica muestra muestra_rapida muestra_molecular muestra_antigenica sintomaticos defunciones, by(ubigeo fecha_resultado)
sort ubigeo fecha_resultado
rename fecha_resultado fecha

tempfile dis_fecha_resultado
save "`dis_fecha_resultado'"

* Colapsar fecha_recuperado
use "`ind'"
collapse (first) provincia_ubigeo departamento_ubigeo distrito provincia departamento (count) recuperado, by(ubigeo fecha_recuperado)
sort ubigeo fecha_recuperado
rename fecha_recuperado fecha

tempfile dis_fecha_recuperado
save "`dis_fecha_recuperado'"

* Colapsar fecha_inicio
use "`ind2'"
collapse (first) provincia_ubigeo departamento_ubigeo distrito provincia departamento (count) inicio inicio_molecular inicio_rapida inicio_antigenica, by(ubigeo fecha_inicio)
sort ubigeo fecha_inicio
rename fecha_inicio fecha

tempfile dis_fecha_inicio
save "`dis_fecha_inicio'"

* Merge
use "`dis_fecha_resultado'"
merge 1:1 fecha ubigeo using "`dis_fecha_inicio'", nogenerate update replace
merge 1:1 fecha ubigeo using "`dis_fecha_recuperado'", nogenerate update replace
sort ubigeo fecha
* Generar cumulativos
bysort ubigeo: gen total_positivo = sum(positivo)
bysort ubigeo: gen total_positivo_rapida = sum(positivo_rapida)
bysort ubigeo: gen total_positivo_molecular = sum(positivo_molecular)
bysort ubigeo: gen total_positivo_antigenica = sum(positivo_antigenica)
bysort ubigeo: gen total_muestra = sum(muestra)
bysort ubigeo: gen total_muestra_rapida = sum(muestra_rapida)
bysort ubigeo: gen total_muestra_molecular = sum(muestra_molecular)
bysort ubigeo: gen total_muestra_antigenica = sum(muestra_antigenica)
bysort ubigeo: gen total_recuperado = sum(recuperado)
bysort ubigeo: gen total_sintomaticos = sum(sintomaticos)
bysort ubigeo: gen total_defunciones = sum(defunciones)
bysort ubigeo: gen total_inicio = sum(inicio)
bysort ubigeo: gen total_inicio_molecular = sum(inicio_molecular)
bysort ubigeo: gen total_inicio_rapida = sum(inicio_rapida)
bysort ubigeo: gen total_inicio_antigenica = sum(inicio_antigenica)

** Generar tasa de positividad

gen posi = positivo/muestra
gen posi_rapida = positivo_rapida/muestra_rapida
gen posi_molecular = positivo_molecular/muestra_molecular
gen posi_antigenica = positivo_antigenica/muestra_antigenica

*** Generar primera y segunda ola para variables de interés
encode distrito, gen(dis)
xtset dis fecha 

bysort dis: gen dias = _n

** Total positivo
gen primera_ola_positivo = 195.positivo
gen segunda_ola_positivo = F272.positivo

** Defunciones
gen primera_ola_defunciones = F219.defunciones
gen segunda_ola_defunciones = F310.defunciones

** Tasa de positividad molecular
gen primera_ola_tasamolecular = F188.posi_molecular
gen segunda_ola_tasamolecular = F286.posi_molecular

* Exportar a CSV
sort ubigeo fecha
export delimited using "${main}/data_distrital.csv", replace

**** Exportar en formato wide

*drop provincia_ubigeo departamento_ubigeo provincia distrito departamento total_positivo total_positivo_rapida total_positivo_molecular total_muestra total_muestra_rapida total_muestra_molecular total_recuperado total_sintomaticos total_defunciones total_inicio total_inicio_molecular total_inicio_rapida dis dias primera_ola_positivo segunda_ola_positivo primera_ola_defunciones segunda_ola_defunciones primera_ola_tasamolecular segunda_ola_tasamolecular

*reshape wide positivo positivo_rapida positivo_molecular muestra muestra_rapida muestra_molecular sintomaticos defunciones inicio inicio_molecular inicio_rapida recuperado posi posi_rapida posi_molecular, i(fecha) j(ubigeo) string

*tsset fecha
*tsfill
*export delimited using "${main}/data_distrital_wide.csv", replace

********************************************************************************

** 3.2 Exportar a nivel provincial

*Colapsar fecha_resultado
use "`ind'", clear
collapse (first) provincia (count) positivo positivo_rapida positivo_molecular positivo_antigenica muestra muestra_rapida muestra_molecular muestra_antigenica sintomaticos defunciones, by(fecha_resultado provincia_ubigeo)
sort provincia_ubigeo fecha_resultado
rename fecha_resultado fecha

tempfile prov_fecha_resultado
save "`prov_fecha_resultado'"

*Colapsar fecha_recuperado
use "`ind'"
collapse (first) provincia (count) recuperado, by(fecha_recuperado provincia_ubigeo)
sort provincia_ubigeo fecha_recuperado
rename fecha_recuperado fecha

tempfile prov_fecha_recuperado
save "`prov_fecha_recuperado'"

*Colapsar fecha_inicio
use "`ind2'"
collapse (first) provincia (count) inicio inicio_molecular inicio_rapida inicio_antigenica, by(fecha_inicio provincia_ubigeo)
sort provincia_ubigeo fecha_inicio
rename fecha_inicio fecha

tempfile prov_fecha_inicio
save "`prov_fecha_inicio'"

* Merge
use "`prov_fecha_resultado'"
merge 1:1 fecha provincia_ubigeo using "`prov_fecha_inicio'", nogenerate update replace
merge 1:1 fecha provincia_ubigeo using "`prov_fecha_recuperado'", nogenerate update replace
sort provincia_ubigeo fecha

* Generar cumulativos
bysort provincia_ubigeo : gen total_positivo = sum(positivo)
bysort provincia_ubigeo : gen total_positivo_rapida = sum(positivo_rapida)
bysort provincia_ubigeo : gen total_positivo_molecular = sum(positivo_molecular)
bysort provincia_ubigeo : gen total_positivo_antigenica = sum(positivo_antigenica)
bysort provincia_ubigeo : gen total_muestra = sum(muestra)
bysort provincia_ubigeo : gen total_muestra_rapida = sum(muestra_rapida)
bysort provincia_ubigeo : gen total_muestra_molecular = sum(muestra_molecular)
bysort provincia_ubigeo : gen total_muestra_antigenica = sum(muestra_antigenica)
bysort provincia_ubigeo : gen total_recuperado = sum(recuperado)
bysort provincia_ubigeo : gen total_sintomaticos = sum(sintomaticos)
bysort provincia_ubigeo : gen total_defunciones = sum(defunc)
bysort provincia_ubigeo: gen total_inicio = sum(inicio)
bysort provincia_ubigeo: gen total_inicio_molecular = sum(inicio_molecular)
bysort provincia_ubigeo: gen total_inicio_rapida = sum(inicio_rapida)
bysort provincia_ubigeo: gen total_inicio_antigenica = sum(inicio_antigenica)

** Generar tasa de positividad

gen posi = positivo/muestra
gen posi_rapida = positivo_rapida/muestra_rapida
gen posi_molecular = positivo_molecular/muestra_molecular
gen posi_antigenica = positivo_antigenica/muestra_antigenica

*** Generar primera y segunda ola para variables de interés
encode provincia, gen(prov)
xtset prov fecha 

bysort prov: gen dias = _n

** Total positivo
gen primera_ola_positivo = 195.positivo
gen segunda_ola_positivo = F272.positivo

** Defunciones
gen primera_ola_defunciones = F219.defunciones
gen segunda_ola_defunciones = F310.defunciones

** Tasa de positividad molecular
gen primera_ola_tasamolecular = F188.posi_molecular
gen segunda_ola_tasamolecular = F286.posi_molecular


* Exportar a CSV
export delimited using "${main}/data_provincial.csv", replace

**** Exportar en formato wide
*replace provincia = subinstr(provincia, " ", "", .)

*drop provincia_ubigeo total_positivo total_positivo_rapida total_positivo_molecular total_muestra total_muestra_rapida total_muestra_molecular total_recuperado total_sintomaticos total_defunciones total_inicio total_inicio_molecular total_inicio_rapida prov dias primera_ola_positivo segunda_ola_positivo primera_ola_defunciones segunda_ola_defunciones primera_ola_tasamolecular segunda_ola_tasamolecular

*reshape wide positivo positivo_rapida positivo_molecular muestra muestra_rapida muestra_molecular sintomaticos defunciones inicio inicio_molecular inicio_rapida recuperado posi posi_rapida posi_molecular, i(fecha) j(provincia) string

*tsset fecha
*tsfill

*export delimited using "${main}/data_provincial_wide.csv", replace

********************************************************************************

** 3.3 Exportar a nivel regional

*Colapsar  fecha_resultado
use "`ind'", clear
collapse (count) positivo positivo_rapida positivo_molecular positivo_antigenica muestra muestra_rapida muestra_molecular muestra_antigenica sintomaticos defunciones, by(fecha_resultado)
sort fecha_resultado
rename fecha_resultado fecha

tempfile dpto_fecha_resultado
save "`dpto_fecha_resultado'"

*Colapsar  fecha_recuperado

use "`ind'"
collapse (count) recuperado, by(fecha_recuperado )
sort  fecha_recuperado
rename fecha_recuperado fecha

tempfile dpto_fecha_recuperado
save "`dpto_fecha_recuperado'"

*Colapsar fecha_inicio
use "`ind2'"
collapse (count) inicio inicio_molecular inicio_rapida inicio_antigenica, by(fecha_inicio)
sort fecha_inicio
rename fecha_inicio fecha

tempfile dpto_fecha_inicio
save "`dpto_fecha_inicio'"

* Merge
use "`dpto_fecha_resultado'"
merge 1:1 fecha using "`dpto_fecha_inicio'", nogenerate  
merge 1:1 fecha using "`dpto_fecha_recuperado'", nogenerate 
sort fecha

* Generar cumulativos
gen total_positivo = sum(positivo)
gen total_positivo_rapida = sum(positivo_rapida)
gen total_positivo_molecular = sum(positivo_molecular) 
gen total_positivo_antigenica = sum(positivo_antigenica) 
gen total_muestra = sum(muestra)
gen total_muestra_rapida = sum(muestra_rapida)
gen total_muestra_molecular = sum(muestra_molecular)
gen total_muestra_antigenica = sum(muestra_antigenica)
gen total_recuperado = sum(recuperado)
gen total_sintomaticos = sum(sintomaticos)
gen total_defunciones = sum(defunc)
gen total_inicio = sum(inicio)
gen total_inicio_molecular = sum(inicio_molecular)
gen total_inicio_rapida = sum(inicio_rapida)
gen total_inicio_antigenica = sum(inicio_antigenica)

** Generar tasa de positividad

gen posi = positivo/muestra
gen posi_rapida = positivo_rapida/muestra_rapida
gen posi_molecular = positivo_molecular/muestra_molecular
gen posi_antigenica = positivo_antigenica/muestra_antigenica

*** Generar primera y segunda ola para variables de interés
tsset fecha 

gen dias = _n

** Total positivo
gen primera_ola_positivo = 195.positivo
gen segunda_ola_positivo = F272.positivo

** Defunciones
gen primera_ola_defunciones = F219.defunciones
gen segunda_ola_defunciones = F310.defunciones

** Tasa de positividad molecular
gen primera_ola_tasamolecular = F188.posi_molecular
gen segunda_ola_tasamolecular = F286.posi_molecular

* Exportar a CSV
export delimited using "${main}/data_regional.csv", replace

********************************************************************************
