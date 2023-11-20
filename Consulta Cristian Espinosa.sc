
//Crear una función con nombre (integracion) que use el método de Simpson para
//calcular el valor aproximado de cualquier función . Se recomienda analizar lo
//siguiente:
//1. El tipo de dato que devolverá la función integracion
//El tipo de dato que devolvera la funcion es de tipo Double
//2. ¿Cuáles son los parámetros que recibe la función?
//Reciben como parametro:
// - Una funcion que devuelve un doble
// - a: Es un parámetro de tipo Double
// - b: Es un parámetro de tipo Double

def integral(f: Double => Double, a:Double, b:Double): Double =
  (b-a) * (( f(a) + (4 * f((a+b)/2)) + f(b) )/6)

// Calcula el error que se presenta en cada aproximación. El error es igual al valor
// absoluto de la resta entre el valor esperado y el valor obtenido, así:
// |valorEsperado - valorObtenido|
// Para calcular el error, debes elaborar una función que haga el cálculo respectivo

def calcularError(valEsp: Double, valObt:Double): Double =
  math.abs(valEsp- valObt)

//Aproxima el valor de las siguientes integrales definidas usando la función
//integracion creada en el paso 1.

// Primera Integral
val fn1: Double => Double = (x: Double) => -math.pow(x,2) + 8*x - 12
val res1: Double = integral(fn1, 3, 5)
val valEsp1: Double = 7.33
val error1: Double = calcularError(valEsp1, res1)


// Segunda Integral
val fn2: Double => Double  = (x: Double) => (3 * math.pow(x,2))
val res2: Double = integral(fn2, 0, 2)
val valEsp2: Double = 8
val error2: Double = calcularError(valEsp2, res2)


// Tercera Integral
val fn3: Double => Double  = (x:Double) => (x + (2 * math.pow(x,2)) - math.pow(x,3)  + ( 5 * math.pow(x, 4)))
val res3: Double =integral(fn3, -1, 1)
val valEsp3: Double = 3.333
val error3: Double = calcularError(valEsp3, res3)


// Cuarta Integral
val fn4: Double => Double  =(x:Double) => ((2*x) + 1) / ((math.pow(x, 2)) + x)
val res4: Double =integral(fn4,1,2)
val valEsp4: Double = 1.09861
val error4:  Double = calcularError(valEsp4, res4)


// Quinta Integral
val fn5: Double => Double  =(x:Double) => math.pow(Math.E, x)
val res5: Double =integral(fn5,0,1)
val valEsp5: Double = 1.71828
val error5 : Double = calcularError(valEsp5, res5)


// Sexta Integral
val fn6: Double => Double  =(x:Double) => 1 / math.sqrt(x -1)
val res6: Double =integral(fn6, 2,3)
val valEsp6: Double = 0.82842
val error6: Double = calcularError(valEsp6, res6)


// Septima Integral
val fn7: Double => Double  = (x:Double) =>  1 / (1 + math.pow(x,2))
val res7: Double = integral(fn7, 0,1)
val valEsp7: Double = 0.785398
val error7 : Double = calcularError(valEsp7, res7)
