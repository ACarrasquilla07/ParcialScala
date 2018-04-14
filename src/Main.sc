//Parcial Andres Carrasquilla

trait Tipo {
  type T
  def value: T
}

object Cheques extends Tipo {
  type T = String
  def value: T = "Cheques"
}

object Ahorros extends Tipo {
  type T = String
  def value: T = "Ahorros"
}

object Fiduciaria extends Tipo {
  type T = String
  def value: T = "Fiduciaria"
}



trait Estado {
  type Est
  def value: Est
}

object Nueva extends Estado {
  type Est = String
  def value: Est = "Nueva"
}

object Activa extends Estado {
  type Est = String
  def value: Est = "Activa"
}

object Cerrada extends Estado {
  type Est = String
  def value: Est = "Cerrada"
}

case class Cuenta (numeroCuenta: Int,
                   codigoPropietario: Int,
                   valorCuenta: Long,
                   tipoCuenta: Tipo,
                   estado: Estado,
                   estadoTransaccion: Boolean)

case class Transferencia(cuentaADebitar: Cuenta,
                         cuentaAConsignar: Cuenta,
                         estadoTransferencia: Boolean)

def debitar(cuenta: Cuenta, valorDebitar: Int): Cuenta ={
  if(cuenta.valorCuenta >= valorDebitar &&
    cuenta.estado == Activa){
    new Cuenta(cuenta.numeroCuenta ,
      cuenta.codigoPropietario,
      cuenta.valorCuenta - valorDebitar,
      cuenta.tipoCuenta,
      cuenta.estado, true)
  }
  else{
    new Cuenta(cuenta.numeroCuenta ,
      cuenta.codigoPropietario,
      cuenta.valorCuenta,
      cuenta.tipoCuenta,
      cuenta.estado, false)
  }

}

def consignar(cuenta: Cuenta, valorCosignacion: Int): Cuenta ={
  if(cuenta.estado == Cerrada){
    new Cuenta(cuenta.numeroCuenta,
      cuenta.codigoPropietario,
      cuenta.valorCuenta + valorCosignacion,
      cuenta.tipoCuenta, cuenta.estado, false)
  }
  else {
    new Cuenta(cuenta.numeroCuenta,
      cuenta.codigoPropietario,
      cuenta.valorCuenta + valorCosignacion,
      cuenta.tipoCuenta, Activa, true)
  }
}

def transferir(cuentaADebitar: Cuenta,
               cuentaAConsignar: Cuenta,
               valorTransferencia: Int): Transferencia ={
  val debito = debitar(cuentaADebitar, valorTransferencia)
  if(debito.estadoTransaccion &&
    cuentaADebitar.estado == Activa &&
    cuentaAConsignar.estado == Activa
  ) {
    val consign = consignar(cuentaAConsignar, valorTransferencia)
    new Transferencia(debito, consign, true)
  } else{
    new Transferencia(cuentaADebitar, cuentaAConsignar, false)
  }
}

def obtenerBalance(cuenta: Cuenta): Long ={
  cuenta.valorCuenta
}

def crearCuenta(numeroCuenta: Int,
                idPropietario: Int,
                tipoCuenta: Tipo): Cuenta ={
    new Cuenta (numeroCuenta,
      idPropietario,
      0,
      tipoCuenta,
      Nueva,
      false)
}

def cerrarCuenta(cuenta: Cuenta): Cuenta ={
  new Cuenta(cuenta.numeroCuenta ,
    cuenta.codigoPropietario,
    cuenta.valorCuenta,
    cuenta.tipoCuenta,
    Cerrada, false)
}

