package phonenumber

object PhoneNumber {

  def clean(n: String): Option[String] = {
    val digits = n.filter(c => c.isDigit)
    val body = if (digits.head == '1') digits.tail else digits
    if (
      body.length == 10 && body.head != '0' && !List('0', '1').contains(body(3))
    ) Option(body)
    else None
  }

}
