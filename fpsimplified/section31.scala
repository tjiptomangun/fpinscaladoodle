//A Quick review of Case Classes

case class Person (
	firstName: String,
	lastName: String,
	age: Int
)

val emily1 = Person("Emily", "Maness", 25)

val emily2 = emily1.copy(lastName = "Wells", age = 26);

case class BillingInfo (creditCards: Seq[CreditCard]);

case class Name (
	firstName: String,
	mi: String,
	lastName: String
)

case class User (
	id: Int,
	name: Name,
	billingInfo: BillingInfo,
	phone: String,
	email: String
)

case class CreditCard(
	name: Name,
	number: String,
	month: Int,
	year: Int,
	cvv: String 
)

val hannahsName = Name (
	firstName = "Hannah",
	mi = "C",
	lastName = "Jones"
)

val hannah1 = User(
	id = 1,
	name = hannahsName,
	phone = "907-555-1212",
	email = "hannah@hannahjones.com"
	billingInfo(
		creditCards = Seq(
			CreditCard (
				name = hannahsName,
				number = "1111111111",
				month = 3,
				year = 2020,
				cvv = "123"
			)
		)
	)
)

val hannah2 = hannah1.copy(phone = "710-1213-3222");
val newName = hannah2.name.copy(lastName = "Smith");
val hannah3 = hannah2.copy(name = newName);
val oldCC = hannah2.billingInfo.creditCards(0);
val newCC = oldCC.copy(new = newName);
val newCCs = Seq(newCC);
val hannah4 = hannah3.copy(billingInfo = BillingInfo(newCCs));
