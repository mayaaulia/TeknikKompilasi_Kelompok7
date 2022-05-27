import basic

while True:
	text = input('Kelompok 7>> ')
	if text.strip() == "": continue
	hasil, error = basic.run('<stdin>', text)

	if error:
		print(error.as_string())
	elif hasil:
		if len(hasil.elements) == 1:
			print(repr(hasil.elements[0]))
		else:
			print(repr(hasil))
