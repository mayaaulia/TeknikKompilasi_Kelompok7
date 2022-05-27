def string_with_arrows(text, pos_awal, pos_akhir):
	result = ''

	# Calculate indices
	index_awal = max(text.rfind('\n', 0, pos_awal.index), 0)
	index_akhir = text.find('\n', index_awal + 1)
	if index_akhir < 0: index_akhir = len(text)
	
	# Generate each line
	banyak_baris = pos_akhir.baris - pos_awal.baris + 1
	for i in range(banyak_baris):
		# Calculate line columns
		garis_kolom = text[index_awal:index_akhir]
		kolom_awal = pos_awal.kolom if i == 0 else 0
		kolom_akhir = pos_akhir.kolom if i == banyak_baris - 1 else len(garis_kolom) - 1

		# Append to result
		result += garis_kolom + '\n'
		result += ' ' * kolom_awal + '^' * (kolom_akhir - kolom_awal)

		# Re-calculate indices
		index_awal = index_akhir
		index_akhir = text.find('\n', index_awal + 1)
		if index_akhir < 0: index_akhir = len(text)

	return result.replace('\t', '')
