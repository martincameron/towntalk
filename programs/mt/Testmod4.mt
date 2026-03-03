
(Test module.)
Module "Testmod4"
	Channels 8
	Sequence 0,1,1,1
	Instrument 1 Name "Bass" Volume 64 Waveform Square   Octave -3 Chorus 256
	Instrument 2 Name "Str2" Volume 64 Waveform Sawtooth Octave -1 Chorus 512
	Instrument 3 Name "Str3" Volume 64 Waveform Sawtooth Octave  0 Chorus 256
	Macro 1 Scale C-D-EF-G-A-B Root G-2
		Note G-2-1840
		Note --------
		Note G-3-1---
		Note --------
		Note G-2-1---
		Note --------
		Note G-3-1---
		Note --------
		Note G-2-1---
		Note --------
		Note G-3-1---
		Note --------
		Note G-2-1---
		Note --------
		Note G-3-1---
		Note --------
		Note E-2-1---
		Note --------
		Note E-3-1---
		Note --------
		Note E-2-1---
		Note --------
		Note E-3-1---
		Note --------
		Note F-2-1---
		Note --------
		Note F-3-1---
		Note --------
		Note F-2-1---
		Note --------
		Note F-3-1---
		Note --------
	Macro 2 Scale C-D-EF-G-A-B Root G-2
		Note G-2-2025
		Note A-3--C01
		Note C-3--C04
		Note G-2--C09
		Note A-3--C10
		Note C-3--C19
		Note G-2--C24
	Macro 3 Scale C-D-EF-G-A-B Root G-2
		Note G-2-3C01
		Note -----C04
		Note -----C09
		Note -----C10
		Note -----C19
		Note -----C24
	Pattern 0
		Row "00 -------- -------- -------- -------- -------- -------- -------- G-2-1---"
		Row "32 -------- -------- -------- -------- -------- -------- -------- G-2-1---"
	Pattern 1
		Row "00 G-2-2--- -------- -------- -------- -------- -------- -------- G-2-1---"
		Row "04 -------- B-3-2--- -------- -------- -------- -------- -------- --------"
		Row "06 -------- -------- D-3-2--- -------- -------- -------- -------- --------"
		Row "10 -------- -------- -------- F-3-2--- -------- -------- -------- --------"
		Row "12 -------- -------- -------- -------- A-4-2--- -------- -------- --------"
		Row "14 -------- -------- -------- -------- -------- C-4-2--- -------- --------"
		Row "16 -------- -------- -------- -------- -------- -------- E-4-2--- --------"
		Row "26 G-4-3--- -------- -------- -------- -------- -------- -------- --------"
		Row "32 -------- -------- -------- -------- -------- -------- -------- G-2-1---"
(End.)
