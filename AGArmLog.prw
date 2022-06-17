
#INCLUDE "Protheus.ch"

User Function AGArmLog

Local aSavAre := GetArea()

axCadastro("SZI","Armazéns Logísticos",".T.",".T.")

RestArea(aSavAre)

Return

/*
SZI - Armazem Logistico (SZI010 Modo=C)
1  ZI_FILIAL+ZI_FORNECE+ZI_LOJA

Campo           Tipo  Tam  Título        Descrição             SF3   Val. Usuário
----------------------------------------------------------------------------------------------------------------
ZI_FILIAL   01  C       2  Filial        Filial do Sistema
ZI_FORNECE  02  C       6  Fornecedor    Fornecedor            SA2   ExistCpo("SA2")
ZI_LOJA     03  C       4  Loja          Loja
ZI_LOCDEST  04  C       2  Alm. Destino  Almoxarifado Destino  NNR   ExistCpo("NNR")
ZI_LOCALIZ  05  C      15  Localização   Localização           SBE   ExistCpo("SBE",M->ZI_LOCDEST+M->ZI_LOCALIZ)
ZI_TPMOVIM  06  C       3  Tipo Movim.   Tipo Movimento        SF5   ExistCpo("SF5")
*/

User Function AGRetLogis(cFornece, cLoja, lRetArr)
Static aFornec := Nil
Static cBakFil := Nil
Local aSavAre  := Nil
Local nScan    := Nil

If cBakFil == Nil .Or. (! cBakFil == cFilAnt)
	cBakFil := cFilAnt
	aFornec := Nil
Endif

If aFornec == Nil
	aSavAre  := GetArea()
	aFornec  := {}

	dbSelectArea("SZI")
	dbSeek(xFilial("SZI"))

	Do While ! Eof() .And. ZI_FILIAL == xFilial("SZI")
		Aadd(aFornec, {ZI_FORNECE, ZI_LOJA, ZI_LOCDEST, ZI_LOCALIZ, ZI_TPMOVIM})
		dbSkip()
	Enddo
	RestArea(aSavAre)
Endif

If cFornece <> Nil .And. cLoja <> Nil
	nScan := aScan(aFornec, {|z| z[1] == cFornece .And. z[2] == cLoja})

	If lRetArr == Nil
		Return(nScan > 0)
	Else
		If nScan > 0
			Return(aFornec[nScan])
		Else
			Return({"", "", "", "", ""})
		Endif
	Endif
Endif

Return(aFornec)

