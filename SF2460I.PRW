
#INCLUDE "Protheus.ch"


User Function P3Estorna
Local aArrSD2 := Nil

If Len(aArrSD2 := CheckTerc()) > 0
	EstornaRequ()
Endif
Return


User Function SF2460I
Local aArrSD2 := Nil

If Len(aArrSD2 := CheckTerc()) > 0
	TransfAlmox(aArrSD2)
Endif
Return

Static Function CheckTerc()  // SF2 posicionado
Local aArrSD2 := {}

If SF2->F2_TIPO <> "B" .Or. (! U_AGRetLogis(SF2->F2_CLIENTE, SF2->F2_LOJA))
	Return(aArrSD2)
Endif

//ItensNota(cNum, cSerie, dEmissao, cCliente, cLoja)
aArrSD2 := ItensNota(SF2->F2_DOC, SF2->F2_SERIE, SF2->F2_EMISSAO, SF2->F2_CLIENTE, SF2->F2_LOJA)

Return(aArrSD2)



Static Function TransfAlmox(aArrSD2)
Static cAlmoxDest := Nil
Static cLocaliz   := Nil
Static cTipoMov   := Nil
Static lTrfVal    := Nil
Local nLoop       := Nil
Local cD3Doc      := Nil
Local aD3Recnos   := Nil
Local aCab        := Nil
Local aAuto       := Nil
Local aCabSDA     := Nil
Local aItemSDB    := Nil
Local cObs        := Nil
Local aConfig     := Nil

Private lMsErroAuto := .F.

SD2->(dbGoto(aArrSD2[1]))

aConfig := U_AGRetLogis(SD2->D2_CLIENTE, SD2->D2_LOJA, .T.) // {ZI_FORNECE, ZI_LOJA, ZI_LOCDEST, ZI_LOCALIZ, ZI_TPMOVIM}

cAlmoxDest := aConfig[3]
cLocaliz   := aConfig[4]
cTipoMov   := aConfig[5]

lTrfVal    := Posicione("SF5",1,xFilial("SF5") + cTipoMov, "F5_VAL") == "S"

cD3Doc     := NextSeq()

aCab       := {{"D3_DOC"     , cD3Doc            , Nil },;
               {"D3_TM"      , cTipoMov          , Nil },;
               {"D3_EMISSAO" , dDataBase         , Nil }}

aAuto := {}

For nLoop := 1 To Len(aArrSD2)
	SD2->(dbGoto(aArrSD2[nLoop]))

	CriaSB2(SD2->D2_COD, cAlmoxDest)
	// PARA RECUPERAR REGISTROS PARA ESTORNO
	// AND SUBSTRING(D3_OBSERVA, 9, 12) = '000040120-1  '
	cObs := "P3-Nota " + SD2->D2_DOC + "-" + SD2->D2_SERIE
	Aadd(aAuto, {})
	Aadd(Atail(aAuto), {"D3_COD"     , SD2->D2_COD       , Nil })
	Aadd(Atail(aAuto), {"D3_UM"      , SD2->D2_UM        , Nil })
	Aadd(Atail(aAuto), {"D3_QUANT"   , SD2->D2_QUANT     , Nil })
	Aadd(Atail(aAuto), {"D3_CF"      , "DE9"             , Nil })
	Aadd(Atail(aAuto), {"D3_LOCAL"   , cAlmoxDest        , Nil })
	Aadd(Atail(aAuto), {"D3_LOTECTL" , SD2->D2_LOTECTL   , Nil })
	Aadd(Atail(aAuto), {"D3_NUMLOTE" , SD2->D2_NUMLOTE   , Nil })
	Aadd(Atail(aAuto), {"D3_DTVALID" , SD2->D2_DTVALID   , Nil })

	If Localiza(SD2->D2_COD)
		Aadd(Atail(aAuto), {"D3_LOCALIZ" , cLocaliz          , Nil })
	Endif

	Aadd(Atail(aAuto), {"D3_OBSERVA" , cObs              , Nil })

	If lTrfVal
		Aadd(Atail(aAuto), {"D3_CUSTO1" , SD2->D2_CUSTO1 , Nil })
	Endif

Next

lMsErroAuto := .F.
MSExecAuto({|x,y,z| Mata241(x,y,z)},aCab,aAuto,3)

If lMsErroAuto
	MostraErro()
	Return
Endif

aD3Recnos := SD3_Recnos(cD3Doc, cObs)

For nLoop := 1 To Len(aD3Recnos)
	SD3->(dbGoto(aD3Recnos[nLoop]))

	//Cabecalho com a informaçãoo do item e NumSeq que sera endereçado.
	aCabSDA := {{"DA_PRODUTO" , SD3->D3_COD   , Nil},;
	            {"DA_NUMSEQ"  , SD3->D3_NUMSEQ, Nil}}

	//Dados do item que será endereçado
	aItemSDB := {{"DB_ITEM"     , "0001"        , Nil},;
	             {"DB_ESTORNO"  , " "           , Nil},;
	             {"DB_LOCALIZ"  , cLocaliz      , Nil},;
	             {"DB_DATA"     , dDataBase     , Nil},;
	             {"DB_QUANT"    , SD3->D3_QUANT , Nil}}

	lMsErroAuto := .F.
	//Executa o endere?amento do item
	MATA265( aCabSDA, {aItemSDB}, 3)

	If lMsErroAuto
	    MostraErro()
		Return
	Endif

Next

Return


Static Function NextSeq
Static nCount   := 1
Local nLast     := 0
Local nTam      := Nil
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()

nTam  := TamSx3("D3_DOC")[1] - 2

cQuery := " SELECT "                                     + ;
          " MAX(SUBSTRING(D3_DOC, 3, 9)) LAST_DOC "      + ;
          " FROM " + RetSqlName("SD3") + " "             + ;
          " WHERE D_E_L_E_T_ = ' '  "                    + ;
          " AND D3_FILIAL = '" + xFilial("SD3") + "' "   + ;
          " AND D3_DOC LIKE 'P3%' "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

nLast := Val(Alltrim(LAST_DOC)) + 1

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return("P3" + StrZero(nLast, nTam))




Static Function ItensNota(cNum, cSerie, dEmissao, cCliente, cLoja)
Local aRecnos   := {}
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "")
Local aSavAre   := GetArea()

cQuery := " SELECT SD2.R_E_C_N_O_ D2_RECNO "                                                                         + ;
          " FROM " + RetSqlName("SD2") + " SD2 "                                                                     + ;
          " LEFT JOIN " + RetSqlName("SF4") + " SF4 ON F4_CODIGO = D2_TES "                                          + ;
          " WHERE "                                                                                                  + ;
          "     SD2.D_E_L_E_T_ = ' '  AND D2_FILIAL = '" + xFilial("SD2") + "' "                                     + ;
          " AND SF4.D_E_L_E_T_ = ' '  AND F4_FILIAL = '" + xFilial("SF4") + "' "                                     + ;
          " AND F4_ESTOQUE = 'S' AND F4_PODER3 = 'R' "                                                               + ;
          " AND D2_DOC = '" + cNum + "' AND D2_SERIE = '" + cSerie + "' AND D2_EMISSAO = '" + Dtos(dEmissao) + "' "  + ;
          " AND D2_CLIENTE = '" + cCliente + "' AND D2_LOJA = '" + cLoja + "' "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

Do While ! Eof()
	Aadd(aRecnos, D2_RECNO)
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)

Return(aRecnos)


Static Function EstornaRequ()
Static nCount   := 1
Local aCab      := Nil
Local aAuto     := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local cQuery    := Nil

cQuery := " SELECT R_E_C_N_O_ D3_RECNO FROM " + RetSqlName("SD3") + " "               + ;
          " WHERE D_E_L_E_T_ = ' '  AND D3_FILIAL = '" + xFilial("SD3") + "' "        + ;
          " AND D3_ESTORNO <> 'S' "                                                   + ;
          " AND D3_DOC LIKE 'P3%' AND UPPER(D3_OBSERVA) LIKE 'P3-NOTA%' "             + ;
          " AND SUBSTRING(D3_OBSERVA, 9, 12) = '" + SF2->F2_DOC + "-" + SF2->F2_SERIE + "' "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

Do While ! Eof()
	SD3->(dbGoto((cAliasTop)->D3_RECNO))
	If aCab == Nil
		aAuto := {}
		aCab  := {{"D3_DOC"     , SD3->D3_DOC    , Nil}, ;
                  {"D3_TM"      , SD3->D3_TM     , Nil},;
                  {"D3_EMISSAO" , SD3->D3_EMISSAO, Nil}}
	Endif

	Aadd(aAuto, {})
	Aadd(Atail(aAuto), {"D3_COD"     , SD3->D3_COD       , Nil })
	Aadd(Atail(aAuto), {"D3_UM"      , SD3->D3_UM        , Nil })
	Aadd(Atail(aAuto), {"D3_QUANT"   , SD3->D3_QUANT     , Nil })
	Aadd(Atail(aAuto), {"D3_CF"      , SD3->D3_CF        , Nil })
	Aadd(Atail(aAuto), {"D3_LOCAL"   , SD3->D3_LOCAL     , Nil })
	Aadd(Atail(aAuto), {"D3_LOTECTL" , SD3->D3_LOTECTL   , Nil })
	Aadd(Atail(aAuto), {"D3_NUMLOTE" , SD3->D3_NUMLOTE   , Nil })
	Aadd(Atail(aAuto), {"D3_DTVALID" , SD3->D3_DTVALID   , Nil })
	Aadd(Atail(aAuto), {"D3_LOCALIZ" , SD3->D3_LOCALIZ   , Nil })
	dbSelectArea(cAliasTop)
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

lMsErroAuto := .F.

MSExecAuto({|x,y,z| MATA241(x,y,z)}, aCab, aAuto, 6)  // Estorno

If lMsErroAuto
	MostraErro()
Endif

Return

Static Function SD3_Recnos(cD3Doc, cObs)
Static nCount   := 1
Local aSavAre   := GetArea()
Local aRecnos   := {}
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local cQuery    := Nil

cQuery := " SELECT R_E_C_N_O_ D3_RECNO FROM " + RetSqlName("SD3") + " "               + ;
          " WHERE D_E_L_E_T_ = ' '  AND D3_FILIAL = '" + xFilial("SD3") + "' "        + ;
          " AND D3_ESTORNO <> 'S' "                                                   + ;
          " AND D3_DOC = '" + cD3Doc + "' "                                           + ;
          " AND D3_OBSERVA = '" + cObs + "' "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

Do While ! Eof()
	Aadd(aRecnos, (cAliasTop)->D3_RECNO)
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)

Return(aRecnos)


Static Function MySoma1(cNum, nParteFixa)
Local aSoma := Nil
Local cRet  := Nil

Default nParteFixa := 1

If Empty(cNum)
	Return(Strzero(1, Len(cNum)))
Endif

aSoma := {Left(cNum, nParteFixa), Alltrim(Substr(cNum, nParteFixa + 1))}

cRet  := Pad(aSoma[1] + StrZero(Val(aSoma[2]) + 1, Len(aSoma[2])), Len(cNum))

Return(cRet)

