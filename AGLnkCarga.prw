
#INCLUDE "Protheus.ch"

Static aLinkCarga := Nil

User Function ACGetCarga(nAcao, cVar, cSet)  // nAcao 0=Reset/Init; 1=Set (se cSet informado) ou retorna atual
Local nPos := Nil

If aLinkCarga == Nil .Or. nAcao == 0
	aLinkCarga := {{"TipoCarga", ""}, ;  // F=Fracionada; R=Rateada
	               {"Numero"   , ""}}    // Num Carga OU Docto Saida-Serie
Endif

// Se cVar for numerico, considera posicao do array

If cVar == Nil
	Return("")
Endif

If ValType(cVar) == "C"
	nPos := aScan(aLinkCarga, {|z| Upper(z[1]) == Upper(cVar)})
Else
	nPos := cVar
Endif

If nPos == 0 .Or. nPos > Len(aLinkCarga)
	Return("")
Endif

If cSet == Nil .Or. nAcao == Nil .Or. nAcao # 1
	Return(aLinkCarga[nPos, 2])
Else
	aLinkCarga[nPos, 2] := cSet
	Return(cSet)
Endif

Return("")

User Function AGLnkCarga(lNoAsk, lAtuField)
Local oDlg           := Nil
Local lOk            := .T.
Local nCombo         := 1
Local oCombo         := Nil
Local cSet           := Nil
Local cLinkCarga     := Nil
Local nLineButton    := Nil
Local nColButton     := Nil
Local nSpaceButton   := 2

Local nWidthButton   :=  42
Local nDlgWidth      := 320
Local nDlgHeight     := 140

Default lNoAsk       := .F.
Default lAtuField    := .F.

If (! Empty(cEspecie)) .And. (! Alltrim(cEspecie) $ "CTE;CTR")
	U_ACGetCarga(0)
	Return(.T.)
Endif

If ((! INCLUI) .And. (! Empty(SF1->F1_XCARGA))) .Or. lAtuField
	U_ACGetCarga(1, 1, If("-" $ SF1->F1_XCARGA, "F", "R"))  // cVar 1=Tipo Carga / 2=Fracionado
	U_ACGetCarga(1, 2, SF1->F1_XCARGA)
	If lNoAsk .And. !  Empty(SF1->F1_XCARGA)
		Return(.T.)
	Endif
Endif

nCombo := If(U_ACGetCarga(, 1) == "F", 2, 1)

DEFINE MSDIALOG oDlg TITLE "Tipo de Carga" From 0,0 to nDlgHeight, nDlgWidth  PIXEL

nLineButton := __DlgHeight(oDlg) - 26
nColButton  := __DlgWidth( oDlg) -  4

@ 12,  18 Say   "Selecione o Tipo de Carga"      Size 300,   6 Of oDlg Pixel

@ 26, 18 COMBOBOX oCombo VAR nCombo ITEMS {"Com Rateio", "Fracionada"} On Change (nCombo := oCombo:nAt) SIZE 90,006 OF oDlg PIXEL

oCombo:nAt := nCombo

TBrowseButton():New(nLineButton    , nColButton - (2 * (nWidthButton + nSpaceButton)), "Cancela"   , oDlg,{|| lOk := .F., oDlg:End()},nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton    , nColButton - (1 * (nWidthButton + nSpaceButton)), "Prosseguir", oDlg,{|| lOk := .T., oDlg:End()},nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

If lOk .And. nCombo > 0
	// ACGetCarga(nAcao, cVar, cSet)  //   // nAcao 0=Reset/Init; 1=Set (se cSet informado) ou retorna atual

	cSet := If(nCombo == 1, "R", "F")
	If ! cSet == U_ACGetCarga(, 1)
		U_ACGetCarga(1, 2, "")  // Se trocou tipo de carga, reseto documento
	Endif
	U_ACGetCarga(1, 1, cSet)
	If nCombo == 1  // Rateio
		GetCarga()
	Else            // Fracionada
		GetNFSaida()
	Endif
Endif

If lAtuField .And. (! INCLUI)
	cLinkCarga := U_ACGetCarga(, 2)
	If (! Empty(cLinkCarga))
		RecLock("SF1", .F.)
		SF1->F1_XCARGA := cLinkCarga
		MsUnlock()
	Endif
Endif

Return(.T.)

Static Function GetCarga
Local aObjs     := Array(10)
Local lOk       := .F.
Local cCarga    := Space(6)
Local nWidthBut := 38

cCarga := Pad(U_ACGetCarga(, 2), 6)

DEFINE MSDIALOG oDlg TITLE "Identificação da Carga" FROM 000, 000  TO 130, 270 COLORS 0, 16777215 PIXEL

@ 025, 008  SAY   aObjs[1] PROMPT "Número da Carga" SIZE 070, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 022, 058  MSGET aObjs[2] VAR    cCarga            SIZE 016, 010 OF oDlg COLORS 0, 16777215 PIXEL

TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (2 * (nWidthBut + 2)), "Cancela" , oDlg,{|| lOk := .F., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (1 * (nWidthBut + 2)), "Confirma", oDlg,{|| lOk := .T., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

If lOk
	U_ACGetCarga(1, 2, cCarga)
Endif

Return


Static Function GetNFSaida
Local aObjs     := Array(10)
Local lOk       := .F.
Local cNota     := ""
Local cSerie    := ""
Local nTraco    := Nil
Local nWidthBut := 38

cSerie := U_ACGetCarga(, 2)

If ! Empty(cSerie)
	If (nTraco := Rat("-", cSerie)) > 0
		cNota  := Left(  cSerie, nTraco - 1)
		cSerie := Substr(cSerie, nTraco + 1)
	Endif
Endif

cNota  := Pad(cNota , Len(SF2->F2_DOC  ))
cSerie := Pad(cSerie, Len(SF2->F2_SERIE))


DEFINE MSDIALOG oDlg TITLE "Carga Fracionada - Docto Saída" FROM 000, 000  TO 150, 270 COLORS 0, 16777215 PIXEL

@ 017, 008  SAY   aObjs[1] PROMPT "Documento Saída"   SIZE 070, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 014, 058  MSGET aObjs[2] VAR    cNota               SIZE 014, 010 OF oDlg COLORS 0, 16777215 PIXEL

@ 037, 008  SAY   aObjs[1] PROMPT "Série Nota Fiscal" SIZE 070, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 034, 058  MSGET aObjs[2] VAR    cSerie              SIZE 004, 010 OF oDlg COLORS 0, 16777215 PIXEL

TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (2 * (nWidthBut + 2)), "Cancela" , oDlg,{|| lOk := .F., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (1 * (nWidthBut + 2)), "Confirma", oDlg,{|| lOk := .T., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

If lOk
	U_ACGetCarga(1, 2, Alltrim(cNota) + "-" + Alltrim(cSerie))
Endif

Return











User Function AGSqlSeek(cAlias, aChave, cSeek)
Static nCount   := 1
Local cQuery    := Nil
Local nScan     := Nil
Local nLoop     := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()
Local lAchou    := .F.
Local aStru     := (cAlias)->(dbStruct())

cQuery := " SELECT "                                          + ;
          " R_E_C_N_O_ SQL_RECNO "                                + ;
          " FROM " + RetSqlName(Upper(Alltrim(cAlias))) + " " + ;
          " WHERE "                                           + ;
          " D_E_L_E_T_ = ' ' "

If (nScan := aScan(aStru, {|z| "_FILIAL" $ z[1]})) > 0
	cQuery += " AND " + aStru[nScan, 1] + " = '" + xFilial(cAlias) + "' "
Endif

For nLoop := 1 To Len(aChave)
	If (nScan := aScan(aStru, {|z| z[1] == aChave[nLoop]})) > 0
		cQuery += " AND " + aStru[nScan, 1] + " = '" + Left(cSeek, aStru[nScan, 3]) + "' "
		cSeek  := Substr(cSeek, aStru[nScan, 3] + 1)
	Endif
Next

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

If (lAchou := ! Eof())
	(cAlias)->(dbGoto((cAliasTop)->SQL_RECNO))
Endif

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return(lAchou)


User Function AGQry2Recnos(cAlias, cWhere, cOrderBy)
Static nCount   := 1
Local aSavAre   := GetArea()
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local cQry    := Nil
Local nLoop   := Nil
Local aStru     := (cAlias)->(dbStruct())
Local aRecnos := {}
Local aWhere  := {}

If cWhere <> Nil .And. (! Empty(cWhere))
	Aadd(aWhere, cWhere)
Endif

Aadd(aWhere, " D_E_L_E_T_ = ' ' ")

If (nScan := aScan(aStru, {|z| "_FILIAL" $ z[1]})) > 0
	Aadd(aQuery, aStru[nScan, 1] + " = '" + xFilial(cAlias) + "' ")
Endif

cQry := " SELECT R_E_C_N_O_ FROM " + RetSqlName(cAlias) + " "

For nLoop := 1 to Len(aWhere)
	If nLoop == 1
		cQry += " WHERE "
	Else
		cQry += " AND "
	Endif
	cQry += aWhere[nLoop]
Next

If cOrderBy <> Nil .And. (! Empty(cOrderBy))
	cQry += " ORDER BY " + cOrderBy + " "
Endif

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

Do While ! Eof()
	Aadd(aRecnos, R_E_C_N_O_)
	dbSkip()
Enddo

(cAlias)->(dbCloseArea())

RestArea(aSavAre)

Return(aRecnos)


User Function AGCargToNotas(cCarga)
Static nCount   := 1
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aRecnos   := {}
Local aSavAre   := GetArea()

If "-" $ cCarga  // Carga fracionada
	cCarga := Pad(cCarga, Len(SF2->F2_DOC + SF2->F2_SERIE) + 1)
	cQuery := "  SELECT "                                                                                   + ;
	          "  SF2.R_E_C_N_O_ F2_RECNO, F2_DOC, F2_SERIE, F2_CLIENTE, F2_LOJA, F2_EMISSAO "               + ;
	          "  FROM " + RetSqlName("SF2") + " SF2 "                                                       + ;
	          "  WHERE "                                                                                    + ;
	          "  SF2.D_E_L_E_T_ = ' '  AND F2_FILIAL = '" + xFilial("SF2") + "' "                           + ;
	          "  AND F2_DOC + '-' + F2_SERIE = '" + cCarga + "' "
Else // Carga rateada
	cQuery := " SELECT DISTINCT "                                                                               + ;
	          " SF2.R_E_C_N_O_ F2_RECNO, ZR_CARGA, ZR_CODIGO, ZQ_CLIENTE, ZQ_LOJA, "                            + ;
	          " C9_NFISCAL, C9_SERIENF "                                                                        + ;
	          " FROM " + RetSqlName("SZR") + " SZR "                                                            + ;
	          " RIGHT JOIN " + RetSqlName("SZQ") + " SZQ ON ZQ_ROMAN = ZR_CODIGO "                              + ;
	          "                            AND ZQ_FILIAL = '" + xFilial("SZQ") + "' "                           + ;
	          "                            AND SZQ.D_E_L_E_T_ = ' ' "                                           + ;
	          " LEFT JOIN " + RetSqlName("SC9") + "  SC9 ON ZQ_ROMAN = C9_ROMAN "                               + ;
	          "                            AND ZQ_PEDIDO = C9_PEDIDO "                                          + ;
	          "                            AND C9_FILIAL = '" + xFilial("SC9") + "' "                           + ;
	          "                            AND SC9.D_E_L_E_T_ = ' ' "                                           + ;
	          " RIGHT JOIN " + RetSqlName("SF2") + " SF2 ON F2_DOC = C9_NFISCAL AND F2_SERIE = C9_SERIENF "     + ;
	          "                            AND F2_CLIENTE = ZQ_CLIENTE AND F2_LOJA = ZQ_LOJA "                  + ;
	          "                            AND SF2.D_E_L_E_T_ = ' ' "                                           + ;
	          "                            AND F2_FILIAL = '" + xFilial("SF2") + "' "                           + ;
	          " WHERE ZR_FILIAL = '" + xFilial("SZR") + "' AND SZR.D_E_L_E_T_ = ' ' "                           + ;
	          "       AND ZR_CARGA = '" + cCarga + "' "
Endif

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

Do While ! Eof()
	Aadd(aRecnos, F2_RECNO)
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return(aRecnos)

/*
User Function AGArrRoman(cCarga)
Local aNotas := Nil
Local cNotas := ""
Local nLoop  := Nil
Local cQuery := Nil

Processa({|| aNotas := U_AGCargToNotas(cCarga)}, "Buscando romaneios")

aEval(aNotas, {|z| SF2->(dbGoto(z)), cNotas += If(Empty(cNotas), ",", "") + "'" + SF2->F2_DOC + "-" + SF2->F2_SERIE})

IF A
*/
