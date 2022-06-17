
#INCLUDE "Protheus.ch"

User Function AGLibFrete
//User Function iEditBrw(cTitulo, aBotoes, aCabec, aDados, nDlgWidth, nDlgHeight, nWidthButton, aColsWidth)
Local oDlg           := Nil
Local oList          := Nil
Local nLoop          := Nil
local lOk            := .F.
Local lChk           := .F.
Local cLine          := Nil
Local cTitulo        := "Liberação de Frete"
Local oOk            := Nil
Local oNo            := Nil

Local cWhere         := Nil
Local aCabec         := Nil
Local aDados         := Nil
Local aRecnos        := Nil
Local aColunas       := Nil
Local aObjs          := Array(10)
Local nTotalMarcado  := 0

Local nLineButton    := Nil
Local nColButton     := Nil
Local nSpaceButton   := 2

Local nWidthButton   :=  40

Local nDlgWidth    := 1100
Local nDlgHeight   :=  560

Private nColCarga  := Nil
Private lEstorna   := .F.   // MV_PAR09 1=Liberar; 2=Estornar
Private cLegButton := Nil

AjustaSX1()

/*
+============================================+
| Parametros utilizados                      |
| Grupo AGLIBFRETE                           |
|                                            |
| MV_PAR01 - Da Carga/Docto Entrada ?  C(15) |
| MV_PAR02 - Ate Carga/Docto Entrada ? C(15) |
| MV_PAR03 - Do Fornecedor ?           C( 6) |
| MV_PAR04 - Loja de ?                 C( 4) |
| MV_PAR05 - Ate Fornecedor ?          C( 6) |
| MV_PAR06 - Loja Ate ?                C( 4) |
| MV_PAR07 - Emissao CTE De ?          D( 8) |
| MV_PAR08 - Emissao CTE Ate ?         D( 8) |
| MV_PAR09 1=Liberar; 2=Estornar       N( 1) |
+============================================+
*/

If ! Pergunte("AGLIBFRETE", .T.)
	Return
Endif

lEstorna := MV_PAR09 == 2

cWhere := " AND F1_XCARGA  BETWEEN '" + Alltrim(MV_PAR01) + "' AND '" + Alltrim(MV_PAR02) + "' " + ;
          " AND F1_FORNECE BETWEEN '" + Alltrim(MV_PAR03) + "' AND '" + Alltrim(MV_PAR05) + "' " + ;
          " AND F1_LOJA    BETWEEN '" + Alltrim(MV_PAR04) + "' AND '" + Alltrim(MV_PAR06) + "' " + ;
          " AND F1_EMISSAO BETWEEN '" + Dtos(   MV_PAR07) + "' AND '" + Dtos(   MV_PAR08) + "' " + ;
          " AND F1_ESPECIE IN ('CTE','CTR') "

If lEstorna
	cWhere += " AND F1_XSTAFRT  = 'S' "
Else
	cWhere += " AND F1_XSTAFRT <> 'S' "
Endif

Processa({|| aCabec := FretAprov(cWhere)}, "Buscando informações de frete")  // FretAprov(cWhere)  // -> {aCab, aDados, aRecnos (SZ6)}

aDados    := aClone(aCabec[2])
aRecnos   := aClone(aCabec[3])
aColunas  := aClone(aCabec[4])
aCabec    := aClone(aCabec[1])

nColCarga := aScan(aColunas, {|z| z[2] == "F1_XCARGA"})

If Len(aDados) <= 0
	MsgInfo("Sem dados para exibir")
	Return
Endif

DEFINE MSDIALOG oDlg TITLE cTitulo From 0,0 to nDlgHeight, nDlgWidth /* of oMainWnd */ PIXEL


nLineButton := __DlgHeight(oDlg) - 24
nColButton  := __DlgWidth( oDlg) -  4

oList := TWBrowse():New( 03, 03, __DlgWidth(oDlg) - 6, __DlgHeight(oDlg) - 30, {|| { NoScroll } }, aCabec, /*aColsWidth*/, oDlg,,,,, {|| If(Len(oList:aArray) > 0, (oList:aArray[oList:nAt, 1] := ! oList:aArray[oList:nAt, 1], AtuTotal(oList, aRecnos, aObjs[2])), (Alert("Nenhum item exibido janela será fechada"), oDlg:End())) },,,,,,,.T.,,.T.,,.F.,,, )
oList:SetArray(aDados)

oOk  := LoadBitmap( GetResources(), "LBOK")
oNo  := LoadBitmap( GetResources(), "LBNO")

cLine := "{ || {If(oList:aArray[oList:nAt,1], oOk, oNo)"

For nLoop := 2 to Len(aCabec)
	cLine += ", oList:aArray[oList:nAt, " + Str(nLoop, 3) + "] "
Next

cLine += "}}"
oList:bLine := &(cLine)

@ nLineButton, 03 CheckBox oChkBox Var  lChk Prompt "Marca/Desmarca Todos" Message "Marca/Desmarca Todos" Size 70, 007 Pixel Of oDlg on Click (aEval(oList:aArray, {|z,w| z[1] := lChk}), oList:Refresh(), AtuTotal(oList, aRecnos, aObjs[2]))

@ nLineButton + 2, 150  SAY   aObjs[1] PROMPT "Total Marcado:" SIZE 070, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ nLineButton    , 190  MSGET aObjs[2] VAR    nTotalMarcado    SIZE 055, 007 OF oDlg COLORS 0, 16777215 PIXEL  When .F.  PICTURE "@E 999,999,999.99"

If lEstorna  // MV_PAR09 1=Liberar; 2=Estornar
	cLegButton := "Estornar"
Else
	cLegButton := "Liberar"
Endif

TBrowseButton():New(nLineButton, nColButton - nWidthButton - (6 * (nWidthButton + nSpaceButton)), "Marca Carga"  , oDlg, {|| MarcaCarga(oList, aRecnos, aObjs[2])}       , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (5 * (nWidthButton + nSpaceButton)), "Dados Boleto" , oDlg, {|| BoletoCarga(oList, aColunas, aRecnos)}      , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (4 * (nWidthButton + nSpaceButton)), "Docto CTE"    , oDlg, {|| VisCTE(aRecnos[oList:nAt])}                 , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (3 * (nWidthButton + nSpaceButton)), "NFs x CTE"    , oDlg, {|| NotasCTE(aRecnos[oList:nAt])}               , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (2 * (nWidthButton + nSpaceButton)), "Romaneios"    , oDlg, {|| ViewRoman(aRecnos[oList:nAt])}              , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (1 * (nWidthButton + nSpaceButton)), cLegButton     , oDlg, {|| aRecnos := LibCte(oList, aRecnos, aObjs[2])}, nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(nLineButton, nColButton - nWidthButton - (0 * (nWidthButton + nSpaceButton)), "Fechar"       , oDlg, {|| lOk := .F., oDlg:End()}                     , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED


Static Function LibCte(oList, aRecnos, oObjTot)
Local aRecMail  := {}
Local aDados    := aClone(oList:aArray)
Local aNewRecn  := {}
Local nLoop     := Nil

If aScan(aDados, {|z| z[1]}) == 0
	MsgInfo("Nenhum item marcado, marque os itens que deseja " + cLegButton)
Else
	oList:aArray := {}
	For nLoop := 1 To Len(aDados)
		If aDados[nLoop, 1]
			SF1->(dbGoto(aRecnos[nLoop]))
			Aadd(aRecMail, aRecnos[nLoop])
			RecLock("SF1", .F.)
			If lEstorna
				SF1->F1_XSTAFRT := " "
				SF1->F1_XAPRVFR := " "
				SF1->F1_XDTAPFR := Stod(" ")
			Else
				SF1->F1_XSTAFRT := "S"
				SF1->F1_XAPRVFR := __cUserId
				SF1->F1_XDTAPFR := dDataBase
			Endif
			MsUnlock()
		Else
			Aadd(aNewRecn, aRecnos[nLoop])
			Aadd(oList:aArray, aDados[nLoop])
		Endif
	Next
	oList:Refresh()
	AtuTotal(oList, aNewRecn, oObjTot)
	If ! lEstorna
		EnviaMail(aRecMail)  // Envia email em segundo plano
		MsgInfo("E-Mails enviados")
	Else
		MsgInfo("Liberações dos itens marcados foram estornados")
	Endif
Endif

Return(aNewRecn)

Static Function BoletoCarga(oList, aColunas, aRecnos)
Local aSavAre    := Nil
Local aObjs      := Array(10)
Local lOk        := .F.
Local nWidthBut  := 38
Local aDados     := Nil
Local nPosCarga  := aScan(aColunas, {|z| z[2] == "F1_XCARGA"})
Local nPosBoleto := aScan(aColunas, {|z| z[2] == "ZV_NUMERO"})
Local nPosValor  := aScan(aColunas, {|z| z[2] == "ZV_VALOR" })
Local cCarga     := Nil
//Local nValor    := Nil
//Local dVencto   := Nil
Local cNumero    := Nil
Local nLoop      := Nil

If Len(aDados := oList:aArray) == 0 .Or. nPosCarga == 0
	MsgInfo("Erro nos dados")
	Return
Endif

cCarga  := aDados[oList:nAt, nPosCarga ]
cNumero := aDados[oList:nAt, nPosBoleto]
nValor  := aDados[oList:nAt, nPosValor ]

If Empty(cCarga)
	MsgInfo("Carga não informada")
	Return
Endif

//dVencto := Posicione("SZV", 1, xFilial("SZV") + cCarga, "ZV_VENCTO")

//cNumero := Posicione("SZV", 1, xFilial("SZV") + cCarga, "ZV_NUMERO")

DEFINE MSDIALOG oDlg TITLE "Boleto de Carga" FROM 000, 000  TO 160, 410 COLORS 0, 16777215 PIXEL

@ 017, 008  SAY   aObjs[1] PROMPT "Carga"         SIZE 036, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 014, 048  MSGET aObjs[2] VAR    cCarga          SIZE 046, 010 OF oDlg COLORS 0, 16777215 PIXEL When .F.

@ 037, 008  SAY   aObjs[1] PROMPT "Número Boleto" SIZE 036, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 034, 048  MSGET aObjs[2] VAR    cNumero         SIZE 050, 010 OF oDlg COLORS 0, 16777215 PIXEL

@ 037, 118  SAY   aObjs[1] PROMPT "Valor Boleto" SIZE 036, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 034, 153  MSGET aObjs[2] VAR    nValor         SIZE 050, 010 OF oDlg COLORS 0, 16777215 PIXEL Picture "@E 999,999,999.99"

/*
@ 037, 113  SAY   aObjs[3] PROMPT "Vencimento"   SIZE 036, 007 OF oDlg COLORS 0, 16777215 PIXEL
@ 034, 148  MSGET aObjs[4] VAR    dVencto        SIZE 050, 010 OF oDlg COLORS 0, 16777215 PIXEL
*/

TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (2 * (nWidthBut + 2)), "Cancela" , oDlg,{|| lOk := .F., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 26, __DlgWidth( oDlg) -  4 - (1 * (nWidthBut + 2)), "Confirma", oDlg,{|| lOk := .T., oDlg:End()},nWidthBut,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

If lOk
	aSavAre := {SZV->(GetArea()), GetArea()}
	dbSelectArea("SZV")
	If ! dbSeek(xFilial("SZV") + cCarga)
		RecLock("SZV", .T.)
		SZV->ZV_FILIAL := xFilial("SZV")
		SZV->ZV_CARGA  := cCarga
		SZV->ZV_USUA   := __cUserId
	Else
		RecLock("SZV", .F.)
	Endif
//	SZV->ZV_VENCTO := dVencto
	SZV->ZV_VALOR  := nValor
	SZV->ZV_NUMERO := cNumero
	MsUnlock()

	For nLoop := 1 To Len(oList:aArray)
		If oList:aArray[nLoop, nColCarga] == cCarga
			oList:aArray[nLoop, nPosValor ] := nValor
			oList:aArray[nLoop, nPosBoleto] := cNumero
		Endif
	Next
	RestArea(aSavAre[1])
	RestArea(aSavAre[2])
Endif

Return



Static Function MarcaCarga(oList, aRecnos, oObj)
Local cCarga := oList:aArray[oList:nAt, nColCarga]
Local nLoop  := Nil

For nLoop := 1 To Len(oList:aArray)
	If oList:aArray[nLoop, nColCarga] == cCarga
		oList:aArray[nLoop, 1] := .T.
	Endif
Next

AtuTotal(oList, aRecnos, oObj)
Return


Static Function AtuTotal(oList, aRecnos, oObjTot)
Local nTotal := 0
Local nLoop  := Nil
Local aArray := oList:aArray

For nLoop := 1 To Len(aArray)
	If aArray[nLoop, 1]
		SF1->(dbGoto(aRecnos[nLoop]))
		nTotal += SF1->F1_VALBRUT
	Endif
Next

Eval(oObjTot:bSetGet, nTotal)
oObjTot:Refresh()
Return

Static Function VisCTE(nReg)
Private aRotina := {{"", "AxPesqui"   , 0, 1, 0}, ;
                    {"", "A103NFiscal", 0, 2, 0}}
SF1->(dbGoto(nReg))

Processa({|| A103NFiscal("SF1",nReg,2)}, "Abrindo Documento de Entrada")
Return

Static Function NotasCTE(nReg)
SF1->(dbGoto(nReg))

Private cEspecie := SF1->F1_ESPECIE
Private INCLUI   := .F.
Private ALTERA   := .T.

U_AGNtFrete(SF1->F1_XCARGA)

Return

Static Function EnviaMail(aSF1Recnos)

U_AGMailFrt(,, aSF1Recnos)
//StartJob("U_AGMailFrt", GetEnvServer(), .F., cEmpAnt,cFilAnt, aSF1Recnos)
Return

User Function AGMailFrt(cEmpPar, cFilPar, aSF1Recnos)
Local nLoop   := Nil
Local aCargas := {}
Local lJob    := Nil

If (lJob := Select("SX3") == 0)
	RpcSetType( 3 )
	RpcSetEnv(cEmpPar, cFilPar,,,GetEnvServer(),, {})
Endif

For nLoop := 1 to Len(aSF1Recnos)
	SF1->(dbGoto(aSF1Recnos[nLoop]))
	If aScan(aCargas, {|z| z == SF1->F1_XCARGA}) == 0
		U_AGSndFrete(SF1->F1_XCARGA, aSF1Recnos)  // Mando todos os recnos e filtro pela carga e pelos recnos
		Aadd(aCargas, SF1->F1_XCARGA)
	Endif
	RecLock("SF1", .F.)
	SF1->F1_XMAILFR := "S"
	MsUnlock()
Next

If lJob
	RpcClearEnv(.F.)
Endif
Return

Static Function FretAprov(cWhere)  // -> {aCab, aDados, aRecnos}
Static nCount   := 1
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()
Local aColunas  := Nil
Local aCab      := {}
Local aDados    := {}
Local aRecnos   := {}
Local bLine     := Nil

Default cWhere  := ""

If (! Empty(cWhere)) .And. Left(Upper(Ltrim(cWhere)), 4) <> "AND "
	cWhere := " AND " + cWhere
Endif

cQuery := " SELECT "                                                                                                 + ;
          " F1_DOC, F1_SERIE, F1_EMISSAO, F1_FORNECE, A2_NOME, "                                                     + ;
          " F1_VALBRUT, F1_ORIGLAN, F1_ESPECIE, F1_XAPRVFR, "                                                        + ;
          " F1_XCARGA, F1_XSTAFRT, F1_XDTAPFR, F1_XMAILFR, "                                                         + ;
          " COALESCE(ZV_VALOR, 0) ZV_VALOR, COALESCE(ZV_VENCTO, '') ZV_VENCTO, COALESCE(ZV_NUMERO, ' ') ZV_NUMERO, " + ;
          " SF1.R_E_C_N_O_ F1_RECNO "                                                                                + ;
          " FROM " + RetSqlName("SF1") + " SF1 "                                                                     + ;
          " LEFT JOIN " + RetSqlName("SA2") + " SA2 ON F1_FORNECE = A2_COD AND F1_LOJA = A2_LOJA "                   + ;
		  "           AND A2_FILIAL = '" + xFilial("SA2") + "' AND SA2.D_E_L_E_T_ = ' ' "                            + ;
          " LEFT JOIN " + RetSqlName("SZV") + " SZV ON ZV_CARGA = F1_XCARGA "                                        + ;
		  "           AND ZV_FILIAL = '" + xFilial("SZV") + "' AND SZV.D_E_L_E_T_ = ' ' "                            + ;
          " WHERE "                                                                                                  + ;
          " F1_FILIAL = '" + xFilial("SF1") + "' AND SF1.D_E_L_E_T_ = ' ' "                                          + ;
            cWhere + " "                                                                                             + ;
          " ORDER BY F1_XCARGA "


dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

TcSetField(cAliasTop, "F1_EMISSAO", "D", 8, 0)

aColunas  := {{""            , ".F."        }, ;
              {"Carga"       , "F1_XCARGA"  }, ;
              {"Nº Boleto"   , "ZV_NUMERO"  }, ;
              {"Vr Boleto"   , "ZV_VALOR"   }, ;
              {"Nota Frete"  , "F1_DOC"     }, ;
              {"Serie"       , "F1_SERIE"   }, ;
		      {"Emissao"     , "F1_EMISSAO" }, ;
		      {"Valor CTE"   , "F1_VALBRUT" }, ;
		      {"Fornecedor"  , "F1_FORNECE" }, ;
		      {"Razão Social", "A2_NOME"    }}

bLine    := "{|| {"
aEval(aColunas, {|z,w| Aadd(aCab, z[1])})
aEval(aColunas, {|z,w| bLine += If(w > 1, ", ", "") + z[2]   })
bLine += "}}"
bLine := &(bLine)

Do While ! Eof()
	Aadd(aDados  , Eval(bLine))
	Aadd(aRecnos , F1_RECNO   )
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return({aCab, aDados, aRecnos, aColunas})



Static Function ViewRoman(nF1Recno)
//User Function iEditBrw(cTitulo, aBotoes, aCabec, aDados, nDlgWidth, nDlgHeight, nWidthButton, aColsWidth)
Local oDlg           := Nil
Local oList          := Nil
local lOk            := .F.
Local cTitulo        := "Romaneios da Carga"
Local aCabec         := Nil
Local aDados         := Nil
Local aRecnos        := Nil

Local nLineButton    := Nil
Local nColButton     := Nil
Local nSpaceButton   := 2

Local nWidthButton   :=  36

Local nDlgWidth    :=  900
Local nDlgHeight   :=  460

SF1->(dbGoto(nF1Recno))

If "-" $ SF1->F1_XCARGA
	MsgInfo("Para carga fracionada não existe romaneio")
	Return
Endif

Processa({|| aCabec := BuscaRoman(SF1->F1_XCARGA)}, "Buscando informações de romaneios")

aDados  := aClone(aCabec[2])
aRecnos := aClone(aCabec[3])
aCabec  := aClone(aCabec[1])

If Len(aDados) <= 0
	MsgInfo("Sem dados para exibir")
	Return
Endif

DEFINE MSDIALOG oDlg TITLE cTitulo From 0,0 to nDlgHeight, nDlgWidth /* of oMainWnd */ PIXEL

nLineButton := __DlgHeight(oDlg) - 24
nColButton  := __DlgWidth( oDlg) -  4

oList := TWBrowse():New( 03, 03, __DlgWidth(oDlg) - 6, __DlgHeight(oDlg) - 30, {|| { NoScroll } }, aCabec, /*aColsWidth*/, oDlg,,,,, {|| oList:aArray[oList:nAt, 1] := ! oList:aArray[oList:nAt, 1] },,,,,,,.T.,,.T.,,.F.,,, )
oList:SetArray(aDados)

oList:bLine := {|| oList:aArray[oList:nAt] }

TBrowseButton():New(nLineButton, nColButton - nWidthButton - (0 * (nWidthButton + nSpaceButton)), "Fechar" , oDlg, {|| lOk := .F., oDlg:End()}       , nWidthButton,10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

Return



Static Function BuscaRoman(cCarga)
Static nCount   := 1
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()
Local aColunas  := Nil
Local aCab      := {}
Local aDados    := {}
Local aRecnos   := {}
Local bLine     := Nil

cQuery := " SELECT "                                         + ;
          " DISTINCT "                                       + ;
          " ZR_CODIGO, ZR_CARGA, ZR_VLFRETE, ZR_NOMECLI, "   + ;
          " ZR_MUN, ZR_EST, R_E_C_N_O_ ZR_RECNO "            + ;
          " FROM " + RetSqlName("SZR") + " "                 + ;
          " WHERE ZR_CARGA = '" + cCarga + "' "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

TcSetField(cAliasTop, "ZR_VLFRETE", "N", 14, 2)

aColunas := {{"Cód. Romaneio" , "ZR_CODIGO" }, ;
             {"Num. da Carga" , "ZR_CARGA"  }, ;
             {"Vr. Frete"     , "ZR_VLFRETE"}, ;
             {"Nome Cliente"  , "ZR_NOMECLI"}, ;
             {"Municipio"     , "ZR_MUN"    }, ;
             {"Estado"        , "ZR_EST"    }}

bLine    := "{|| {"
aEval(aColunas, {|z,w| Aadd(aCab, z[1])})
aEval(aColunas, {|z,w| bLine += If(w > 1, ", ", "") + z[2]   })
bLine += "}}"
bLine := &(bLine)

Do While ! Eof()
	Aadd(aDados  , Eval(bLine))
	Aadd(aRecnos , ZR_RECNO   )
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return({aCab, aDados, aRecnos, aColunas})



Static Function AjustaSX1(cPerg)
Local aRegs := {}

Default cPerg := "AGLIBFRETE"

Aadd(aRegs,{cPerg, "01", "Da Carga/Docto Entrada ?    ", "Da Carga/Docto Entrada      ", "Da Carga/Docto Entrada      ", "mv_ch1", "C", 15, 0, 0, "G", "", "MV_PAR01", "       ", "       ", "       ", "          ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "   "})
Aadd(aRegs,{cPerg, "02", "Ate Carga/Docto Entrada ?   ", "Ate Carga/Docto Entrada     ", "Ate Carga/Docto Entrada     ", "mv_ch2", "C", 15, 0, 0, "G", "", "MV_PAR02", "       ", "       ", "       ", "999999    ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "   "})
Aadd(aRegs,{cPerg, "03", "Do Fornecedor ?             ", "¿De Proveedor ?             ", "From Supplier ?             ", "mv_ch3", "C",  6, 0, 0, "G", "", "MV_PAR03", "       ", "       ", "       ", "          ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "SA2", "S", "001"})
Aadd(aRegs,{cPerg, "04", "Loja de ?                   ", "                            ", "                            ", "mv_ch4", "C",  4, 0, 0, "G", "", "MV_PAR04", "       ", "       ", "       ", "          ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "002"})
Aadd(aRegs,{cPerg, "05", "Ate Fornecedor ?            ", "Ate Fornecedor ?            ", "Ate Fornecedor ?            ", "mv_ch5", "C",  6, 0, 0, "G", "", "MV_PAR05", "       ", "       ", "       ", "999999    ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "SA2", "S", "001"})
Aadd(aRegs,{cPerg, "06", "Loja Ate ?                  ", "Loja Ate ?                  ", "Loja Ate ?                  ", "mv_ch6", "C",  4, 0, 0, "G", "", "MV_PAR06", "       ", "       ", "       ", "9999      ", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "002"})
Aadd(aRegs,{cPerg, "07", "Emissao CTE De ?            ", "Emissao CTE De ?            ", "Emissao CTE De ?            ", "mv_ch7", "D",  8, 0, 0, "G", "", "MV_PAR07", "       ", "       ", "       ", "01/01/2022", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "   "})
Aadd(aRegs,{cPerg, "08", "Emissao CTE Ate ?           ", "Emissao CTE Ate ?           ", "Emissao CTE Ate ?           ", "mv_ch8", "D",  8, 0, 0, "G", "", "MV_PAR08", "       ", "       ", "       ", "31/12/2030", "", "        ", "        ", "        ", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "   "})
Aadd(aRegs,{cPerg, "09", "Liberar/Estornar Liberação ?", "Liberar/Estornar Liberação ?", "Liberar/Estornar Liberação ?", "mv_ch9", "N",  1, 0, 0, "C", "", "MV_PAR09", "Liberar", "Liberar", "Liberar", "          ", "", "Estornar", "Estornar", "Estornar", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "   ", "S", "   "})

LValidPerg(aRegs)
Return


Static FUNCTION lValidPerg( aRegs, aNewSx1Stru )
LOCAL aArea		:= GetArea()
LOCAL nARegs	:= Len( aRegs )
LOCAL aSx1Stru  := SX1->( DbStruct() )
LOCAL nI
LOCAL nPos
LOCAL cSx1Field
LOCAL nTamSX1   := Len(SX1->X1_GRUPO)
//Estrutura do SX1 da Versao 6.09
DEFAULT aNewSx1Stru := { 	;
'X1_GRUPO  ', 'X1_ORDEM  ', 'X1_PERGUNT', 'X1_PERSPA ', 'X1_PERENG ', 'X1_VARIAVL', 'X1_TIPO   ', ;
'X1_TAMANHO', 'X1_DECIMAL', 'X1_PRESEL ', 'X1_GSC    ', 'X1_VALID  ', 'X1_VAR01  ', 'X1_DEF01  ', ;
'X1_DEFSPA1', 'X1_DEFENG1', 'X1_CNT01  ', 'X1_VAR02  ', 'X1_DEF02  ', 'X1_DEFSPA2', 'X1_DEFENG2', ;
'X1_CNT02  ', 'X1_VAR03  ', 'X1_DEF03  ', 'X1_DEFSPA3', 'X1_DEFENG3', 'X1_CNT03  ', 'X1_VAR04  ', ;
'X1_DEF04  ', 'X1_DEFSPA4', 'X1_DEFENG4', 'X1_CNT04  ', 'X1_VAR05  ', 'X1_DEF05  ', 'X1_DEFSPA5', ;
'X1_DEFENG5', 'X1_CNT05  ', 'X1_F3     ', 'X1_PYME   ', 'X1_GRPSXG '  }

SX1->(DbSetOrder(1))
For nI := 1 to nARegs
	If !SX1->(dbSeek(PADR(aRegs[nI][1],nTamSX1)+aRegs[nI][2]))
		RecLock('SX1',.T.)
		Aeval( aNewSx1Stru, { |cField, nJ| cSx1Field := cField, ;
		Iif( ( nPos := Ascan( aSx1Stru, { |aField| aField[1] == ;
		Rtrim( cSx1Field ) } ) ) > 0, ;
		Iif(Len(aRegs[nI])>=nJ,FieldPut( nPos, aRegs[nI][nJ] ),Nil), NIL ) } )
		MsUnlock()
	Endif
Next nI
RestArea( aArea )
Return NIL



