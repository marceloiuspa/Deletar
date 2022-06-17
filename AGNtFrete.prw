
#INCLUDE "Protheus.ch"

Static aDados    := Nil
Static aBakDados := Nil
Static lAlterou  := .F.
Static lCheckSX6 := Nil

User Function AgRetReset(lReset, lRetDados, lRetAlt, aFixDados)
Default lReset    := .F.
Default lRetDados := .F.
Default lRetAlt   := .F.

If lCheckSX6 == Nil
	lCheckSX6 := .T.
	AjustaSX6({{"MV_EMAPFRT", "C", "", "E-Mails destinatários de informativo de aprovação de pagamento de frete (CTR/CTE)"}, ;
	           {"MV_USAPFRT", "C", "", "Códigos de usuários quue farão aprovação de pagamento de frete (CTR/CTE)"}})
Endif

If aFixDados <> Nil
	aDados := aClone(aFixDados)
	Return
Endif

If lReset
	aDados   := Nil
	lAlterou := .F.
Endif

If lRetDados
	Return(aClone(aDados))
Endif

If lRetAlt
	Return(lAlterou)
Endif

Return

User Function AGNtFrete(cLinkCarga)
Local aCabec       := {"Nota Fiscal", "Serie", "Emissão", "Cliente"}
Local cTitulo      := "Amarração Docto Saída x NF Frete"
Local oDlg         := Nil
Local oList        := Nil
Local nWidthButton :=  40
Local nSpaceButton :=   2

If Empty(cEspecie)
	MsgInfo("Para informar Doctos de Saída associados à Nota de Conhecimento de Frete, informar a Espécie da nota")
	Return
Endif

If ! Alltrim(cEspecie) $ "CTR;CTE"
	MsgInfo("Usar somente para conhecimentos de frete")
	Return
Endif

If cLinkCarga == Nil
	cLinkCarga := U_ACGetCarga(, 2)

	If Empty(cLinkCarga)
		U_AGLnkCarga(.T.)  // Se preenchido não pergunta
		cLinkCarga := U_ACGetCarga(, 2)
	Endif
Endif

If Empty(cLinkCarga)
	MsgInfo("CTE sem definição de Carga/NF Saída")
	Return
Endif

aDados := U_AGGetSZ6(cLinkCarga)

If aDados == Nil .Or. Len(aDados) == 0
	aDados := {{"", "", "", ""}}
Endif

aBakDados := aClone(aDados)

DEFINE MSDIALOG oDlg TITLE cTitulo From 0,0 to 360, 660 PIXEL

oList := TWBrowse():New( 03, 03, __DlgWidth(oDlg) - 6, __DlgHeight(oDlg) - 30, {|| { NoScroll } }, aCabec, /*aColsWidth*/, oDlg,,,,,,,,,,,,.T.,,.T.,,.F.,,, )
oList:SetArray(aDados)

oList:bLine := {|| oList:aArray[oList:nAt] }

TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 3), "Vis. Nota"      , oDlg, {|| VisNota(oList)  }, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 2), "TES/CFOPs"      , oDlg, {|| ListaTES(oList) }, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 1), "Fechar"         , oDlg, {|| oDlg:End()      }, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

Return



Static Function PosicNota(oList, lMsg)
Local cSeek := Nil
Local lRet  := Nil

Default lMsg := .F.

If Len(oList:aArray) == 0 .Or. Empty(cSeek := oList:aArray[oList:nAt, 1] + oList:aArray[oList:nAt, 2])
	If lMsg
		MsgInfo("Nota Inválida. Selecione outra nota")
	Endif
	Return(.F.)
Endif

SF2->(dbSetOrder(1))  //  F2_FILIAL+F2_DOC+F2_SERIE+F2_CLIENTE+F2_LOJA+F2_FORMUL+F2_TIPO
If lRet := SF2->(dbSeek(xFilial("SF2") + cSeek))
	dbSelectArea("SD2")
	dbSetOrder(3)  //  D2_FILIAL+D2_DOC+D2_SERIE+D2_CLIENTE+D2_LOJA+D2_COD+D2_ITEM
	dbSeek(xFilial("SD2")+SF2->F2_DOC+SF2->F2_SERIE+SF2->F2_CLIENTE+SF2->F2_LOJA)
Endif

Return(lRet)

Static Function VisNota(oList)

If PosicNota(oList, .T.)
	A920NFSai("SF2", SF2->(Recno()), 0)
Endif

Return

Static Function SelectNota
Local lOk          := .F.
Local aObjs        := Array(10)
Local oDlg         := Nil
Local cTitulo      := "Seleção de Docto de Saída"
Local nWidthButton :=  36
Local nSpaceButton :=   2

cDoc   := Space(Len(SF2->F2_DOC  ))
cSerie := Space(Len(SF2->F2_SERIE))

DEFINE MSDIALOG oDlg TITLE cTitulo FROM 000, 000  TO 130, 320 PIXEL

@ 025, 008 SAY   aObjs[1] PROMPT "Docto Saída" SIZE 50, 007 OF oDlg PIXEL
@ 022, 040 MSGET aObjs[2] VAR    cDoc          SIZE 40, 010 OF oDlg PIXEL F3 "SF2VEI"

@ 025, 100 SAY   aObjs[3] PROMPT "Série"       SIZE 30, 007 OF oDlg PIXEL
@ 022, 120 MSGET aObjs[4] VAR    cSerie        SIZE 20, 010 OF oDlg PIXEL


TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 2), "Cancela"        , oDlg, {|| lOk := .F., oDlg:End()}, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)
TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 1), "Confirma"       , oDlg, {|| lOk := .T., oDlg:End()}, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED

Return(If(lOk, {cDoc, cSerie}, Nil))

Static Function ListaTES(oListBrw)
Local aDados       := RetTES(oListBrw)
Local aCabec       := {"TES", "CFOP", "Finalidade TES", "Quantidade"}
Local cTitulo      := "TES e CFOPs Doctos de Saída"
Local oDlg         := Nil
Local oList        := Nil
Local nWidthButton :=  40
Local nSpaceButton :=   2

If Len(aDados) == 0
	MsgInfo("Notas não informadas")
	Return
Endif

DEFINE MSDIALOG oDlg TITLE cTitulo From 0,0 to 220, 360 PIXEL

oList := TWBrowse():New( 03, 03, __DlgWidth(oDlg) - 6, __DlgHeight(oDlg) - 30, {|| { NoScroll } }, aCabec, /*aColsWidth*/, oDlg,,,,,,,,,,,,.T.,,.T.,,.F.,,, )
oList:SetArray(aDados)

oList:bLine := {|| oList:aArray[oList:nAt] }

TBrowseButton():New(__DlgHeight(oDlg) - 24, __DlgWidth(oDlg) - 2 - (nWidthButton * 1), "Fecha" , oDlg, {|| oDlg:End()}, nWidthButton - nSpaceButton, 10,,oDlg:oFont,.F.,.T.,.F.,,.F.,,,.F.)

ACTIVATE MSDIALOG oDlg CENTERED




Static Function RetTES(oList)
Static nCount   := 1
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()
Local cNotas    := ""
Local nLoop     := Nil
Local aDados    := {}

For nLoop := 1 To Len(oList:aArray)
	cNotas += If(Empty(cNotas), "", ", ") + "'" + oList:aArray[nLoop, 1] + "-" + oList:aArray[nLoop, 2] + "'"
Next

cQuery := " SELECT D2_TES, D2_CF, F4_TEXTO, COUNT(*) QUANT "                                               + ;
          " FROM " + RetSqlName("SD2") + " "                                                               + ;
          " LEFT JOIN " + RetSqlName("SF4") + " ON F4_CODIGO = D2_TES "                                    + ;
          " WHERE "                                                                                        + ;
          "     " + RetSqlName("SD2") + ".D_E_L_E_T_ = ' '  AND D2_FILIAL = '" + xFilial("SD2") + "' "     + ;
          " AND " + RetSqlName("SF4") + ".D_E_L_E_T_ = ' '  AND F4_FILIAL = '" + xFilial("SF4") + "' "     + ;
          " AND D2_DOC + '-' + D2_SERIE IN (" + cNotas + ") "                                              + ;
          " GROUP BY D2_TES, D2_CF, F4_TEXTO "                                                             + ;
          " ORDER BY D2_TES, D2_CF "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

TcSetField(cAliasTop, "QUANT", "N", 4, 0)

Do While ! Eof()
	Aadd(aDados, {D2_TES, D2_CF, F4_TEXTO, QUANT})
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return(aDados)


/*
User Function AgAtuSZ6()
Local cUpdate := Nil
Local aNotas  := Nil
Local nLoop   := Nil

If Empty(cNFiscal) .Or. Empty(cA100For) .Or. Empty(cLoja)
	Return
Endif

cUpdate := " DELETE FROM " + RetSqlName("SZ6") + " "                                 + ;
           " WHERE Z6_FILIAL = '" + xFilial("SZ6") + "' AND D_E_L_E_T_ = ' ' "       + ;
           " AND Z6_CONHEC   = '" + cNFiscal + "' AND Z6_SERCON = '" + cSerie + "' " + ;
           " AND Z6_FORNECE  = '" + cA100For + "' AND Z6_LOJA   = '" + cLoja + "' "

If TCSQLExec(cUpdate) < 0
   	MsgStop("Erro SQL " + TCSQLError())
EndIf

aNotas := U_AgRetReset(, .T.)

For nLoop := 1 To Len(aNotas)
	RecLock("SZ6", .T.)
	SZ6->Z6_FILIAL  := xFilial("SZ6")
	SZ6->Z6_DOC     := aNotas[nLoop, 1]
	SZ6->Z6_SERIE   := aNotas[nLoop, 2]
	SZ6->Z6_CONHEC  := cNFiscal
	SZ6->Z6_SERCON  := cSerie
	SZ6->Z6_FORNECE := cA100For
	SZ6->Z6_LOJA    := cLoja
	MsUnlock()
Next
Return
*/


User Function AGGetSZ6(cLinkCarga)
Local aSavAre    := GetArea()
Local aRecnosSF2 := {}
Local aNotasSF2  := {}
Local nLoop      := Nil

Default cLinkCarga := U_ACGetCarga(, 2)  // nAcao 0=Reset/Init; 1=Set (se cSet informado) ou retorna atual

aRecnosSF2 := U_AGCargToNotas(cLinkCarga)

If IsInCallStack( "MATA103" )
	If Empty(cNFiscal) .Or. Empty(cA100For) .Or. Empty(cLoja)
		U_AgRetReset(.T.)
		Return(Nil)
	Endif
Endif

dbSelectArea("SF2")

For nLoop := 1 To Len(aRecnosSF2)
	dbGoto(aRecnosSF2[nLoop])
	Aadd(aNotasSF2, {F2_DOC, F2_SERIE, Dtoc(SF2->F2_EMISSAO), Left(Posicione("SA1",1,xFilial("SA1")+SF2->(F2_CLIENTE+F2_LOJA),"A1_NOME"),20)})
Next

RestArea(aSavAre)

If Len(aNotasSF2) == 0
	U_AgRetReset(.T.)
Endif

U_AgRetReset(,,, aNotasSF2)

Return(aNotasSF2)


User Function AGSndFrete(cCarga, aSF1Recnos)
Local cBodyHtml  := Nil
Local cBodyNotas := ""
Local cQryRecnos := ""
Local cQuery     := Nil
Local cAliasTop  := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "")
Local aSavAre    := {SF1->(GetArea()), SA2->(GetArea()), GetArea()}
Local cDestMail  := Alltrim(GetMV("MV_EMAPFRT"))  // "E-Mails destinatários de informativo de aprovação de pagamento de frete (CTR/CTE)"
Local lGerou     := .F.

Default cCarga   := SF1->F1_XCARGA

If Empty(cDestMail) .Or. Empty(cCarga)
	Return
Endif

aEval(aSF1Recnos, {|z| cQryRecnos += If(cQryRecnos == "", "", ", ") + Alltrim(Str(z))})

//'<TD class=Titulo style="FONT-SIZE: 14px; WIDTH: 100%; ' + Chr(13) + Chr(10) + ;
//'FLOAT: right; FONT-WEIGHT: bold; COLOR: #1a3984; TEXT-ALIGN: center?>' + Chr(13) + Chr(10) + ;

//'<BR>Valor do Boleto: ##VALOR## ' + Chr(13) + Chr(10) + ;
//'<BR>Primeiro Vencimento: ##VENCTO## ' + Chr(13) + Chr(10) + ;

cBodyHtml := ;
'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">' + ;
'<HTML><HEAD>' + ;
'<META content="text/html; charset=windows-1252" http-equiv=Content-Type>' + ;
'<META name=GENERATOR content="MSHTML 11.00.9600.19597"></HEAD>' + ;
'<BODY>' + ;
'<P>' + ;
'<DIV>&nbsp;</DIV>' + ;
'<P><IMG style="HEIGHT: 77px; WIDTH: 239px" ' + ;
'src="https://static.wixstatic.com/media/b486f5_082d02ceb80e479a94274cb04eae64a3.png/v1/fill/w_155,h_48,al_c/b486f5_082d02ceb80e479a94274cb04eae64a3.png" ' + ;
'width=155 height=47> </P>' + ;
'<P><FONT color=#1a3984 size=5 face=Calibri>Aprovacao de Pagamento de Frete a ' + ;
'Fornecedor de Transporte <BR>Aprovado por: ##APROVADOR## </FONT></P>' + ;
'<P><FONT size=5 face=Calibri><FONT size=5 face=Calibri>Carga: ##CARGA## ' + ;
'<BR>Numero do Boleto: ##BOLETO## <BR></FONT></FONT><FONT size=5' + ;
'face=Calibri><FONT size=5 face=Calibri>Valor: ##VALOR## </FONT></FONT></P><FONT' + ;
'size=5 face=Calibri><FONT size=5 face=Calibri></FONT>' + ;
'<P><FONT size=5 face=Calibri>Documentos CTEs:</FONT> </P>' + ;
'<DIV id=itens class=itens ' + ;
'style="BORDER-TOP: rgb(26,57,132) 1px solid; HEIGHT: auto; BORDER-RIGHT: rgb(26,57,132) 1px solid; BORDER-BOTTOM: rgb(26,57,132) 1px solid; PADDING-BOTTOM: 4px; PADDING-TOP: 4px; PADDING-LEFT: 4px; MARGIN: -45px 2px 0px; BORDER-LEFT: rgb(26,57,132) 1px solid; PADDING-RIGHT: 4px; BACKGROUND-COLOR: rgb(244,244,244); box-shadow: 1px 2px 4px rgba(117,117,117,0.3); -moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3); -webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3)"> ' + ;
'' + ;
'<TABLE class=tabitens ' + ;
'style="FONT-SIZE: 12px; HEIGHT: 1px; BORDER-TOP-COLOR: rgb(153,153,153); BORDER-LEFT-COLOR: rgb(153,153,153); COLOR: rgb(79,79,79); BORDER-BOTTOM-COLOR: rgb(153,153,153); TEXT-ALIGN: center; BORDER-RIGHT-COLOR: rgb(153,153,153)" ' + ;
'cellSpacing=0 width="100%" border=1>' + ;
'  <TBODY>' + ;
'  <TR id=tablecabec>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="5%" ' + ;
'    align=center>Docto. CTE</TD>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="5%" ' + ;
'    align=center>Serie</TD>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="7%" ' + ;
'    align=center>Transportadora</TD>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="30%" ' + ;
'    align=center>Razao Social</TD>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="7%" ' + ;
'    align=center>Emissao</TD>' + ;
'    <TD style="COLOR: white; BACKGROUND-COLOR: rgb(105,139,182)" width="10%" ' + ;
'    align=center>Valor</TD></TR>##CTES##</TBODY></TABLE></DIV>' + ;
'<P><FONT size=2 face=Calibri>##OBS## </FONT></P></FONT></BODY></HTML>'

cQuery := " SELECT "                                                                                                + ;
          " F1_DOC, F1_SERIE, F1_XCARGA, F1_EMISSAO, F1_VALBRUT, F1_XAPRVFR, F1_FORNECE, F1_LOJA, "                 + ;
		  " A2_NOME, A2_CGC, "                                                                                      + ;
          " COALESCE(ZV_VALOR, 0) ZV_VALOR, COALESCE(ZV_VENCTO, '') ZV_VENCTO, COALESCE(ZV_NUMERO, ' ') ZV_NUMERO " + ;
          " FROM " + RetSqlName("SF1") + " SF1 "                                                                    + ;
          " LEFT JOIN " + RetSqlName("SA2") + " SA2 ON A2_COD = F1_FORNECE AND A2_LOJA = F1_LOJA "                  + ;
          "                                 AND SA2.D_E_L_E_T_ = ' '  AND A2_FILIAL = '" + xFilial("SA2") + "' "    + ;
          " LEFT JOIN " + RetSqlName("SZV") + " SZV ON ZV_CARGA = F1_XCARGA "                                       + ;
		  "           AND ZV_FILIAL = '" + xFilial("SZV") + "' AND SZV.D_E_L_E_T_ = ' ' "                           + ;
          " WHERE "                                                                                                 + ;
          " SF1.D_E_L_E_T_ = ' '  AND F1_FILIAL = '" + xFilial("SF1") + "' "                                        + ;
          " AND SF1.F1_XCARGA = '" + cCarga + "' "                                                                  + ;
          " AND SF1.R_E_C_N_O_ IN (" + cQryRecnos + ") "                                                            + ;
          " ORDER BY F1_XCARGA "

dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

TcSetField(cAliasTop, "F1_EMISSAO", "D",  8, 0)
TcSetField(cAliasTop, "F1_VALBRUT", "N", 14, 2)
TcSetField(cAliasTop, "ZV_VENCTO" , "D",  8, 0)

If ! Eof()
	lGerou    := .T.
	cBodyHtml := StrTran(cBodyHtml, "##APROVADOR##",         Alltrim(UsrRetName(F1_XAPRVFR)))
	cBodyHtml := StrTran(cBodyHtml, "##CARGA##"    ,         Alltrim(cCarga))
	cBodyHtml := StrTran(cBodyHtml, "##BOLETO##"   ,         Alltrim(ZV_NUMERO))
	cBodyHtml := StrTran(cBodyHtml, "##VALOR##"    , "R$ " + Alltrim(Transform(ZV_VALOR, "@E 999,999,999.99")))
//	cBodyHtml := StrTran(cBodyHtml, "##VENCTO##"   , Dtoc(ZV_VENCTO))
Endif


Do While ! Eof()

	cBodyNotas +=  '<TR id=tableitem style="BACKGROUND-COLOR: white">'                      + ;
	               "<TD>" + Alltrim(F1_DOC)                                       + "</TD>" + ;
	               "<TD>" + Alltrim(F1_SERIE)                                     + "</TD>" + ;
	               "<TD>" + Alltrim(F1_FORNECE + "-" + F1_LOJA)                   + "</TD>" + ;
	               "<TD>" + Alltrim(A2_NOME)                                      + "</TD>" + ;
	               "<TD>" + Dtoc(   F1_EMISSAO)                                   + "</TD>" + ;
	               "<TD>" + Alltrim(Transform(F1_VALBRUT, "@E 999,999,999.99"))   + "</TD>" + ;
	               "</TR>"
	dbSkip()
Enddo

cBodyHtml := StrTran(cBodyHtml, "##CTES##" , cBodyNotas)

(cAliasTop)->(dbCloseArea())

/*
https://www.devmedia.com.br/comandos-e-tags-html5/23618
<table> Define uma tabela;
<tbody> Define o corpo da tabela;
<td> Define uma célula da tabela;

<div> Define uma seção no documento;
<body> Define o corpo da página;
<br> Insere uma quebra de linha simples;
<b> Define um texto em negrito;
<p> Define um parágrafo;
<tr> Define uma linha da tabela;
*/

cBodyHtml := StrTran(cBodyHtml, "##NOTAS##", cBodyNotas)

If lGerou
	cBodyHtml := StrTran(cBodyHtml, "##OBS##", "E-Mail gerado em " + Dtoc(Date()) + " as " + Time() + " hs")
Else
	cBodyHtml := StrTran(cBodyHtml, "##OBS##", "Nao foram encontrados Documentos de Saida relacionados a carga")
Endif

U_EnvMail("Aprovação Pagamento CTE ref. carga " + cCarga, cBodyHtml, cDestMail, '','','',.T.)

RestArea(aSavAre[1])
RestArea(aSavAre[2])
RestArea(aSavAre[3])


Return



/*
Array no formato {{X6_VAR, X6_TIPO, X6_CONTEUD, X6_DESCRIC}} */
Static Function AjustaSX6(aArray)
Local aSavAre := SX6->(SaveArea1({"SX6"}))
Local nLoop

For nLoop := 1 to Len(aArray)
	If ! GetMV(Pad(Upper(aArray[nLoop, 1]), Len(SX6->X6_VAR)), .T.)
		RecLock("SX6", .T.)
		SX6->X6_VAR     := Upper(aArray[nLoop, 1])
		SX6->X6_TIPO    := Upper(aArray[nLoop, 2])
		SX6->X6_DESCRIC := Memoline(aArray[nLoop, 4], Len(SX6->X6_DESCRIC), 1)
		SX6->X6_DESC1   := Memoline(aArray[nLoop, 4], Len(SX6->X6_DESC1  ), 2)
		SX6->X6_DESC2   := Memoline(aArray[nLoop, 4], Len(SX6->X6_DESC1  ), 3)
		SX6->X6_CONTEUD := aArray[nLoop, 3]
		SX6->X6_PROPRI  := "U"
		SX6->X6_DSCSPA  := SX6->X6_DESCRIC
		SX6->X6_DSCENG  := SX6->X6_DESCRIC
		SX6->X6_DSCSPA1 := SX6->X6_DESC1
		SX6->X6_DSCENG1 := SX6->X6_DESC1
		SX6->X6_DSCSPA2 := SX6->X6_DESC2
		SX6->X6_DSCENG2 := SX6->X6_DESC2
		SX6->X6_CONTSPA := SX6->X6_CONTEUD
		SX6->X6_CONTENG := SX6->X6_CONTEUD
		MsUnlock()
	Endif
Next
RestArea1(aSavAre)
Return

