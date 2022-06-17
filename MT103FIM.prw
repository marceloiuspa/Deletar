#INCLUDE "protheus.ch"
#define DS_MODALFRAME   128
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ MT103FIM  ºAutor ³ Djonata Guizzo      º Data ³ 15/04/2014 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Endereça automaticamente os produtos que entrarem no almox.º±±
±±º          ³ 10 ou 80 									    		  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Agrocete 							                	  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºAlterações³ 27/08/2019 - Tiago Scheneider - VAMILLY                    º±±
±±º          ³ Endereçamento automatico de retorno de insdustrialização   º±±
±±º          ³ no endereço IND											  º±±
±±º          ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function MT103FIM()

	Local lRet
	Local nOpcao     := PARAMIXB[1]   // Opção Escolhida pelo usuario no aRotina
	Local nConfirma  := PARAMIXB[2]   // Se o usuario confirmou a operação de gravação da NFE
	Local cLinkCarga := Nil

	Private aHeaderEx := {}

	If SF1->F1_TIPO == "N"  .and. nOpcao == 3 .and. nConfirma == 1

		WEnder()

	Endif

	If Alltrim(SF1->F1_ESPECIE) $ "CTE;CTR" .And. nConfirma == 1 .And. (nOpcao == 2 .Or. nOpcao == 3)  // Inclusao ou visualizacao
		cLinkCarga := U_ACGetCarga(, 2)
		If (! Empty(cLinkCarga)) .And. (! Alltrim(cLinkCarga) == Alltrim(SF1->F1_XCARGA))
			RecLock("SF1", .F.)
			SF1->F1_XCARGA := cLinkCarga
			MsUnlock()
		Endif

		U_UpdCteNum()
		If U_AgRetReset(,, .T.) // AgRetReset(lReset, lRet, lRetAlt)
			U_AgAtuSZ6()
		Endif
	Endif
	U_AgRetReset(.T.) // AgRetReset(lReset, lRet, lRetAlt)
Return

Static Function WEnder()
	Local aAreaSd1 := SD1->(GetArea())

	SD1->(dbSetOrder(1))
	SD1->(dbGoTop())
	SD1->(dbSeek(xFilial("SD1")+SF1->F1_DOC+SF1->F1_SERIE+SF1->F1_FORNECE+SF1->F1_LOJA))

	dbSelectArea("SF4")
	SF4->(dbSetOrder(1))

	cCHAVE := xFilial("SD1")+SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA

	While ! SD1->(Eof()) .And. SD1->D1_FILIAL+SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA==cCHAVE
		SF4->(dbGoTop())
		SF4->(dbSeek(xFilial("SF4")+SD1->D1_TES))
		// Se for devolução de terceiros e controla endereço
		// Irá endereçar o produto automaticamente no "IND"
		//If SF4->F4_ESTOQUE == 'S' .AND. Localiza(SD1->D1_COD) .AND. SD1->D1_LOCAL $ '10/80'
		If SF4->F4_ESTOQUE == 'S' .AND. Localiza(SD1->D1_COD)
			If SD1->D1_LOCAL $ '10/80'
				WEnderAut('ARM'+SD1->D1_LOCAL)
			ElseIf SD1->D1_LOCAL $ '15'
				WAlertEnd()
			ElseIf SF4->F4_PODER3 = 'D' .AND. SUBSTR(SD1->D1_CF,2,3) == '902' .AND. SF1->F1_FORNECE = '002524'
				WEnderAut('IND')
			EndIf
		EndIf

		SD1->(dbSkip())

	EndDo

	RestArea(aAreaSd1)

Return

Static Function WEnderAut(_cEnd)
	Local aCab 	:= {}
	Local aItem := {}
	Local aAreaTmp := SD1->(GetArea())
	Private lMsErroAuto := .F.

	aCab := {	{"DA_PRODUTO" 	,SD1->D1_COD    ,NIL},;
             	{"DA_LOTECTL"	,SD1->D1_LOTECTL,Nil},; //Lote
             	{"DA_DOC"		,SD1->D1_DOC    ,Nil},; //Doc
             	{"DA_NUMLOTE"	,SD1->D1_NUMLOTE,Nil},; //Sub-Lote
             	{"DA_NUMSEQ"	,SD1->D1_NUMSEQ ,Nil},; //Número de Seq
             	{"DA_LOCAL"		,SD1->D1_LOCAL	,NIL}}

   	aItem := {{	{"DB_ITEM"		,"0001"			,NIL},;
             	{"DB_LOCALIZ"	,_cEnd			,NIL},; // ARM10 OU ARM80
             	{"DB_DATA"		,dDataBase		,NIL},;
             	{"DB_QUANT"		,SD1->D1_QUANT	,NIL}}}

   	lMsErroAuto := .F.

   	MSExecAuto({|x,y,z| mata265(x,y,z)},aCab,aItem,3) //Distribui

   	If lMsErroAuto
		DisarmTransaction()
      	MostraErro()
  	EndIf

  	RestArea(aAreaTmp)

Return

Static Function WAlertEnd

Local cPara 		:= ''
Local _cHtml   	:= ''
Local _cCabec		:= ''
Local _cItens		:= ''
Local _cRodape	:= ''
Local _cItem		:= ''
Local cSubject 	:= ''
Local _hEnter  	:= CHR(13) + CHR(10) // Quebra a linha
Local cCc       := ''

	cSubject 	:= 'WorkFlow Agrocete - Produto a Endereçar: ' +Dtoc(DDatabase)

	_cCabec += '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	_cCabec += '<html xmlns="http://www.w3.org/1999/xhtml">
	_cCabec += '<head>
	_cCabec += '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	_cCabec += '<title>Produto a Endereçar</title>
	_cCabec += '</head>
	_cCabec += '<body>
	_cCabec += '<p>
	_cCabec += '<div >
	_cCabec += '    <table id="topo">
	_cCabec += '    	<tr>
	_cCabec += '        	<td>
	_cCabec += '		      <img width="100%" height="100%" src="https://static.wixstatic.com/media/b486f5_082d02ceb80e479a94274cb04eae64a3.png/v1/fill/w_155,h_48,al_c/b486f5_082d02ceb80e479a94274cb04eae64a3.png" alt="Logo Agrocete">
	_cCabec += '			</td>
	_cCabec += '            <td class="Titulo" style="color: #1A3984;text-align: center;font-weight: bold;font-size: 14px;float: right;width: 100%;">
	_cCabec += '			    Produto: '+SD1->D1_COD
	_cCabec += '            </td>
	_cCabec += '        </td>
	_cCabec += '    </table>
	_cCabec += '</div>
	_cCabec2 := '<p>&nbsp;	</p>
	_cCabec2 := '<p>&nbsp;	</p>
	_cCabec2 += '<div class="itens" id="itens" style="	background-color: #F4F4F4;border: 1px solid #1A3984;margin-top: -45px;margin-right: 2px;margin-bottom: 0px;margin-left: 2px;padding: 4px;box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);">
	_cCabec2 += '<table width="100%" border="1"  cellspacing="0" class="tabitens" style="color: #4F4F4F;text-align: center;border-color: #999;font-size: 12px;">
	_cCabec2 += '  <tr id="tablecabec">
	_cCabec2 += '    <td width="10%" align="center"  style="background-color:#698BB6; color: white;">Nota</td>
	_cCabec2 += '    <td width="5%"  align="center"  style="background-color:#698BB6; color: white;">Item</td>
	_cCabec2 += '    <td width="10%" align="center"  style="background-color:#698BB6; color: white;">Quantidade</td>
	_cCabec2 += '    <td width="30%" align="center"  style="background-color:#698BB6; color: white;">Produto</td>
	_cCabec2 += '    <td width="20%" align="center"  style="background-color:#698BB6; color: white;">Cliente</td>
	_cCabec2 += '    <td width="15%" align="center"  style="background-color:#698BB6; color: white;">Nome</td>
	_cCabec2 += '  </tr>
	_cItem += '  <tr id="tableitem" style="background-color:white;">
	_cItem += '    <td >@@nota@@</td>
	_cItem += '    <td >@@item@@</td>
	_cItem += '    <td >@@quant@@</td>
	_cItem += '    <td >@@produto@@</td>
	_cItem += '    <td >@@descri@@</td>
	_cItem += '    <td >@@nome@@</td>
	_cItem += '  </tr>
	_cRodape += '</table>
	_cRodape += '</div>
	_cRodape += '</body>
	_cRodape += '</html>

	_cItens += _cItem
	_cItens := StrTran(_cItens,"@@nota@@",SD1->D1_DOC)
	_cItens := StrTran(_cItens,"@@item@@",SD1->D1_ITEM)
	_cItens := StrTran(_cItens,"@@quant@@",TRANSFORM(SD1->D1_QUANT,"@E 999,999,999.99"))
	_cItens := StrTran(_cItens,"@@produto@@",SD1->D1_COD+" - "+ALLTRIM(POSICIONE("SB1",1,XFILIAL("SB1")+ SD1->D1_COD ,"B1_DESC")))
	_cItens := StrTran(_cItens,"@@descri@@",SD1->D1_FORNECE)
	_cItens := StrTran(_cItens,"@@nome@@",ALLTRIM(POSICIONE('SA2',1, xFilial('SA2') +SD1->D1_FORNECE+SD1->D1_LOJA, 'A2_NREDUZ')))

	_cHtml+= _cCabec2+_cItens
	_cHtml+= _cRodape
	If _cItens <> ''

		If Upper(AllTrim(GetEnvServer())) $ 'TESTE/P11TESTE'
			U_EnvMail(cSubject,_cHtml,'logs@agrocete.com','','','',.T.)
		Else
			U_EnvMail(cSubject,_cHtml,SuperGetMv('MV_AGREND',,'logs@agrocete.com',),'','','',.T.)
		EndIf

	Endif

Return


User Function UpdCteNum
Static nCount   := 1
Local cTransp   := Forn2Transp(SF1->F1_FORNECE)
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()

If Empty(cTransp)
	Return(.F.)
Endif

cQuery := "  SELECT R_E_C_N_O_ ZR_RECNO FROM "                                                              + ;
          "  " + RetSqlName("SZR") + " WHERE ZR_NUMCTE = ' ' AND ZR_CODIGO IN (SELECT ZR_CODIGO "           + ;
          "  FROM " + RetSqlName("SZR") + " WHERE ZR_NUMCTE = ' ' AND ZR_TRANSP = '" + cTransp + "' "       + ;
          "  AND " + RetSqlName("SZR") + ".D_E_L_E_T_ = ' '  AND ZR_FILIAL = '" + xFilial("SZR") + "' "     + ;
          "  GROUP BY ZR_CODIGO) "                                                                          + ;
          "  AND " + RetSqlName("SZR") + ".D_E_L_E_T_ = ' '  AND ZR_FILIAL = '" + xFilial("SZR") + "' "
//u_exporqry(,cquery)
dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

TcSetField(cAliasTop, "ZR_RECNO", "N", 12, 0)

Do While ! Eof()
	SZR->(dbGoto((cAliasTop)->ZR_RECNO))
	RecLock("SZR", .F.)
	SZR->ZR_NUMCTE := SF1->(F1_DOC+F1_SERIE)
	MsUnlock()
	dbSelectArea(cAliasTop)
	dbSkip()
Enddo

(cAliasTop)->(dbCloseArea())

RestArea(aSavAre)
Return

Static Function Forn2Transp(cFornece)
Static nCount   := 1
Local cQuery    := Nil
Local cAliasTop := "TRB" + Dtos(Date()) + StrTran(Time(), ":", "") + StrZero(nCount ++, 4)
Local aSavAre   := GetArea()
Local cTransp   := Nil

cQuery := "  SELECT "                                                                                       + ;
          "  A4_COD "                                                                                       + ;
          "  FROM " + RetSqlName("SA2") + " "                                                               + ;
          "  LEFT JOIN " + RetSqlName("SA4") + " ON A2_CGC = A4_CGC "                                       + ;
          "  WHERE "                                                                                        + ;
          "      " + RetSqlName("SA2") + ".D_E_L_E_T_ = ' '  AND A2_FILIAL = '" + xFilial("SA2") + "' "     + ;
          "  AND " + RetSqlName("SA4") + ".D_E_L_E_T_ = ' '  AND A4_FILIAL = '" + xFilial("SA4") + "' "     + ;
          "  AND COALESCE(A4_CGC, ' ') <> ' ' "                                                             + ;
          "  AND A4_COD = '" + cFornece + "' "
//u_exporqry(,cquery)
dbUseArea(.T., "TOPCONN", TcGenQry(,,cQuery), cAliasTop, .F., .T.)

If ! Eof()
	cTransp := A4_COD
Endif

RestArea(aSavAre)

Return(cTransp)

