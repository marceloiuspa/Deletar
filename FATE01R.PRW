#include "rwmake.ch"
#include "protheus.ch"
#include "topconn.ch"
#include "tbiconn.ch"
#Define CRLF CHR(13)+CHR(10)

/**********************************************************************************************************************************/
/** SIGAFAT - Faturamento                                                                                                        **/
/** Romaneio de Despacho                                                                                                         **/
/** Autor: Rogério Souza Pereira                                                                                                 **/
/** SINFO Sistemas - Curitiba/PR                                                                                                 **/
/** Microsiga Software S. A.                                                                                                     **/
/** Data: 06/05/2008                                                                                                             **/
/** Agrocete Indústria e Comércio de Produtos Agropecuários Ltda.                                                                **/
/** Ultima Atualiação: 06/05/2008                                                                                                **/
/**********************************************************************************************************************************/

user function FatE01R()
	// tela de romaneio de despacho
	private oDlgRom := nil
	private lDlgRom := .F.

	// checkbox para pedidos liberados
	private oChLibTot := nil
	private lChLibTot := .F.

	// checkbox para pedidos liberados por crédito
	private oChLibCre := nil
	private lChLibCre := .F.

	// checkbox para pedidos liberados por estoque
	private oChLibEst := nil
	private lChLibEst := .T.

	// checkbox para todos os pedidos
	private oChTodos := nil
	private lChTodos := .F.

	// getdados para os itens a selecionar
	private oGdRom := nil
	private aCpoRom := {}
	private aGdRom := {}

	// bitmaps de legenda
	private oBmpBlEst := LoadBitmap( GetResources(), "BR_LARANJA" )
	private oBmpBlCre := LoadBitmap( GetResources(), "BR_AZUL" )
	private oBmpLib   := LoadBitmap( GetResources(), "BR_VERDE" )
	private oBmpBloq  := LoadBitmap( GetResources(), "BR_PRETO" )

	private oBmpLeg1 := nil
	private oBmpLeg2 := nil
	private oBmpLeg3 := nil
	private oBmpLeg4 := nil

	// bitmaps de marcação de registro
	private oBmpNo := LoadBitmap( GetResources(), "LBNO" )
	private oBmpOk := LoadBitmap( GetResources(), "LBOK" )

	// labels para sub totais
	private oLbTotLit := nil
	private nLbTotLit := 0
	private oLbTotEmb := nil
	private nLbTotEmb := 0
	private oLbTotFar := nil
	private nLbTotFar := 0
	private oLbTotPes := nil
	private nLbTotPes := 0
	private oLbTotVal := nil
	private nLbTotVal := 0

	// combobox para o tipo do frete
	private oCbFrete := nil
	private aCbFrete := {"CIF", "FOB"}
	private cCbFrete := "CIF"

	// edit para o valor do frete
	private oEdFrete := nil
	private nEdFrete := 0

	// edit para o código do cliente
	private oEdCodCli := nil
	private cEdCodCli := space(tamSx3("A1_COD")[1])

	// edit para o loja do cliente
	private oEdLojaCli := nil
	private cEdLojaCli := space(tamSx3("A1_LOJA")[1])

	// edit para o nome do cliente
	private oEdNomeCli := nil
	private cEdNomeCli := space(tamSx3("A1_NOME")[1])

	// array de itens selecionados
	private aItensSel := {}

	// tela de separacao de item
	private oDlgSep := nil
	private lDlgSep := .F.

	// get para numero do pedido
	private oEdSNumPed := nil
	private cEdSNumPed := space(tamSx3("C9_PEDIDO")[1])

	// get para o item do pedido
	private oEdSItemPd := nil
	private cEdSItemPd := space(tamSx3("C9_ITEM")[1])

	// get para o cliente
	private oEdSCodCli := nil
	private cEdSCodCli := space(tamSx3("A1_COD")[1])
	private oEdSLojCli := nil
	private cEdSLojCli := space(tamSx3("A1_LOJA")[1])
	private oEdSNomCli := nil
	private cEdSNomCli := space(tamSx3("A1_NOME")[1])

	// destino
	private oEdSMunCli := nil
	private cEdSMunCli := space(tamSx3("A1_MUN")[1])
	private oEdSEstCli := nil
	private cEdSEstCli := space(tamSx3("A1_EST")[1])

	// produto
	private oEdSCodPro := nil
	private cEdSCodPro := space(tamSx3("B1_COD")[1])
	private oEdSDesPro := nil
	private cEdSDesPro := space(tamSx3("B1_DESC")[1])

	// litros
	private oEdSQtdLit := nil
	private nEdSQtdLit := 0

	// embalagem
	private oEdSQtdEmb := nil
	private nEdSQtdEmb := 0

	// fardos
	private oEdSQtdFar := nil
	private nEdSQtdFar := 0

	// peso bruto
	private oEdSQtdPes := nil
	private nEdSQtdPes := 0

	// checkbox para calculo automatico dos outros campos
	private oChAutCalc := nil
	private lChAutCalc := .T.

	// tela para mostrar o rateio do frete calculado pelo sistema
	private oDlgRatFrt := nil
	private lDlgRatFrt := .F.

	// array de pedidos em que serão rateados o frete
	private aPedFrete := {}

	// listbox de rateio
	private oLbxRatFrt := nil
	private aLbxRatFrt := {}

	// codigo da transportadora
	private oEdCodTran := nil
	private cEdCodTran := space(tamSx3("A4_COD")[1])

	// descricao da transportadora
	private oEdDesTran := nil
	private cEdDesTran := space(tamSx3("A4_NOME")[1])

	// campo para ordenar na pesquisa
	private cCpoOrd := "SA1.A1_COD, SA1.A1_LOJA"

	//Objeto para Tabela Temporária
	Private oTRB

	// monta a tela de romaneio de despacho
	mntDlgRom()

return nil


/**********************************************************************************************************************************/
/** static function mntDlgRom()                                                                                                  **/
/** desenha a tela de Romaneio de Despacho                                                                                       **/
/**********************************************************************************************************************************/
static function mntDlgRom()

	// barra de ferramentas
	local oBar := nil

	// define a tela
	define msDialog oDlgRom from 001, 001 to 595, 950 title "Romaneio de Despacho" pixel

	// monta a barra de botoes
	define buttonBar oBar size 25,25 3D TOP of oDlgRom
	define button resource "OK" of oBar action dlgRomOk() prompt "OK" toolTip "Confirma o Romaneio"
	define button resource "CANCEL" of oBar action ::end() prompt "Fechar" toolTip "Fechar"
	define button resource "CARGA" of oBar action separItem() prompt "Separar" toolTip "Separa as quantidades para carregamento"
	define button resource "OMSDIVIDE" of oBar action aglutMark() prompt "Aglutinar" toolTip "Aglutinar itens marcados"
	define button resource "AUTOM" of oBar action E01RRom(), atuGdRom() prompt "Romaneios" toolTip "Romaneios gerados"
	oBar:bRClicked:={ || AllwaysTrue() }

	// divisao  da tela para os filtros
	@ 012, 002 to 030, 305 title "Selecionar:"

	// monta as opções de filtro
	@ 018, 006 checkbox oChLibTot var lChLibTot size 100, 008 pixel of oDlgRom prompt "Ped. Liberados (Crédito e Estoque)" pixel
	oChLibTot:bChange := {|| checkFil(oChLibTot) }

	@ 018, 100 checkbox oChLibCre var lChLibCre size 100, 008 pixel of oDlgRom prompt "Ped. Liberados de Crédito" pixel
	oChLibCre:bChange := {|| checkFil(oChLibCre) }

	@ 018, 175 checkbox oChLibEst var lChLibEst size 100, 008 pixel of oDlgRom prompt "Ped. Liberados de Estoque" pixel
	oChLibEst:bChange := {|| checkFil(oChLibEst) }

	@ 018, 250 checkbox oChTodos var lChTodos size 100, 008 pixel of oDlgRom prompt "Todos os Pedidos" pixel
	oChTodos:bChange := {|| checkFil(oChTodos) }

	// divisao da tela para o cliente
	@ 012, 307 to 030, 472 title "Cliente:"

	@ 019, 311 say "Código:" pixel
	@ 018, 330 msget oEdCodCli var cEdCodCli size 030, 008 picture "@!" f3 "SA1" valid valSa1() of oDlgRom pixel
	@ 018, 361 msget oEdLojaCli var cEdLojaCli size 004, 008 picture "@!" valid valSa1() of oDlgRom pixel
	@ 018, 382 msget oEdNomeCli var cEdNomeCli size 088, 008 when .F. of oDlgRom pixel

	// divisao dos sub-totais
	@ 235, 002 to 280, 094 title "Sub Totais:"

	@ 241, 006 say "Litros:" pixel
	@ 241, 040 say oLbTotLit var nLbTotLit size 050, 009 right picture "@E 9,999,999,999.99" color CLR_BLUE pixel

	@ 248, 006 say "Embalagem:" pixel
	@ 248, 040 say oLbTotEmb var nLbTotEmb size 050, 009 right picture "@E 9,999,999,999" color CLR_BLUE pixel

	@ 255, 006 say "Fardos:" pixel
	@ 255, 040 say oLbTotFar var nLbTotFar size 050, 009 right picture "@E 9,999,999,999" color CLR_BLUE pixel

	@ 262, 006 say "Peso Bruto:" pixel
	@ 262, 040 say oLbTotPes var nLbTotPes size 050, 009 right picture "@E 9,999,999,999.99" color CLR_BLUE pixel

	@ 269, 006 say "Valor Total:" pixel
	@ 269, 040 say oLbTotVal var nLbTotVal size 050, 009 right picture "@E 9,999,999,999.99" color CLR_BLUE pixel

	// divisao da legenda
	@ 235, 100 to 280, 210 title "Legenda:"

	@ 244, 106 bitmap oBmpLeg3 RESNAME "BR_VERDE" size 010, 010 NoBorder of oDlgRom pixel
	@ 244, 117 say "Item Liberado" color CLR_GREEN of oDlgRom pixel

	@ 252, 106 bitmap oBmpLeg1 RESNAME "BR_LARANJA" size 010, 010 NoBorder of oDlgRom pixel
	@ 252, 117 say "Item Bloqueado Por Estoque" color CLR_GREEN of oDlgRom pixel

	@ 260, 106 bitmap oBmpLeg2 RESNAME "BR_AZUL" size 010, 010 NoBorder of oDlgRom pixel
	@ 260, 117 say "Item Bloqueado Por Crédito" color CLR_GREEN of oDlgRom pixel

	@ 268, 106 bitmap oBmpLeg4 RESNAME "BR_PRETO" size 010, 010 NoBorder of oDlgRom pixel
	@ 268, 117 say "Item Bloqueado Por Crédito e Estoque" color CLR_GREEN of oDlgRom pixel

	// monta o getDados
	mntGdRom()

	// divisao do frete
	@ 235, 216 to 280, 310 title "Frete:"

	@ 241, 220 say "Tipo:" of oDlgRom pixel
	@ 249, 220 combobox oCbFrete var cCbFrete items aCbFrete size 080, 100 of oDlgRom pixel

	@ 260, 220 say "Valor:" of oDlgRom pixel
	@ 268, 220 msget oEdFrete var nEdFrete size 080, 008 picture "@E 9,999,999,999.99" of oDlgRom pixel

	// divisao da transportadora
	@ 235, 315 to 280, 472 title "Transportadora/Veículo:"

	@ 241, 319 say "Selecione a Transportadora:" of oDlgRom pixel
	@ 249, 319 msget oEdCodTran var cEdCodTran size 030, 008 picture "@!" f3 "SA4" valid valTransp() of oDlgRom pixel
	@ 249, 350 msget oEdDesTran var cEdDesTran size 120, 008 when .F. picture "@!" of oDlgRom pixel

	// atualiza o browse
	atuGdRom()

	// ativa a janela de romaneio
	activate msDialog oDlgRom centered

return nil

/**********************************************************************************************************************************/
/** static function valTransp()                                                                                                  **/
/** validação da transportadora                                                                                                  **/
/**********************************************************************************************************************************/
static function valTransp()

	// retorno da função
	local lRet := .F.

	if empty(cEdCodTran)
		lRet := .T.
		cEdDesTran := ""
		return lRet
	endIf


	SA4->(dbSetOrder(1))
	if ( !SA4->(dbSeek(xFilial("SA4") + cEdCodTran)) )
		msgAlert("Transportadora não cadastrada!")
		cEdDesTran := ""
		return lRet
	endIf

	// atualiza o nome da transportadora
	cEdDesTran := SA4->A4_NOME
	lRet := .T.

return lRet

/**********************************************************************************************************************************/
/** static function valVeic()                                                                                                    **/
/** validação do veiculo                                                                                                         **/
/**********************************************************************************************************************************/
static function valVeic()

	// retorno da função
	local lRet := .T.

return lRet



/**********************************************************************************************************************************/
/** static function aglutMark()                                                                                                  **/
/** aglutina os itens marcados                                                                                                   **/
/**********************************************************************************************************************************/
static function aglutMark()

	// array de itens aglutinados
	local aAglutIts := {}

	// array de itens da sequencia
	local aItensSeq := {}

	// contador de loop
	local nI := 0

	// posição do item de array
	local nPosItem := 0

	// flag de itens diferentes
	local lItensDif := .F.

	// soma das quantidades a aglutinar
	local nSomaIts := 0

	// flag de erro
	local lErro := .F.

	// mensagem de erro
	local cMsgErro := ""

	// loop sobre os itens
	for nI := 1 to len( aGdRom )

	// verifica se o item está marcado
	if ( aGdRom[nI][02] == "S" )

		// verifica se o item já foi adicionado ao array
		nPosItem := aScan( aAglutIts, {|x| x[01] == aGdRom[nI][07] .and. x[02] == aGdRom[nI][08] .and. x[03] == aGdRom[nI][09]} )
		if ( nPosItem == 0 )

			// adiciona o item ao array
			aAdd( aAglutIts, { aGdRom[nI][07] ,; // pedido
							   aGdRom[nI][08] ,; // item
							   aGdRom[nI][09] ,; // produto
							   aGdRom[nI][11]  ; // quantidade
							 }                 ;
				)

		else

			// incrementa a quantidade
			aAglutIts[nPosItem][04] += aGdRom[nI][11]

		endIf


	  // adiciona o item na sequencia
	  aAdd( aItensSeq, { aGdRom[nI][07] ,; // pedido
						 aGdRom[nI][08] ,; // item
						 aGdRom[nI][16] ,; // sequencia
						 aGdRom[nI][09] ,; // produto
						 aGdRom[nI][01] ,; // status
						 aGdRom[nI][15]  ; // lote
					   }                 ;
		  )

	  // soma os itens
	  nSomaIts += aGdRom[nI][11]

	endIf

	next nI


	// valida os itens selecionados (pedido, item, produto e status )
	lItensDif := .F.
	for nI := 1 to len(aItensSeq)

		// se nao for o ultimo item
		if ( nI != len(aItensSeq) )

				// compara com o proximo
				if ( aItensSeq[nI][01] != aItensSeq[nI + 1][01] .or. aItensSeq[nI][02] != aItensSeq[nI + 1][02] .or. ;
				   aItensSeq[nI][04] != aItensSeq[nI + 1][04] .or. aItensSeq[nI][05] != aItensSeq[nI + 1][05] .or. ;
				   !empty(aItensSeq[nI][06]) )
				lItensDif := .T.
				exit
				endIf

		endIf

	next nI

	// verifica se o usuario selecionou itens diferentes
	if ( lItensDif )
		msgAlert("Somente itens iguais ou sem lote podem ser aglutinados! O sistema leva em consideração Pedido, Item, Produto, Situação e Lote!")
		return nil
	endIf


	if ( len(aItensSeq) <= 1 )
		msgAlert("É necessário marcar ao menos 2 itens para aglutinar!")
		return nil
	endIf


	// confirma a aglutinação dos itens
	if ( !msgYesNo("Confirma a aglutinação dos itens?") )
		return nil
	endIf

	begin transaction

	// aglutina com o primeiro item e deleta os demais
	for nI := 1 to len(aItensSeq)

	  // posiciona o SC9
		SC9->(dbSetOrder(1))
		if ( SC9->(dbSeek(xFilial("SC9") + aItensSeq[nI][01] + aItensSeq[nI][02] + aItensSeq[nI][03] + aItensSeq[nI][04])) )

			if ( nI == 1 )

				// aglutina
				recLock( "SC9", .F. )
				SC9->C9_QTDLIB := nSomaIts
				msUnlock()

			else

				// deleta o item
				recLock( "SC9", .F. )
				SC9->(dbDelete())
				msUnlock()

			endIf

		else

			// controle de erros
			lErro := .T.
			cMsgErro := "O registro de liberação do SC9 não foi localizado:" + chr(13) + ;
					  "Pedido: " + aItensSeq[nI][01] + chr(13) + ;
					  "Item: " + aItensSeq[nI][02] + chr(13) + ;
					  "Seq. " + aItensSeq[nI][03] + chr(13) + ;
					  "Produto: " + aItensSeq[nI][04]
			exit

		endIf

	next nI


	// tratamento de erros
	if ( lErro )
		disarmTransactions()
		msgAlert(cMsgErro)
	endIf

	end transaction


	// desmarca todos os itens
	for nI := 1 to len(aGdRom)

	// verifica se esta marcado
	if ( aGdRom[nI][02] == "S" )
		aGdRom[nI][02] := "N"
	endIf


	next nI

	// atualiza a pesquisa
	aItensSel := {}
	atuGdRom()
	atuSubTot()

return nil


/**********************************************************************************************************************************/
/** static function E01RRom()                                                                                                    **/
/** acessa o cadastro de romaneios fiscais                                                                                       **/
/**********************************************************************************************************************************/
static function E01RRom()

	// area
	local aArea := getArea()

	private cDelFunc := "allwaysTrue()"
	private cCadastro := "Romaneios Fiscais"
	private aRotina := { { "Pesquisar",  "AxPesqui",  0, 1 } ,;
					   { "Visualizar", "AxVisual",  0, 2 } ,;
					   { "Alterar",    "U_E01RAlt(.F.)", 0, 4 } ,;
					   { "Excluir",    "U_E01RDel", 0, 5 } ,;
					   { "Rat. Frete", "U_E01RRat", 0, 2 } ,;
					   { "Imprimir",   "U_E01RImp", 0, 2 } ,;
					   { "Carga",    	 "U_F01CARGA",0, 3 } ,;
					   { "Cad.Motivos","U_F01CADMOT",0, 4 } ,;
					   { "Cad.Grupos"	,"U_AGRAT003",0, 4 } ,;
									   { "Cotações",    "U_Fat01Cot",0, 4 } ,;
					   { "Final. Carreg.", "U_F01CARRE",0, 4 } ,;
					   { "Etiqueta", "U_F01ETIQ",0, 4 } ,;
					   { "Legenda",    "U_E01RLeg", 0, 3 },;
					   { "Aprov Frete",  "U_E01Fret", 0, 3 },;
					   { "Inf.Comp.", "U_E01OBS",0,2}}

	private aCores := { {"U_E01RSts(ZR_CODIGO) == '1' .AND. ZR_STATUS <> '1'", "BR_VERDE"    } ,; // Faturamento Pendente
					  {"U_E01RSts(ZR_CODIGO) == '1' .AND. ZR_STATUS == '1'", "BR_AMARELO"  } ,; // Faturado parcialmente
					  {"U_E01RSts(ZR_CODIGO) == '3'", "BR_VERMELHO" }  ; // Totalmente faturado
					}


	SZR->(dbSetOrder(1))
	dbSelectArea("SZR")
	mBrowse(6, 1, 22, 75, "SZR", nil, , , , , aCores)
	restArea(aArea)

return nil


/**********************************************************************************************************************************/
/** user function E01RLeg()                                                                                                      **/
/** monta a visualização de legendas                                                                                             **/
/**********************************************************************************************************************************/
user function E01RLeg()

	// monta a visualização da legenda
	brwLegenda ( cCadastro, "Legenda", { {"BR_VERDE",    "Faturamento Pendente" } ,;
										 {"BR_AMARELO",  "Carga Pronta"} ,;
										 {"BR_VERMELHO", "Faturado Totalmente"  }  ;
									 }                                           ;
			 )

return nil


/**********************************************************************************************************************************/
/** user function E01RSts( cRoman )                                                                                              **/
/** retorna o status de faturamento do romaneio                                                                                  **/
/**********************************************************************************************************************************/
user function E01RSts( cRoman )

	// retorno da função
	local cRet := "1"
							  `
	// area
	local aArea := getArea()

	// query
	local cQr := ""


	// monta a query para verificar a situação de faturado dos itens
	cQr := " select sum( case SC9.C9_NFISCAL when '' then 1 else 0 end  ) ZQ_PEND,
	cQr += "        sum( case SC9.C9_NFISCAL when '' then 0 else 1 end  ) ZQ_FAT

	cQr += "   from " + RetSqlName("SC9") + " SC9
	/*cQr += "  where SC9.D_E_L_E_T_ = ' '
	cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
	cQr += "    and SC9.C9_ROMAN = '" + cRoman + "'
	*/
	cQr += "  where SC9.C9_ROMAN = '" + cRoman + "'
	cQr += "    and SC9.D_E_L_E_T_ = ''
	cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'


	TcQuery cQr new alias "QSZQ"
	restArea(aArea)

	// testa os valores
	if ( QSZQ->ZQ_PEND != 0 ) .and. ( QSZQ->ZQ_FAT != 0 )
		// parcialmente faturado
		cRet := "2"
	elseIf ( QSZQ->ZQ_FAT == 0 )
		// pendente
		cRet := "1"
	else
		// totalmente faturado
		cRet := "3"
	endIf

	// fecha a query
	QSZQ->(dbCloseArea())


return cRet


/**********************************************************************************************************************************/
/** user function E01RRat()                                                                                                      **/
/** rotina para visualizar o rateio de frete gerado neste romaneio                                                               **/
/**********************************************************************************************************************************/
user function E01RRat()

	// soma dos itens do romaneio
	local nSomaRom := 0

	// soma do item
	local nSomaIte := 0

	// proporcao
	local nProp := 0

	// valor do frete
	local nVlFrt := 0

	// posição do item de array
	local nPosItem := 0

	// contador de loop
	local nI := 0



	// recupera os itens do rateio
	getRatFrt( SZR->ZR_CODIGO )

	// faz a soma dos itens
	QSZQ->(dbEval({|| nSomaRom += QSZQ->ZQ_LITROS * QSZQ->C6_PRCVEN }))

	// loop sobre os itens do rateio
	aLbxRatFrt := {}
	QSZQ->(dbGoTop())
	while ( !QSZQ->(eof()) )

	// calcula as proporcoes
	nSomaIte := QSZQ->ZQ_LITROS * QSZQ->C6_PRCVEN
	nProp := (nSomaIte * 100) / nSomaRom
	nVlFrt := QSZQ->ZS_VLRAT

	// verifica se o item ja esta no array
	nPosItem := aScan( aLbxRatFrt, {|x| x[1] == QSZQ->C6_NUM} )
	if ( nPosItem == 0 )

		// adiciona o item no array
		aAdd( aLbxRatFrt, { QSZQ->C6_NUM ,; // numero do pedido
							nSomaRom     ,; // valor total do pedido
							nSomaIte     ,; // valor do item do romaneio
							nProp        ,; // percentual de proporção do frete
							nVlFrt        ; // valor do frete rateado
						  }               ;
			)

	else

		// atualiza os valores
		aLbxRatFrt[nPosItem][02] := nSomaRom
		aLbxRatFrt[nPosItem][03] += nSomaIte
		aLbxRatFrt[nPosItem][04] := (aLbxRatFrt[nPosItem][03] * 100) / nSomaRom
		aLbxRatFrt[nPosItem][05] := nVlFrt//nVlFrt * (aLbxRatFrt[nPosItem][04] / 100)

	endIf

	// proximo registro
	QSZQ->(dbSkip())

	endDo

	// fecha a query
	QSZQ->(dbCloseArea())


	// formata os campos para visualização
	for nI := 1 to len(aLbxRatFrt)
		aLbxRatFrt[nI][02] := transform(aLbxRatFrt[nI][02], "@E 9,999,999,999.99")
		aLbxRatFrt[nI][03] := transform(aLbxRatFrt[nI][03], "@E 9,999,999,999.99")
		aLbxRatFrt[nI][04] := transform(aLbxRatFrt[nI][04], "@E 999.99")
		aLbxRatFrt[nI][05] := transform(aLbxRatFrt[nI][05], "@E 9,999,999,999.99")
	next nI

	// mostra o resultado em um browse
	showRatFrt()

return nil


/**********************************************************************************************************************************/
/** static function getRatFrt( cRoman )                                                                                          **/
/** recupera o rateio de frete do romaneio de despacho                                                                           **/
/**********************************************************************************************************************************/
static function getRatFrt( cRoman )

	// area
	local aArea := getArea()

	// query
	local cQr := ""


	// monta a query para recuperar o rateio
	cQr := " select SC6.C6_NUM,
	cQr += "        SZQ.ZQ_LITROS,
	cQr += "        SC6.C6_PRCVEN,
	cQr += "        SZS.ZS_VLRAT
	cQr += "   from " + retSqlName("SZQ") + " SZQ

	cQr += "   left join " + retSqlName("SC6") + " SC6
	cQr += "     on SC6.D_E_L_E_T_ = ' '
	cQr += "    and SC6.C6_FILIAL = '" + xFilial("SC6") + "'
	cQr += "    and SC6.C6_NUM = SZQ.ZQ_PEDIDO
	cQr += "    and SC6.C6_ITEM = SZQ.ZQ_ITEMPV
	cQr += "    and SC6.C6_PRODUTO = SZQ.ZQ_PRODUTO

	cQr += "   left join " + retSqlName("SZS") + " SZS
	cQr += "     on SZS.D_E_L_E_T_ = ' '
	cQr += "    and SZS.ZS_FILIAL = '" + xFilial("SZS") + "'
	cQr += "    and SZS.ZS_PEDIDO = SZQ.ZQ_PEDIDO
	cQr += "    and SZS.ZS_ROMAN = SZQ.ZQ_ROMAN

	cQr += "  where SZQ.D_E_L_E_T_ = ' '
	cQr += "    and SZQ.ZQ_FILIAL = '" + xFilial("SZQ") + "'
	cQr += "    and SZQ.ZQ_ROMAN = '" + cRoman + "'

	TcQuery cQr new alias "QSZQ"
	restArea(aArea)

return nil


/**********************************************************************************************************************************/
/** user function E01RAlt()                                                                                                      **/
/** rotina para alterar os itens do romaneio fiscal                                                                              **/
/**********************************************************************************************************************************/
user function E01RAlt(lVisualiz)

	// contador de loop
	local nI := 0
	local nA := 0
	local nJ := 0

	// clone do aRotina
	local aBkpRot := aClone(aRotina)

	// flag de execução modelo 3
	local lMod3Ok := .F.

	// campos da enchoice
	local aCpoEnc := {}

	// posicao do pedido no aCols
	local nPosPed := 0

	// posicao do item do pedido no aCols
	local nPosIte := 0

	// posicao da sequencia no aCols
	local nPosSeq := 0

	// posicao do produto no aCols
	local nPosPro := 0

	// posicao da quantidade liberada
	local nPosLit := 0

	// posicao da quantidade de embalagens
	local nPosEmb := 0

	// posicao da quantidade de fardos
	local nPosFar := 0

	// posicao do peso bruto
	local nPosPes := 0

	// totalizados dos litros digitado
	local nTotLit := 0

	// totalizador das embalagens
	local nTotEmb := 0

	// totalizador de fardos
	local nTotFar := 0

	// totalizador de peso
	local nTotPes := 0

	// saldo da diferença de liberação
	local nSaldoDif := 0

	// total disponível no SC9 para montagem de cargas
	local nTotC9Dis := 0

	// tratamento de erros
	local lErro := .F.
	local cMsgErro := ""

	// saldo do SC9 para a nova liberação
	local nSaldoSc9 := 0

	// proximo sequencial do SC9
	local cSeqSc9 := ""

	// macro substituicao
	local cMacro := ""

	// total do romaneio
	local nTotRom := 0

	// total do item separado
	local nVlItemSp := 0

	// posição no item de array
	local nPosItem := 0

	// percentual de proporção de frete
	local nPropFret := 0

	// valor de rateio do frete
	local nVlFrtRat := 0

	// pedidos alterados por este romaneio
	local aPedRom := {}

	Local lContinua := .T.
	Local aCamposTabela
	Local nCampo
	Local cCampo

	// compatibilidade de rotina
	Default lVisualiz:= .F.

	// monta o cadastro modelo3
	private aRotina := { { "Pesquisa", "AxPesqui", 0 , 1 } ,;
					   { "Visual",   "AxVisual", 0 , 2 } ,;
					   { "Inclui",   "AxInclui", 0 , 3 } ,;
					   { "Altera",   "AxAltera", 0 , 4 } ,;
					   { "Exclui",   "AxDeleta", 0 , 5 }  ;
					 }

	// colunas
	private aCols   := {}

	// cabeçalho
	private aHeader := {}

	// variável auxiliar
	private nUsado := 0

	// clone do SC9
	private aSc9Clone := {}


	// Cria variaveis M->????? da Enchoice
	RegToMemory( "SZR", .F. )

	// monta o mestre
	// dbSelectArea('SX3')
	// SX3->(dbSetOrder(1))
	// SX3->(dbGoTop())
	// SX3->(dbSeek("SZQ"))
	aHeader := {}

	// loop sobre os campos do SX3
	nUsado := 0
	/*while ( SX3->(!Eof()) ) .and. ( SX3->X3_ARQUIVO == "SZQ" )


	if !(allTrim(SX3->X3_CAMPO) $ "ZQ_FILIAL|ZQ_ROMAN")

		// incrementa a variavel de usado
		nUsado := (nUsado + 1)

		// adiciona o item ao aHeader
		aAdd( aHeader, { trim(SX3->X3_TITULO) ,;
						 SX3->X3_CAMPO        ,;
						 SX3->X3_PICTURE      ,;
						 SX3->X3_TAMANHO      ,;
						 SX3->X3_DECIMAL      ,;
						 "AllwaysTrue()"      ,;
						 SX3->X3_USADO        ,;
						 SX3->X3_TIPO         ,;
						 SX3->X3_ARQUIVO      ,;
						 SX3->X3_CONTEXTO      ;
					   }                       ;
			)

	endIf

	// próximo registro
	SX3->(dbSkip())

	end*/

	aCamposTabela := FWSX3Util():GetAllFields( 'SZQ' , .T. )

	For nCampo := 1 To Len(aCamposTabela)
		cCampo := AllTrim(aCamposTabela[nCampo])

		If !(allTrim(cCampo) $ "ZQ_FILIAL|ZQ_ROMAN")

			nUsado := (nUsado + 1)

			Aadd(aHeader,{	AllTrim(FWX3Titulo(cCampo)),;
							cCampo,;
							GetSX3Cache(cCampo,"X3_PICTURE"),;
							GetSX3Cache(cCampo,"X3_TAMANHO"),;
							GetSX3Cache(cCampo,"X3_DECIMAL"),;
							"AllwaysTrue()",;
							GetSX3Cache(cCampo,"X3_USADO"),;
							GetSX3Cache(cCampo,"X3_TIPO"),;
							GetSX3Cache(cCampo,"X3_ARQUIVO"),;
							GetSX3Cache(cCampo,"X3_CONTEXT") })

		EndIf
		// dbSelectArea("SX3")
		// dbSkip()

	Next nCampo

	// recupera os  itens do romaneio
	aCols := {}
	SZQ->(dbSetOrder(1))
	SZQ->(dbSeek(xFilial("SZQ") + SZR->ZR_CODIGO))

	// loop sobre os itens do romaneio
	while ( SZQ->ZQ_ROMAN == SZR->ZR_CODIGO ) .and. ( !SZQ->(eof()) )

	// adiciona o item ao aCols
	aAdd( aCols, array(nUsado + 1) )

	// recupera o valor dos campos
	for nI := 1 to nUsado

	  // atribui o valor ao acols
	  aCols[len(aCols)][nI] := SZQ->(fieldGet(SZQ->(fieldPos(aHeader[nI][02]))))

	next nI

	// atribui o flag de deleção
	aCols[len(aCols)][nUsado+1] := .F.

	// proximo registro
	SZQ->(dbSkip())

	endDo

	// inicializa as variáveis do modelo 3
	aCpoEnc := { "ZR_CODIGO"  ,;
			   "ZR_DATA"    ,;
			   "ZR_HORA"    ,;
			   "ZR_LITROS"  ,;
			   "ZR_EMBALAG" ,;
			   "ZR_FARDOS"  ,;
			   "ZR_PESOBRT" ,;
			   "ZR_VLFRETE"   ;
			 }

	// executa modelo 3
	lMod3Ok := Modelo3( "Alteração de Romaneio", "SZR", "SZQ", aCpoEnc, "allwaysTrue()", "allwaysTrue()", 3, 3, "allwaysTrue()" )

	// verifica se o usuário confirmou
	if ( !lMod3Ok )
	return nil
	endIf

	//Compatibilidade para aproveitar código
	if lVisualiz
	return nil
	Endif

	// recupera a posição dos itens
	nPosPed := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_PEDIDO"  } )
	nPosIte := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_ITEMPV"  } )
	nPosSeq := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_SEQUEN"  } )
	nPosPro := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_PRODUTO" } )
	nPosLit := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_LITROS"  } )
	nPosEmb := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_EMBALAG" } )
	nPosFar := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_FARDOS"  } )
	nPosPes := aScan( aHeader, {|x| allTrim(x[2]) == "ZQ_PESOBRT" } )

	// valida a posicao dos itens
	if ( nPosPed == 0 .or. nPosIte == 0 .or. nPosSeq == 0 .or. nPosPro == 0 .or. nPosLit == 0 .or. nPosEmb == 0 .or. nPosFar == 0 .or. nPosPes == 0 )
	// alerta o usuário
	msgAlert("Os campos ZQ_PEDIDO, ZQ_ITEMPV, ZQ_SEQUEN, ZQ_LITROS e ZQ_PRODUTO devem estar marcados como Browse no Configurador!")
	return nil
	endIf


	// inicializa as variaveis de controle de erro
	lErro := .F.
	cMsgErro := ""

	// inicia transação SQL
	begin transaction

		// deleta o rateio do frete
		// recupera os pedidos que tiveram o frete alterado pelo romaneio
		getPedRom( SZR->ZR_CODIGO )

		// loop sobre os pedidos do romaneio
		while ( !QSC5->(eof()) )

		// adiciona o pedido ao array
		aAdd( aPedRom, QSC5->ZQ_PEDIDO )

		// proximo registro
		QSC5->(dbSkip())

		endDo

		// fecha a query
		QSC5->(dbCloseArea())


		// loop sobre os pedidos para alterar o valor do frete
		for nI := 1 to len(aPedRom)

		// verifica se houve rateio do frete no pedido
		SZS->(dbSetOrder(1))
		if ( SZS->(dbSeek(xFilial("SZS") + SZR->ZR_CODIGO + aPedRom[nI])) )

			// posiciona o pedido
			SC5->(dbSetOrder(1))
			if ( SC5->(dbSeek(xFilial("SC5") + SZS->ZS_PEDIDO)) )
				// restaura o frete do pedido
				recLock("SC5", .F. )
				SC5->C5_FRETEAG -= SZS->ZS_VLRAT

				// tratamento de arredondamento decimal
				if ( SC5->C5_FRTROMA < 0 )
					SC5->C5_FRTROMA := 0
				endIf

				// verifica se chegou a zero
				if ( SC5->C5_FRTROMA == 0 )
					// restaura o frete digitado pelo usuário no lançamento do pedido
					SC5->C5_FRETEAG := SC5->C5_FRTOLD
					SC5->C5_FRTOLD := 0
				endIf

				msUnlock()

			endIf

			// deleta o rateio do frete
			recLock( "SZS", .F. )
			SZS->(dbDelete())
			msUnlock()

		endIf

		next nI

		// atualiza o cabeçalho do romaneio
		if ( !lErro )
			recLock( "SZR", .F. )
			SZR->ZR_VLFRETE := M->ZR_VLFRETE
			SZR->ZR_TRANSP := M->ZR_TRANSP
			SZR->ZR_NOMTRAN := M->ZR_NOMTRAN
			SZR->ZR_NUMCTE  := M->ZR_NUMCTE
			SZR->ZR_CODVEIC := M->ZR_CODVEIC
			SZR->ZR_NOMVEIC := M->ZR_NOMVEIC
			SZR->ZR_TPFRETE := M->ZR_TPFRETE
			msUnlock()
		endIf

		// refaz o rateio do frete
		if ( !lErro )

		  // monta a lista de rateio do frete
		  aPedFrete := {}
		  for nI := 1 to len(aCols)

			//Alessandro 20150825
			DbSelectArea('SC5')
			SC5->(DBSetOrder(1))
			IF SC5->(DbSeek(xFilial('SC5')+aCols[nI][nPosPed]))
				//Reclock('SC5',.F.)
				//SC5->C5_TPFRETE:=IIF(SZR->ZR_TPFRETE == 'FOB','F','C')
				//MsUnlock()
			Endif
			//Fim Alessandro

			// posiciona o item do pedido no SC6
			SC6->(dbSetOrder(1))
			if ( !SC6->(dbSeek(xFilial("SC6") + aCols[nI][nPosPed] + aCols[nI][nPosIte] + aCols[nI][nPosPro])) )
				// item nao encontrado
				msgAlert("Não foi possível localizar o item do pedido conforme abaixo:" + chr(13) + ;
					   "Pedido: " + aCols[nI][nPosPed] + chr(13) + ;
					   "Item: " + aCols[nI][nPosIte] + chr(13) + ;
					   "Produto: "  + aCols[nI][nPosPro])
				lContinua := .F.
				Exit
			endIf

			// recupera o valor total do pedido
			nTotRom += SC6->C6_PRCVEN * aCols[nI][nPosLit]

			// calcula o valor total do item separado
			nVlItemSp := SC6->C6_PRCVEN * aCols[nI][nPosLit]

			// verifica se o pedido já está no array
			nPosItem := aScan( aPedFrete, {|x| x[01] == aCols[nI][nPosPed]} )
			if ( nPosItem == 0 )

				// adiciona o item
				aAdd( aPedFrete, { aCols[nI][nPosPed] ,; // numero do pedido
								 0.00               ,; // valor total dos itens do romaneio
								 nVlItemSp          ,; // valor do item separado
								 0.00               ,; // percentual de proporção do frete
								 0.00                ; // valor do frete rateado
							   }                     ;
				  )

			else

			  // incrementa o valor adicionado
			  aPedFrete[nPosItem][03] += nVlItemSp

			endIf


		  next nI

		If lContinua
			// atualiza o valor total dos itens do romaneio no pedido
			for nI := 1 to len(aPedFrete)

				// atribui o valor total dos itens do romaneio
				aPedFrete[nI][02] := nTotRom

				// calcula o percentual de proporção do frete
				nPropFret := (aPedFrete[nI][03] * 100) / nTotRom
				aPedFrete[nI][04] := nPropFret

				// calcula o valor siga	do frete rateado
				nVlFrtRat := M->ZR_VLFRETE * (nPropFret / 100)
				aPedFrete[nI][05] := nVlFrtRat

			next nI


			// atualiza o array para visualizar o resumo no browse
			aLbxRatFrt := {}
			for nI := 1 to len(aPedFrete)
				// adiciona o item no array
				aAdd( aLbxRatFrt, { aPedFrete[nI][01] ,; // numero do pedido
									transform(aPedFrete[nI][02], "@E 9,999,999,999.99") ,; // valor total do pedido
									transform(aPedFrete[nI][03], "@E 9,999,999,999.99") ,; // valor do item do romaneio
									transform(aPedFrete[nI][04], "@E 999.99")           ,; // percentual de proporção do frete
									transform(aPedFrete[nI][05], "@E 9,999,999,999.99")  ; // valor do frete rateado
								  }                                                      ;
					)
			next nI

			// mostra o resultado em um browse
			showRatFrt()
			if ( !lDlgRatFrt )
				// cancelado pelo usuário
				lErro := .T.
				cMsgErro := ""
			endIf


			  // grava o rateio do frete
			// loop sobre os pedidos de frete para atualizar o valor do frete no pedido de venda
			for nI := 1 to len( aPedFrete )

				// verifica se existe valor do frete
				if ( aPedFrete[nI][05] > 0 )

					// posiciona o pedido de vendas
					SC5->(dbSetOrder(1))
					if ( SC5->(dbSeek(xFilial("SC5") + aPedFrete[nI][01])) )

						// atualiza o valor do frete
						recLock( "SC5", .F. )
						//Alessandro - 20150825
						//SC5->C5_TPFRETE := Iif( SZR->ZR_TPFRETE == 'FOB', "F", "C" )//Iif( oCbFrete:nAt == 1, "C", "F" )
						Iif( oCbFrete:nAt == 1, "C", "F" )
						//Fim
						if ( SC5->C5_FRTROMA == 0 )
						  SC5->C5_FRTOLD := SC5->C5_FRETEAG
						endIf

						SC5->C5_FRTROMA += aPedFrete[nI][05]
						SC5->C5_FRETEAG := SC5->C5_FRTROMA
						msUnlock()


						// verifica se o frete informado no pedido manualmente é menor que a soma do frete no cálculo do romaneio fiscal
						if ( SC5->C5_FRTOLD != 0 ) .and. ( SC5->C5_FRTOLD < SC5->C5_FRTROMA ) .and. ( SC5->C5_FRTOLD != SC5->C5_FRTROMA )
						  // alerta o usuário sobre o valor do frete
						  msgAlert( "O valor do frete informado no pedido é inferior ao valor rateado:" + chr(13) + ;
									"Pedido: " + SC5->C5_NUM + chr(13) + ;
									"Frete Informado: " + allTrim(transform(SC5->C5_FRTOLD, "@E 9,999,999,999.99")) + chr(13) + ;
									"Frete rateado: " + allTrim(transform(SC5->C5_FRTROMA, "@E 9,999,999,999.99")) ;
								  )
						endIf


						// grava a tabela de rateio de frete
						recLock( "SZS", .T. )
						SZS->ZS_FILIAL := xFilial("SZS")
						SZS->ZS_ROMAN := SZR->ZR_CODIGO
						SZS->ZS_PEDIDO := aPedFrete[nI][01]
						SZS->ZS_VLRAT := aPedFrete[nI][05]
						msUnlock()

					else
						lErro := .T.
						cMsgErro := "O pedido " + aPedFrete[nI][01] + " não foi localizado!"
					endIf

				endIf

			next nI

		EndIf

		endIf



		// tratamento de erros da rotina
		if ( lErro )

			disarmTransactions()
			if ( !empty(cMsgErro) )
				msgAlert(cMsgErro)
			endIf

		endIf

	end transaction

return nil


/**********************************************************************************************************************************/
/** static function getSC9Disp( cNumPed, cItem, cProd, cRoman )                                                                  **/
/** recupera a quantidade de itens disponíveis no Sc9 para montagem de romaneio                                                  **/
/**********************************************************************************************************************************/
static function getSC9Disp( cNumPed, cItem, cProd, cRoman )

  // retorno da função
  local nRet := 0

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // monta a query para fazer pesquisa
  cQr := " select SC9.C9_PEDIDO,
  cQr += "        SC9.C9_ITEM,
  cQr += "        SC9.C9_SEQUEN,
  cQr += "        SC9.C9_PRODUTO,
  cQr += "        SC9.C9_QTDLIB
  cQr += "   from " + retSqlName("SC9") + " SC9
  cQr += "  where SC9.D_E_L_E_T_ = ' '
  cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
  cQr += "    and SC9.C9_PEDIDO = '" + cNumPed + "'
  cQr += "    and SC9.C9_ITEM = '" + cItem + "'
  cQr += "    and SC9.C9_PRODUTO = '" + cProd + "'
  cQr += "    and SC9.C9_NFISCAL = ''
  cQr += "    and SC9.C9_SERIENF = ''
  cQr += "    and SC9.C9_ROMAN = ''

  TcQuery cQr new alias "QSC9"
  restArea(aArea)

return nil



/**********************************************************************************************************************************/
/** static function getTotC9Ds( cNumPed, cItem, cProd, cRoman )                                                                  **/
/** recupera a quantidade de itens disponíveis no Sc9 para montagem de romaneio                                                  **/
/**********************************************************************************************************************************/
static function getTotC9Ds( cNumPed, cItem, cProd, cRoman )

  // retorno da função
  local nRet := 0

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // monta a query para fazer pesquisa
  cQr := " select sum(SC9.C9_QTDLIB) C9_QTDLIB
  cQr += "   from " + retSqlName("SC9") + " SC9
  cQr += "  where SC9.D_E_L_E_T_ = ' '
  cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
  cQr += "    and SC9.C9_PEDIDO = '" + cNumPed + "'
  cQr += "    and SC9.C9_ITEM = '" + cItem + "'
  cQr += "    and SC9.C9_PRODUTO = '" + cProd + "'
  cQr += "    and SC9.C9_NFISCAL = ''
  cQr += "    and SC9.C9_SERIENF = ''
  cQr += "    and SC9.C9_ROMAN = ''

  TcQuery cQr new alias "QSC9"
  restArea(aArea)

  nRet := QSC9->C9_QTDLIB
  QSC9->(dbCloseArea())

return nRet


/**********************************************************************************************************************************/
/** user function E01RDel()                                                                                                      **/
/** rotina para deletar o romaneio fiscal                                                                                        **/
/**********************************************************************************************************************************/
user function E01RDel()

  // area
  local aArea := getArea()

  // query
  local cQr := ""

  // pedidos do romaneio
  local aPedRom := {}

  // contador de loop
  local nI := 0



  // confirma o usuário
  if ( !msgYesNo("Confirma a exclusão do Romaneio nº " + SZR->ZR_CODIGO + "?") )
    return nil
  endIf


  begin transaction

	  // recupera os pedidos que tiveram o frete alterado pelo romaneio
	  getPedRom( SZR->ZR_CODIGO )

	  // loop sobre os pedidos do romaneio
	  while ( !QSC5->(eof()) )

	    // adiciona o pedido ao array
	    aAdd( aPedRom, QSC5->ZQ_PEDIDO )

	    // proximo registro
	    QSC5->(dbSkip())

	  endDo

	  // fecha a query
	  QSC5->(dbCloseArea())


	  // loop sobre os pedidos para alterar o valor do frete
	  for nI := 1 to len(aPedRom)

	    // verifica se houve rateio do frete no pedido
	    SZS->(dbSetOrder(1))
	    if ( SZS->(dbSeek(xFilial("SZS") + SZR->ZR_CODIGO + aPedRom[nI])) )

	      // posiciona o pedido
	      SC5->(dbSetOrder(1))
	      if ( SC5->(dbSeek(xFilial("SC5") + SZS->ZS_PEDIDO)) )
	        // restaura o frete do pedido
	        recLock("SC5", .F. )
	        SC5->C5_FRTROMA -= SZS->ZS_VLRAT

	        // tratamento de arredondamento decimal
	        if ( SC5->C5_FRTROMA < 0 )
	          SC5->C5_FRTROMA := 0
	        endIf
	        SC5->C5_FRETEAG := SC5->C5_FRTROMA

	        // verifica se chegou a zero
	        if ( SC5->C5_FRTROMA == 0 )
	          // restaura o frete digitado pelo usuário no lançamento do pedido
	          SC5->C5_FRETEAG := SC5->C5_FRTOLD
	          SC5->C5_FRTOLD := 0
	        endIf

	        msUnlock()

	      endIf

	      // deleta o rateio do frete
	      recLock( "SZS", .F. )
	      SZS->(dbDelete())
	      msUnlock()

	    endIf

	  next nI


	  // deleta os itens
	  cQr := " update " + retSqlName("SZQ")
	  cQr += "    set D_E_L_E_T_ = '*'
	  cQr += "  where D_E_L_E_T_ = ' '
	  cQr += "    and ZQ_ROMAN = '" + SZR->ZR_CODIGO + "'
	  TcSqlExec(cQr)

	  // deleta o romaneio
	  cQr := " update " + retSqlName("SZR")
	  cQr += "    set D_E_L_E_T_ = '*'
	  cQr += "  where D_E_L_E_T_ = ' '
	  cQr += "    and ZR_CODIGO = '" + SZR->ZR_CODIGO + "'
	  TcSqlExec(cQr)

	  // limpa o campo de romaneio fiscal
	  cQr := " update " + retSqlName("SC9")
	  cQr += "    set C9_ROMAN = '" + space(tamSx3("C9_ROMAN")[1]) + "'
	  cQr += "  where C9_ROMAN = '" + SZR->ZR_CODIGO + "'
	  TcSqlExec(cQr)

  end transaction

return nil


/**********************************************************************************************************************************/
/** static function getPedRom()                                                                                                  **/
/** recupera a lista de pedidos do romaneio fiscal                                                                               **/
/**********************************************************************************************************************************/
static function getPedRom( cCodRoman )

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // monta a query para retornar os pedidos do romaneio
  cQr := " select distinct SZQ.ZQ_PEDIDO
  cQr += "   from " + retSqlName("SZQ") + " SZQ
  cQr += "  where SZQ.D_E_L_E_T_ = ' '
  cQr += "    and SZQ.ZQ_FILIAL = '" + xFilial("SZQ") + "'
  cQr += "    and SZQ.ZQ_ROMAN = '" + cCodRoman + "' "

  TcQuery cQr new alias "QSC5"
  restArea(aArea)

return nil


/**********************************************************************************************************************************/
/** static function dlgRomOk()                                                                                                   **/
/** faz a validação e checagem dos dados para gerar o valor do frete rateado para os pedidos informados                          **/
/**********************************************************************************************************************************/
static function dlgRomOk()
	Local nRoman:=0
	Local cCLiAtu:=''
	Local cNexCli:=''
  // area
  local aArea := getArea()

  // contador de loop
  local nI := 0

  // posicao do item de array
  local nPosItem := 0

  // valor total do pedido
  local nVlTotPed := 0

  // valor do item separado
  local nVlItemSp := 0

  // percentual de proporção do frete
  local nPropFret := 0

  // valor do frete rateado
  local nVlFrtRat := 0

  // total dos itens do romaneio
  local nTotRom := 0


  // verifica se foram selecionados itens
  if ( len(aItensSel) == 0 )
    // informa o usuario
    msgAlert("Não foram selecionados itens para o Romaneio!")
    return nil
  endIf

	//Alessandro
 	For nRoman :=1 to len(aItensSel)
	 	if ( nRoman != len(aItensSel) )
			//if aItensSel[nRoman][1]+aItensSel[nRoman][2] <> aItensSel[nRoman+1][1]+aItensSel[nRoman+1][2]
			SC6->(dbSetOrder(1))
		    SC6->(dbSeek(xFilial("SC6") + aItensSel[nRoman][02] + aItensSel[nRoman][03] + aItensSel[nRoman][05]))
		    cCli:= SC6->C6_CLI+SC6->C6_LOJA
		    SC6->(dbSeek(xFilial("SC6") + aItensSel[nRoman+1][02] + aItensSel[nRoman+1][03] + aItensSel[nRoman+1][05]))
		    cNextCli:= SC6->C6_CLI+SC6->C6_LOJA

			if cCli <> cNextCli
				Alert('Somente Romaneios para Clientes/Lojas Iguais!!')
				Return Nil
			Endif

		Endif
	 Next nRoman
  //Fim Alessandro

  // monta a lista de rateio do frete
  aPedFrete := {}
  for nI := 1 to len(aItensSel)

    // posiciona o item do pedido no SC6
    SC6->(dbSetOrder(1))
    if ( !SC6->(dbSeek(xFilial("SC6") + aItensSel[nI][02] + aItensSel[nI][03] + aItensSel[nI][05])) )
      // item nao encontrado
      msgAlert("Não foi possível localizar o item do pedido conforme abaixo:" + chr(13) + ;
               "Pedido: " + aItensSel[nI][02] + chr(13) + ;
               "Item: " + aItensSel[nI][03] + chr(13) + ;
               "Produto: "  + aItensSel[nI][05])
      return nil
    endIf

    // recupera o valor total do pedido
    nTotRom += SC6->C6_PRCVEN * aItensSel[nI][06]

    // calcula o valor total do item separado
    nVlItemSp := SC6->C6_PRCVEN * aItensSel[nI][06]

    // verifica se o pedido já está no array
    nPosItem := aScan( aPedFrete, {|x| x[01] == aItensSel[nI][02]} )
    if ( nPosItem == 0 )

      // adiciona o item
      aAdd( aPedFrete, { aItensSel[nI][02] ,; // numero do pedido
                         0.00              ,; // valor total dos itens do romaneio
                         nVlItemSp         ,; // valor do item separado
                         0.00              ,; // percentual de proporção do frete
                         0.00               ; // valor do frete rateado
                       }                    ;
          )

    else

      // incrementa o valor adicionado
      aPedFrete[nPosItem][03] += nVlItemSp

    endIf


  next nI


  // atualiza o valor total dos itens do romaneio no pedido
  for nI := 1 to len(aPedFrete)

    // atribui o valor total dos itens do romaneio
    aPedFrete[nI][02] := nTotRom

    // calcula o percentual de proporção do frete
    nPropFret := (aPedFrete[nI][03] * 100) / nTotRom
    aPedFrete[nI][04] := nPropFret

    // calcula o valor siga	do frete rateado
    nVlFrtRat := nEdFrete * (nPropFret / 100)
    aPedFrete[nI][05] := nVlFrtRat

  next nI


  // atualiza o array para visualizar o resumo no browse
  aLbxRatFrt := {}
  for nI := 1 to len(aPedFrete)
    // adiciona o item no array
    aAdd( aLbxRatFrt, { aPedFrete[nI][01] ,; // numero do pedido
                        transform(aPedFrete[nI][02], "@E 9,999,999,999.99") ,; // valor total do pedido
                        transform(aPedFrete[nI][03], "@E 9,999,999,999.99") ,; // valor do item do romaneio
                        transform(aPedFrete[nI][04], "@E 999.99")           ,; // percentual de proporção do frete
                        transform(aPedFrete[nI][05], "@E 9,999,999,999.99")  ; // valor do frete rateado
                      }                                                      ;
        )
  next nI

  // mostra o resultado em um browse
  showRatFrt()
  if ( !lDlgRatFrt )
    // cancelado pelo usuário
    return nil
  endIf

  // chama o procedimento para gravar o romaneio
  processa({|lEnd| grvRoman()})

return nil


/**********************************************************************************************************************************/
/** static function grvRoman()                                                                                                   **/
/** faz a gravação do romaneio                                                                                                   **/
/**********************************************************************************************************************************/
static function grvRoman()
	//Query para verificar se alguma vez foi feito um romaneio
	Local cQuery:=''
	Local nCount:=0

  // contador de loop
  local nI := 0
  local nA := 0

  // código do romaneio
  local cCodRoman := ""

  // flag de erro
  local lErro := .F.
  local cMsgErro := ""

  // proximo sequencial do SC9
  local cSeqSc9 := ""

  // macro de substituição
  local cMacro := ""

  Local nCampo
  Local aCamposTabela

  // clone do registro SC9
  private aSC9Clone := {}

  // totaliza a regra
  procRegua( len(aItensSel) + 1 )

  // inicia a transação para gravação do romaneio
  begin transaction

    // grava o romaneio fiscal
    incProc("Gravando Romaneio Fiscal...")

    // busca o próximo sequencial de romaneio
    // cCodRoman := getSx8Num("SZR", "ZR_CODIGO")
    // confirmSx8()

    cCodRoman := GetNextRom()

    recLock( "SZR", .T. )
    SZR->ZR_FILIAL := xFilial("SZR")
    SZR->ZR_CODIGO := cCodRoman
    SZR->ZR_DATA := dDataBase
    SZR->ZR_HORA := time()
    SZR->ZR_LITROS := nLbTotLit
    SZR->ZR_EMBALAG := nLbTotEmb
    SZR->ZR_FARDOS := nLbTotFar
    SZR->ZR_PESOBRT := nLbTotPes
    SZR->ZR_USUARIO := retCodUsr()
    SZR->ZR_VLFRETE := nEdFrete
    SZR->ZR_TRANSP := cEdCodTran
    SZR->ZR_NOMTRAN := cEdDesTran
//    SZR->ZR_CODVEIC := cEdCodVeic
//    SZR->ZR_NOMVEIC := cEdDesVeic
    SZR->ZR_TPFRETE := Iif( oCbFrete:nAt == 1, "CIF", "FOB" )
    SZR->ZR_VALITS := nLbTotVal


    // posiciona o primeiro item para gravar os dados do cliente
    SC9->(dbSetOrder(1))
    if ( SC9->(dbSeek(xFilial("SC9") + aItensSel[1][02] + aItensSel[1][03] + aItensSel[1][04] + aItensSel[1][05])) )

      // posiciona o pedido
      SC5->(dbSetOrder(1))
      if ( SC5->(dbSeek(XFilial("SC5") + SC9->C9_PEDIDO)) )

        if ( !(SC5->C5_TIPO $ "DB") )

			    // grava os dados do cliente
			  	if ( !empty(SC9->C9_CLIENTE) )

			  	  // posiciona o cliente
			  	  SA1->(dbSetOrder(1))
			  	  if ( SA1->(dbSeek(xFilial("SA1") + SC9->C9_CLIENTE + SC9->C9_LOJA)) )

				  	  SZR->ZR_CLIENTE := SA1->A1_COD
				  	  SZR->ZR_LOJACLI := SA1->A1_LOJA
				  	  SZR->ZR_NOMECLI := SA1->A1_NOME
				  	  SZR->ZR_MUN := SA1->A1_MUN
				  	  SZR->ZR_EST := SA1->A1_EST

			  	  endIf

			  	endIf

		    else

			    // grava os dados do fornecedor
			  	if ( !empty(SC9->C9_CLIENTE) )

			  	  // posiciona o cliente
			  	  SA2->(dbSetOrder(1))
			  	  if ( SA2->(dbSeek(xFilial("SA2") + SC9->C9_CLIENTE + SC9->C9_LOJA)) )

				  	  SZR->ZR_CLIENTE := SA2->A2_COD
				  	  SZR->ZR_LOJACLI := SA2->A2_LOJA
				  	  SZR->ZR_NOMECLI := SA2->A2_NOME
				  	  SZR->ZR_MUN := SA2->A2_MUN
				  	  SZR->ZR_EST := SA2->A2_EST

			  	  endIf

			  	endIf

		    endIf

	    endIf

    endIf

    // libera a trava do SZR
    SZR->(msUnlock())


    // loop sobre os itens selecionados
    for nI := 1 to len( aItensSel )

      // atualiza a barra de progressos
      incProc("Atualizando itens do Romaneio...")

      // faz a divisão do SC9
      SC9->(dbSetOrder(1))
      if ( SC9->(dbSeek(xFilial("SC9") + aItensSel[nI][02] + aItensSel[nI][03] + aItensSel[nI][04] + aItensSel[nI][05])) )

      	// verifica se há necessidade de divisão do SC9
      	if ( aItensSel[nI][06] < SC9->C9_QTDLIB )

	        // recupera a estrutura do SC9
	        //SX3->(dbSetOrder(1))
	        //SX3->(dbSeek("SC9"))

	        // loop sobre o SX3
	        aSc9Clone := {}
			/*
	        while (SX3->X3_ARQUIVO == "SC9") .and. ( !SX3->(eof()) )

	          if ( X3Uso(SX3->X3_USADO) ) .and. ( SX3->X3_VISUAL != "V" )

		          // adiciona o item no array
		          aAdd( aSc9Clone, { SX3->X3_CAMPO                         ,; // nome do campo
		                             &( "SC9->" + allTrim(SX3->X3_CAMPO) )  ; // conteudo
		                           }                                        ;
		              )

	          endIf

	          // proximo registro
	          SX3->(dbSkip())

	        endDo
	        */

			aCamposTabela := FWSX3Util():GetAllFields( 'SC9' , .T. )

            For nCampo := 1 To Len(aCamposTabela)
                cCampo := AllTrim(aCamposTabela[nCampo])

                If X3Uso(GetSX3Cache(cCampo,"X3_USADO")) .And. GetSX3Cache(cCampo,"X3_VISUAL") <> "V"

                    aAdd( aSc9Clone,    { ;
                                            cCampo                         ,; // nome do campo
		                                    &( "SC9->" + allTrim(cCampo) )  ; // conteudo
		                                } ;
		                )

                EndIf

            Next nCampo

	        // Djonata - 20140708

	        // busca o proximo sequencial do SC9
	        cSeqSc9 := getSeqSc9( SC9->C9_PEDIDO, SC9->C9_ITEM, SC9->C9_PRODUTO )

	        If Localiza(SC9->C9_PRODUTO)
	        	dbSelectArea('SDC')
	        	SDC->(dbSetOrder(1))
	        	SDC->(dbGoTop())
	        	If SDC->(dbSeek(xFilial('SDC')+SC9->C9_PRODUTO+SC9->C9_LOCAL+'SC6'+SC9->C9_PEDIDO+SC9->C9_ITEM+SC9->C9_SEQUEN+SC9->C9_LOTECTL))
			        // recupera a estrutura do SDC
			        SX3->(dbGoTop())
			        SX3->(dbSeek("SDC"))

			        // loop sobre o SX3"
			        aSDCClone := {}
			        /*while (SX3->X3_ARQUIVO == "SDC") .and. ( !SX3->(eof()) )

			          if ( X3Uso(SX3->X3_USADO) ) //.and. ( SX3->X3_VISUAL != "V" )

				          // adiciona o item no array
				          aAdd( aSDCClone, { SX3->X3_CAMPO                         ,; // nome do campo
				                             &( "SDC->" + allTrim(SX3->X3_CAMPO) )  ; // conteudo
				                           }                                        ;
				              )

			          endIf

			          // proximo registro
			          SX3->(dbSkip())

			        endDo*/

					aCamposTabela := FWSX3Util():GetAllFields( 'SDC' , .T. )

                    For nCampo := 1 To Len(aCamposTabela)
                        cCampo := AllTrim(aCamposTabela[nCampo])

                        If X3Uso(GetSX3Cache(cCampo,"X3_USADO")) .And. GetSX3Cache(cCampo,"X3_VISUAL") <> "V"

                            aAdd( aSDCClone,    { ;
                                                    cCampo                         ,; // nome do campo
                                                    &( "SDC->" + allTrim(cCampo) )  ; // conteudo
                                                } ;
                                )

                        EndIf

                    Next nCampo

					recLock( "SDC", .F. )
				    SDC->DC_QUANT := SDC->DC_QUANT - aItensSel[nI][06]
			        SDC->(msUnlock())

					recLock("SDC", .T.)

			        // loop sobre os campos do SC9
			        for nA := 1 to len(aSDCClone)

			          // atualiza o sc9
			          cMacro := "SDC->" + allTrim(aSDCClone[nA][1]) + " := aSDCClone[" + allTrim(str(nA)) + "][2]"
			          &(cMacro)

			        next nA

			        // atualiza o romaneio fiscal e a sequencia
			        SDC->DC_FILIAL := xFilial("SDC")
			        SDC->DC_ROMAN := cCodRoman
			        SDC->DC_SEQ := cSeqSc9
			        SDC->DC_QUANT := aItensSel[nI][06]
			        SDC->DC_ORIGEM := 'SC6'
			        SDC->DC_PRODUTO := SC9->C9_PRODUTO
							SDC->DC_TRT			:= cSeqSc9
							SDC->DC_PEDIDO	:= SC9->C9_PEDIDO
							SDC->DC_ITEM		:= SC9->C9_ITEM
							SDC->DC_QTDORIG	:= aItensSel[nI][06]
		          SDC->(msUnlock())

						EndIf
					EndIf
	    		// Fim Djonata

	        // deduz o saldo do SC9
	        recLock( "SC9", .F. )
		      SC9->C9_QTDLIB := SC9->C9_QTDLIB - aItensSel[nI][06]
	        msUnlock()


	        // inclui novo registro no SC9 com o item de romaneio atual
	        recLock("SC9", .T.)

	        // loop sobre os campos do SC9
	        for nA := 1 to len(aSC9Clone)

	          // atualiza o sc9
	          cMacro := "SC9->" + allTrim(aSC9Clone[nA][1]) + " := aSc9Clone[" + allTrim(str(nA)) + "][2]"
	          &(cMacro)

	        next nA

	        // atualiza o romaneio fiscal e a sequencia
	        SC9->C9_FILIAL := xFilial("SC9")
	        SC9->C9_ROMAN := cCodRoman
	        SC9->C9_SEQUEN := cSeqSc9
	        SC9->C9_QTDLIB := aItensSel[nI][06]
	        SC9->C9_IDENTB6:= POSICIONE('SC6',1,xFilial('SC6')+aItensSel[nI][2]+aItensSel[nI][3],'C6_IDENTB6')
          msUnlock()



	      else

	        // não há, apenas altera o numero do romaneio no SC9
	        recLock( "SC9", .F. )
	        SC9->C9_ROMAN := cCodRoman
	        msUnlock()

        endIf

      else
        // tratamento de erros
        lErro := .T.
        cMsgErro := "A liberação do produto abaixo não foi localizada na SC9:" + chr(13) + ;
                    "Pedido: " + aItensSel[nI][02] + chr(13) + ;
                    "Item: " + aItensSel[nI][03] + chr(13) + ;
                    "Seq.: " + aItensSel[nI][04] + chr(13) + ;
                    "Produto: " + aItensSel[nI][05] + chr(13) + ;
                    "Entre em contato com o Suporte Técnico!"
        exit
      endIf


      if ( !lErro )

	      // grava o item do romaneio
	      recLock( "SZQ", .T. )
	      SZQ->ZQ_FILIAL  := xFilial("SZQ")
	      SZQ->ZQ_ROMAN   := cCodRoman
	      SZQ->ZQ_PEDIDO  := aItensSel[nI][02]
	      SZQ->ZQ_ITEMPV  := aItensSel[nI][03]
	      SZQ->ZQ_SEQUEN  := SC9->C9_SEQUEN
	      SZQ->ZQ_LITROS  := aItensSel[nI][06]
	      SZQ->ZQ_EMBALAG := aItensSel[nI][07]
	      SZQ->ZQ_FARDOS  := aItensSel[nI][08]
	      SZQ->ZQ_PESOBRT := aItensSel[nI][09]
	      SZQ->ZQ_PRODUTO := aItensSel[nI][05]


	      // posiciona o cliente para gravar os dados no romaneio
	      SA1->(dbSetOrder(1))
	      if ( SA1->(dbSeek(xFilial("SA1") + SC9->C9_CLIENTE + SC9->C9_LOJA)) )

		      // grava os dados no item do romaneio
		      SZQ->ZQ_CLIENTE := SC9->C9_CLIENTE
		      SZQ->ZQ_LOJA := SC9->C9_LOJA
		      SZQ->ZQ_NOMECLI := SA1->A1_NOME
		      SZQ->ZQ_UF := SA1->A1_EST
		      SZQ->ZQ_MUN := SA1->A1_MUN

	      endIf

	      msUnlock()

      endIf

    next nI


    // verifica se houve erro no processo anterior
    if ( !lErro )

	    // loop sobre os pedidos de frete para atualizar o valor do frete no pedido de venda
	    for nI := 1 to len( aPedFrete )
	      //Alessandro Grava o Tipo do Frete no Pedido de Acordo, com o que foi gerado no Romaneio
				if ( SC5->(dbSeek(xFilial("SC5") + aPedFrete[nI][01])) )
				 //	Reclock('SC5',.F.)
					//SC5->C5_TPFRETE := Iif( oCbFrete:nAt == 1, "C", "F" )
					//MsUnlock()
				Endif
	      //Fim Alessandro

	      // verifica se existe valor do frete
	      if ( aPedFrete[nI][05] > 0 )

		      // posiciona o pedido de vendas
		      SC5->(dbSetOrder(1))
		      if ( SC5->(dbSeek(xFilial("SC5") + aPedFrete[nI][01])) )

		        // atualiza o valor do frete
		        recLock( "SC5", .F. )
		        //SC5->C5_TPFRETE := Iif( oCbFrete:nAt == 1, "C", "F" )

		        if ( SC5->C5_FRTROMA == 0 )
		          SC5->C5_FRTOLD := SC5->C5_FRETEAG
		        endIf

		        SC5->C5_FRTROMA += aPedFrete[nI][05]
		        SC5->C5_FRETEAG := SC5->C5_FRTROMA
		        msUnlock()

		        // verifica se o frete informado no pedido manualmente é menor que a soma do frete no cálculo do romaneio fiscal
		        if ( SC5->C5_FRTOLD != 0 ) .and. ( SC5->C5_FRTOLD < SC5->C5_FRTROMA ) .and. ( SC5->C5_FRTOLD != SC5->C5_FRTROMA )
		          // alerta o usuário sobre o valor do frete
		          msgAlert( "O valor do frete informado no pedido é inferior ao valor rateado:" + chr(13) + ;
		                    "Pedido: " + SC5->C5_NUM + chr(13) + ;
		                    "Frete Informado: " + allTrim(transform(SC5->C5_FRTOLD, "@E 9,999,999,999.99")) + chr(13) + ;
		                    "Frete rateado: " + allTrim(transform(SC5->C5_FRTROMA, "@E 9,999,999,999.99")) ;
		                  )
		        endIf



	          // grava a tabela de rateio de frete
	          recLock( "SZS", .T. )
	          SZS->ZS_FILIAL := xFilial("SZS")
	          SZS->ZS_ROMAN := cCodRoman
	          SZS->ZS_PEDIDO := aPedFrete[nI][01]
	          SZS->ZS_VLRAT := aPedFrete[nI][05]
	          msUnlock()

		      else
		        lErro := .T.
		        cMsgErro := "O pedido " + aPedFrete[nI][01] + " não foi localizado!"
		      endIf

	      endIf

	    next nI

    endIf

    //Verifica se já foi feito um romaneio com aquele número.
		cQuery := "SELECT ZR_CODIGO FROM SZR010 WITH(nolock) WHERE ZR_CODIGO ='"+cCodRoman+"' AND ZR_FILIAL = '01' AND R_E_C_N_O_ < "+cValToChar(SZR->(RECNO()))

		If Select ('Query')>0
			Query->(DBCloseArea())
		EndIf

		TCQuery cQuery New Alias 'Query'

		Query->(DbGoTOp())

		While ! Query->(Eof())
			nCount++
			Query->(DbSkip())
		End

		IF nCount >0 .And. !lErro

	    FATE1RWFEXC(cCodRoman)

	  Endif

 		Query->(DBCloseArea())

    // tratamento de erro no procedimento
    if ( lErro )
      disarmTransactions()
    endIf


  // finaliza a transação
  end transaction


  if ( lErro )
    msgAlert(cMsgErro)
  else

    // limpa as variáveis utilizadas
    aItensSel := {}
    aPedFrete := {}
    nEdFrete := 0
    oGdRom:setFocus()
		atuGdRom()
		atuSubTot()

    msgInfo("Romaneio nº " + cCodRoman + " gerado com sucesso!!!")
  endIf


return nil

/**********************************************************************************************************************************/
/** static function GetNextRom()                                                                                                 **/
/** retorna o próximo sequencial do romaneio                                                                                     **/
/**********************************************************************************************************************************/
static function GetNextRom()

// retorno da função
local cRet := ""

// query
local cQr := ""

// area local
local aArea := GetArea()

Local oGet1
Local cGet1 := "Define variable value"
Local oSay1
Local oSButton1
Local aAreaSZR:= SZR->(GetArea())
Static oDlg

  // recupera o ultimo numero do romaneio
  cQr := " select max(SZR.ZR_CODIGO) ZR_CODIGO
  cQr += "   from " + RetSqlName("SZR") + " SZR
  cQr += "  where SZR.D_E_L_E_T_ = ' '
  cQr += "    and SZR.ZR_FILIAL = '" + XFilial("SZR") + "'

  TcQuery cQr new alias "QNXT"
  RestArea(aArea)

  // retorna o proximo codigo e fecha a query
  cRet := QNXT->ZR_CODIGO
  QNXT->(dbCloseArea())


  // verifica o numero do romaneio
  if ( Empty(cRet) )
    cRet := StrZero( 1, 9 )
  else
    cRet := StrZero( Val(cRet) + 1, 9 )
  endIf

 DEFINE MSDIALOG oDlg TITLE "Alteração do Número do Romaneio" FROM 000, 000  TO 090, 150 COLORS 0, 16777215 PIXEL

    @ 005, 004 SAY oSay1 PROMPT "Número do Romaneio:" SIZE 061, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 015, 004 MSGET oGet1 VAR cRet SIZE 060, 010 OF oDlg COLORS 0, 16777215 PIXEL
    DEFINE SBUTTON oSButton1 FROM 031, 037 TYPE 01 OF oDlg ENABLE ACTION oDlg:End()

  ACTIVATE MSDIALOG oDlg CENTERED

    aAreaSZR:= SZR->(GetArea())
		SZR->(DBSetOrder(1))
		IF SZR->(DBSeek(xFilial('SZR')+cRet))
			Alert('Já Existe Romaneio com Esse Número !!!')
			cRet := GetNextRom()
		Endif
		RestArea(aAreaSZR)

return cRet


/**********************************************************************************************************************************/
/** static function getSeqSc9( cSc9Ped, cSc9Item, cSc9Prod )                                                                     **/
/** busca o próximo sequencial de liberação do SC9                                                                               **/
/**********************************************************************************************************************************/
static function getSeqSc9(cSc9Ped, cSc9Item, cSc9Prod)

  // retorno da função
  local cRet := ""

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // monta a query para buscar o proximo sequencial
  cQr := " select max(SC9.C9_SEQUEN) C9_SEQUEN
  cQr += "   from " + retSqlName("SC9") + " SC9
  cQr += "  where SC9.D_E_L_E_T_ = ' '
  cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
  cQr += "    and SC9.C9_PEDIDO = '" + cSc9Ped + "'
  cQr += "    and SC9.C9_ITEM = '" + cSc9Item + "'
  cQr += "    and SC9.C9_PRODUTO = '" + cSc9Prod + "'

  TcQuery cQr new alias "QSC9"
  restArea(aArea)

  cRet := soma1(QSC9->C9_SEQUEN)
  QSC9->(dbCloseArea())


return cRet


/**********************************************************************************************************************************/
/** static function showRatFrt()                                                                                                 **/
/** mostra o rateio do frete e solicita a confirmação do usuário                                                                 **/
/**********************************************************************************************************************************/
static function showRatFrt()

  // barra de botoes
  local oBar := nil

  // inicializa o flag
  lDlgRatFrt := .F.

  // define a tela
  define msDialog oDlgRatFrt from 001, 001 to 280, 430 title "Rateio de Frete" pixel

  // monta a barra de botoes
	define buttonBar oBar size 25,25 3D TOP of oDlgRatFrt
	define button resource "OK" of oBar action lDlgRatFrt := .T., ::end() prompt "OK" toolTip "Confirma o rateio"
	define button resource "CANCEL" of oBar action ::end() prompt "Cancelar" toolTip "Cancelar a operação"
	oBar:bRClicked:={ || AllwaysTrue() }


  // divisao  da tela para os filtros
  @ 012, 002 to 125, 216 title "Valor a ser rateado entre os pedidos:"

  // cria o listBox para mostrar os valores
  @ 020, 005 ListBox oLbxRatFrt Fields header "Pedido", "Valor Total", "Valor do Item", "% Prop.", "Rateio Frete" size 208, 102 ;
                                              colsizes 010, 040, 040, 040, 010 of oDlgRatFrt pixel
  oLbxRatFrt:SetArray(aLbxRatFrt)
  oLbxRatFrt:bLine := {|| { aLbxRatFrt[oLbxRatFrt:nAt][01] ,; // numero do pedido
  												  aLbxRatFrt[oLbxRatFrt:nAt][02] ,; // valor total do pedido
  												  aLbxRatFrt[oLbxRatFrt:nAt][03] ,; // valor do Item
  												  aLbxRatFrt[oLbxRatFrt:nAt][04] ,; // percentual de proporção
  												  aLbxRatFrt[oLbxRatFrt:nAt][05]  ; // valor do frete
  												}                                 ;
                      }



  activate MsDialog oDlgRatFrt centered


return nil




/**********************************************************************************************************************************/
/** static function separItem()                                                                                                  **/
/** faz a separação do item                                                                                                      **/
/**********************************************************************************************************************************/
static function separItem()

  // valida se o item está liberado do estoque
  //if (aGdRom[oGdRom:nAt][01] == oBmpBlEst) .or. (aGdRom[oGdRom:nAt][01] == oBmpBloq)
  //  msgAlert("Este item ainda não foi liberado do Estoque! Apenas itens liberados pelo Estoque podem ser selecionados no Romaneio!")
  //  return nil
  //endIf

  // monta a tela de separação de item
  mntDlgSep()

return nil


/**********************************************************************************************************************************/
/** static function mntDlgSep()                                                                                                  **/
/** monta a tela de separação de itens                                                                                           **/
/**********************************************************************************************************************************/
static function mntDlgSep()


  // barra de ferramentas
  local oBar := nil

  // posição do item de array
  local nPosItem := 0


  // inicializa o flag
  lDlgSep := .F.

  // verifica se o item já foi alterado
  nPosItem := aScan( aItensSel, {|x| x[01] == aGdRom[oGdRom:nAt][17] .and. ; // filial
                                     x[02] == aGdRom[oGdRom:nAt][07] .and. ; // pedido
                                     x[03] == aGdRom[oGdRom:nAt][08] .and. ; // item
                                     x[04] == aGdRom[oGdRom:nAt][16] .and. ; // sequencia
                                     x[05] == aGdRom[oGdRom:nAt][09]       ; // produto
                                }                                          ;
                   )


  // atualiza o valor das variaveis
  cEdSNumPed := aGdRom[oGdRom:nAt][07]
  cEdSItemPd := aGdRom[oGdRom:nAt][08]
  cEdSCodCli := substr(aGdRom[oGdRom:nAt][03], 1, tamSx3("A1_COD")[1])
  cEdSLojCli := substr(aGdRom[oGdRom:nAt][03], tamSx3("A1_COD")[1]+2, tamSx3("A1_LOJA")[1])
  cEdSNomCli := aGdRom[oGdRom:nAt][04]
  cEdSMunCli := aGdRom[oGdRom:nAt][05]
  cEdSEstCli := aGdRom[oGdRom:nAt][06]
  cEdSCodPro := aGdRom[oGdRom:nAt][09]
  cEdSDesPro := aGdRom[oGdRom:nAt][10]

  if ( nPosItem == 0 )
    // recupera os valores do browse
	  nEdSQtdLit := aGdRom[oGdRom:nAt][11]
	  nEdSQtdEmb := aGdRom[oGdRom:nAt][12]
	  nEdSQtdFar := aGdRom[oGdRom:nAt][13]
	  nEdSQtdPes := aGdRom[oGdRom:nAt][14]
  else
    // recupera os valores do array de seleções do romaneio
	  nEdSQtdLit := aItensSel[nPosItem][06]
	  nEdSQtdEmb := aItensSel[nPosItem][07]
	  nEdSQtdFar := aItensSel[nPosItem][08]
	  nEdSQtdPes := aItensSel[nPosItem][09]
  endIf


  // define a tela
  define msDialog oDlgSep from 001, 001 to 280, 430 title "Carregamento" pixel

  // monta a barra de botoes
	define buttonBar oBar size 25,25 3D TOP of oDlgSep
	define button resource "OK" of oBar action separItOk() prompt "OK" toolTip "Gravar separação"
	define button resource "CANCEL" of oBar action ::end() prompt "Cancelar" toolTip "Cancelar a operação"
	oBar:bRClicked:={ || AllwaysTrue() }


  // divisao  da tela para os filtros
  @ 012, 002 to 125, 216 title "Separar Item:"

  // pedido de venda
  @ 020, 006 say "Pedido:" pixel of oDlgSep
  @ 019, 038 get oEdSNumPed var cEdSNumPed picture pesqPict("SC9", "C9_PEDIDO") size 030, 009 when .F. pixel of oDlgSep

  // item do pedido
  @ 020, 070 say "Item:" pixel of oDlgSep
  @ 019, 083 get oEdSItemPd var cEdSItemPd picture pesqPict("SC9", "C9_ITEM") size 030, 009 when .F. pixel of oDlgSep

  // dados do cliente
  @ 033, 006 say "Cliente:" pixel of oDlgSep
  @ 032, 038 get oEdSCodCli var cEdSCodCli size 030, 009 picture pesqPict("SA1", "A1_COD") when .F. pixel of oDlgSep
  @ 032, 070 get oEdSLojCli var cEdSLojCli size 008, 009 picture pesqPict("SA1", "A1_LOJA") when .F. pixel of oDlgSep
  @ 032, 093 get oEdSNomCli var cEdSNomCli size 120, 009 picture pesqPict("SA1", "A1_NOME") when .F. pixel of oDlgSep

  // destino
  @ 046, 006 say "Destino:" pixel of oDlgSep
  @ 045, 038 get oEdSMunCli var cEdSMunCli size 070, 009 picture pesqPict("SA1", "A1_MUN") when .F. pixel of oDlgSep
  @ 045, 110 get oEdSEstCli var cEdSEstCli size 010, 009 picture pesqPict("SA1", "A1_EST") when .F. pixel of oDlgSep

  // produto
  @ 059, 006 say "Produto:" pixel of oDlgSep
  @ 058, 038 get oEdSCodPro var cEdSCodPro size 060, 009 picture pesqPict("SB1", "B1_COD") when .F. pixel of oDlgSep
  @ 058, 100 get oEdSDesPro var cEdSDesPro size 113, 009 picture pesqPict("SB1", "B1_DESC") when .F. pixel of oDlgSep


  // litros
  @ 072, 006 say "Litros:" color CLR_BLUE pixel of oDlgSep
  @ 071, 038 get oEdSQtdLit var nEdSQtdLit picture pesqPict("SZR", "ZR_LITROS") size 060, 009 valid valQtdLit() pixel of oDlgSep

  // embalagens
  @ 085, 006 say "Embalagens:" color CLR_BLUE pixel of oDlgSep
  @ 084, 038 get oEdSQtdEmb var nEdSQtdEmb picture pesqPict("SZR", "ZR_EMBALAG") size 060, 009 valid valQtdEmb() pixel of oDlgSep

  // fardos
  @ 098, 006 say "Fardos:" color CLR_BLUE pixel of oDlgSep
  @ 097, 038 get oEdSQtdFar var nEdSQtdFar size 060, 009 picture pesqPict("SZR", "ZR_FARDOS") valid valQtdFar() pixel of oDlgSep

  // peso bruto
  @ 111, 006 say "Peso Bruto:" color CLR_BLUE pixel of oDlgSep
  @ 110, 038 get oEdSQtdPes var nEdSQtdPes size 060, 009 picture pesqPict("SZR", "ZR_PESOBRT") pixel of oDlgSep


  // checkbox para calculo automatico dos valores
	@ 071, 100 checkbox oChAutCalc var lChAutCalc size 100, 008 pixel of oDlgSep prompt "&Recálculo automático" pixel



  // ativa a tela
  activate MsDialog oDlgSep centered


return nil


/**********************************************************************************************************************************/
/** static function separItOk()                                                                                                  **/
/** valida a separação dos itens digitados                                                                                       **/
/**********************************************************************************************************************************/
static function separItOk()

  // area
  local aArea := getArea()

  // qtde embalagem calculada
  local nQtdEmbCal := 0

  // qtde fardos calculada
  local nQtdFarCal := 0

  // peso bruto calculado
  local nQtdPesCal := 0

  // posicao do item de array
  local nPosItem := 0

  // posiciona o produto
  SB1->(dbSetOrder(1))
  if ( !SB1->(dbSeek(xFilial("SB1") + aGdRom[oGdRom:nAt][09])) )
	  msgAlert("O produto deste item não foi localizado!")
    return nil
  endIf

  // calcula a qtde de embalagens
  nQtdEmbCal := noRound( nEdSQtdLit / SB1->B1_QTDLIT )
  if ( nQtdEmbCal != nEdSQtdEmb )
    if ( !msgYesNo("Existe diferença entre a quantidade de Embalagens digitada e calculada pelo sistema!" + chr(13) + ;
                   "Digitado: " + allTrim(transform(nEdSQtdEmb, pesqPict("SZR", "ZR_EMBALAG"))) + chr(13) + ;
                   "Calculado: " + allTrim(transform(nQtdEmbCal, pesqPict("SZR", "ZR_EMBALAG"))) + chr(13) + ;
                   "Deseja continuar mesmo assim?") )
      return nil
    endIf
  endIf


  // calcula a qtde de fardos
  nQtdFarCal := noRound( nEdSQtdLit / SB1->B1_QTDLIT / SB1->B1_QTDFAR )
  if ( nQtdFarCal != nEdSQtdFar )
    if ( !msgYesNo("Existe diferença entre a quantidade de Fardos digitada e calculada pelo sistema!" + chr(13) + ;
                   "Digitado: " + allTrim(transform(nEdSQtdFar, pesqPict("SZR", "ZR_FARDOS"))) + chr(13) + ;
                   "Calculado: " + allTrim(transform(nQtdFarCal, pesqPict("SZR", "ZR_FARDOS"))) + chr(13) + ;
                   "Deseja continuar mesmo assim?") )
      return nil
    endIf
  endIf

  // calcula o peso bruto
  nQtdPesCal := noRound(nEdSQtdLit * SB1->B1_PESBRU)
  if ( nQtdPesCal != round(nEdSQtdPes, 2) )
    if ( !msgYesNo("Existe diferença entre o peso bruto digitado e calculado pelo sistema!" + chr(13) + ;
                   "Digitado: " + allTrim(transform(nEdSQtdPes, pesqPict("SZR", "ZR_PESOBRT"))) + chr(13) + ;
                   "Calculado: " + allTrim(transform(nQtdPesCal, pesqPict("SZR", "ZR_PESOBRT"))) + chr(13) + ;
                   "Deseja continuar mesmo assim?") )
      return nil
    endIf
  endIf


  // posiciona o item de array
  nPosItem := aScan( aItensSel, {|x| x[01] == aGdRom[oGdRom:nAt][17] .and. ; // filial
                                     x[02] == aGdRom[oGdRom:nAt][07] .and. ; // pedido
                                     x[03] == aGdRom[oGdRom:nAt][08] .and. ; // item
                                     x[04] == aGdRom[oGdRom:nAt][16] .and. ; // sequencia
                                     x[05] == aGdRom[oGdRom:nAt][09]       ; // produto
                                }                                          ;
                   )

  // verifica se o item já existe no array
  if ( nPosItem == 0 )
    // nao existe, inclui novo
    aAdd( aItensSel, { aGdRom[oGdRom:nAt][17]               ,; // filial
                       aGdRom[oGdRom:nAt][07]               ,; // pedido
                       aGdRom[oGdRom:nAt][08]               ,; // item
                       aGdRom[oGdRom:nAt][16]               ,; // sequencia
                       aGdRom[oGdRom:nAt][09]               ,; // produto
                       nEdSQtdLit                           ,; // qtde litros
                       nEdSQtdEmb                           ,; // qtde embalagens
                       nEdSQtdFar                           ,; // qtde fardos
                       nEdSQtdPes                           ,; // peso bruto
                       nEdSQtdLit * aGdRom[oGdRom:nAt][19]   ; // valor total
                     }                                       ;
        )

    // atualiza o item como marcado
    aGdRom[oGdRom:nAt][02] := "S"

  else
    // existe, altera
    aItensSel[nPosItem][06] := nEdSQtdLit
    aItensSel[nPosItem][07] := nEdSQtdEmb
    aItensSel[nPosItem][08] := nEdSQtdFar
    aItensSel[nPosItem][09] := nEdSQtdPes
    aItensSel[nPosItem][10] := nEdSQtdLit * aGdRom[oGdRom:nAt][19]
  endIf

   //Alessandro
  aGdRom[oGdRom:nAt][20]:=  nEdSQtdLit
  aGdRom[oGdRom:nAt][21]:=  aGdRom[oGdRom:nAt][11] - nEdSQtdLit
	//Fim ALessandro

  // chama a rotina para atualizar os totais
  atuSubTot()

  // fecha a janela se alteracao do item
  lDlgSep := .T.
  oDlgSep:end()

  //Alessandro
   oGdRom:refresh()
  //Fim ALesandro

return nil


/**********************************************************************************************************************************/
/** static function valQtdEmb()                                                                                                  **/
/** valida a quantidade de embalagens digitada                                                                                   **/
/**********************************************************************************************************************************/
static function valQtdEmb()

  // retorno da função
  local lRet := .F.

  // area
  local aArea := getArea()


  // qtde zero
  if ( nEdSQtdEmb <= 0 )
    //msgAlert("Informe a quantidade de embalagens para prosseguir!")
    //return lRet
  endIf

  // verifica se a quantidade é superior ao total liberado pelo estoque
  if ( nEdSQtdEmb > aGdRom[oGdRom:nAt][12] )
    msgAlert("Quantidade a separar superior ao liberado pelo Estoque!")
    return lRet
  endIf


  // quantidade ok, verifica se é para recalcular os outros itens
  if ( lChAutCalc )

    // posiciona o produto
    SB1->(dbSetOrder(1))
    if ( SB1->(dbSeek(xFilial("SB1") + aGdRom[oGdRom:nAt][09])) )
	    // recalcula os outros campos (fardos, litros e peso bruto)
	    nEdSQtdLit := noRound(SB1->B1_QTDLIT * nEdSQtdEmb)
	    nEdSQtdFar := noRound( nEdSQtdLit / SB1->B1_QTDLIT / SB1->B1_QTDFAR )
	    nEdSQtdPes := noRound(nEdSQtdLit * SB1->B1_PESBRU)
	  else
	    msgAlert("O produto deste item não foi localizado!")
	    return lRet
    endIf

  endIf


  // procedimento concluído
  restArea(aArea)
  lRet := .T.

return lRet


/**********************************************************************************************************************************/
/** static function valQtdFar()                                                                                                  **/
/** valida a quantidade de fardos digitada                                                                                       **/
/**********************************************************************************************************************************/
static function valQtdFar()

  // retorno da função
  local lRet := .F.

  // area
  local aArea := getArea()


  // qtde zero
  if ( nEdSQtdFar <= 0 )
    //msgAlert("Informe a quantidade de fardos para prosseguir!")
    //return lRet
  endIf

  // verifica se a quantidade é superior ao total liberado pelo estoque
  if ( nEdSQtdFar > aGdRom[oGdRom:nAt][13] )
    msgAlert("Quantidade a separar superior ao liberado pelo Estoque!")
    return lRet
  endIf


  // quantidade ok, verifica se é para recalcular os outros itens
  if ( lChAutCalc )

    // posiciona o produto
    SB1->(dbSetOrder(1))
    if ( SB1->(dbSeek(xFilial("SB1") + aGdRom[oGdRom:nAt][09])) )
	    // recalcula os outros campos (embalagens, litros e peso bruto)
	    nEdSQtdLit := noRound(SB1->B1_QTDLIT * SB1->B1_QTDFAR * nEdSQtdFar)
	    nEdSQtdEmb := noRound( nEdSQtdLit / SB1->B1_QTDLIT )
	    nEdSQtdPes := noRound(nEdSQtdLit * SB1->B1_PESBRU)
	  else
	    msgAlert("O produto deste item não foi localizado!")
	    return lRet
    endIf

  endIf


  // procedimento concluído
  restArea(aArea)
  lRet := .T.

return lRet



/**********************************************************************************************************************************/
/** static function valQtdLit()                                                                                                  **/
/** valida a quantidade de litros digitada                                                                                       **/
/**********************************************************************************************************************************/
static function valQtdLit()

  // retorno da função
  local lRet := .F.

  // area
  local aArea := getArea()


  // qtde zero
  if ( nEdSQtdLit <= 0 )
    //msgAlert("Informe a quantidade de litros para prosseguir!")
    //return lRet
  endIf

  // verifica se a quantidade é superior ao total liberado pelo estoque
  if ( nEdSQtdLit > aGdRom[oGdRom:nAt][11] )
    msgAlert("Quantidade a separar superior ao liberado pelo Estoque!")
    return lRet
  endIf


  // quantidade ok, verifica se é para recalcular os outros itens
  if ( lChAutCalc )

    // posiciona o produto
    SB1->(dbSetOrder(1))
    if ( SB1->(dbSeek(xFilial("SB1") + aGdRom[oGdRom:nAt][09])) )
	    // recalcula os outros campos (embalagens, fardos e peso bruto)
	    nEdSQtdEmb := noRound( nEdSQtdLit / SB1->B1_QTDLIT )
	    nEdSQtdFar := noRound( nEdSQtdLit / SB1->B1_QTDLIT / SB1->B1_QTDFAR )
	    nEdSQtdPes := noRound( nEdSQtdLit * SB1->B1_PESBRU )
	  else
	    msgAlert("O produto deste item não foi localizado!")
	    return lRet
    endIf

  endIf


  // procedimento concluído
  restArea(aArea)
  lRet := .T.

return lRet



/**********************************************************************************************************************************/
/** static function valSa1()                                                                                                     **/
/** valida o cliente informado                                                                                                   **/
/**********************************************************************************************************************************/
static function valSa1()

  // retorno da função
  local lRet := .F.

  // area atual
  local aArea := getArea()


  // verifica se o código do cliente foi preenchido
  if empty(cEdCodCli)
    cEdLojaCli := space(tamSx3("A1_LOJA")[1])
    cEdNomeCli := space(tamSx3("A1_NOME")[1])
		checkFil(oChLibTot)
    return .T.
  else

	  // dados preenchidos, valida o cadastro do cliente
	  SA1->(dbSetOrder(1))
		if ( SA1->(dbSeek(xFilial("SA1") + cEdCodCli)) )
		  cEdNomeCli := SA1->A1_NOME

	  endIf

  endIf

  // verifica se a loja foi preenchida
  if empty(cEdLojaCli)
  	restArea(aArea)
  	checkFil(oChLibTot)
    return .T.
  endIf


  // dados preenchidos, valida o cadastro do cliente
  SA1->(dbSetOrder(1))
	if ( !SA1->(dbSeek(xFilial("SA1") + cEdCodCli + cEdLojaCli)) )
    msgAlert("O cliente informado não está cadastrado!")
    cEdLojaCli := space(tamSx3("A1_LOJA")[1])
    oEdCodCli:setFocus()
  	restArea(aArea)
		checkFil(oChLibTot)
    return .T.
  endIf

  // cliente ok
  cEdNomeCli := SA1->A1_NOME
  lRet := .T.
	checkFil(oChLibTot)
  restArea(aArea)

return lRet

/**********************************************************************************************************************************/
/** static function checkFil( oCheck )                                                                                           **/
/** procedimento para checagem dos checkbox quando o usuário selecionar                                                          **/
/**********************************************************************************************************************************/
static function checkFil( oCheck )

  local lRet := .F.

  if (oCheck == oChLibTot) .and. (lChLibTot)
	  lChLibCre := .F.
	  lChLibEst := .F.
	  lChTodos := .F.
  endIf

  if oCheck == oChLibCre .and. (lChLibCre)
	  lChLibTot := .F.
	  lChLibEst := .F.
	  lChTodos := .F.
  endIf

  if oCheck == oChLibEst .and. (lChLibEst)
	  lChLibTot := .F.
	  lChLibCre := .F.
	  lChTodos := .F.
  endIf

  if oCheck == oChTodos .and. (lChTodos)
	  lChLibCre := .F.
	  lChLibEst := .F.
	  lChLibTot := .F.
  endIf

  //Alessandro - Bug nos totalizadores
  	aItensSel:= {}
  //Fim

  oChLibTot:refresh()
  oChLibCre:refresh()
  oChLibEst:refresh()
  oChTodos:refresh()

  // refaz o filtro dos dados
  atuGdRom()

return nil


/**********************************************************************************************************************************/
/** static function atuGdRom()                                                                                                   **/
/** monta os dados do getDados()                                                                                                 **/
/**********************************************************************************************************************************/
static function atuGdRom()

  // status do registros
  local oStatus := nil

  // flag de marcação do item
  local cFlagItem := "N"

  // posicao do item de array
  local nPosItem := 0

  // saldo em estoque
  local nSaldoB2 := 0


  // busca os dados de acordo com os parametros de seleção
  getSc9Rom()


  // inicializa o array de dados
  aGdRom := {}

  // loop sobre os dados
  while ( !QSC9->(eof()) )

    // verifica o flag do item
    nPosItem := aScan( aItensSel, { |x| x[1] == QSC9->C9_FILIAL .and. x[2] == QSC9->C9_PEDIDO .and. x[3] == QSC9->C9_ITEM .and. ;
                                        x[4] == QSC9->C9_SEQUEN .and. x[5] == QSC9->C9_PRODUTO                                  ;
                                  }                                                                                             ;
                     )
    cFlagItem := Iif( nPosItem > 0, "S", "N")

    // verifica se existe saldo sem romaneio
    if ( (QSC9->C9_QTDLIB - QSC9->ZQ_LITROS) > 0 )

      // posiciona o Sb2
      SB2->(dbSetOrder(1))
      if ( SB2->(dbSeek(xFilial("SB2") + QSC9->C9_PRODUTO + QSC9->C9_LOCAL)) )

	      // calcula o saldo em estoque
	      nSaldoB2 := SaldoSB2( .F.       ,; // considera necessidade
	                            .T.       ,; // considera empenho
	                            dDataBase ,; // data final
	                            .F.       ,; // considera poder de terceiros
	                            .F.       ,; // considera NPT
	                            "SB2"     ,; // alias SB2
	                            0         ,; // qtde empenhada do movimento
	                            0         ,; // qtde empenhada no projeto
	                            .T.        ; // substrai a reserva?
	                          )
      else
        nSaldoB2 := 0
      endIf

		  // adiciona o registro no Browse
		  // Alessandro 20150812
		    cMP:=     POSICIONE("SB1",1,xFilial("SB1")+Alltrim(QSC9->C9_PRODUTO),"B1_TIPO")
			cNfOr:=   POSICIONE("SC6",1,xFilial("SC6")+QSC9->C9_PEDIDO+QSC9->C9_ITEM,"C6_NFORI")
			cItemOr:= POSICIONE("SC6",1,xFilial("SC6")+QSC9->C9_PEDIDO+QSC9->C9_ITEM,"C6_ITEMORI")
			cSerOr:=  POSICIONE("SC6",1,xFilial("SC6")+QSC9->C9_PEDIDO+QSC9->C9_ITEM,"C6_SERIORI")
			cLoteOr:= POSICIONE("SD1",1,xFilial("SD1")+cNfOr+cSerOr+QSC9->C9_CLIENTE+QSC9->C9_LOJA+QSC9->C9_PRODUTO+cItemOr,"D1_LOTECTL")
			//Fim Alessandro
		  AAdd( aGdRom, { getSc9Stat()                                                                         ,; // status
		                  cFlagItem                                                                            ,; // marca
		                  QSC9->C9_CLIENTE + " " + QSC9->C9_LOJA                                               ,; // cod. cliente/fornecedor
		                  allTrim( Iif(QSC9->C5_TIPO $ "DB", QSC9->A2_NOME, QSC9->A1_NOME) )                   ,; // nome cliente/fornecedor
		                  allTrim( Iif(QSC9->C5_TIPO $ "DB", QSC9->A2_MUN, QSC9->A1_MUN) )                     ,; // cidade
		                  Iif(QSC9->C5_TIPO $ "DB", QSC9->A2_EST, QSC9->A1_EST)                                ,; // uf
		                  QSC9->C9_PEDIDO                                                                      ,; // numero pedido
		                  QSC9->C9_ITEM                                                                        ,; // item pedido
		                  allTrim(QSC9->C9_PRODUTO)                                                            ,; // cod. produto
		                  allTrim(QSC9->B1_DESC)                                                               ,; // desc. produto
		                  round(QSC9->C9_QTDLIB - QSC9->ZQ_LITROS, 2)                                          ,; // litros
		                  noRound((QSC9->C9_QTDLIB - QSC9->ZQ_LITROS) / QSC9->B1_QTDLIT)                       ,; // embalagens
		                  noRound(( (QSC9->C9_QTDLIB - QSC9->ZQ_LITROS) / QSC9->B1_QTDLIT) / QSC9->B1_QTDFAR)  ,; // fardos
		                  noRound( (QSC9->C9_QTDLIB - QSC9->ZQ_LITROS) * QSC9->B1_PESBRU)                      ,; // peso bruto
		                  IIF(QSC9->C9_CLIENTE=="001701".And.cMp=="MP",cLoteOr,QSC9->C9_LOTECTL)               ,; // lote
		                  QSC9->C9_SEQUEN                                                                      ,; // seq. liberacao SC9
		                  QSC9->C9_FILIAL                                                                      ,; // filial
		                  nSaldoB2                                                                             ,; // saldo em estoque
		                  QSC9->C6_PRCVEN                                                                       ; // valor unitário do item
		                  ,0;
		                  ,0;
		                }                                                                                       ;
		      )

	  endIf

    // proximo registro
    QSC9->(dbSkip())

  endDo

  // fecha a query
  QSC9->(dbCloseArea())


  if ( len(aGdRom) == 0 )

	  // adiciona o registro no Browse
	  AAdd( aGdRom, { oBmpBloq     ,; // status
	                  "N"          ,; // marca
	                  ""           ,; // cod. cliente
	                  ""           ,; // nome cliente
	                  ""           ,; // cidade
	                  ""           ,; // uf
	                  ""           ,; // numero pedido
	                  ""           ,; // item pedido
	                  ""           ,; // cod. produto
	                  ""           ,; // desc. produto
	                  0            ,; // litros
	                  0            ,; // embalagens
	                  0            ,; // fardos
	                  0            ,; // peso bruto
	                  ""           ,; // lote
	                  ""           ,; // seq. liberacao SC9
	                  ""           ,; // filial
	                  0            ,; // saldo em estoque
	                  0,             ; // valor unitario do item
	                  0,;
	                  0;
	                }               ;
	      )

  endIf

  // atualiza o browse
  oGdRom:setArray(aGdRom)
  oGdRom:bLine := {|| { aGdRom[oGdRom:nAt][01]                                                    ,; // status
                        Iif(aGdRom[oGdRom:nAt][02] == "S", oBmpOk, oBmpNo)                        ,; // sel.
                        aGdRom[oGdRom:nAt][03]                                                    ,; // cliente/loja
                        substr(aGdRom[oGdRom:nAt][04], 1, 25)                                     ,; // nome
                        aGdRom[oGdRom:nAt][05]                                                    ,; // cidade
                        aGdRom[oGdRom:nAt][06]                                                    ,; // uf
                        aGdRom[oGdRom:nAt][07]                                                    ,; // pedido
                        aGdRom[oGdRom:nAt][08]                                                    ,; // item
                        aGdRom[oGdRom:nAt][09]                                                    ,; // cod. produto
                        aGdRom[oGdRom:nAt][10]                                                    ,; // desc. produto
                        allTrim(transform(aGdRom[oGdRom:nAt][11], pesqPict("SZQ", "ZQ_LITROS")))  ,; // litros
                        allTrim(transform(aGdRom[oGdRom:nAt][12], pesqPict("SZQ", "ZQ_EMBALAG"))) ,; // embalagem
                        allTrim(transform(aGdRom[oGdRom:nAt][13], pesqPict("SZQ", "ZQ_FARDOS")))  ,; // fardos
                        allTrim(transform(aGdRom[oGdRom:nAt][14], pesqPict("SZQ", "ZQ_PESOBRT"))) ,; // peso bruto
                        aGdRom[oGdRom:nAt][15]                                                    ,; // lote
                        allTrim(transform(aGdRom[oGdRom:nAt][18], pesqPict("SB2", "B2_QATU")))    ,; // saldo em estoque
                        allTrim(transform(aGdRom[oGdRom:nAt][19], pesqPict("SC6", "C6_PRCVEN")))  ,; // valor unitário
                        allTrim(transform(aGdRom[oGdRom:nAt][20],	pesqPict("SB2", "B2_QATU")))		,;
                        allTrim(transform(aGdRom[oGdRom:nAt][21],	pesqPict("SB2", "B2_QATU")))	  ;
                      }                                                                            ;
                  }
  oGdRom:refresh()


  // chama a rotina para atualizar os sub totais
  atuSubTot()


return nil


/**********************************************************************************************************************************/
/** static function getSc9Stat()                                                                                                 **/
/** recupera o status do registro SC9                                                                                            **/
/**********************************************************************************************************************************/
static function getSc9Stat()

  // retorno da funcao
  local oRet := nil

  // verifica o status
  if ( QSC9->C9_BLEST != "  " .and. QSC9->C9_BLCRED != "  " )
    // bloqueio de crédito e estoque
    oRet := oBmpBloq
  elseif ( QSC9->C9_BLEST != "  " )
    // bloqueio de estoque
    oRet := oBmpBlEst
  elseif ( QSC9->C9_BLCRED != "  " )
    // bloqueio de credito
    oRet := oBmpBlCre
  else
    // produto liberado
    oRet := oBmpLib
  endIf

return oRet


/**********************************************************************************************************************************/
/** static function mntGdRom()                                                                                                   **/
/** monta o getDados do romaneio                                                                                                 **/
/**********************************************************************************************************************************/
static function mntGdRom()

  // retorno da função
  local oRet := nil

  // contador de loop
  local nI := 0


  // monta os registros
  aGdRom := { { oBmpBlEst ,; // status
                oBmpNo    ,; // marca
                ""        ,; // cod. cliente + loja
                ""        ,; // nome cliente
                ""        ,; // cidade
                ""        ,; // uf
                ""        ,; // numero pedido
                ""        ,; // item pedido
                ""        ,; // cod. produto
                ""        ,; // desc. produto
                 0        ,; // litros
                 0        ,; // embalagens
                 0        ,; // fardos
                 0        ,; // peso bruto
                ""        ,; // lote
                ""        ,; // seq. liberacao Sc9
                ""         ; // filial
              }            ;
            }

  // monta o array de campos da query
  aCpoCom := {}
  aAdd( aCpoCom, "S" )
  aAdd( aCpoCom, "X" )
  aAdd( aCpoCom, "Cliente/Loja" )
  aAdd( aCpoCom, "Nome" )
  aAdd( aCpoCom, "Cidade" )
  aAdd( aCpoCom, "UF" )
  aAdd( aCpoCom, "Pedido" )
  aAdd( aCpoCom, "Item" )
  aAdd( aCpoCom, "Cod. Prod." )
  aAdd( aCpoCom, "Desc. Prod." )
  aAdd( aCpoCom, "Litros" )
  aAdd( aCpoCom, "Embal." )
  aAdd( aCpoCom, "Fardos" )
  aAdd( aCpoCom, "Peso Brt." )
  aAdd( aCpoCom, "Lote" )
  aAdd( aCpoCom, "Saldo Disp." )
//Alessandro
  aAdd( aCpoCom, "Preço" )
  aAdd( aCpoCom, "Selecionado" )
  aAdd( aCpoCom, "Restante" )

  oGdRom := TWBrowse():New( 2.4, 0.3, 470, 200,,aCpoCom,{10,10,10,10,10,10,10,10,10,10,28,28,28,28,28,10,10,10,10}, oDlgRom,,,,,,,,,,,,.T.)
  oGdRom:bLDblClick := {|| dgRomDbK() }
  oGdRom:bHeaderClick := {|x, y| dbRomUp(y) }
  oGdRom:setArray(aGdRom)
  oGdRom:bLine := {|| { aGdRom[oGdRom:nAt][01]                                                    ,; // status
                        aGdRom[oGdRom:nAt][02]                                                    ,; // sel.
                        aGdRom[oGdRom:nAt][03]                                                    ,; // cliente/loja
                        substr(aGdRom[oGdRom:nAt][04], 1, 25)                                     ,; // nome
                        aGdRom[oGdRom:nAt][05]                                                    ,; // cidade
                        aGdRom[oGdRom:nAt][06]                                                    ,; // uf
                        aGdRom[oGdRom:nAt][07]                                                    ,; // pedido
                        aGdRom[oGdRom:nAt][08]                                                    ,; // item
                        aGdRom[oGdRom:nAt][09]                                                    ,; // cod. produto
                        aGdRom[oGdRom:nAt][10]                                                    ,; // desc. produto
                        allTrim(transform(aGdRom[oGdRom:nAt][11], pesqPict("SZQ", "ZQ_LITROS")))  ,; // litros
                        allTrim(transform(aGdRom[oGdRom:nAt][12], pesqPict("SZQ", "ZQ_EMBALAG"))) ,; // embalagem
                        allTrim(transform(aGdRom[oGdRom:nAt][13], pesqPict("SZQ", "ZQ_FARDOS")))  ,; // fardos
                        allTrim(transform(aGdRom[oGdRom:nAt][14], pesqPict("SZQ", "ZQ_PESOBRT"))) ,; // peso bruto
                        aGdRom[oGdRom:nAt][15]                                                    ,; // lote
                        allTrim(transform(aGdRom[oGdRom:nAt][18], pesqPict("SB2", "B2_QATU")))    ,; // saldo em estoque
	                      allTrim(transform(aGdRom[oGdRom:nAt][19], pesqPict("SC6", "C6_PRCVEN")))  ,; // valor unitário
                        allTrim(transform(aGdRom[oGdRom:nAt][20], pesqPict("SB2", "B2_QATU")))    ,; // saldo em estoque
                        allTrim(transform(aGdRom[oGdRom:nAt][21], pesqPict("SB2", "B2_QATU")))    ; // saldo em estoque
                      }                                                                            ;
                  }

return nil


/**********************************************************************************************************************************/
/** static function dgRomUp( nColClk )                                                                                           **/
/** responde ao evento HeaderClick do browse oGdRom                                                                              **/
/**********************************************************************************************************************************/
static function dbRomUp( nColClk )

  if ( nColClk == 3 )
    // ordena por cliente+loja
  	cCpoOrd := "SA1.A1_COD, SA1.A1_LOJA"
  elseif ( nColClk == 4 )
    // ordena por nome do cliente
  	cCpoOrd := "SA1.A1_NOME"
  elseif ( nColClk == 5 .or. nColClk == 6 )
    // ordena por uf+cidade
  	cCpoOrd := "SA1.A1_EST, SA1.A1_MUN"
  elseif ( nColClk == 7 )
    // ordena por numero do pedido
  	cCpoOrd := "SC9.C9_PEDIDO"
  elseif ( nColClk == 9 )
    // ordena por codigo do produto
  	cCpoOrd := "SB1.B1_COD"
  elseif ( nColClk == 10 )
    // ordena por descrição do produto
  	cCpoOrd := "SB1.B1_DESC"
  else
    // ordena por cliente+loja
  	cCpoOrd := "SA1.A1_COD, SA1.A1_LOJA"
  endIf

  // atualiza a grid
  atuGdRom()

return nil


/**********************************************************************************************************************************/
/** static function dgRomDbk()                                                                                                   **/
/** responde ao evento DoubleClick do browse oGdRom                                                                              **/
/**********************************************************************************************************************************/
static function dgRomDbK()

  // posicao do item de array
  local nPosItem := 0

  // flag de inclusao
  local lInc := .F.

  // valida se o item está liberado do estoque
  //if (aGdRom[oGdRom:nAt][01] == oBmpBlEst) .or. (aGdRom[oGdRom:nAt][01] == oBmpBloq)
  //  msgAlert("Este item ainda não foi liberado do Estoque! Apenas itens liberados pelo Estoque podem ser selecionados no Romaneio!")
  //  return nil
  //endIf


  // chama a alteração do item (se for marcação do item)
  if ( aGdRom[oGdRom:nAt][02] == "N" )

	  // chama a rotina para alterar os valores da separacão
	  separItem()
	  if ( !lDlgSep )
	    return nil
	  endIf
	  lInc := .T.

  endIf

  // recupera o item selecionad
  nPosItem := aScan( aItensSel, { |x| x[1] == aGdRom[oGdRom:nAt][17] .and. x[2] == aGdRom[oGdRom:nAt][07] .and. ;
                                      x[3] == aGdRom[oGdRom:nAt][08] .and. x[4] == aGdRom[oGdRom:nAt][16] .and. ;
                                      x[5] == aGdRom[oGdRom:nAt][09]                                            ;
                                }                                                                               ;
                   )


  // verifica se o item foi localizado
  if ( nPosItem == 0 )
    msgAlert("Erro ao localizar o item! Contate o suporte técnico!")
    return nil
  endIf


  // modifica o flag de marcação do Browse
  if ( aGdRom[oGdRom:nAt][02] == "S" ) .and. ( !lInc )

    aGdRom[oGdRom:nAt][02] := "N"

    aDel(aItensSel, nPosItem)
    aSize(aItensSel, len(aItensSel) - 1)

    //Alessandro
  	aGdRom[oGdRom:nAt][20]:=  0
	  aGdRom[oGdRom:nAt][21]:=  0
	  //Alessandro
  else
    aGdRom[oGdRom:nAt][02] := "S"
  endIf

  // atualiza o browse
  oGdRom:refresh()

  // atualiza os sub-totais
  atuSubTot()


  // atualiza o tipo do frete de acordo com o pedido
  SC5->(dbSetOrder(1))
  if ( SC5->(dbSeek(xFilial("SC5") + aGdRom[oGdRom:nAt][07])) )

    if ( SC5->C5_TPFRETE == "C" )
      oCbFrete:select(1)
      //cCbFrete := "CIF"
    else
      oCbFrete:select(2)
      //oCbFrete:nAt := 2
      //cCbFrete := "FOB"
    endIf

    oCbFrete:refresh()

  endIf

return nil


/**********************************************************************************************************************************/
/** static function atuSubTot()                                                                                                  **/
/** atualiza os sub-totais do browse                                                                                             **/
/**********************************************************************************************************************************/
static function atuSubTot()

  // contador de loop
  local nI := 0

  // inicializa as variaveis de totalizadores
  nLbTotLit := 0
  nLbTotEmb := 0
  nLbTotFar := 0
  nLbTotPes := 0
  nLbTotVal := 0

  // loop sobre os itens do romaneio
  for nI := 1 to len(aItensSel)

	  nLbTotLit += aItensSel[nI][06]
	  nLbTotEmb += aItensSel[nI][07]
	  nLbTotFar += aItensSel[nI][08]
	  nLbTotPes += aItensSel[nI][09]
	  nLbTotVal += aItensSel[nI][10]

  next nI


  // atualiza os labels de totais
	oLbTotLit:setText(nLbTotLit)
	oLbTotEmb:setText(nLbTotEmb)
	oLbTotFar:setText(nLbTotFar)
	oLbTotPes:setText(nLbTotPes)
	oLbTotVal:setText(nLbTotVal)

  oGdRom:refresh()

return nil


/**********************************************************************************************************************************/
/** static function getSc9Rom()                                                                                                  **/
/** recupera os itens dos pedidos de acordo com o filtro do usuário                                                              **/
/**********************************************************************************************************************************/
static function getSc9Rom()

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // monta a query para recuperar os dados
  cQr := " select SC5.C5_TIPO,
  cQr += "        SC9.C9_CLIENTE,
  cQr += "        SC9.C9_LOJA,
  cQr += "        SA1.A1_NOME,
  cQr += "        SA1.A1_MUN,
  cQr += "        SA1.A1_EST,
  cQr += "        SA2.A2_NOME,
  cQr += "        SA2.A2_MUN,
  cQr += "        SA2.A2_EST,
  cQr += "        SC9.C9_PEDIDO,
  cQr += "        SC9.C9_ITEM,
  cQr += "        SC9.C9_PRODUTO,
  cQr += "        SB1.B1_DESC,
  cQr += "        SC9.C9_QTDLIB,
  cQr += "        SB1.B1_QTDLIT,
  cQr += "        SB1.B1_QTDFAR,
  cQr += "        SB1.B1_PESBRU,
  cQr += "        SC9.C9_LOTECTL,
  cQr += "        SC9.C9_BLEST,
  cQr += "        SC9.C9_BLCRED,
  cQr += "        SC9.C9_BLWMS,
  cQr += "        SC9.C9_SEQUEN,
  cQr += "        SC9.C9_FILIAL,
  cQr += "        SC9.C9_LOCAL,
  cQr += "        SC6.C6_PRCVEN,

  cQr += "        ( select sum(SZQ.ZQ_LITROS)
  cQr += "            from " + retSqlName("SZQ") + " SZQ
  cQr += "           where SZQ.D_E_L_E_T_ = ' '
  cQr += "             and SZQ.ZQ_FILIAL = '" + xFilial("SZQ") + "'
  cQr += "             and SZQ.ZQ_PEDIDO = SC9.C9_PEDIDO
  cQr += "             and SZQ.ZQ_ITEMPV = SC9.C9_ITEM
  cQr += "             and SZQ.ZQ_SEQUEN = SC9.C9_SEQUEN
  cQr += "             and SZQ.ZQ_PRODUTO = SC9.C9_PRODUTO
  cQr += "        ) ZQ_LITROS

  cQr += "   from " + retSqlName("SC9") + " SC9

  cQr += "   left join " + retSqlName("SA1") + " SA1
  cQr += "     on SA1.D_E_L_E_T_ = ' '
  cQr += "    and SA1.A1_FILIAL = '" + xFilial("SA1") + "'
  cQr += "    and SA1.A1_COD = SC9.C9_CLIENTE
  cQr += "    and SA1.A1_LOJA = SC9.C9_LOJA

  cQr += "   left join " + retSqlName("SA2") + " SA2
  cQr += "     on SA2.D_E_L_E_T_ = ' '
  cQr += "    and SA2.A2_FILIAL = '" + xFilial("SA2") + "'
  cQr += "    and SA2.A2_COD = SC9.C9_CLIENTE
  cQr += "    and SA2.A2_LOJA = SC9.C9_LOJA

  cQr += "   left join " + retSqlName("SC6") + " SC6
  cQr += "     on SC6.D_E_L_E_T_ = ' '
  cQr += "    and SC6.C6_FILIAL = '" + xFilial("SC6") + "'
  cQr += "    and SC6.C6_NUM = SC9.C9_PEDIDO
  cQr += "    and SC6.C6_ITEM = SC9.C9_ITEM
  cQr += "    and SC6.C6_PRODUTO = SC9.C9_PRODUTO

  cQr += "  left join " + RetSqlName("SC5") + " SC5
  cQr += "     on SC5.D_E_L_E_T_ = ' '
  cQr += "    and SC5.C5_FILIAL = '" + XFilial("SC5") + "'
  cQr += "    and SC5.C5_NUM = SC9.C9_PEDIDO

  cQr += "   left join " + retSqlName("SB1") + " SB1
  cQr += "     on SB1.D_E_L_E_T_ = ' '
  cQr += "    and SB1.B1_FILIAL = '" + xFilial("SB1") + "'
  cQr += "    and SB1.B1_COD = SC9.C9_PRODUTO

  cQr += "  where SC9.D_E_L_E_T_ = ' '
  cQr += "    and SC9.C9_FILIAL = '" + xFilial("SC9") + "'

  // elimina as liberações faturadas
  cQr += "    and SC9.C9_NFISCAL = ''
  cQr += "    and SC9.C9_SERIENF = ''

  // elimina os itens que já estão em romaneio
  cQr += "    and SC9.C9_ROMAN = ''

  // pedidos liberados (credito e estoque)
  if ( lChLibTot )
    cQr += "  and SC9.C9_BLEST = '' and SC9.C9_BLCRED = '' and ( SC9.C9_BLWMS >= '05' or SC9.C9_BLWMS = '  ' )
  endIf

  // pedidos liberados por crédito
  if ( lChLibCre )
    cQr += "  and SC9.C9_BLCRED = ''
  endIf

  // pedidos liberados por estoque
  if ( lChLibEst )
    cQr += "  and SC9.C9_BLEST = ''
  endIf

  // filtra o cliente
  if ( !empty(cEdCodCli) )
    cQr += "  and SC9.C9_CLIENTE = '" + cEdCodCli + "'
  endIf

  if ( !empty(cEdLojaCli) )
    cQr += "  and SC9.C9_LOJA = '" + cEdLojaCli + "'
  endIf

  // ajusta a ordem da pesquisa
  cQr += "  order by " + cCpoOrd

  TcQuery cQr new alias "QSC9"
  restArea(aArea)

return nil


/**********************************************************************************************************************************/
/** user function E01RImp()                                                                                                      **/
/** imprime o romaneio de despacho                                                                                               **/
/**********************************************************************************************************************************/
user function E01RImp()

  // descrição do relatóro
  local cDesc1 := "Este programa tem como objetivo imprimir o relatório"
  local cDesc2 := "de Romaneio de Despacho."
  local cDesc3 := ""

  // número de linhas
  local nLi := 80

  // ordem de impressão
  local aOrd := {}

  // compressão
  local lCmp := .F.

  // array de parametros da pergunta
  local aArrayPerg := {}



  // título do relatório
  private cTit := "Romaneio de Despacho"

  // nome do programa
  private cProg := "FAR01R"

  // tamanho
  private cTam := "M"

  // fonte do relatorio
  private nTipo := 15

  // nome do arquivo do relatório
  private cRel := ""

  // linha
  private nLin := 0

  // nome da pergunta de parâmetros
  private cPerg := cProg

  private limite := 132


  // variáveis exclusivas do gerenciador de impressão
  private aReturn := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 0 }
  private m_pag := 1


  // cria as perguntas do relatório
  aAdd( aArrayPerg, { cPerg ,; // nome da pergunta
                      "Cod. Veiculo?" ,; // descrição
                      "C",; // tipo
                      tamSx3("ZD_CODVEI")[1],; // tamanho
                      tamSx3("ZD_CODVEI")[2],; // decimais
                      1,; // indice de pre seleção (combo)
                      "G",; // tipo de objeto
                      "",; // rotina de validação do Sx1
                      "SZD",; // F3
                      "",; // grupo de perguntas
                      "",; // item 1 do combo
                      "",; // item 2 do combo
                      "",; // item 3 do combo
                      ,; // item 4 do combo
                      ,; // item 5 do combo
                      {"Digite o código do veículo ou pressione " ,;
                       "a tecla <F3> para pesquisar." } ; // array de help
                    } ;
      )

  aAdd( aArrayPerg, { cPerg ,; // nome da pergunta
	 										"Observ. 1" ,; // descrição
                      "C",; // tipo
                      80,; // tamanho
                      0,; // decimais
                      1,; // indice de pre seleção (combo)
                      "G",; // tipo de objeto
                      "",; // rotina de validação do Sx1
                      "",; // F3
                      "",; // grupo de perguntas
                      "",; // item 1 do combo
                      "",; // item 2 do combo
                      "",; // item 3 do combo
                      ,; // item 4 do combo
                      ,; // item 5 do combo
                      {"Informe uma observação para que seja   " ,;
                       "impressa neste Romaneio."} ; // array de help
                    }  ;
      )

  aAdd( aArrayPerg, { cPerg ,; // nome da pergunta
	 										"Observ. 2" ,; // descrição
                      "C",; // tipo
                      80,; // tamanho
                      0,; // decimais
                      1,; // indice de pre seleção (combo)
                      "G",; // tipo de objeto
                      "",; // rotina de validação do Sx1
                      "",; // F3
                      "",; // grupo de perguntas
                      "",; // item 1 do combo
                      "",; // item 2 do combo
                      "",; // item 3 do combo
                      ,; // item 4 do combo
                      ,; // item 5 do combo
                      {"Informe uma observação para que seja   " ,;
                       "impressa neste Romaneio."} ; // array de help
                    }  ;
      )

  aAdd( aArrayPerg, { cPerg ,; // nome da pergunta
	 										"Observ. 3" ,; // descrição
                      "C",; // tipo
                      80,; // tamanho
                      0,; // decimais
                      1,; // indice de pre seleção (combo)
                      "G",; // tipo de objeto
                      "",; // rotina de validação do Sx1
                      "",; // F3
                      "",; // grupo de perguntas
                      "",; // item 1 do combo
                      "",; // item 2 do combo
                      "",; // item 3 do combo
                      ,; // item 4 do combo
                      ,; // item 5 do combo
                      {"Informe uma observação para que seja   " ,;
                       "impressa neste Romaneio."} ; // array de help
                    }  ;
      )



  // cria a lista de perguntas do relatório
  Processa({ |lEnd| procSx1(aArrayPerg) })

  // chama a criação das variáveis de pergunta
  Pergunte(cPerg, .F.)

  // chama a tela de seleção de impressão
  cRel := SetPrint(nil, cProg, cPerg, @cTit, cDesc1, cDesc2, cDesc3, .F., nil, .T., cTam, nil, nil)

  // verifica se o usuário cancelou
  if (nLastKey == 27)
	  return nil
  endIf

  // chama a impressao
  setDefault(aReturn, nil)

  // verifica se o usuário cancelou
  if (nLastKey == 27)
	  return nil
  endIf

  // chama a função para gerar o relatório
  Processa( {|lEnd| doFaR01R()} )

  // ajusta o dispositivo para a tela
  Set Device To Screen

  // se selecionou em disco, chama o visualizador de impressao
  if (aReturn[5] == 1)
	  set printer to
	  OurSpool(cRel)
  endIf

  // finaliza a impressão
  Ms_Flush()
  PrnFlush()

return nil


/**********************************************************************************************************************************/
/** static function doFaR01R()                                                                                                   **/
/** processa a impressão do romaneio                                                                                             **/
/**********************************************************************************************************************************/
static function doFaR01R()

  // código e loja do cliente
  local cCodCli := ""
  local cLojCli := ""

  // código do pedido
  local cCodPed := ""


  // totalizadores parciais
  local nParLit := 0
  local nParEmb := 0
  local nParFar := 0
  local nParPes := 0

  // totalizadores gerais
  local nGerLit := 0
  local nGerEmb := 0
  local nGerFar := 0
  local nGerPes := 0

  // quebra por produto
  local cQrProd := ""


  // posiciona a transportadora
  SA4->(dbSetOrder(1))
  SA4->(dbSeek(xFilial("SA4") + SZR->ZR_TRANSP))

  // recupera as variáveis de cod. do cliente, loja e pedido
  getPedCli( @cCodCli, @cLojCli, @cCodPed )

  // posiciona o pedido
  SC5->(dbSetOrder(1))
  SC5->(dbSeek(xFilial("SC5") + cCodPed))


  if ( SC5->C5_TIPO $ "DB" )
    // posiciona o fornecedor
    SA2->(dbSetOrder(1))
    SA2->(dbSeek(xFilial("SA2") + SZR->ZR_CLIENTE + SZR->ZR_LOJACLI))
  else
    // posiciona o cliente
    SA1->(dbSetOrder(1))
    SA1->(dbSeek(xFilial("SA1") + SZR->ZR_CLIENTE + SZR->ZR_LOJACLI))
  endIf

  // posiciona o vendedor
  SA3->(dbSetOrder(1))
  SA3->(dbSeek(xFilial("SA3") + SC5->C5_VEND1))

  // posiciona o cadastro de veiculos
  SZD->(dbSetOrder(1))
  SZD->(dbSeek(xFilial("SZD") + mv_par01))


  // inicializa a linha do relatório
  nLin := 6

  // imprime o cabeçalho da página
  cabec(cTit, "", "", cProg, cTam, nTipo)


  @ nLin, 001 pSay "Num. Romaneio...:"
  @ nLin, 019 pSay SZR->ZR_CODIGO
  @ nLin, 074 pSay "Emissão..:"
  @ nLin, 085 pSay DToC(SZR->ZR_DATA)
  @ nLin, 097 pSay "Valor Frete:"
  @ nLin, 110 pSay transform(SZR->ZR_VLFRETE, "@E 9,999,999,999.99")
  PLinha(1)

  if ( SC5->C5_TIPO $ "DB" )

    // imprime os dados do fornecedor
	  @ nLin, 001 pSay "Fornecedor......:"
	  @ nLin, 019 pSay allTrim(SA2->A2_COD) + "/" + allTrim(SA2->A2_LOJA) + " - " + Alltrim(SA2->A2_NOME)
		@ nLin, 097 pSay "Valor Itens:"
		@ nLin, 110 pSay transform(SZR->ZR_VALITS, "@E 9,999,999,999.99")
	  PLinha(1)

	  @ nLin, 001 pSay "Cidade/UF.......:"
	  @ nLin, 019 pSay allTrim(SA2->A2_MUN) + "/" + allTrim(SA2->A2_EST)
	  PLinha(1)

	  @ nLin, 001 pSay "CNPJ............:"
	  @ nLin, 019 pSay Iif( empty(SA2->A2_CGC), "", transform(allTrim(SA2->A2_CGC), "@R 99.999.999/9999-99") )

		// Alterado Djonata - 20140924
		@ nLin, 074 pSay "Dt.Saida Prev.:"
  	@ nLin, 084 pSay dToC(SZR->ZR_SAIDA)

		@ nLin, 107 pSay "Núm.Carga...:"
  	@ nLin, 121 pSay SZR->ZR_CARGA

		PLinha(1)

	  @nLin,001 pSay "Endereço........:"
		@nLin,019 pSay Rtrim(SA2->A2_END)+IIF(!Empty(SA2->A2_COMPLEM),' / '+rtrim(SA2->A2_COMPLEM),'')
		@nLin,107 pSay "Pallets.....:"
		@nLin,121 pSay SZR->ZR_PALLETS

	  PLinha(1)

  else

    // imprime os dados do cliente
	  @ nLin, 001 pSay "Cliente.........:"
	  @ nLin, 019 pSay allTrim(SA1->A1_COD) + "/" + allTrim(SA1->A1_LOJA) + " - " + Alltrim(SA1->A1_NOME)
		@ nLin, 097 pSay "Valor Itens:"
		@ nLin, 110 pSay transform(SZR->ZR_VALITS, "@E 9,999,999,999.99")
	  PLinha(1)

	  @ nLin, 001 pSay "Cidade/UF.......:"
	  @ nLin, 019 pSay allTrim(SA1->A1_MUN) + "/" + allTrim(SA1->A1_EST)
 	  //Alessandro
	  @ nLin, 074 pSay "Dt. Real:"
  	@ nLin, 084 pSay '|'+DToC(SZR->ZR_MOTDAT3)+'|'
		//Fim ALessandro
	  PLinha(1)

	  @ nLin, 001 pSay "CNPJ............:"
	  @ nLin, 019 pSay Iif( empty(SA1->A1_CGC), "", transform(allTrim(SA1->A1_CGC), "@R 99.999.999/9999-99") )

	// Alterado Djonata - 20140924
		@ nLin, 074 pSay "Dt. Prev.:"
  	@ nLin, 084 pSay dToC(SZR->ZR_SAIDA)

		@ nLin, 107 pSay "Núm.Carga...:"
  	@ nLin, 121 pSay SZR->ZR_CARGA

		PLinha(1)

	  @nLin,001 pSay "Endereço........:"
		@nLin,019 pSay Rtrim(SA1->A1_END)+IIF(!Empty(SA1->A1_COMPLEM),' / '+rTrim(SA1->A1_COMPLEM),'')
		@nLin,107 pSay "Pallets.....:"
		@nLin,121 pSay SZR->ZR_PALLETS
	  PLinha(1)

  endIf


  @ nLin, 001 pSay "Transportadora..:"
  @ nLin, 019 pSay allTrim(SZR->ZR_TRANSP) + " - " + allTrim(SZR->ZR_NOMTRAN)

  if ( len(allTrim(SA4->A4_CGC)) == 11 )
	  @ nLin, 074 pSay "CPF......:"
	  @ nLin, 084 pSay Iif( empty(SA4->A4_CGC), "",  transform(SA4->A4_CGC, "@R 999.999.999-99") )
  else
	  @ nLin, 074 pSay "CNPJ.....:"
	  @ nLin, 084 pSay Iif( empty(SA4->A4_CGC), "",  transform(SA4->A4_CGC, "@R 99.999.999/9999-99") )
  endIf

  @ nLin, 107 pSay "Tipo Frete..:"
  @ nLin, 121 pSay SZR->ZR_TPFRETE
  PLinha(1)

  @ nLin, 001 pSay "Vendedor........:"
  @ nLin, 019 pSay allTrim(SA3->A3_COD) + " - " + allTrim(SA3->A3_NOME)
  @ nLin, 074 pSay "Placa....:"
  @ nLin, 085 pSay SZD->ZD_PLACA
  @ nLin, 107 pSay "UF Veiculo..:"
  @ nLin, 121 pSay SZD->ZD_UFVEI
  PLinha(1)

  @ nLin, 001 pSay "Motorista.......:"
  @ nLin, 019 pSay SZD->ZD_CODVEI + ' - ' + IIF(SZD->ZD_BLOQ=='1','MOTORISTA BLOQUEADO',SZD->ZD_MOTOR)
  @ nLin, 074 pSay "RG.......:"
  @ nLin, 085 pSay SZD->ZD_RG
  @ nLin, 107 pSay "UF Emissor..:"
  @ nLin, 121 pSay SZD->ZD_SSP
  PLinha(1)

  @ nLin, 000 pSay __PrtThinLine()
  PLinha(1)

  // linha separando o cabeçalho dos itens
  //@ nLin, 001 pSay "Produto"
  //@ nLin, 017 pSay "Descrição do Material"
  //@ nLin, 060 pSay "Litros"
  //@ nLin, 069 pSay "Embalagens"
  //@ nLin, 086 pSay "Fardos"
  //@ nLin, 099 pSay "Peso Bruto"
  //@ nLin, 110 pSay "Lote"
  //@ nLin, 121 pSay "Pedido/It"
  @ nLin, 001 pSay "Produto    Descrição do Material                Litros   Embalagens      Fardos     Peso Bruto    Lote         Ender.     Pedido/It"
  //"Produto    Descrição do Material                Litros   Embalagens      Fardos     Peso Bruto    Lote         Ender.     Pedido/It
  // SGUAP0001  GRAP SUPER GUN AGROPART. 12X1     999,999.9|   999,999.9|   999,999.9|   999,999.99|  004456        001A       008814/03
  // 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
  //          1         2         3         4         5         6         7         8         9         10        11        12        13
  PLinha(1)

  @ nLin, 000 pSay __PrtThinLine()
  PLinha(1)

  // recupera os itens a imprimir no romaneio
  getItsImp()

  // loop sobre os itens
  cQrProd := ""
  while ( !QSZQ->(eof()) )

    // verifica a quebra
    if ( (cQrProd != QSZQ->C9_PRODUTO) .and. (!empty(cQrProd)) )

		  // imprime o totalizador do ultimo produto
		  @ nLin, 046 pSay "---------|------------|------------|-------------|-----------"
		  PLinha(1)

		  // imprime os totais
		  @ nLin, 046 pSay transform(nParLit, "@E 999,999.9") + "|"
		  @ nLin, 059 pSay transform(nParEmb, "@E 999,999.9") + "|"
		  @ nLin, 072 pSay transform(nParFar, "@E 999,999.9") + "|"
		  @ nLin, 085 pSay transform(nParPes, "@E 999,999.99") + "|"
		  PLinha(1)

		  @ nLin, 046 pSay "-------------------------------------------------------------"
		  PLinha(1)

		  // reseta as variaveis
		  cQrProd := QSZQ->C9_PRODUTO
		  nParLit := 0
		  nParEmb := 0
		  nParFar := 0
		  nParPes := 0

    endIf

	  //Alessandro 20150812
	  cMP:=     POSICIONE("SB1",1,xFilial("SB1")+Alltrim(QSZQ->C9_PRODUTO),"B1_TIPO")
		cNfOr:=   POSICIONE("SC6",1,xFilial("SC6")+QSZQ->C9_PEDIDO+QSZQ->C9_ITEM,"C6_NFORI")
		cItemOr:= POSICIONE("SC6",1,xFilial("SC6")+QSZQ->C9_PEDIDO+QSZQ->C9_ITEM,"C6_ITEMORI")
		cSerOr:=  POSICIONE("SC6",1,xFilial("SC6")+QSZQ->C9_PEDIDO+QSZQ->C9_ITEM,"C6_SERIORI")
		//NÃO PUXAR LOTE DA NOTA ORIGINAL QUANDO A TES MOVIMENTAR ESTOQUE
		cTES := POSICIONE("SC6",1,xFilial("SC6")+QSZQ->C9_PEDIDO+QSZQ->C9_ITEM,"C6_TES")
		cEst := Posicione("SF4",1,xFilial("SF4")+cTES,"F4_ESTOQUE")
		cLoteOr:= POSICIONE("SD1",1,xFilial("SD1")+cNfOr+cSerOr+SA1->A1_COD+SA1->A1_LOJA+QSZQ->C9_PRODUTO+cItemOr,"D1_LOTECTL")

    // imprime os itens
	  @ nLin, 001 pSay SubStr(QSZQ->C9_PRODUTO,1,9)
	  @ nLin, 012 pSay allTrim(substr(QSZQ->B1_DESC, 1, 30))
	  @ nLin, 046 pSay transform(QSZQ->ZQ_LITROS, "@E 999,999.9") + "|"
	  @ nLin, 059 pSay transform(QSZQ->ZQ_EMBALAG, "@E 999,999.9") + "|"
	  //Everson
	  @ nLin, 072 pSay transform(QSZQ->ZQ_FARDOS, "@E 999,999.9") + "|"
 	  //@ nLin, 072 pSay 'XXXXXXXX'
	  //Fim Everson
	  @ nLin, 085 pSay transform(QSZQ->ZQ_PESOBRT, "@E 999,999.99") + "|"
	  //@ nLin, 098 pSay IIF(SA1->A1_COD=="001701".And.cMp=="MP" ,Alltrim(cLoteOr),allTrim(QSZQ->C9_LOTECTL))
	  @ nLin, 098 pSay IIF(SA1->A1_COD=="001701".And.cMp=="MP" .And. cEst <> "S" ,Alltrim(cLoteOr),allTrim(QSZQ->C9_LOTECTL))
	  //Alessandro Solicitação Everson - Mesmo que tenha nota deve mostrar o endereço
	  //if Empty(QSZQ->DC_LOCALIZ)
			//	getlocaliz()
	  	//@ nLin, 112 pSay SubStr(TMPDC->DC_LOCALIZ,1,6)
	  //Else
	  	@ nLin, 112 pSay SubStr(QSZQ->DC_LOCALIZ,1,6)
	  //Endif
	  @ nLin, 123 pSay allTrim(QSZQ->C9_PEDIDO) + "/" + allTrim(QSZQ->C9_ITEM)

	  //"Produto    Descrição do Material                Litros   Embalagens      Fardos     Peso Bruto    Lote         Ender.     Pedido/It
	  // SGUAP0001  GRAP SUPER GUN AGROPART. 12X1     999,999.9|   999,999.9|   999,999.9|   999,999.99|  004456        001A       008814/03
	  // 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
	  //          1         2         3         4         5         6         7         8         9         10        11        12        13

    PLinha(1)

    // faz a totalizacao
    nParLit += QSZQ->ZQ_LITROS
    nParEmb += QSZQ->ZQ_EMBALAG
    nParFar += QSZQ->ZQ_FARDOS
    nParPes += QSZQ->ZQ_PESOBRT

    nGerLit += QSZQ->ZQ_LITROS
    nGerEmb += QSZQ->ZQ_EMBALAG
    nGerFar += QSZQ->ZQ_FARDOS
    nGerPes += QSZQ->ZQ_PESOBRT

		cQrProd := QSZQ->C9_PRODUTO

    // proximo item
    QSZQ->(dbSkip())

  endDo

  // fecha a query
  QSZQ->(dbCloseArea())


  // imprime o totalizador do ultimo produto
  @ nLin, 046 pSay "---------|------------|------------|-------------|-----------"
  PLinha(1)

  @ nLin, 046 pSay transform(nParLit, "@E 999,999.9") + "|"
  @ nLin, 059 pSay transform(nParEmb, "@E 999,999.9") + "|"
  @ nLin, 072 pSay transform(nParFar, "@E 999,999.9") + "|"
  @ nLin, 085 pSay transform(nParPes, "@E 999,999.99") + "|"

  PLinha(1)

  @ nLin, 000 pSay __PrtThinLine()
  PLinha(1)

  // imprime os totais
  @ nLin, 001 pSay "T O T A I S"
  @ nLin, 044 pSay transform(nGerLit, "@E 9,999,999.9")
  @ nLin, 057 pSay transform(nGerEmb, "@E 9,999,999.9")
  @ nLin, 070 pSay transform(nGerFar, "@E 9,999,999.9")
  @ nLin, 083 pSay transform(nGerPes, "@E 9,999,999.99")
  PLinha(1)

  @ nLin, 000 pSay __PrtThinLine()
  PLinha(1)

  // imprime a observação
  @ nLin, 001 pSay allTrim(mv_par02)
  PLinha(1)
  @ nLin, 001 pSay allTrim(mv_par03)
  PLinha(1)
  @ nLin, 001 pSay allTrim(mv_par04)
  PLinha(1)
  @ nLin, 001 pSay allTrim(SZR->ZR_OBSFIXA)
  PLinha(5)

  // espaço para prazo de entrega
  @ nLin, 001 pSay "Prazo de Entrega..:_____________________________________"
  PLinha(3)
  @ nLin, 001 pSay "Data..............:_______/_______/_______

  // imprime o rodapé da página
  //roda(0, cTit, cTam)

return nil


/**********************************************************************************************************************************/
/** static function getItsImp()                                                                                                  **/
/** recupera os itens a imprimir no romaneio                                                                                     **/
/**********************************************************************************************************************************/
static function getItsImp()

  // area
  local aArea := getArea()

  // query
  local cQr := ""


cQr += " select
cQr += " 	distinct SC9.C9_PRODUTO,
cQr += " 	SB1.B1_DESC,
cQr += " 	SC9.C9_QTDLIB ZQ_LITROS,
//cQr += " iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else SC9.C9_QTDLIB / SB1.B1_QTDLIT
cQr += " 	end ZQ_EMBALAG,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else case SB1.B1_QTDFAR
cQr += " 		when 0 then 0
cQr += " 		else SC9.C9_QTDLIB / SB1.B1_QTDLIT / SB1.B1_QTDFAR
cQr += " 		end
cQr += " 	end ZQ_FARDOS,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	SC9.C9_QTDLIB * SB1.B1_PESBRU ZQ_PESOBRT,
cQr += " 	SC9.C9_LOTECTL,
cQr += " 	SC9.C9_PEDIDO,
cQr += " 	SC9.C9_ITEM,
cQr += " 	'' AS DC_LOCALIZ, SC9.C9_SEQUEN
cQr += " from
cQr += 	+ retSqlName("SC9") + " SC9
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " left join
cQr += 		retSqlName("SB1") + " SB1
cQr += " 	on
cQr += " 		SB1.D_E_L_E_T_ = ' '
cQr += "    and SB1.B1_FILIAL = '" + xFilial("SB1") + "'
cQr += " 		and SB1.B1_COD = SC9.C9_PRODUTO
cQr += " where
cQr += " 	SC9.D_E_L_E_T_ = ' '
cQr += " 	and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
cQr += " 	and SC9.C9_ROMAN = '" + SZR->ZR_CODIGO + "'
cQr += "  AND SC9.C9_BLEST <> ''
cQr += " UNION ALL
cQr += " select
cQr += " 	distinct SC9.C9_PRODUTO,
cQr += " 	SB1.B1_DESC,
cQr += " 	SC9.C9_QTDLIB ZQ_LITROS,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else SC9.C9_QTDLIB / SB1.B1_QTDLIT
cQr += " 	end ZQ_EMBALAG,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else case SB1.B1_QTDFAR
cQr += " 		when 0 then 0
cQr += " 		else SC9.C9_QTDLIB / SB1.B1_QTDLIT / SB1.B1_QTDFAR
cQr += " 		end
cQr += " 	end ZQ_FARDOS,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	SC9.C9_QTDLIB * SB1.B1_PESBRU ZQ_PESOBRT,
cQr += " 	SC9.C9_LOTECTL,
cQr += " 	SC9.C9_PEDIDO,
cQr += " 	SC9.C9_ITEM,
cQr += " 	'' AS DC_LOCALIZ, SC9.C9_SEQUEN
cQr += " from
cQr += 	 retSqlName("SC9") + " SC9
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " left join
cQr += 		retSqlName("SB1") + " SB1
cQr += " 	on
cQr += " 		SB1.D_E_L_E_T_ = ' '
cQr += "    and SB1.B1_FILIAL = '" + xFilial("SB1") + "'
cQr += " 		and SB1.B1_COD = SC9.C9_PRODUTO
cQr += " inner join
cQr += 		retSqlName("SC6") + " SC6
cQr += " 	on
cQr += " 		SC6.D_E_L_E_T_ = ' '
cQr += "    and SC6.C6_FILIAL = '" + xFilial("SC6") + "'
cQr += " 		and SC6.C6_NUM = SC9.C9_PEDIDO"
cQr += " 		and SC6.C6_ITEM = SC9.C9_ITEM"
cQr += " inner join
cQr += 		retSqlName("SF4") + " SF4"
cQr += " 	on
cQr += " 		SF4.D_E_L_E_T_ = ' '
cQr += "    and SF4.F4_FILIAL = '" + xFilial("SF4") + "'
cQr += " 		and SC6.C6_TES = SF4.F4_CODIGO"
cQr += " where
cQr += " 	SC9.D_E_L_E_T_ = ' '
cQr += " 	and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
cQr += " 	and SC9.C9_ROMAN = '" + SZR->ZR_CODIGO + "'
//cQr += " 	AND SB1.B1_LOCALIZ <> 'S'  "//DESABILITAR PARA produtos recem habilitados controle de endereço
//cQr += " AND ((SB1.B1_LOCALIZ <> 'S' OR SF4.F4_ESTOQUE = 'N') AND SC9.C9_LOTECTL = '' ) "  removida validacao por lote no arquivo apos realizar liberacao de estoque
cQr += " AND ((SB1.B1_LOCALIZ <> 'S' OR SF4.F4_ESTOQUE = 'N') /* AND SC9.C9_LOTECTL = '' */) "
cQr += "  AND SC9.C9_BLEST = ''
cQr += " UNION ALL
cQr += " (select
cQr += " 	 SC9.C9_PRODUTO,
cQr += " 	SB1.B1_DESC,
cQr += " 	SUM(SDC.DC_QUANT) ZQ_LITROS,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else SUM(SDC.DC_QUANT) / SB1.B1_QTDLIT
cQr += " 	end ZQ_EMBALAG,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	case SB1.B1_QTDLIT
cQr += " 		when 0 then 0
cQr += " 		else case SB1.B1_QTDFAR
cQr += " 		when 0 then 0
cQr += " 		else SUM(SDC.DC_QUANT) / SB1.B1_QTDLIT / SB1.B1_QTDFAR
cQr += " 		end
cQr += " 	end ZQ_FARDOS,
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " 	SUM(SDC.DC_QUANT) * SB1.B1_PESBRU ZQ_PESOBRT,
cQr += " 	SDC.DC_LOTECTL C9_LOTECTL,
cQr += " 	SDC.DC_PEDIDO C9_PEDIDO,
cQr += " 	SDC.DC_ITEM C9_ITEM,
cQr += " 	DC_LOCALIZ, SC9.C9_SEQUEN
cQr += " from
cQr += 	 retSqlName("SC9") + " SC9
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " left join
cQr += 		retSqlName("SB1") + " SB1
cQr += " 	on
cQr += " 		SB1.D_E_L_E_T_ = ' '
cQr += "    and SB1.B1_FILIAL = '" + xFilial("SB1") + "'
cQr += " 		and SB1.B1_COD = SC9.C9_PRODUTO
//cQr += "  iuspa - aqui dava erro de compilação
cQr += " inner join
cQr +=  	retSqlName('SDC') + " SDC
cQr += " 	ON
cQr += " 		SDC.D_E_L_E_T_ = ''
cQr += " 		AND  DC_FILIAL+DC_PRODUTO+DC_LOCAL+DC_ORIGEM+DC_PEDIDO+DC_ITEM+DC_SEQ=C9_FILIAL+C9_PRODUTO+C9_LOCAL+'SC6'+C9_PEDIDO+C9_ITEM+C9_SEQUEN
cQr += " where
cQr += " 	SC9.D_E_L_E_T_ = ' '
cQr += " 	and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
cQr += " 	and SC9.C9_ROMAN = '" + SZR->ZR_CODIGO + "'
cQr += " 	AND SB1.B1_LOCALIZ = 'S'
cQr += " GROUP BY
cQr += " 	C9_PRODUTO,
cQr += " 	B1_DESC,
cQr += " 	B1_QTDLIT,
cQr += " 	B1_QTDFAR,
cQr += " 	B1_PESBRU,
cQr += " 	DC_LOTECTL,
cQr += " 	DC_PEDIDO,
cQr += " 	DC_ITEM,
cQr += " 	DC_LOCALIZ, SC9.C9_SEQUEN)
cQr += " order by
cQr += " 	SC9.C9_PRODUTO

//	MemoWrite('C:\TEMP\FAR01.SQL',CQR)  iuspa
  TcQuery cQr new alias "QSZQ"
  restArea(aArea)


// Tratativa para buscar os romaneios faturados antes da implantação do endereçamento
If QSZQ->(EOF())
  cQr := " select SC9.C9_PRODUTO,
  cQr += "         SB1.B1_DESC,
  cQr += "         SC9.C9_QTDLIB ZQ_LITROS,

  cQr += "         case SB1.B1_QTDLIT
  cQr += "           when 0 then 0
  cQr += "           else SC9.C9_QTDLIB / SB1.B1_QTDLIT
  cQr += "         end ZQ_EMBALAG,

  cQr += "         case SB1.B1_QTDLIT
  cQr += "           when 0 then 0
  cQr += "           else case SB1.B1_QTDFAR
  cQr += "                  when 0 then 0
  cQr += "                  else SC9.C9_QTDLIB / SB1.B1_QTDLIT / SB1.B1_QTDFAR
  cQr += "                end
  cQr += "         end ZQ_FARDOS,

  cQr += "         SC9.C9_QTDLIB * SB1.B1_PESBRU ZQ_PESOBRT,
  cQr += "         SC9.C9_LOTECTL,
  cQr += "         SC9.C9_PEDIDO,
  cQr += "         '' DC_LOCALIZ,
  cQr += "         SC9.C9_ITEM
  cQr += "    from " + retSqlName("SC9") + " SC9

  cQr += "    left join " + retSqlName("SB1") + " SB1
  cQr += "      on SB1.D_E_L_E_T_ = ' '
  cQr += "     and SB1.B1_FILIAL = '" + xFilial("SB1") + "'
  cQr += "     and SB1.B1_COD = SC9.C9_PRODUTO

  cQr += "   where SC9.D_E_L_E_T_ = ' '
  cQr += "     and SC9.C9_FILIAL = '" + xFilial("SC9") + "'
  cQr += "     and SC9.C9_ROMAN = '" + SZR->ZR_CODIGO + "'

  cQr += "   order by SC9.C9_PRODUTO


	QSZQ->(dbCloseArea())
	TcQuery cQr new alias "QSZQ"

EndIf

return nil


/**********************************************************************************************************************************/
/** static function getPedCli( cCodCli, cLojCli, cCodPed )                                                                       **/
/** recupera os dados do código do cliente, loja e codigo do pedido                                                              **/
/**********************************************************************************************************************************/
static function getPedCli( cCodCli, cLojCli, cCodPed )

  // area
  local aArea := getArea()

  // query
  local cQr := ""


  // recupera o primeiro pedido do romaneio
  cQr := " select ZQ_CLIENTE,
  cQr += "        ZQ_LOJA,
  cQr += "        ZQ_PEDIDO
  cQr += "   from " + retSqlName("SZQ") + " SZQ
  cQr += "  where SZQ.D_E_L_E_T_ = ' '
  cQr += "    and SZQ.ZQ_FILIAL = '" + xFilial("SZQ") + "'
  cQr += "    and SZQ.ZQ_ROMAN = '" + SZR->ZR_CODIGO + "'

  TcQuery cQr new alias "QSZQ"
  restArea(aArea)

  // atribui os retornos
  cCodCli := QSZQ->ZQ_CLIENTE
  cLojCli := QSZQ->ZQ_LOJA
  cCodPed := QSZQ->ZQ_PEDIDO

  // fecha a query
  QSZQ->(dbCloseArea())

return nil


/**********************************************************************************************************************************/
/** static function PLinha( nQtdLin )                                                                                            **/
/** cria a lista de perguntas do relatório                                                                                       **/
/**********************************************************************************************************************************/
static function PLinha( nQtdLin )

  // faz o saldo de linhas
  if ( (nLin + nQtdLin) > 80 )

  	// imprime o rodapé da página
  	//roda(0, cTit, cTam)
    nLin := 6
    cabec(cTit, "", "", cProg, cTam, nTipo)

  else

    // pula linha
    nLin += nQtdLin

  endIf

return nil


/**********************************************************************************************************************************/
/** static function procSx1()                                                                                                    **/
/** cria a lista de perguntas do relatório                                                                                       **/
/**********************************************************************************************************************************/
static function procSx1( aPergs )

  // array com os parametros
  local aParPerg := aPergs
  Local nI

  /* aParPerg[]
       aParPerg[n][01] : nome da pergunta
       aParPerg[n][02] : descrição
       aParPerg[n][03] : tipo
       aParPerg[n][04] : tamanho
       aParPerg[n][05] : decimais
       aParPerg[n][06] : indice de pre selecao de combo
       aParPerg[n][07] : tipo de objeto ( G=Edit|S=Text|C=Combo|R=Range|F=File|E=Expression|K=Check )
       aParPerg[n][08] : rotina de validação do SX1
       aParPerg[n][09] : F3
       aParPerg[n][10] : grupo de perguntas
       aParPerg[n][11] : item 1 do combo
       aParPerg[n][11] : item 2 do combo
       aParPerg[n][11] : item 3 do combo
       aParPerg[n][11] : item 4 do combo
       aParPerg[n][11] : item 5 do combo
       aParPerg[n][12] : array de help
  */

  // indice da pergunta
  local cIndice := "00"

  // parametro mv_ch
  local cVarCh := "mv_ch0"

  // parametro mv_par
  local cVarPar := "mv_par00"



  // inicializa as variaveis
  cIndice := "00"
  cVarCh := "mv_ch0"
  cVarPar := "mv_par00"

  // inicializa a barra de progressos
  procRegua(len(aParPerg))

  // loop sobre os parametros a adicionar
  for nI := 1 to len(aParPerg)

    // incrementa os contadores
    cIndice := soma1(cIndice)
    cVarCh := soma1(cVarCh)
    cVarPar := soma1(cVarPar)

    // incrementa a barra de progressos
    incProc("Criando perguntas " + allTrim(cIndice) + "/" + strZero(len(aParPerg), 2) + "..." )

	  // adiciona o parametro
	  putSX1( aParPerg[nI][01] ,; // nome da pergunta
	          cIndice          ,; // indice
	          aParPerg[nI][02] ,; // descricao portugues
	          aParPerg[nI][02] ,; // descricao espanhol
	          aParPerg[nI][02] ,; // descricao ingles
	          cVarCh           ,; // variavel mv_ch
	          aParPerg[nI][03] ,; // tipo
	          aParPerg[nI][04] ,; // tamanho
	          aParPerg[nI][05] ,; // decimais
	          aParPerg[nI][06] ,; // indice de pre-seleção (combo)
	          aParPerg[nI][07] ,; // tipo do objeto
	          aParPerg[nI][08] ,; // validação
	          aParPerg[nI][09] ,; // F3
	          aParPerg[nI][10] ,; // grupo de perguntas
	          " "              ,; // parametro pyme
	          cVarPar          ,; // variavel mv_par
	          aParPerg[nI][11] ,; // item 1 do combo (portugues)
	          aParPerg[nI][11] ,; // item 1 do combo (espanhol)
	          aParPerg[nI][11] ,; // item 1 do combo (ingles)
	          ""               ,; // conteudo padrao da pergunta
	          aParPerg[nI][12] ,; // item 2 do combo (portugues)
	          aParPerg[nI][12] ,; // item 2 do combo (espanhol)
	          aParPerg[nI][12] ,; // item 2 do combo (ingles)
	          aParPerg[nI][13] ,; // item 3 do combo (portugues)
	          aParPerg[nI][13] ,; // item 3 do combo (espanhol)
	          aParPerg[nI][13] ,; // item 3 do combo (ingles)
	          aParPerg[nI][14] ,; // item 4 do combo (portugues)
	          aParPerg[nI][14] ,; // item 4 do combo (espanhol)
	          aParPerg[nI][14] ,; // item 4 do combo (ingles)
	          aParPerg[nI][15] ,; // item 5 do combo (portugues)
	          aParPerg[nI][15] ,; // item 5 do combo (espanhol)
	          aParPerg[nI][15] ,; // item 5 do combo (ingles)
	          aParPerg[nI][16] ,; // memo de help (portugues)
	          aParPerg[nI][16] ,; // memo de help (espanhol)
	          aParPerg[nI][16] ,; // memo de help (ingles)
	          "" ; // help
	        )

  next nI

return nil

#Include "PROTHEUS.CH"
//--------------------------------------------------------------
/*/{Protheus.doc} MyFunction
Description

@param xParam Parameter Description
@return xRet Return Description
@author Djonata Guizzo - guizzo@vamilly.com.br
@since 25/09/2014
/*/
//--------------------------------------------------------------
User Function F01CARGA()
Local oCheckBo1
Local lCheckBo1 := .F.
Local lSMail1:= lSMail2:= lSMail3:= lSMail4:= .F.
Local oGet2
Local cGet2 := Space(9)
Local oGet3
Local cGet3 := Space(6)
Local oGet4
Local dGet4 := sToD('')
Local oGet5
Local dGet5 := sToD('')
Local dGet6:=dGet7:=dGet8:=sToD('')
Local oGet6
Local oGet7
Local oGet8
Local oSay1
Local oSay2
Local oSay3
Local oSay4
Local oSay5
Local oSButton2
Local oSButton3
Local oGetUsr1,oGetUsr2,oGetUsr3,oGetUsr4
Local cUsr1:= SZR->ZR_USR1
Local cUsr2:= SZR->ZR_USR2
Local cUsr3:= SZR->ZR_USR3
Local cUsr4:= SZR->ZR_USR4
Local lOk := .F.
Local aBox1 := aBox2 := aBox3 := aBox4:= {}
Local cXBox1 := cXBox2 := cXBox3 := cxBox4 := ''
Local cDescGrp4:= cDescGrp3:= cDescGrp2:= cDescGrp1 := ''
Static oDlg

  SZT->(DbSetOrder(1))
  SZT->(DbGotop())
  While !SZT->(EOF())
     aadd(aBox1,SZT->ZT_CODIGO+'-'+SZT->ZT_DESCR)
     SZT->(dBSKIP())
  end

  aBox2:= AClone(aBox1)
  aBox3:= AClone(aBox1)
  aBox4:= AClone(aBox1)

	cGet2 := SZR->ZR_CODIGO
	cGet3 := SZR->ZR_CARGA
	dGet4 := Iif(Empty(SZR->ZR_SAIDA),dDataBase,SZR->ZR_SAIDA)
	dGet5 := Iif(Empty(SZR->ZR_RDATA),dDataBase,SZR->ZR_RDATA)

	dGet6 := Iif(Empty(SZR->ZR_MOTDAT1),sToD(''),ZR_MOTDAT1)
	dGet7 := Iif(Empty(SZR->ZR_MOTDAT2),sToD(''),ZR_MOTDAT2)
	dGet8 := Iif(Empty(SZR->ZR_MOTDAT3),sToD(''),ZR_MOTDAT3)

	cXBox1 := IIF(Empty(SZR->ZR_MOTIVO1),aBox1[1],SZR->ZR_MOTIVO1)
	cXBox2 :=	IIF(Empty(SZR->ZR_MOTIVO2),aBox2[1],SZR->ZR_MOTIVO2)
	cXBox3 :=	IIF(Empty(SZR->ZR_MOTIVO3),aBox3[1],SZR->ZR_MOTIVO3)
	cXBox4 :=	IIF(Empty(SZR->ZR_MOTIVO4),aBox4[1],SZR->ZR_MOTIVO4)

	lCheckBo1 := SZR->ZR_STATUS == '1'

	If Empty(cGet3)
		cGet3 := GetCarga()
	EndIf

  DEFINE MSDIALOG oDlg TITLE "Carga do Romaneio" FROM 000, 000  TO 280, 600 COLORS 0, 16777215 PIXEL

    @ 011, 010 SAY oSay1 PROMPT "Romaneio:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 023, 010 SAY oSay2 PROMPT "Carga:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 035, 010 SAY oSay2 PROMPT "Saída Prev:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 047, 010 SAY oSay2 PROMPT "Saída Real 1:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 059, 010 SAY oSay3 PROMPT "Saída Real 2:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 071, 010 SAY oSay4 PROMPT "Saída Real 3:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 083, 010 SAY oSay5 PROMPT "Saída Real 4:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    //@ 047, 010 CHECKBOX oCheckBo1 VAR lCheckBo1 PROMPT "Carregamento Finalizado" SIZE 078, 008 OF oDlg COLORS 0, 16777215  PIXEL
    DEFINE SBUTTON oSButton2 FROM 100, 031 TYPE 01 OF oDlg ENABLE ACTION {||lOk:=.T.,oDlg:End()}
    DEFINE SBUTTON oSButton3 FROM 100, 072 TYPE 02 OF oDlg ENABLE ACTION {||oDlg:End()}
    @ 008, 045 MSGET oGet2 VAR cGet2 SIZE 053, 010 OF oDlg COLORS 0, 16777215 READONLY PIXEL
    @ 021, 045 MSGET oGet3 VAR cGet3 SIZE 053, 010 OF oDlg COLORS 0, 16777215 PIXEL
    @ 033, 045 MSGET oGet4 VAR dGet4 SIZE 053, 010 OF oDlg VALID dGet8:=dGet7:=dGet6:=dGet5:=dGet4 COLORS 0, 16777215 PIXEL
    @ 044, 045 MSGET oGet5 VAR dGet5 SIZE 053, 010 OF oDlg VALID dGet8:=dGet7:=dGet6:=dGet5 COLORS 0, 16777215 PIXEL
    @ 055, 045 MSGET oGet6 VAR dGet6 SIZE 053, 010 OF oDlg VALID dGet8:=dGet7:=dGet6 COLORS 0, 16777215 PIXEL
    @ 066, 045 MSGET oGet7 VAR dGet7 SIZE 053, 010 OF oDlg VALID dGet8:=dGet7 COLORS 0, 16777215 PIXEL
    @ 077, 045 MSGET oGet8 VAR dGet8 SIZE 053, 010 OF oDlg COLORS 0, 16777215 PIXEL

    // Usuários

    Iif(!Empty(SZR->ZR_USR1),cDescGrp1:= Posicione('SZG',1,xFilial('SZG')+cUsr1,'ZG_DESCRI'),'')
    Iif(!Empty(SZR->ZR_USR2),cDescGrp2:= Posicione('SZG',1,xFilial('SZG')+cUsr2,'ZG_DESCRI'),'')
    Iif(!Empty(SZR->ZR_USR3),cDescGrp3:= Posicione('SZG',1,xFilial('SZG')+cUsr3,'ZG_DESCRI'),'')
    Iif(!Empty(SZR->ZR_USR4),cDescGrp4:= Posicione('SZG',1,xFilial('SZG')+cUsr4,'ZG_DESCRI'),'')


    @ 042, 185 MSGET cUsr1 SIZE 025, 010 F3 'SZG' Valid {||cDescGrp1:= Posicione('SZG',1,xFilial('SZG')+cUsr1,'ZG_DESCRI')} /*When SZR->ZR_OK1 <> 'S'*/ OF oDlg COLORS 0, 16777215 PIXEL
    @ 054, 185 MSGET cUsr2 SIZE 025, 010 F3 'SZG' Valid {||cDescGrp2:= Posicione('SZG',1,xFilial('SZG')+cUsr2,'ZG_DESCRI')} /*When SZR->ZR_OK2 <> 'S'*/ OF oDlg COLORS 0, 16777215 PIXEL
		@ 066, 185 MSGET cUsr3 SIZE 025, 010 F3 'SZG' Valid {||cDescGrp3:= Posicione('SZG',1,xFilial('SZG')+cUsr3,'ZG_DESCRI')} /*When SZR->ZR_OK3 <> 'S'*/ OF oDlg COLORS 0, 16777215 PIXEL
		@ 078, 185 MSGET cUsr4 SIZE 025, 010 F3 'SZG' Valid {||cDescGrp4:= Posicione('SZG',1,xFilial('SZG')+cUsr4,'ZG_DESCRI')} /*When SZR->ZR_OK4 <> 'S'*/ OF oDlg COLORS 0, 16777215 PIXEL

		// Fim Usuários

		// Nomes Grupos

    @ 042, 220 MSGET oGrupo1 VAR cDescGrp1 SIZE 060, 010 F3 'SZG' When .F. OF oDlg COLORS 0, 16777215 PIXEL
    @ 054, 220 MSGET oGrupo2 VAR cDescGrp2 SIZE 060, 010 F3 'SZG' When .F. OF oDlg COLORS 0, 16777215 PIXEL
		@ 066, 220 MSGET oGrupo3 VAR cDescGrp3 SIZE 060, 010 F3 'SZG' When .F. OF oDlg COLORS 0, 16777215 PIXEL
		@ 078, 220 MSGET oGrupo4 VAR cDescGrp4 SIZE 060, 010 F3 'SZG' When .F. OF oDlg COLORS 0, 16777215 PIXEL

    oGrupo1:Refresh()
    oGrupo2:Refresh()
    oGrupo3:Refresh()
    oGrupo4:Refresh()

		// FIm Nomes Grupos

		// Envio de Workflow

		@ 042, 285 CHECKBOX lSMail1 PROMPT "1" SIZE 048, 008 OF oDlg COLORS 0, 16777215 PIXEL on Click MostraMsg(1,lSMail1)
		@ 054, 285 CHECKBOX lSMail2 PROMPT "2" SIZE 048, 008 OF oDlg COLORS 0, 16777215 PIXEL on Click MostraMsg(2,lSMail2)
		@ 066, 285 CHECKBOX lSMail3 PROMPT "3" SIZE 048, 008 OF oDlg COLORS 0, 16777215 PIXEL on Click MostraMsg(3,lSMail3)
		@ 078, 285 CHECKBOX lSMail4 PROMPT "4" SIZE 048, 008 OF oDlg COLORS 0, 16777215 PIXEL on Click MostraMsg(4,lSMail4)

    // FIm Wf

    oGet5:Refresh()
    oGet6:Refresh()
    oGet7:Refresh()
    oGet8:Refresh()

    @ 044, 105 ComboBox cXBox1 Items aBox1 SIZE 072, 010 OF oDlg COLORS 0, 16777215 PIXEL
		@ 055, 105 ComboBox cXBox2 Items aBox2 SIZE 072, 010 OF oDlg COLORS 0, 16777215 PIXEL
	  @ 066, 105 ComboBox cXBox3 Items aBox3 SIZE 072, 010 OF oDlg COLORS 0, 16777215 PIXEL
    @ 077, 105 ComboBox cXBox4 Items aBox4 SIZE 072, 010 OF oDlg COLORS 0, 16777215 PIXEL

  ACTIVATE MSDIALOG oDlg CENTERED

	If lOk
		RecLock('SZR',.F.)
			SZR->ZR_CARGA := cGet3
			//SZR->ZR_STATUS := IIf(lCheckBo1,'1','')
			SZR->ZR_SAIDA := dGet4
			SZR->ZR_RDATA := dGet5
			SZR->ZR_MOTDAT1 := dGet6
			SZR->ZR_MOTDAT2 := dGet7
			SZR->ZR_MOTDAT3 := dGet8
			SZR->ZR_MOTIVO1 := cXBox1
			SZR->ZR_MOTIVO2 := cXBox2
			SZR->ZR_MOTIVO3 := cXBox3
			SZR->ZR_MOTIVO4 := cXBox4
			SZR->ZR_USR1		:= cUsr1
			SZR->ZR_USR2		:= cUsr2
			SZR->ZR_USR3		:= cUsr3
			SZR->ZR_USR4		:= cUsr4
		MsUnlock()

		// Envia E-mails para os usuários que por algum motivo atrasaram as cargas
		IF lSMail1 .And. !Empty(cUsr1)
			Fate01Atr(cUsr1,1)
		Endif

		if lSMail2 .And. !Empty(cUsr2)
			Fate01Atr(cUsr2,2)
		Endif

		if lSMail3 .And. !Empty(cUsr3)
			Fate01Atr(cUsr3,3)
		Endif

		if lSMail4 .And. !Empty(cUsr4)
			Fate01Atr(cUsr4,4)
		Endif

	EndIf
Return

User Function F01CARRE()
Local oCheckBo1
Local lCheckBo1 := .F.
Local oGet2
Local cGet2 := Space(9)
Local oGet3
Local cGet3 := Space(6)
Local oGet4
Local dGet4 := sToD('')
Local oSay1
Local oSay2
Local oSButton2
Local oSButton3
Local lOk := .F.
Static oDlg

	cGet2 := SZR->ZR_CODIGO
	cGet3 := SZR->ZR_CARGA
	dGet4 := SZR->ZR_SAIDA
	lCheckBo1 := SZR->ZR_STATUS == '1'

	DEFINE MSDIALOG oDlg TITLE "Carga do Romaneio" FROM 000, 000  TO 145, 220 COLORS 0, 16777215 PIXEL

    @ 011, 010 SAY oSay1 PROMPT "Romaneio:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 023, 010 SAY oSay2 PROMPT "Carga:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 035, 010 SAY oSay2 PROMPT "Saída:" SIZE 033, 007 OF oDlg COLORS 0, 16777215 PIXEL
    @ 047, 010 CHECKBOX oCheckBo1 VAR lCheckBo1 PROMPT "Carregamento Finalizado" SIZE 078, 008 OF oDlg COLORS 0, 16777215 PIXEL
    DEFINE SBUTTON oSButton2 FROM 058, 031 TYPE 01 OF oDlg ENABLE ACTION {||lOk:=.T.,oDlg:End()}
    DEFINE SBUTTON oSButton3 FROM 058, 072 TYPE 02 OF oDlg ENABLE ACTION {||oDlg:End()}
    @ 008, 045 MSGET oGet2 VAR cGet2 SIZE 053, 010 OF oDlg COLORS 0, 16777215 READONLY PIXEL
    @ 021, 045 MSGET oGet3 VAR cGet3 SIZE 053, 010 OF oDlg COLORS 0, 16777215 READONLY PIXEL
    @ 033, 045 MSGET oGet4 VAR dGet4 SIZE 053, 010 OF oDlg COLORS 0, 16777215 READONLY PIXEL

  ACTIVATE MSDIALOG oDlg CENTERED

	If lOk
		RecLock('SZR',.F.)
			SZR->ZR_STATUS := IIf(lCheckBo1,'1','')
		MsUnlock()
	EndIf
Return

Static Function GetCarga()
	Local _cCarga := ''
	Local _cQry := "SELECT MAX(ZR_CARGA) AS CODCARGA FROM SZR010 WHERE D_E_L_E_T_ = '' AND ZR_CARGA <> '999999' "

	If Select('TMP9') > 0
		TMP9->(dbCloseArea())
	EndIf

	TcQuery _cQry New Alias 'TMP9'

	_cCarga := Soma1(TMP9->CODCARGA)

	TMP9->(dbCloseArea())
Return _cCarga

Static Function Getlocaliz()
Local cQueryDC := ''
Private cEndOld := ''
cQueryDC := "SELECT DC_LOCALIZ FROM SDC010 WHERE DC_PEDIDO ='"+QSZQ->C9_PEDIDO+"' AND DC_PRODUTO ='"+QSZQ->C9_PRODUTO+"' AND DC_ROMAN ='"+SZR->ZR_CODIGO+"' AND D_E_L_E_T_=''"

If Select('TMPDC') > 0
	TMPDC->(dbCloseArea())
EndIf

TcQuery cQueryDC New Alias 'TMPDC'

cEndOld:= TMPDC->DC_LOCALIZ

Return

User Function F01CADMOT()
Local _aArea := GetArea()

axCadastro("SZT","Cadastro de Motivos",".T.",".T.")

RestArea(_aArea)

Return


User Function F01ETIQ()
Local oGet1
Local _cNum := Space(10)
Local oGet2
Local _nVolume := cValToChar(Round(SZR->ZR_FARDOS,2))
Local oGroup1
Local oGroup2
Local oGroup3
Local oSay1
Local oSay2
Local oSay3
Local oSay4
Local oSay5
Local oSay6
Local oSay7
Local oSay8
Local oSButton1
Local oSButton2
Static oDlg
Local lExec:= .F.
Local oCheckBo1
Local lCheckBo1 := .F.
Local oFinCar
Local lFinCar := IIf( (SZR->ZR_STATUS=='1') .Or. U_E01RSts(SZR->ZR_CODIGO) == '3' ,.T.,.F.)
Local cBxPorta := cBxTam:= '1'
Local lGo := .F.
Local aBox := {"1=LPT1","2=LPT2","3=LPT3"}
Local aBoxTam := {"1=Pequena","2=Grande"}

DEFINE MSDIALOG oDlg TITLE "Etiquetas" FROM 000, 000  TO 425, 450 COLORS 0, 16777215 PIXEL

	@ -005, 000 GROUP oGroup1 TO 212, 225 OF oDlg COLOR 0, 16777215 PIXEL
  @ 013, 010 GROUP oGroup2 TO 116, 212 OF oDlg COLOR 0, 16777215 PIXEL
  @ 131, 010 GROUP oGroup3 TO 185, 213 OF oDlg COLOR 0, 16777215 PIXEL
  //Combo Box
  @ 015, 023 ComboBox cBxPorta Items aBox Size 072,010 PIXEL OF oDlg
  @ 015, 093 ComboBox cBxTam Items aBoxTam Size 072,010 PIXEL OF oDlg

  @ 035, 023 SAY oSay3 PROMPT "Cliente:" SIZE 025, 010 OF oDlg COLORS 0, 16777215 PIXEL
	@ 035, 053 SAY oSay3 PROMPT Alltrim(SZR->ZR_NOMECLI) SIZE 150, 010 OF oDlg COLORS 0, 16777215 PIXEL
  @ 052, 023 SAY oSay4 PROMPT "Estado:" SIZE 030, 010 OF oDlg COLORS 0, 16777215 PIXEL
 	@ 052, 053 SAY oSay3 PROMPT SZR->ZR_EST SIZE 025, 010 OF oDlg COLORS 0, 16777215 PIXEL
  @ 070, 023 SAY oSay5 PROMPT "Transp.:" SIZE 035, 010 OF oDlg COLORS 0, 16777215 PIXEL
 	@ 070, 053 SAY oSay3 PROMPT Alltrim(SZR->ZR_NOMTRAN) SIZE 150, 010 OF oDlg COLORS 0, 16777215 PIXEL
  @ 091, 023 SAY oSay6 PROMPT "Romaneio:" SIZE 038, 010 OF oDlg COLORS 0, 16777215 PIXEL
  @ 091, 053 SAY oSay6 PROMPT SZR->ZR_CODIGO SIZE 038, 010 OF oDlg COLORS 0, 16777215 PIXEL
  if lFincar
	  @ 091, 133 CHECKBOX oFinCar VAR lFinCar PROMPT "Finaliza Carregamento" SIZE 078, 008 OF oDlg When .F. COLORS 0, 16777215 PIXEL
	else
		@ 091, 133 CHECKBOX oFinCar VAR lFinCar PROMPT "Finaliza Carregamento" SIZE 078, 008 OF oDlg COLORS 0, 16777215 PIXEL
	Endif
  @ 163, 023 SAY oSay7 PROMPT "Volume:" SIZE 036, 010 OF oDlg COLORS 0, 16777215 PIXEL
  @ 144, 063 CHECKBOX oCheckBo1 VAR lCheckBo1 PROMPT "ESP." SIZE 078, 008 OF oDlg COLORS 0, 16777215 PIXEL
  @ 144, 023 SAY oSay8 PROMPT "Numero:" SIZE 030, 010 OF oDlg COLORS 0, 16777215 PIXEL
   @ 143, 097 MSGET oGet1 VAR _cNum SIZE 101, 013 OF oDlg COLORS 0, 16777215 PIXEL
  if lFincar == .F.
	  @ 163, 097 MSGET oGet2 VAR _nVolume SIZE 101, 013 OF oDlg COLORS 0, 16777215 PIXEL
	else
	  @ 163, 097 MSGET oGet2 VAR _nVolume SIZE 101, 013 OF oDlg when .F.  COLORS 0, 16777215 PIXEL
	Endif

  DEFINE SBUTTON oSButton1 FROM 198, 164 TYPE 01 ENABLE ACTION {||lGo:=.T.,oDlg:End()} of oDlg
  DEFINE SBUTTON oSButton2 FROM 198, 195 TYPE 02 Action oDlg:End() OF oDlg ENABLE

ACTIVATE MSDIALOG oDlg CENTERED

If lGo
	RecLock('SZR',.F.)
		SZR->ZR_STATUS := IIf(lFinCar,'1','')
	MsUnlock()

	/*configuracoes mais utilizadas de porta
	cPorta := "COM2:9600,n,8,1"
	cPorta := "COM2:9600,n,8,2"
	cPorta := "COM2:9600,n,7,1"
	cPorta := "COM2:9600,n,7,2"
	cPorta := "COM2:9600,e,8,1"
	cPorta := "COM2:9600,e,8,2"
	cPorta := "COM2:9600,e,7,1"
	cPorta := "COM2:9600,e,7,2"
	cPorta := "LPT1"         */

	if cBxPorta == '1'
		cPorta := "LPT1" // Mapeamento feito atraves de NET USE
	ElseIf cBxPorta =='2'
		cPorta := "LPT2"
	Elseif cBxPorta =='3'
		cPorta := "LPT3"
	Endif

	cModelo := "ZEBRA"

	MSCBPRINTER(cModelo, cPorta,,10,.F.,,,,,,.F.,)

	MSCBCHKSTATUS(.F.)

	MSCBBEGIN(1,6)
	MSCBSAY(30,10,"Cliente:"+SZR->ZR_CLIENTE, "N","A","026,008")
	MSCBSAY(30,15,"Romaneio:"+SZR->ZR_CODIGO, "N","A","026,008")
	MSCBSAY(40,15,"UF:"+SZR->ZR_EST, "N","A","026,008")
	MSCBSAY(50,15,Dtoc(dDatabase), "N","A","026,008")

	MSCBEND()
	MSCBCLOSEPRINTER()

Endif

Return

User Function E01OBS()
// variáveis auxiliares
local oDlgObs := Nil
Local oEdtInf := Nil
Local lExec := .F.
Local cEdtInf := SZR->ZR_OBSFIXA
Local cPallet:=SZR->ZR_PALLETS
Local oSay1
Local oGet1
// define a tela
define MsDialog oDlgObs from 000, 000 to 194, 500 title "Informações Fixas Romaneio" pixel

@ 005, 005 GET oEdtInf VAR cEdtInf MEMO SIZE 240, 70 OF oDlg PIXEL

@ 085,010 Say oSay1 Prompt 'Pallets' SIZE 040, 012 PIXEL OF oDlgObs
@ 080,030 MsGet oGet1 VAR cPallet SIZE 040, 012 PIXEL OF oDlgObs
@ 080,150 BUTTON 'Confirmar' SIZE 040, 012 PIXEL OF oDlgObs ACTION (lExec := .T., oDlgObs:End())
@ 080,190 Button "Cancelar" SIZE 040, 012 PIXEL OF oDlgObs ACTION (lExec := .F., oDlgObs:End())

// ativa a tela
activate MsDialog oDlgObs centered

if lExec
	Reclock('SZR',.F.)
		SZR->ZR_OBSFIXA:= cEdtInf
		SZR->ZR_PALLETS:=cPallet
	SZR->(MsUnlock())
Endif

return Nil

Static Function Fate01Atr(cUsr,nOpca)
Local _cHtml   	:= ''
Local _cCabec		:= ''
Local _cItens		:= ''
Local _cRodape	:= ''
Local _cItem		:= ''
Local cSubject 	:= 'WorkFlow Agrocete - Notificação de Atraso '+cValToChar(nOpca)+' Romaneio: '+SZR->ZR_CODIGO
Local cPara     := 'fabiano.nascimento@agrocete.com'
Local cCC				:= ''
Local cObs			:= iif(nOpca==1,SZR->ZR_MSGEMA1,iif(nOpca==2,SZR->ZR_MSGEMA2,iif(nOpca==3,SZR->ZR_MSGEMA3,iif(nOpca==4,SZR->ZR_MSGEMA4,''))))
Local dOldDate	:= iif(nOpca==1,SZR->ZR_SAIDA,iif(nOpca==2,SZR->ZR_RDATA,iif(nOpca==3,SZR->ZR_MOTDAT1,iif(nOpca==4,SZR->ZR_MOTDAT2,''))))
Local dNewDate	:= iif(nOpca==1,SZR->ZR_RDATA,iif(nOpca==2,SZR->ZR_MOTDAT1,iif(nOpca==3,SZR->ZR_MOTDAT2,iif(nOpca==4,ZR_MOTDAT3,''))))
Local aAreaSZF	:= SZF->(GetArea())

SZF->(DbSetOrder(1))
If SZF->(DbSeek(xFilial('SZF')+Alltrim(cUsr)))

	While !SZF->(Eof()) .And. SZF->ZF_GRUPO == Alltrim(cUsr)

		cPara+=';'+Alltrim(SZF->ZF_EMAIL)

		SZF->(DBSkip())

	End

Endif

_cCabec += '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
_cCabec += '<html xmlns="http://www.w3.org/1999/xhtml">
_cCabec += '<head>
_cCabec += '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
_cCabec += '<title>Atraso em Carga</title>
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
_cCabec += '			    Notificação de Atraso'
_cCabec += '            </td>
_cCabec += '        </td>
_cCabec += '    </table>
_cCabec += '</div>

_cCabec += '<div class="cabec" id="cabec" style="border: 1px solid #1A3984; background-color: #FFFFFF; color: #737270; height: auto; width: auto; padding: 10px; margin: 2px; font-size: 14px; box-shadow: 1px 2px 4px  rgba(117, 117, 117, 0.3); -moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3); -webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);">
_cCabec += '  <table width="100%" border="0">
_cCabec += '    <tr>
_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Emissão Wf.: </td>
_cCabec += '      <td width="44%">'+Dtoc(DDatabase)+ '</td>'
_cCabec += '      <td width="12%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Prev. Saida: </td>
_cCabec += '      <td width="44%">'+Dtoc(dOldDate)+ '</td>'
_cCabec += '      <td width="12%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Nova Prev.: </td>
_cCabec += '      <td width="44%">'+Dtoc(dNewDate)+ '</td>'
_cCabec += '    </tr>
_cCabec += '    <tr>
_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Romaneio: </td>
_cCabec += '      <td width="44%">'+SZR->ZR_CODIGO+ '</td>'
_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Cliente: </td>
_cCabec += '      <td width="44%">'+SZR->ZR_NOMECLI+'</td>'
_cCabec += '    </tr>
_cCabec += '    <tr>
_cCabec += '      <td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>
_cCabec += '    </tr>
_cCabec += '  </table>
_cCabec += '</div>

_cCabec2 := '<p>&nbsp;	</p>
_cCabec2 += '<div class="itens" id="itens" style="	background-color: #F4F4F4;border: 1px solid #1A3984;margin-top: -45px;margin-right: 2px;margin-bottom: 0px;margin-left: 2px;padding: 4px;box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);">
_cCabec2 += '<table width="100%" border="1"  cellspacing="0" class="tabitens" style="color: #4F4F4F;text-align: center;border-color: #999;font-size: 12px;">
_cCabec2 += '  <tr id="tablecabec">
//_cCabec2 += '    <td width="15%" align="center"  style="background-color:#698BB6; color: white;">Romaneio</td>
_cCabec2 += '    <td width="100%" align="center"  style="background-color:#698BB6; color: white;">Observação</td>
_cCabec2 += '  </tr>

_cItem += '  <tr id="tableitem" style="background-color:white;">
//_cItem += '    <td >@@Codigo@@</td>
_cItem += '    <td >@@Motivo@@</td>
_cItem += '  </tr>

_cRodape += '</table>
_cRodape += '</div>
_cRodape += '</body>
_cRodape += '</html>

_cHtml := _cCabec
_cItens:= ''
_cItens+= _cItem
//_cItens := StrTran(_cItens,"@@Codigo@@",SZR->ZR_CODIGO)
_cItens := StrTran(_cItens,"@@Motivo@@",cObs)
_cHtml+= _cCabec2+_cItens
_cHtml+= _cRodape

U_EnvMail(cSubject,_cHtml,cPara,cCC,'','',.T.)

Return

User Function Fate01Browse()
Local cAlias	:='SZR'
Local cALtUsrs	:= SuperGetMv("MV_XUSRJST",,'000072')
Local cFiltro	:=	''
Local aCores	:= {}
Local aAreaSZF	:= SZF->(GetArea())
Local cGrupos	:= ''
PRIVATE cCadastro := 'Justificativas'
PRIVATE aRotina   := { }

SZF->(DbSetOrder(2))
If SZF->(DbSeek(xFilial('SZF')+__cUserID))
	While !SZF->(Eof()) .And. SZF->ZF_USER == __cUserID
			cGrupos += SZF->ZF_GRUPO +'/'
		SZF->(DBSkip())
	End
Endif

DbSelectArea('SZR')
SZR->(DBSetOrder(1))
cGrupos := ForMatIn(cGrupos,'/')
cGrupos := StrTran(cGrupos,",''")

cFiltro :=	" Rtrim(ZR_USR1) IN "+cGrupos+" OR Rtrim(ZR_USR2) IN "+cGrupos+" OR Rtrim(ZR_USR3) IN  "+cGrupos+" OR Rtrim(ZR_USR4) IN "+cGrupos+""

if __cUserID $ cALtUsrs
	cFiltro := ''
Endif

IF Alltrim(SZR->ZR_USR1) $ cGrupos .OR. EMPTY(SZR->ZR_USR1)
	/*aCores := {{"ZR_OK1=='S'",'BR_VERDE'},;
						{"ZR_OK1<>'S'","BR_VERMELHO"}}*/
	aCores := {{"ZR_OK1=='S' .OR. EMPTY(SZR->ZR_USR1)",'BR_VERDE'},;
						{"ZR_OK1<>'S' .AND. !EMPTY(SZR->ZR_USR1)","BR_VERMELHO"}}
ElseIF Alltrim(SZR->ZR_USR2) $ cGrupos .OR. EMPTY(SZR->ZR_USR2)
	/*aCores := {{"ZR_OK2=='S'",'BR_VERDE'},;
						{"ZR_OK2<>'S'","BR_VERMELHO"}}*/
	aCores := {{"ZR_OK2=='S' .OR. EMPTY(SZR->ZR_USR2)",'BR_VERDE'},;
						{"ZR_OK2<>'S' .AND. !EMPTY(SZR->ZR_USR2)","BR_VERMELHO"}}
ElseIF Alltrim(SZR->ZR_USR3) $ cGrupos .OR. EMPTY(SZR->ZR_USR3)
	/*aCores := {{"ZR_OK3=='S'",'BR_VERDE'},;
						{"ZR_OK3<>'S'","BR_VERMELHO"}}*/
	aCores := {{"ZR_OK3=='S' .OR. EMPTY(SZR->ZR_USR3)",'BR_VERDE'},;
						{"ZR_OK3<>'S' .AND. !EMPTY(SZR->ZR_USR3)","BR_VERMELHO"}}
ElseIF Alltrim(SZR->ZR_USR4) $ cGrupos .OR. EMPTY(SZR->ZR_USR4)
	/*aCores := {{"ZR_OK4=='S'",'BR_VERDE'},;
						{"ZR_OK4<>'S'","BR_VERMELHO"}}*/
	aCores := {{"ZR_OK4=='S' .OR. EMPTY(SZR->ZR_USR4)",'BR_VERDE'},;
						{"ZR_OK4<>'S' .AND. !EMPTY(SZR->ZR_USR4)","BR_VERMELHO"}}
Endif

If Len(aCores) < 1 .Or. __cUserID $ cALtUsrs
	aCores := {{"ZR_OK4 =='S' .And. ZR_OK3 == 'S' .And. ZR_OK2 == 'S' .And. ZR_OK1 == 'S' ","BR_AZUL"},;
	{"ZR_OK4 =='S' .Or. ZR_OK3 == 'S' .Or. ZR_OK2 == 'S' .Or. ZR_OK1 == 'S' ","BR_AMARELO"},;
	{"Empty(ZR_USR1) .And. Empty(ZR_USR2) .And. Empty(ZR_USR3) .And. Empty(ZR_USR4)  ","BR_VERDE"},;
	{"(ZR_OK4 <>'S' .Or. ZR_OK3 <> 'S' .Or. ZR_OK2 <> 'S' .Or. ZR_OK1 <> 'S') .And. (ZR_USR1 <> ' ' .Or. ZR_USR2 <> ' ' .Or. ZR_USR3 <> ' ' .Or. ZR_USR4 <> ' ') ","BR_VERMELHO"}}
Endif

AADD(aRotina, { 'Pesquisar'	 , 'AxPesqui' 		,0,1})
AADD(aRotina, { "Visualizar" , "U_E01RAlt(.T.)" ,0,4})
Aadd(aRotina, { 'Justificar' , 'U_FATE01JST'	,0,4})
Aadd(aRotina, { 'Legenda'	 , 'U_FATE01LEG'	,0,5})

mBrowse(6,1,22,75,cAlias,,,,,,aCores,,,,,,,,cFiltro)

RestArea(aAreaSZF)

Return

User Function FATE01JST()
Local cGrupos	 := ''
Local aAreaSZF 	 := SZF->(GetArea())
Local aGrupos	 := {}
Local cCombo	 := '1'
Local lConfirmar :=.F.
Local cText  	 := ''
Local nI

private oDlg := Nil
private cEdtInf:='JUSTIFICATIVA MOTIVO: ' + Alltrim(Substr(SZR->ZR_MOTIVO1,5)) + CHR(13)+CHR(10)
Private oDlg1
Private oSay1
Private oSay2
Private oCxBox1
Private oSBtn1
Private oSBtn2

SZF->(DbSetOrder(2))
SZF->(DbSeek(xFilial('SZF')+__cUserID))
While !SZF->(Eof()) .And. SZF->ZF_USER == __cUserID
		cGrupos += SZF->ZF_GRUPO +'/'
	SZF->(DBSkip())
End

cGrupos:= ForMatIn(cGrupos,'/')
cGrupos:= StrTran(cGrupos,",''")

IIf(!Empty(SZR->ZR_USR1) .And. Alltrim(SZR->ZR_USR1) $ cGrupos,aadd(aGrupos,'1 - '+Substr(SZR->ZR_MOTIVO1,5)),'')
IIf(!Empty(SZR->ZR_USR2) .And. Alltrim(SZR->ZR_USR2) $ cGrupos,aadd(aGrupos,'2 - '+Substr(SZR->ZR_MOTIVO2,5)),'')
IIf(!Empty(SZR->ZR_USR3) .And. Alltrim(SZR->ZR_USR3) $ cGrupos,aadd(aGrupos,'3 - '+Substr(SZR->ZR_MOTIVO3,5)),'')
IIf(!Empty(SZR->ZR_USR4) .And. Alltrim(SZR->ZR_USR4) $ cGrupos,aadd(aGrupos,'4 - '+Substr(SZR->ZR_MOTIVO4,5)),'')

For nI:=1 to Len(aGrupos)
	cCombo:= aGrupos[nI]
	Exit
Next nI

cText := ChangeText(cCombo)

oDlg1   := MSDialog():New( 092,232,500,600,"Justificativa Motivo Enviado Por E-mail",,,.F.,,,,,,.T.,,,.T. )
oSay1 	:= TSay():New( 020,004,{||"Ocorrência"},oDlg1,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,036,012)
oSay2 	:= TSay():New( 050,052,{||cText},oDlg1,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,999,999)
oCBox1	:= TComboBox():New( 020,052,{|u|if(PCount()>0,cCombo:=u,cCombo)},,072,010,oDlg1,,,,CLR_BLACK,CLR_WHITE,.T.,,"",,,,,,, )

oCBox1:SetItems( aGrupos )

oCBox1:bChange:= {|| cText:= ChangeText(cCombo)}
oSay2:Refresh()

oSBtn1  := SButton():New( 180,068,1,{||lConfirmar:=.T.,oDlg1:End()},oDlg1,,"", )
oSBtn2  := SButton():New( 180,100,2,{||lConfirmar:=.F.,oDlg1:End()},oDlg1,,"", )

oDlg1:Activate(,,,.T.)

If lConfirmar
	GrvJust(cCombo)
Endif

RestArea(aAreaSZF)

Return

Static Function ChangeText(cOpcao)
Local cTexto := ''
Local cMotivo := ''
Local nXi


Iif(Substr(cOpcao,1,1) == '1', cMotivo:= SZR->ZR_MSGEMA1, iif(Substr(cOpcao,1,1) == '2', cMotivo:= SZR->ZR_MSGEMA2,iif(Substr(cOpcao,1,1) == '3', cMotivo:= SZR->ZR_MSGEMA3, IIf(Substr(cOpcao,1,1) == '4', cMotivo:= SZR->ZR_MSGEMA4,''))))

cMotivo:= Alltrim(cMotivo)

nLinhas := MLCount(cMotivo,40)
For nXi:= 1 To nLinhas
        cTexto += Alltrim(MemoLine(cMotivo,40,nXi)) + CHR(13)+CHR(10)
Next nXi

Return cTexto

Static Function GrvJust(cCombo)
local nPosCtrl := 0
Local lOk		:= .F.
Local oMemo
Local oDlg
Local lOk := .F.

IF Left(cCombo,1) == '1'
	cEdtInf:= SZR->ZR_OBSUSR1
ElseIF Left(cCombo,1) == '2'
	cEdtInf:= SZR->ZR_OBSUSR2
ElseIF Left(cCombo,1) == '3'
	cEdtInf:= SZR->ZR_OBSUSR3
ElseIF Left(cCombo,1) == '4'
	cEdtInf:= SZR->ZR_OBSUSR4
Endif

// define a tela
DEFINE MSDIALOG oDlg TITLE "Justificativa" FROM 0,0 TO 194,500 PIXEL

	@ 005, 005 GET oMemo VAR cEdtInf MEMO SIZE 242, 68 OF oDlg PIXEL
	@ 080, 200 Button "OK" Size 015, 015 PIXEL OF oDlg action (lOk:= .T., oDlg:ENd())
	@ 080, 220 Button "CANC" Size 020, 015 PIXEL OF oDlg action (lOk:= .F., oDlg:ENd())

ACTIVATE MSDIALOG oDlg CENTERED

If lOk .And. MsgYesNo( 'Deseja Gravar a Justificativa!!!?')

	Reclock('SZR',.F.)

	IF Left(cCombo,1) == '1'
		SZR->ZR_OBSUSR1:= cEdtInf
		SZR->ZR_OK1:='S'
	Endif

	IF Left(cCombo,1) == '2'
		SZR->ZR_OBSUSR2:= cEdtInf
		SZR->ZR_OK2:='S'
	Endif

	IF Left(cCombo,1) == '3'
		SZR->ZR_OBSUSR3:= cEdtInf
		SZR->ZR_OK3:='S'
	Endif

	IF Left(cCombo,1) == '4'
		SZR->ZR_OBSUSR4:= cEdtInf
		SZR->ZR_OK4:='S'
	Endif

	SZR->(MsUnlock())

Endif

Return

User Function FATE01LEG
Local aLegenda := {}

aLegenda := { { "BR_VERDE"				,"OK" },;
			  { "BR_AMARELO"			,"Justificado Parcial" },;
			  { "BR_AZUL"					,"Justificado Total" },;
              { "DISABLE"					,"Não Justificado" } }

BRWLEGENDA( cCadastro, "Legenda", aLegenda )

Return


//Função responsável pela cotação das transportadoas
User Function Fat01Cot

/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±± Declaração de cVariable dos componentes                                 ±±
Ù±±ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
Local cTransp1  := SZR->ZR_TRCOT1
Local cTransp2  := SZR->ZR_TRCOT2
Local cTransp3  := SZR->ZR_TRCOT3
Local cTransp4  := SZR->ZR_TRCOT4
Local cTransp5  := SZR->ZR_TRCOT5
Local nVal1		:= SZR->ZR_VALCOT1
Local nVal2		:= SZR->ZR_VALCOT2
Local nVal3		:= SZR->ZR_VALCOT3
Local nVal4		:= SZR->ZR_VALCOT4
Local nVal5		:= SZR->ZR_VALCOT5
Local lOk		:= .F.
Local cNomeT1:=cNomeT2:=cNomeT3:=cNomeT4:=cNomeT5:=''
Local aAreaSA4 := SA4->(GetArea())

SA4->(DbSetOrder(1))

If !Empty (SZR->ZR_TRCOT1)
	IF SA4->(MsSeek(xFilial('SA4')+SZR->ZR_TRCOT1))
		cNomeT1:= SA4->A4_NOME
	Endif
Endif

If !Empty (SZR->ZR_TRCOT2)
	IF SA4->(MsSeek(xFilial('SA4')+SZR->ZR_TRCOT2))
		cNomeT2:= SA4->A4_NOME
	Endif
Endif

If !Empty (SZR->ZR_TRCOT3)
	IF SA4->(MsSeek(xFilial('SA4')+SZR->ZR_TRCOT3))
		cNomeT3:= SA4->A4_NOME
	Endif
Endif

If !Empty (SZR->ZR_TRCOT4)
	IF SA4->(MsSeek(xFilial('SA4')+SZR->ZR_TRCOT4))
		cNomeT4:= SA4->A4_NOME
	Endif
Endif


If !Empty (SZR->ZR_TRCOT5)
	IF SA4->(MsSeek(xFilial('SA4')+SZR->ZR_TRCOT5))
		cNomeT5:= SA4->A4_NOME
	Endif
Endif

/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±± Declaração de Variaveis Private dos Objetos                             ±±
Ù±±ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
SetPrvt("oDlgCot","oGrpCot")

/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±± Definicao do Dialog e todos os seus componentes.                        ±±
Ù±±ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
oDlgCot    := MSDialog():New( 092,232,501,1117,"Cotações",,,.F.,,,,,,.T.,,,.T. )
oGrpCot    := TGroup():New( 004,004,188,428,"Cotacao",oDlgCot,,,.T.)

TSay():New( 028,012,{||"Transp 1"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,028,008)
TSay():New( 052,012,{||"Transp 2"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 076,012,{||"Transp 3"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 100,012,{||"Transp 4"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 124,012,{||"Tranps 5"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)

oTransp1:= TGet():New( 028,052,{|u| If(PCount()>0,cTransp1:=u,cTransp1)},oGrpCot,060,008,'',{|| cNomeT1:=Posicione('SA4',1,xFilial('SA4')+cTransp1,'A4_NOME'),oTName1:Refresh() },CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"SA4","cTransp1",,)
oTransp2:= TGet():New( 052,052,{|u| If(PCount()>0,cTransp2:=u,cTransp2)},oGrpCot,060,008,'',{|| cNomeT2:=Posicione('SA4',1,xFilial('SA4')+cTransp2,'A4_NOME'),oTName2:Refresh() },CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"SA4","cTransp3",,)
oTransp3:= TGet():New( 076,052,{|u| If(PCount()>0,cTransp3:=u,cTransp3)},oGrpCot,060,008,'',{|| cNomeT3:=Posicione('SA4',1,xFilial('SA4')+cTransp3,'A4_NOME'),oTName3:Refresh() },CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"SA4","cTransp3",,)
oTransp4:= TGet():New( 100,052,{|u| If(PCount()>0,cTransp4:=u,cTransp4)},oGrpCot,060,008,'',{|| cNomeT4:=Posicione('SA4',1,xFilial('SA4')+cTransp4,'A4_NOME'),oTName4:Refresh() },CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"SA4","cTransp4",,)
oTransp5:= TGet():New( 124,052,{|u| If(PCount()>0,cTransp5:=u,cTransp5)},oGrpCot,060,008,'',{|| cNomeT5:=Posicione('SA4',1,xFilial('SA4')+cTransp5,'A4_NOME'),oTName5:Refresh() },CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"SA4","cTransp5",,)

oTName1:= TSay():New( 028,124,{||cNomeT1},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,210,008)
oTName2:= TSay():New( 052,124,{||cNomeT2},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,210,008)
oTName3:= TSay():New( 076,124,{||cNomeT3},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,210,008)
oTName4:= TSay():New( 100,124,{||cNomeT4},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,210,008)
oTName5:= TSay():New( 124,124,{||cNomeT5},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,210,008)

oTName1:CtrlRefresh()
oTName2:CtrlRefresh()
oTName3:CtrlRefresh()
oTName4:CtrlRefresh()
oTName5:CtrlRefresh()

TSay():New( 028,335,{||"Valor"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 052,335,{||"Valor"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 076,335,{||"Valor"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 100,335,{||"Valor"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)
TSay():New( 124,335,{||"Valor"},oGrpCot,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,032,008)

TGet():New( 028,350,{|u| If(PCount()>0,nVal1:=u,nVal1)},oGrpCot,060,008,"@E 999,999.99",,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","nVal1",,)
TGet():New( 052,350,{|u| If(PCount()>0,nVal2:=u,nVal2)},oGrpCot,060,008,"@E 999,999.99",,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","nVal2",,)
TGet():New( 076,350,{|u| If(PCount()>0,nVal3:=u,nVal3)},oGrpCot,060,008,"@E 999,999.99",,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","nVal3",,)
TGet():New( 100,350,{|u| If(PCount()>0,nVal4:=u,nVal4)},oGrpCot,060,008,"@E 999,999.99",,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","nVal4",,)
TGet():New( 124,350,{|u| If(PCount()>0,nVal5:=u,nVal5)},oGrpCot,060,008,"@E 999,999.99",,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","nVal5",,)

SButton():New( 172,232,1,{||lOk:=.T.,oDlgCot:End()},oGrpCot,,"", )
SButton():New( 172,264,2,{||oDlgCot:End()},oGrpCot,,"", )

oDlgCot:Activate(,,,.T.)

If lOk
	IF Reclock('SZR',.F.)
			SZR->ZR_TRCOT1	:= cTransp1
			SZR->ZR_TRCOT2	:= cTransp2
			SZR->ZR_TRCOT3	:= cTransp3
			SZR->ZR_TRCOT4	:= cTransp4
			SZR->ZR_TRCOT5	:= cTransp5
			SZR->ZR_VALCOT1	:= nVal1
			SZR->ZR_VALCOT2	:= nVal2
			SZR->ZR_VALCOT3	:= nVal3
			SZR->ZR_VALCOT4	:= nVal4
			SZR->ZR_VALCOT5	:= nVal5
		SZR->(MsUnlock())
	Else
		Alert('Falha na Gravação, alguem deve estar com alterando o registro!!')
	Endif
Endif

RestArea(aAreaSA4)
Return


//Envia e-mail caso já tenha sido feito um romaneio antigamente (e excluido) com o número novo
Static Function FATE1RWFEXC(cRoman)
Local cQuery := ''
Local _cHtml   	:= ''
Local _cCabec		:= ''
Local _cItens		:= ''
Local _cRodape	:= ''
Local _cItem		:= ''
Local cSubject 	:= 'Romaneio Existente Porém excluído '+cRoman
Local cPara 		:= ''
Local nLitros		:=0
Local nEmbalagem:=0
Local nFardos		:=0
Local nPbruto		:=0

cQuery := "SELECT "+CRLF
cQuery += "	ZR_CODIGO,"+CRLF
cQuery += "	ZR_HORA,"+CRLF
cQuery += "	ZQ_CLIENTE,"+CRLF
cQuery += "	ZQ_LOJA,"+CRLF
cQuery += "	ZR_NOMECLI,"+CRLF
cQuery += "	ZR_MUN,"+CRLF
cQuery += "	ZQ_PEDIDO,"+CRLF
cQuery += "	ZQ_ITEMPV,"+CRLF
cQuery += "	ZQ_PRODUTO,"+CRLF
cQuery += "	B1_DESC,"+CRLF
cQuery += "	ZQ_LITROS,"+CRLF
cQuery += "	ZQ_EMBALAG,"+CRLF
cQuery += "	ZQ_FARDOS,"+CRLF
cQuery +="	ZQ_PESOBRT "+CRLF
cQuery += "FROM "+CRLF
cQuery += " SZR010 SZR INNER JOIN SZQ010 SZQ ON ZQ_ROMAN = ZR_CODIGO AND SZQ.D_E_L_E_T_='' AND ZQ_FILIAL = ZR_FILIAL"+CRLF
cQuery += " INNER JOIN SB1010 SB1 ON B1_COD = ZQ_PRODUTO AND SB1.D_E_L_E_T_='' AND B1_FILIAL = ZR_FILIAL"+CRLF
cQuery += "WHERE   " +CRLF
cQuery += " SZR.D_E_L_E_T_='' "+CRLF
cQuery += " AND ZR_FILIAL='01' AND ZR_CODIGO='"+cRoman+"'"+CRLF
cQuery += "ORDER BY"+CRLF
cQuery += " ZQ_ROMAN,ZQ_PEDIDO,ZQ_ITEMPV"

	If Select('TMP') > 0
		TMP->(DBCloseArea())
	EndIf

	cQuery 	:= ChangeQuery(cQuery)

	TcQuery cQuery New Alias 'TMP'

	DbSelectArea('TMP')

	_cCabec += '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
	_cCabec += '<html xmlns="http://www.w3.org/1999/xhtml">
	_cCabec += '<head>
	_cCabec += '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
	_cCabec += '<title>Romaneio</title>
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
	_cCabec += '			    Romaneio com mesmo Número'
	_cCabec += '            </td>
	_cCabec += '        </td>
	_cCabec += '    </table>
	_cCabec += '</div>
	_cCabec += '<div class="cabec" id="cabec" style="border: 1px solid #1A3984; background-color: #FFFFFF; color: #737270; height: auto; width: auto; padding: 10px; margin: 2px; font-size: 14px; box-shadow: 1px 2px 4px  rgba(117, 117, 117, 0.3); -moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3); -webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);">
	_cCabec += '  <table width="100%" border="0">
	_cCabec += '    <tr>
	_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Romaneio: </td>
	_cCabec += '      <td width="44%" >@@romaneio@@</td> '
	_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Emissão: </td>
	_cCabec += '      <td width="44%">'+Dtoc(DDatabase)+ '</td>'
	_cCabec += '      <td width="6%" class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Hora: </td>
	_cCabec += '      <td width="44%">'+TMP->ZR_HORA+ '</td>'
	_cCabec += '    </tr>
	_cCabec += '    <tr>
	_cCabec += '      <td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>
	_cCabec += '    </tr>
	_cCabec += '    <tr>
	_cCabec += '      <td class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;">Cliente </td>
	_cCabec += '      <td>@@cliente@@</td>
	_cCabec += '      <td class="principal" id="tablecabec" style="color:#1A3984;font-weight: bold;"> </td>
	_cCabec += '      <td></td>
	_cCabec += '    </tr>
	_cCabec += '    <tr>
	_cCabec += '      <td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td>
	_cCabec += '    </tr>
	_cCabec += '  </table>
	_cCabec += '</div>
	_cCabec2 := '<p>&nbsp;	</p>
	_cCabec2 += '<div class="itens" id="itens" style="	background-color: #F4F4F4;border: 1px solid #1A3984;margin-top: -45px;margin-right: 2px;margin-bottom: 0px;margin-left: 2px;padding: 4px;box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-moz-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);-webkit-box-shadow: 1px 2px 4px rgba(117, 117, 117, 0.3);">
	_cCabec2 += '<table width="100%" border="1"  cellspacing="0" class="tabitens" style="color: #4F4F4F;text-align: center;border-color: #999;font-size: 12px;">
	_cCabec2 += '  <tr id="tablecabec">
	_cCabec2 += '    <td width="10%" align="center"  style="background-color:#698BB6; color: white;">Produto</td>
	_cCabec2 += '    <td width="25%" align="center"  style="background-color:#698BB6; color: white;">Descrição</td>
	_cCabec2 += '    <td width="5%" align="center"  style="background-color:#698BB6; color: white;">Litros</td>
	_cCabec2 += '    <td width="5%" align="center"  style="background-color:#698BB6; color: white;">Embalagem</td>
	_cCabec2 += '    <td width="5%" align="center"  style="background-color:#698BB6; color: white;">Fardos</td>
	_cCabec2 += '    <td width="5%" align="center"  style="background-color:#698BB6; color: white;">Peso Bruto</td>
	_cCabec2 += '  </tr>
	_cItem += '  <tr id="tableitem" style="background-color:white;">
	_cItem += '    <td >@@produto@@</td>
	_cItem += '    <td >@@desc@@</td>
	_cItem += '    <td >@@litros@@</td>
	_cItem += '    <td >@@embalagem@@</td>
	_cItem += '    <td >@@fardos@@</td>
	_cItem += '    <td >@@pesobruto@@</td>
	_cItem += '  </tr>
	_cRodape +='						<tfoot>
	_cRodape +='							<th>Total</th>
	_cRodape +='							<td>0</td>
	_cRodape +='							<td>@@TLitros@@</td>
	_cRodape +='							<td>@@TEmb@@</td>
	_cRodape +='							<td>@@TFardos@@</td>
	_cRodape +='							<td>@@TPesoB@@</td>
	_cRodape +='						</tfoot>'
	_cRodape += '</table>
	_cRodape += '</div>
	_cRodape += '</body>
	_cRodape += '</html>

	cPara := SuperGetMv("MV_FATE01W",,'emerson@smsti.com.br')

 	While !TMP->(EOF())
		cRomanAnt:=TMP->ZR_CODIGO
		cCli	:= Alltrim(TMP->ZQ_CLIENTE)+' - Loja:'+Alltrim(TMP->ZR_NOMECLI)

		_cHtml := _cCabec
		_cItens:= ''
		_cHtml:= StrTran(_cHtml,'@@cliente@@',Alltrim(TMP->ZQ_CLIENTE)+' / '+TMP->ZQ_LOJA+' - '+Alltrim(TMP->ZR_NOMECLI))
		_cHtml:= StrTran(_cHtml,'@@romaneio@@',cRoman)

		While !TMP->(EOF()) .And. TMP->ZR_CODIGO == cRomanAnt

				_cItens+= _cItem
 	  		_cItens := StrTran(_cItens,"@@produto@@",Alltrim(TMP->ZQ_PRODUTO))
 	  		_cItens := StrTran(_cItens,"@@desc@@",Alltrim(TMP->B1_DESC))
 	  		_cItens := StrTran(_cItens,"@@litros@@",Transform(TMP->ZQ_LITROS, "@E 999,999,999.99" ))
 	  		_cItens := StrTran(_cItens,"@@embalagem@@",Transform(TMP->ZQ_EMBALAG, "@E 999,999,999.99" ))
 	  		_cItens := StrTran(_cItens,"@@fardos@@",Transform(TMP->ZQ_FARDOS, "@E 999,999,999.99" ))
 	  		_cItens := StrTran(_cItens,"@@pesobruto@@",Transform(TMP->ZQ_PESOBRT, "@E 999,999,999.99" ))

				nLitros		+= TMP->ZQ_LITROS
				nEmbalagem+= TMP->ZQ_EMBALAG
				nFardos		+= TMP->ZQ_FARDOS
				nPbruto		+= TMP->ZQ_PESOBRT


		TMP->(DbSkip())

		Enddo

		_cRodape:= StrTran(_cRodape,'@@TLitros@@'	,Transform(nLitros, "@E 999,999,999.99" ))
		_cRodape:= StrTran(_cRodape,'@@TEmb@@'		,Transform(nEmbalagem, "@E 999,999,999.99" ))
		_cRodape:= StrTran(_cRodape,'@@TFardos@@'	,Transform(nFardos, "@E 999,999,999.99" ))
		_cRodape:= StrTran(_cRodape,'@@TPesoB@@'	,Transform(nPbruto, "@E 999,999,999.99" ))

		_cHtml+= _cCabec2+_cItens
		_cHtml+= _cRodape

		U_EnvMail(cSubject,_cHtml,cPara,'','','',.T.)

 	EndDo

TMP->(DBCloseArea())

Return


// -- Mostra Mensagem para ser enviada posteriormente com o e-mail para os usuários do grupo em questão --

Static Function MostraMsg(nOpcMsg,lMostra)
local nPosCtrl := 0
Local lOk		:= .F.
Local oMemo
Local oDlg
Local lOk := .F.
Local cEdtInf
Default lMostra := .F.

If nOpcMsg == 1
	cEdtInf := SZR->ZR_MSGEMA1
ElseIf nOpcMsg == 2
	cEdtInf := SZR->ZR_MSGEMA2
ElseIf nOpcMsg == 3
	cEdtInf := SZR->ZR_MSGEMA3
Else
	cEdtInf := SZR->ZR_MSGEMA4
Endif

// define a tela
If lMostra
	DEFINE MSDIALOG oDlg TITLE "Informações Adicionais E-mail" FROM 0,0 TO 150,315 PIXEL

		@ 005, 005 GET oMemo VAR cEdtInf MEMO SIZE 145, 060 OF oDlg PIXEL
		@ 065, 140 Button "OK" Size 015, 015 PIXEL OF oDlg action (lOk:= .T., oDlg:ENd())

	ACTIVATE MSDIALOG oDlg CENTERED

	If lOk

		Reclock('SZR',.F.)

		If nOpcMsg == 1
			SZR->ZR_MSGEMA1:=	cEdtInf
		ElseIf nOpcMsg ==2
			SZR->ZR_MSGEMA2:=	cEdtInf
		Elseif nOpcMsg ==3
			SZR->ZR_MSGEMA3:=	cEdtInf
		Else
			SZR->ZR_MSGEMA4:=	cEdtInf
		Endif

		SZR->(MsUnlock())

	Endif
Endif

Return .T.

User Function E01Fret
Local oDlg
Local aCores 	:= {}
Local cPerg		:= "FATE01FRET"
Local aArea		:= GetArea()
Local lFoi		:=	.F.
Local lCheCk1	:= .F.
Local cGetDateIni := FirstDay(MonthSub(dDatabase,1))
Local cGetDateEnd := LastDay(dDatabase)
Private cGetCTR   := Space(09)
Private _stru	  :={}
Private aCpoBro   := {}
Private cQuery 	  := ''
Private cAliasTMP := GetNextAlias()
Private lInverte  := .F.
Private cMark     := GetMark()
Private oMark
Private cArq
Private cNota     := ""

MakeSx1(cPerg)

Pergunte(cPerg,.T.)

CreateData(.T.,,cGetDateIni,cGetDateEnd)

//Define as cores dos itens de legenda.
aCores := {}
//aAdd(aCores,{"TTRB->APROVADO == 'S'","BR_VERDE"	})
//aAdd(aCores,{"TTRB->APROVADO == ' '","BR_AMARELO"})
//aAdd(aCores,{"TTRB->APROVADO == 'N'","BR_VERMELHO"})

//DEFINE MSDIALOG oDlg TITLE "Aprovação Fretes" From 9,0 To 315,800 PIXEL
DEFINE MSDIALOG oDlg TITLE "Aprovação Fretes" From 9,0 To 365,800 PIXEL

	//Cria a MsSelect
	//oCheck1 := TCheckBox():New(01,01,'Lista Todos?',{|| lCheck1 := !lCheck1},oDlg,100,210,,,,,,,,.T.,,,)
	/*oSayCTR := TSay():New(016,008,{||"CTR:"},oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,016,008)
	oGetCTR := TGet():New(016,040,{|u| If(PCount()>0,cGetCTR:=u,cGetCTR)},oDlg,060,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetCTR",,)

	oDataIni   := TSay():New( 004,004,{||"Data de ?"},oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,028,008)
	oDataFim   := TSay():New( 004,116,{||"Data Ate"},oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,024,008)
	oGetDateIn := TGet():New( 004,040,{|u| If(PCount()>0,cGetDateIni:=u,cGetDateIni)},oDlg,060,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetDateIni",,)
	oGetDateEn := TGet():New( 004,148,{|u| If(PCount()>0,cGetDateEnd:=u,cGetDateEnd)},oDlg,060,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetDateEnd",,)
	*/
	//oSayCTR := TSay():New(046,008,{||"CTR:"},oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,046,008)
	//oGetCTR := TGet():New(046,040,{|u| If(PCount()>0,cGetCTR:=u,cGetCTR)},oDlg,090,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetCTR",,)

	oDataIni   := TSay():New( 034,004,{||"Data de ?"},oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,058,008)
	oDataFim   := TSay():New( 034,140,{||"Data Ate"} ,oDlg,,,.F.,.F.,.F.,.T.,CLR_BLACK,CLR_WHITE,054,008)
	oGetDateIn := TGet():New( 034,040,{|u| If(PCount()>0,cGetDateIni:=u,cGetDateIni)},oDlg,090,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetDateIni",,)
	oGetDateEn := TGet():New( 034,172,{|u| If(PCount()>0,cGetDateEnd:=u,cGetDateEnd)},oDlg,090,008,'',,CLR_BLACK,CLR_WHITE,,,,.T.,"",,,.F.,.F.,,.F.,.F.,"","cGetDateEnd",,)

	oMark := MsSelect():New("TTRB","OK","",aCpoBro,@lInverte,@cMark,{64,1,180,400},,,,,aCores)
	oMark:oBrowse:bAllMark := {| | DispAll()}
	oMark:oBrowse:bHeaderClick 	   := {|x, y| CreateData(,,cGetDateIni,cGetDateEnd,y) }
	oMark:bMark := {| | Disp()}

	//oCheck1:bChange := {|| CreateData(lCheCk1) }
	//oGetCTR:bChange		:= {|| CreateData(.T.,cGetCTR,cGetDateIni,cGetDateEnd) }
	oGetDateEn:bChange	:= {|| CreateData(.T.,,cGetDateIni,cGetDateEnd) }
	//oCheck1:CtrlRefresh()

ACTIVATE MSDIALOG oDlg CENTERED ON INIT EnchoiceBar(oDlg,{|| oDlg:End(),lFoi:=.T.},{|| oDlg:End(),lFoi:=.F.})

If lFoi

	TTRB->(DbGoTop())

	IF MV_PAR01 == 2

		//SZ6->(DbSetOrder(2))
		SZ6->(DbSetOrder(1))

		While !TTRB->(Eof())

			If !Empty(TTRB->OK)
			/*
				If SZ6->(DbSeek(xFilial('SZ6')+TTRB->CONHEC+TTRB->SERIE))

					Reclock('SZ6',.F.)
						SZ6->Z6_APROV	:= 'S'
						SZ6->Z6_DTAPROV	:= dDatabase
					SZ6->(MsUnlock())

				Endif

				Endif

		   		TTRB->(DbSkip())

			End */

			If MsgYesNo("Deseja informar a nota da carga: "+TTRB->CARGA+"?")
	  			cNota := Space(3)

	  			_oDlgLib := TDialog():New(0,0,200,250,"Avaliação",,,,,,,,,.T.,,,,,)
				_oDlgLib:lCentered := .T.
				_oDlgLib:lEscClose := .F.  //nao fecha tela com esc
				_oDlgLib:nStyle:=128      	//sem x no canto superior direito

				TSay():New(20,10,{|| "Nota: " },_oDlgLib,,,,,,.T.,CLR_BLACK,CLR_WHITE)
				oUsuAux		:= TGet():New(20,60,bSetGet(cNota),_oDlgLib,60,10,,,,,,,,.T.,,,,,,,,,)
				SButton():New(60,20,01,{|| _oDlgLib:End() }, _oDlgLib,,"Confirma")
				SButton():New(60,80,02,{|| _oDlgLib:End() }, _oDlgLib,,"Cancela")
				_oDlgLib:Activate(,,,,,,)

			EndIf

			  // Atualiza a aprovação no romaneios da carga
			  cQrA := " SELECT CARGA, ROM, NSAIDA, SERS
			  cQrA += " FROM (
			  cQrA += " SELECT C9_ROMAN ROM, ZR_CARGA CARGA, Z6_DOC NSAIDA,  Z6_SERIE SERS
			  cQrA += " FROM SZ6010 SZ6
			  cQrA += " INNER JOIN SF1010 SF1 ON SF1.D_E_L_E_T_ = '' AND F1_DOC = Z6_CONHEC AND Z6_SERCON = F1_SERIE AND Z6_FORNECE = F1_FORNECE
			  cQrA += " INNER JOIN SC9010 SC9 ON SC9.D_E_L_E_T_ = '' AND C9_NFISCAL = Z6_DOC AND Z6_SERIE = C9_SERIENF
			  cQrA += " INNER JOIN SZR010 SZR ON SZR.D_E_L_E_T_ = '' AND ZR_CODIGO = C9_ROMAN
			  cQrA += " WHERE SZ6.D_E_L_E_T_ = '' AND ZR_CARGA <> '' AND ZR_CARGA =  '" +TTRB->CARGA+ "'
			  cQrA += " GROUP BY C9_ROMAN, ZR_CARGA, Z6_DOC, Z6_SERIE ) A

			  MemoWrite('C:\TEMP\FAR01.SQL',cQrA)

		      If Select ("QRYA") > 0
				QRYA->(DbCloseArea())
			  Endif

  			  TcQuery cQrA new alias "QRYA"

  			  While !QRYA->(Eof())

				If SZ6->(DbSeek(xFilial('SZ6')+QRYA->NSAIDA+QRYA->SERS))

					Reclock('SZ6',.F.)
						SZ6->Z6_APROV	:= 'S'
						SZ6->Z6_DTAPROV	:= dDatabase
					SZ6->(MsUnlock())

  			    EndIf

  			  	QRYA->(DbSkip())

  			  EndDo

	  			If !Empty(cNota)
					Begin transaction
					  // atualiza nota no romaneio
					  cQr := " Update " + retSqlName("SZR")
					  cQr += " Set ZR_NOTA = '" + cNota + "'
					  cQr += " Where ZR_CARGA = '" + TTRB->CARGA + "'

					  TcSqlExec(cQr)

					End transaction
				EndIf

	  		  	cNota := ""

			EndIf

			TTRB->(DbSkip())

		EndDo
	Else

		SF8->(DbSetOrder(3))
		While !TTRB->(Eof())
			If !Empty(TTRB->OK)
				If SF8->(DbSeek(xFilial('SF8')+TTRB->CTR+TTRB->SERIE_CTR))
					Reclock('SF8',.F.)
						SF8->F8_APROV	:= 'S'
						SF8->F8_DTAPROV	:= dDatabase
					SF8->(MsUnlock())
				Endif
			Endif
			TTRB->(DbSkip())
		End

	Endif

Endif

//Fecha a Area e elimina os arquivos de apoio criados em disco.
TTRB->(DbCloseArea())
Iif(File(cArq + GetDBExtension()),FErase(cArq  + GetDBExtension()) ,Nil)
RestArea(aArea)
¦Return

//Funcao executada ao Marcar/Desmarcar um registro.

Static Function Disp()

RecLock("TTRB",.F.)
If Marked("OK")
	TTRB->OK := cMark
Else
	TTRB->OK := ""
Endif

TTRB->(MSUNLOCK())

oMark:oBrowse:Refresh()

Return()

Static Function DispAll
Local aArea:= GetArea()
Local lRet := .T.

TTRB->(dbGotop())

While !TTRB->(Eof())

	RecLock('TTRB',.F.)

		If Marked("OK")
			TTRB->OK := ''
		Else
			TTRB->OK := cMark
		Endif

	TTRB->(MSUNLOCK())

	TTRB->(DbSkip())

End

oMark:oBrowse:Refresh()
RestArea(aArea)

Return lRet

Static Function MakeSx1(cPerg)

PutSx1(cPerg,"08","Tipo","","","mv_ch1","N",01,0,0,"C","","","","","mv_par01","Entrada","","","","Saida","","","","","","","","","","","",{"Tipo do Filtro ","Fretes Entrada ou Saidas",""},{""},{""})

Return


Static Function CreateData(lAprov,cCtr,dInicio,dFim,nColClk)
DeFault lAprov := .T.
Default cCtr   := ''
Default nColClk := 0
_stru:= {}

if Select ('TTRB') > 0
	TTRB->(DbCloseArea())
Endif

If MV_PAR01 == 2


   /*	cQuery:="SELECT"+CRLF
	cQuery+="	TMP3.*,"+CRLF
	cQuery+="	ZR_VLFRETE,"+CRLF
	cQuery+="	ZR_CARGA"+CRLF
	cQuery+="FROM("+CRLF
	cQuery+="	SELECT"+CRLF
	cQuery+="		*,"+CRLF
	cQuery+="		(SELECT DISTINCT(C9_ROMAN) FROM "+RetSqlName('SC9')+" WHERE D_E_L_E_T_='' AND C9_ROMAN <> '' AND C9_PEDIDO = TMP2.PEDIDO AND C9_NFISCAL = TMP2.Z6_DOC) AS 'ROMANEIO'"+CRLF
	cQuery+="	FROM("+CRLF
	cQuery+="		SELECT"+CRLF
	cQuery+="			TMP.*"+CRLF
	cQuery+="		FROM"+CRLF
	cQuery+="			(SELECT"+CRLF
	cQuery+="				SZ6.*,"+CRLF
	cQuery+="				(SELECT DISTINCT TOP 1 D2_PEDIDO FROM "+RetSqlName('SD2')+" SD2 WHERE D_E_L_E_T_='' AND D2_DOC = Z6_DOC AND D2_SERIE = Z6_SERIE AND D2_FILIAL = Z6_FILIAL) AS 'PEDIDO',"+CRLF
	cQuery+="				A2_NOME,"+CRLF
	cQuery+="				F1_EMISSAO,"+CRLF
	cQuery+="				F1_DTDIGIT"+CRLF
	cQuery+="			FROM"+CRLF
	cQuery+="				"+RetSqlName('SZ6')+" SZ6 LEFT OUTER JOIN "+RetSqlName('SA2')+" SA2 ON A2_COD = Z6_FORNECE AND Z6_LOJA = A2_LOJA AND SA2.D_E_L_E_T_=''"+CRLF
	cQuery+="				LEFT OUTER JOIN "+RetSqlName('SF1')+" SF1 ON F1_DOC = Z6_CONHEC AND F1_FORNECE = Z6_FORNECE AND F1_LOJA = Z6_LOJA AND SF1.D_E_L_E_T_='' AND F1_FILIAL = Z6_FILIAL"+CRLF
	cQuery+="			WHERE"+CRLF
	cQuery+="				SZ6.D_E_L_E_T_='' "+CRLF
	If !Empty(cCtr)
		cQuery+="			AND Z6_CONHEC='"+cCtr+"'"+CRLF
	Endif
	cQuery+="				)"
	cQuery+="		TMP)"+CRLF
	cQuery+="	TMP2)"+CRLF
	cQuery+="TMP3"+CRLF
	cQuery+="LEFT OUTER JOIN "+RetSqlName('SZR')+" SZR ON ZR_CODIGO = TMP3.ROMANEIO AND SZR.D_E_L_E_T_='' AND ZR_FILIAL = TMP3.Z6_FILIAL"+CRLF
	If !Empty(dFim)
		cQuery+="	WHERE TMP3.F1_EMISSAO BETWEEN '"+Dtos(dInicio)+"' AND '"+Dtos(dFim)+"'"+CRLF
	Endif

	if ( nColClk == 3 )
		cQuery += "ORDER BY TMP3.Z6_DOC"
	elseif ( nColClk == 7 )
  	cQuery += "ORDER BY TMP3.Z6_CONHEC"
	elseif ( nColClk == 12 )
  	cQuery += "ORDER BY TMP3.A2_NOME ASC"
	else
  	cQuery += "ORDER BY TMP3.Z6_DOC"
	endIf        */

	/*cQuery := " SELECT CARGA, TRANSPORTADORA, SAIDA, SUM(VALDOC) CTR, SUM(VALFRT) ROMANEIOS, MAX(APROV) APROVADO"
	cQuery += " FROM (  "
	cQuery += " SELECT Z6_DOC NFS, Z6_CONHEC NFE, F1_VALBRUT VALDOC, C9_ROMAN ROM, ZR_CARGA CARGA, ZR_VLFRETE VALFRT, ZR_SAIDA SAIDA, RTRIM(ZR_TRANSP+' - '+ RTRIM(ZR_NOMTRAN)) TRANSPORTADORA, Z6_APROV APROV "
	cQuery += " FROM SZ6010 SZ6  "
	cQuery += " INNER JOIN SF1010 SF1 ON SF1.D_E_L_E_T_ = '' AND F1_DOC = Z6_CONHEC AND Z6_SERCON = F1_SERIE AND Z6_FORNECE = F1_FORNECE  "
	cQuery += "	AND F1_EMISSAO BETWEEN '"+Dtos(dInicio)+"' AND '"+Dtos(dFim)+"'"+CRLF
	cQuery += " INNER JOIN SC9010 SC9 ON SC9.D_E_L_E_T_ = '' AND C9_NFISCAL = Z6_DOC AND Z6_SERIE = C9_SERIENF  "
	cQuery += " INNER JOIN SZR010 SZR ON SZR.D_E_L_E_T_ = '' AND ZR_CODIGO = C9_ROMAN "
	cQuery += " WHERE SZ6.D_E_L_E_T_ = '' AND ZR_CARGA <> ''   "
	cQuery += " GROUP BY Z6_DOC, Z6_CONHEC, F1_VALBRUT, C9_ROMAN, ZR_CARGA, ZR_VLFRETE, ZR_TRANSP, ZR_NOMTRAN, ZR_SAIDA, Z6_APROV) A   "
	cQuery += " GROUP BY CARGA, TRANSPORTADORA, SAIDA "
	cQuery += " ORDER BY CARGA " */

	cQuery := " SELECT CARGA, TRANSPORTADORA, SAIDA, CTR, ROMANEIOS, APROVADO  "
	cQuery += " FROM (    "
	cQuery += " SELECT CARGA, TRANSPORTADORA, SAIDA, SUM(CTR) CTR, SUM(VALFRT) ROMANEIOS, MAX(APROV) APROVADO "
	cQuery += " FROM (  "
	//cQuery += " SELECT ZR_CARGA CARGA, RTRIM(ZR_TRANSP+' - '+ RTRIM(ZR_NOMTRAN)) TRANSPORTADORA, ZR_SAIDA SAIDA, VALDOC CTR, ZR_VLFRETE VALFRT, MAX(APROV) APROV
	cQuery += " SELECT ZR_CARGA CARGA, RTRIM(ZR_TRANSP+' - '+ RTRIM(ZR_NOMTRAN)) TRANSPORTADORA, ZR_SAIDA SAIDA, VALDOC CTR, SUM(ZR_VLFRETE) VALFRT, MAX(APROV) APROV "
	cQuery += " FROM (  "
	cQuery += " SELECT "
	cQuery += " C9_ROMAN ROM, F1_VALBRUT VALDOC, MAX(Z6_APROV) APROV "
	cQuery += " FROM "
	cQuery += " SZ6010 SZ6  "
	cQuery += " INNER JOIN SF1010 SF1 ON SF1.D_E_L_E_T_ = '' AND F1_DOC = Z6_CONHEC AND Z6_SERCON = F1_SERIE "
	cQuery += " AND Z6_FORNECE = F1_FORNECE  AND F1_EMISSAO BETWEEN '"+Dtos(dInicio)+"' AND '"+Dtos(dFim)+"'"
	cQuery += " INNER JOIN SC9010 SC9 ON SC9.D_E_L_E_T_ = '' AND C9_NFISCAL = Z6_DOC AND Z6_SERIE = C9_SERIENF  "
	cQuery += " GROUP BY C9_ROMAN, F1_VALBRUT) A INNER JOIN SZR010 SZR ON SZR.D_E_L_E_T_ = '' AND ZR_CODIGO = ROM AND ZR_CARGA <> '' "
	//cQuery += " GROUP BY  ZR_CARGA, ZR_TRANSP, ZR_NOMTRAN,ZR_SAIDA, ZR_VLFRETE, VALDOC) B "
	cQuery += " GROUP BY  ZR_CARGA, ZR_TRANSP, ZR_NOMTRAN,ZR_SAIDA, VALDOC) B  "
	cQuery += " GROUP BY CARGA, TRANSPORTADORA, SAIDA "
	cQuery += " ) C WHERE APROVADO = '' "
	cQuery += " ORDER BY  CARGA   "
	//Cria um arquivo de Apoio

   /*	AADD(_stru,{"OK"				,"C",2,0})
	AADD(_stru,{"NOTA"				,"C",9,0})
	AADD(_stru,{"PEDIDO"			,"C",6,0})
	AADD(_stru,{"QTD"	 			,"N",8,2})
	AADD(_stru,{"ROMANEIO"			,"C",9,0})
	AADD(_stru,{"CARGA" 			,"C",6,0})
	AADD(_stru,{"FRETE"	 			,"N",8,2})
	AADD(_stru,{"CONHEC"			,"C",9,0})
	AADD(_stru,{"EMISSAO"			,"D",8,0})
	AADD(_stru,{"SERIE"				,"C",3,0})
	AADD(_stru,{"FORNECEDOR"		,"C",6,0})
	AADD(_stru,{"LOJA"				,"C",4,0})
	AADD(_stru,{"NOME"				,"C",20,0})
	AADD(_stru,{"APROVADO"			,"C",1,0})
	AADD(_stru,{"DATAAPROV"			,"D",8,0})  */

	AADD(_stru,{"OK"				,"C",2,0})
	AADD(_stru,{"CARGA" 			,"C",6,0})
	AADD(_stru,{"TRANSP"            ,"C",45,0})
	AADD(_stru,{"SAIDA"				,"D",8,0})
	AADD(_stru,{"FRETE"	 			,"N",8,2})
	AADD(_stru,{"ROMANEIO"	 		,"N",8,2})
	AADD(_stru,{"APROVADO"			,"C",1,0})

	//Define quais colunas (campos da TTRB) serao exibidas na MsSelect
	/*aCpoBro	:= {{"OK"				,"","Mark","@!"},;
				{"NOTA"				,"","Nota",""},;
				{"PEDIDO"			,"","Pedido",""},;
				{"ROMANEIO"			,"","Romaneio"},;
				{"CARGA"			,"","Carga"},;
				{"CONHEC"			,"","N.Conhecimento"},;
				{"EMISSAO"			,"","Emissão"},;
				{"FRETE"			,"","Valor","@E 999,999.99"},;
				{"FORNECEDOR"		,"","Fornecedor"},;
				{"LOJA"				,"","Loja"},;
				{"NOME"				,"","Nome"},;
				{"APROVADO"			,"","Aprovado"},;
				{"DATAAPROV"		,"","Data Aprov."}}
		                                                    */
	aCpoBro	:= {{"OK"				,"","   ","@!"},;
				{"CARGA"			,"","Carga"},;
				{"TRANSP"			,"","Transportadora"},;
				{"SAIDA"			,"","Saida"},;
				{"FRETE"			,"","Frete","@E 999,999.99"},;
				{"ROMANEIO"			,"","Romaneio","@E 999,999.99"},;
				{"APROVADO"			,"","Aprovado"}}


Else

	cQuery:="SELECT"
	cQuery+="	F8_FILIAL,"+CRLF
	cQuery+="	F8_NFORIG,"+CRLF
	cQuery+="	F8_SERORIG,"+CRLF
	cQuery+="	F8_NFDIFRE,"+CRLF
	cQuery+="	F8_SEDIFRE,"+CRLF
	cQuery+="	F1_VALBRUT,"+CRLF
	cQuery+="	F8_TRANSP,"+CRLF
	cQuery+="	F8_LOJTRAN,"+CRLF
	cQuery+="	A2_NOME,"+CRLF
	cQuery+="	F8_APROV,"+CRLF
	cQuery+="	F8_DTAPROV"+CRLF
	cQuery+="FROM"+CRLF
	cQuery+="	"+RetSqlName('SF8')+" SF8"+CRLF
	cQuery+="	INNER JOIN "+RetSqlName('SA2')+" SA2 ON A2_COD = F8_TRANSP AND A2_LOJA = F8_LOJTRAN AND SA2.D_E_L_E_T_=''"+CRLF
	cQuery+="	INNER JOIN "+RetSqlName('SF1')+" SF1 ON F1_DOC = F8_NFDIFRE AND F1_SERIE = F8_SEDIFRE AND F1_FORNECE = F8_TRANSP AND F1_LOJA = F8_LOJTRAN AND SF1.D_E_L_E_T_='' AND F8_FILIAL = F1_FILIAL"+CRLF
	cQuery+="WHERE"+CRLF
	cQuery+="	SF8.D_E_L_E_T_=''"+CRLF
	cQuery+="	AND F8_FILIAL='01'"+CRLF

	If !Empty(cCtr)
		cQuery+="	AND F8_NFDIFRE='"+cCtr+"'"+CRLF
	Endif
	If !Empty(dFim)
		cQuery+="	AND F8_DTDIGIT BETWEEN '"+Dtos(dInicio)+"' AND '"+Dtos(dFim)+"'"+CRLF
	Endif

	if ( nColClk == 3 )
		cQuery += "ORDER BY F8_NFORIG"
	elseif ( nColClk == 6 )
	  	cQuery += "ORDER BY F8_NFDIFRE ASC"
	elseif ( nColClk == 10 )
	  	cQuery += "ORDER BY A2_NOME ASC"
	else
	  	cQuery += "ORDER BY F8_NFORIG"
	endIf

	AADD(_stru,{"OK"				,"C",2,0})
	AADD(_stru,{"NOTA"				,"C",9,0})
	AADD(_stru,{"PEDIDO"			,"C",6,0})
	AADD(_stru,{"CARGA"				,"C",6,0})
	AADD(_stru,{"SERIE"	 			,"C",3,0})
	AADD(_stru,{"CTR"				,"C",9,0})
	AADD(_stru,{"SERIE_CTR" 		,"C",3,0})
	AADD(_stru,{"VALOR"	 			,"N",8,2})
	AADD(_stru,{"TRANSP"	 		,"C",6,0})
	AADD(_stru,{"LOJA"				,"C",4,0})
	AADD(_stru,{"NOME"				,"C",20,0})
	AADD(_stru,{"APROVADO"			,"C",1,0})
	AADD(_stru,{"DATAAPROV"			,"D",8,0})

	//Define quais colunas (campos da TTRB) serao exibidas na MsSelect
	aCpoBro	:= {{"OK"				,"","Mark","@!"},;
				{"NOTA"				,"","Nota",""},;
				{"PEDIDO"			,"","Pedido",""},;
				{"CARGA"			,"","Carga",""},;
				{"CTR"				,"","Ctr"},;
				{"VALOR"			,"","Valor","@E 999,999.99"},;
				{"TRANSP"			,"","Transp"},;
				{"LOJA"				,"","Loja"},;
				{"NOME"				,"","Nome"},;
				{"APROVADO"			,"","Aprovado"},;
				{"DATAAPROV"		,"","Data Aprovação."}}

Endif

//cArq:=Criatrab(_stru,.T.)
//DBUSEAREA(.t.,,carq,"TTRB")
oTRB := FwTemporaryTable():New('TTRB')
oTRB:SetFields(_stru)
oTRB:Create()

cQuery 	:= ChangeQuery(cQuery)

if Select (cAliasTmp) > 0
	(cAliasTmp)->(DbCloseArea())
Endif

DbUseArea( .T., "TOPCONN", TcGenQry( ,, cQuery ), cAliasTMP, .F., .T. )

Dbselectarea(cAliasTmp)
(cAliasTmp)->(DbGoTop())
If !(cAliasTmp)->(Eof())

	While !(cAliasTmp)->(Eof())

		Reclock('TTRB',.T.)

		If MV_PAR01 == 2

		 /*	TTRB->NOTA			:= (cAliasTmp)->Z6_DOC
			TTRB->PEDIDO		:= (cAliasTmp)->PEDIDO
			TTRB->CARGA			:= (cAliasTmp)->ZR_CARGA
			TTRB->ROMANEIO		:= (cAliasTmp)->ROMANEIO
			TTRB->FRETE			:= (cAliasTmp)->ZR_VLFRETE
			TTRB->CONHEC		:= (cAliasTmp)->Z6_CONHEC
			TTRB->EMISSAO		:= Stod((cAliasTmp)->F1_EMISSAO)
			TTRB->SERIE			:= (cAliasTmp)->Z6_SERCON
			TTRB->FORNECEDOR	:= (cAliasTmp)->Z6_FORNECE
			TTRB->LOJA		 	:= (cAliasTmp)->Z6_LOJA
			TTRB->NOME			:= (cAliasTmp)->A2_NOME
			TTRB->APROVADO		:= (cAliasTmp)->Z6_APROV
			TTRB->DATAAPROV		:= Stod((cAliasTmp)->Z6_DTAPROV)   */

			TTRB->CARGA			:= (cAliasTmp)->CARGA
			TTRB->TRANSP		:= (cAliasTmp)->TRANSPORTADORA
			TTRB->SAIDA			:= Stod((cAliasTmp)->SAIDA)
			TTRB->FRETE			:= (cAliasTmp)->CTR
			TTRB->ROMANEIO		:= (cAliasTmp)->ROMANEIOS
			TTRB->APROVADO		:= (cAliasTmp)->APROVADO

		Else

			TTRB->NOTA			:=	(cAliasTmp)->F8_NFORIG
			TTRB->SERIE			:=	(cAliasTmp)->F8_SERORIG
			TTRB->CTR			:=  (cAliasTmp)->F8_NFDIFRE
			TTRB->SERIE_CTR     :=	(cAliasTmp)->F8_SEDIFRE
			TTRB->VALOR			:=	(cAliasTmp)->F1_VALBRUT
			TTRB->TRANSP		:=	(cAliasTmp)->F8_TRANSP
			TTRB->LOJA			:=	(cAliasTmp)->F8_LOJTRAN
			TTRB->NOME			:=	(cAliasTmp)->A2_NOME
			TTRB->APROVADO		:=	(cAliasTmp)->F8_APROV
			TTRB->DATAAPROV		:=	Stod((cAliasTmp)->F8_DTAPROV)

		Endif

		TTRB->(MsUnlock())

	(cAliasTMP)->(DbSkip())

	End

Else

	MsgInfo('Não foram encontradas informações, verifique os filtros!!')
	Return

Endif
DbSelectArea("TTRB")
TTRB->(DbGotop())
If ValType(oMark) == 'O'
	oMark:oBrowse:Refresh()
Endif

Return
