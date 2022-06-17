#include "protheus.ch"
#include "rwmake.ch"

/*��������������������������������������������������������������������������������������������������
����������������������������������������������������������������������������������������������������
������������������������������������������������������������������������������������������������Ŀ��
���Funcao    | MT100TOK    | Autor: Alessandro Sperandio              	    | Data:  28/07/15    ���
������������������������������������������������������������������������������������������������Ĵ��
���Descricao | Ponto de entrada acionado ap�s o usuario confirmar a inclusao        	  		 ���
���          | do documento de entrada.								 				             ���
������������������������������������������������������������������������������������������������Ĵ��
���Uso       | Especifico Agrocete                                                               ���
������������������������������������������������������������������������������������������������Ĵ��
���                   ATUALIZACOES SOFRIDAS DESDE A CONSTRU�AO INICIAL.                          ���
������������������������������������������������������������������������������������������������Ĵ��
���Programador    | Data     | Motivo da Alteracao                                               ���
������������������������������������������������������������������������������������������������Ĵ��
��� Tiago S.      |09/12/19  | Retirada da valida��es contabeis da nota -  Ticket# 890           ���
��� Pedro A. S    |10/09/21  | Tratamento para quando for NF de saida - Chamado 25872 GLPI 3927  ���
���               |          |                                                                   ���
���               |          |                                                                   ���
���               |          |                                                                   ���
���               |          |                                                                   ���
���               |          |                                                                   ���
���               |          |                                                                   ���
���               |          |                                                                   ���
�������������������������������������������������������������������������������������������������ٱ�
����������������������������������������������������������������������������������������������������
��������������������������������������������������������������������������������������������������*/


User Function MT100TOK()

Local lRet 			:= .T.
Local lRetPad		:= PARAMIXB[1]
Local aArea 		:= GetArea()
Local nPosCfop   := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_CF" 	 })
Local nPosTes    := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_TES" 	 })
Local nPosCFis   := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_CLASFIS" 	 })
Local aColsSD1   := aCols
Local nPosConta  := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_CONTA" 	 })
Local nPosCC     := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_CC" 	 })
Local nPosMotDev := aScan(aHeader, {|x| AllTrim(x[2]) == "D1_XMOTDEV"})

Local nX
//�����������������������������������������������������������������������������������������������Ŀ
//�Caso as validacoes padroes da rotina foram atendidas, entao aplica as validacoes personalizadas�
//�������������������������������������������������������������������������������������������������1

If !IsInCallStack('MATA910') //tirando do documento de entrada manual.
	If lRetPad .and. !empty(nPosTes)
	For nX := 1 To Len(aColsSD1)
			If !aColsSD1[nX][Len(aHeader) + 1] //Sen�o for deletada
          		If cTipo == "D" .And. Empty(aColsSD1[nX][nPosMotDev])
				  	MsgInfo("Para notas de devolu��o, informar o motivo da devolu��o para cada item")
					Return(.F.)
				Endif
				cCfop   := AllTrim( aColsSD1[nX][nPosCfop] )  //Captura a cfop
				cTes    := AllTrim( aColsSD1[nX][nPosTes] )  //Captura a Tes
				cClasFis:= AllTrim( aColsSD1[nX][nPosCfis] )  //Captura a Classifica��o Fiscal
				cConta  := AllTrim( aColsSD1[nX][nPosConta] )  //Captura a Conta Contabil
				cCC     := AllTrim( aColsSD1[nX][nPosCC] )  //Captura o Centro de Custo

				If SubStr(cCfop,1,1) == "3"  .And. SubStr(cClasfis,1,1) == "0"
					Alert('Favor Verificar a Classifica��o Fiscal dos Itens, Caso Seja CFOP 3 deve-se alterar a origem na Classifica��o Fiscal')
					Return .F.
					Exit
				Endif

				//Ticket# 890
				/*If cTipo $ "NCIP"

				If Empty(cConta)
					Alert('� necess�rio informar a Conta Cont�bil',"Conta Cont�bil")
					Return .F.
					Exit
				Endif

				If Left(cConta,1) $ "1|2" .and. !Empty(cCC)
					Alert('N�o informar Centro de Custo para Contas Patrimoniais',"Conta Cont�bil")
					Return .F.
					Exit
				Endif

				If Left(cConta,1) $ "3" .and. Empty(cCC)
					Alert('Obrigat�rio informar o Centro de Custo para Contas de Resultado',"Centro de Custo")
					Return .F.
					Exit
				Endif

				Endif*/

			Endif
		Next nX
	EndIf

	RestArea(aArea)
EndIf

Return lRet
