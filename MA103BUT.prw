#Include 'Protheus.ch'
#Include 'rwmake.ch'

User Function MA103BUT()
	Local aButtons := {}

	//aadd(aButtons, {'Imp.XML', {||U_VAM009()}, 'Imp.XML'})
	aadd(aButtons, {'Amarr.Frete', {||U_COMAT001()}, 'Amarr.Frete'})
//	aadd(aButtons, {'Preenche CTE', {||U_UpdCteNum()}, 'Update CTE_Num'})  // 000024601
	aadd(aButtons, {'CTE-Notas Saída', {||U_AGNtFrete()}, 'Notas Saída x CTE'})
	aadd(aButtons, {'CTE-Define Carga', {||U_AGLnkCarga(,.T.)}, 'CTE-Define Carga'})
	aadd(aButtons, {'Chave nota fiscal', {||U_UCHVNFE()}, 'Chave nota fiscal'})

	If Funname() = "SIMULADEV"
		aadd(aButtons, {'Imprimir Simulação', {||U_ImpSmlDev()}, 'Imprimir Simulação'})
	Endif
	U_ACGetCarga(0)
Return (aButtons)

//Apresenta Chave da Nota fiscal para cópia
User Function UCHVNFE(_cTipo)

  local nPosCtrl := 0
  Local _cChave  := Alltrim(SF1->F1_CHVNFE)

  private oDlg   := Nil

  // define a tela
  define MsDialog oDlg from 000, 000 to 194, 500 title "Chave" pixel

  // divisão da tela
  @ nPosCtrl, 2 to 80, 250 title "Chave de Nota"

  // posiciona o campo Memo para edição
  nPosCtrl += 8
  @ nPosCtrl, 5 Get _cChave size 242, 34 PIXEL

  // posiciona os botoes
  nPosCtrl += 74
  @ nPosCtrl, 210 Button "OK" size 40, 12 action oDlg:End()

  // ativa a tela
  activate MsDialog oDlg centered

Return()
