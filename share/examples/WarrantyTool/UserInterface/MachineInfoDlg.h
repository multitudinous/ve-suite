///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#ifndef __MachineInfoDlg__
#define __MachineInfoDlg__

#include <wx/string.h>
#include <wx/button.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/filepicker.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/checklst.h>
#include <wx/dialog.h>

///////////////////////////////////////////////////////////////////////////

#include <ves/conductor/UIDialog.h>

///////////////////////////////////////////////////////////////////////////////
/// Class MachineInfoDlg
///////////////////////////////////////////////////////////////////////////////
class MachineInfoDlg : public ves::conductor::UIDialog
{
	private:
	
	protected:
		wxButton* m_dataLoadButton;
		wxFilePickerCtrl* m_productDataLoader;
		wxChoice* m_variableChoice00;
		wxChoice* m_variableLogicOperator00;
		wxTextCtrl* m_textInput00;
		wxChoice* m_logicOperator00;
		wxChoice* m_variableChoice01;
		wxChoice* m_variableLogicOperator01;
		wxTextCtrl* m_textInput01;
		wxChoice* m_logicOperator01;
		wxChoice* m_variableChoice02;
		wxChoice* m_variableLogicOperator02;
		wxTextCtrl* m_textInput02;
		wxChoice* m_logicOperator02;
		wxChoice* m_variableChoice03;
		wxChoice* m_variableLogicOperator03;
		wxTextCtrl* m_textInput03;
		wxTextCtrl* m_queryTextCommandCtrl;
		wxChoice* m_choice13;
		wxTextCtrl* m_textCtrl18;
		wxTextCtrl* m_textCtrl19;
		wxChoice* m_manualPartSelectionChoice;
		wxTextCtrl* m_partTextEntry;
		wxCheckListBox* m_displayTextChkList;
		wxStdDialogButtonSizer* m_dialogButtons;
		wxButton* m_dialogButtonsOK;
		wxButton* m_dialogButtonsApply;
		wxButton* m_dialogButtonsCancel;
		
		// Virtual event handlers, overide them in your derived class
		virtual void OnDataLoad( wxFileDirPickerEvent& event ){ event.Skip(); }
		virtual void OnVariableAndLogicalChoice( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnCreateInputText( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnTextQueryEnter( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnPartSelection( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnPartNumberEntry( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnTextChkListToggle( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnQueryApply( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnDialogCancel( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnQueryOK( wxCommandEvent& event ){ event.Skip(); }
		
	
	public:
		MachineInfoDlg( wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxT("Machine Info"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxCLOSE_BOX|wxDEFAULT_DIALOG_STYLE );
		~MachineInfoDlg();
	
};

#endif //__MachineInfoDlg__
