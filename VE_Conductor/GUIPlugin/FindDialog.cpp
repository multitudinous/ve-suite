#include "FindDialog.h"

BEGIN_EVENT_TABLE(FindDialog,wxDialog)
	EVT_CLOSE(FindDialog::OnClose)
	EVT_BUTTON(ID_CANCELBUTTON,FindDialog::CancelButtonClick)
	EVT_BUTTON(ID_FINDBUTTON,FindDialog::FindButtonClick)
END_EVENT_TABLE()

FindDialog::FindDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

FindDialog::~FindDialog()
{
} 

void FindDialog::CreateGUIControls()
{
	SetTitle(wxT("Find"));
	SetIcon(wxNullIcon);
	SetSize(8,8,261,144);
	Center();
	

	UnitLabel = new wxStaticText(this, ID_UNITLABEL, wxT("Unit Operations"), wxPoint(4,22), wxDefaultSize, 0, wxT("UnitLabel"));
	UnitLabel->SetFont(wxFont(10, wxSWISS, wxNORMAL,wxBOLD, FALSE, wxT("Times New Roman")));

	wxArrayString arrayStringFor_WxChoice1;
	WxChoice1 = new wxChoice(this, ID_WXCHOICE1, wxPoint(96,19), wxSize(145,21), arrayStringFor_WxChoice1, 0, wxDefaultValidator, wxT("WxChoice1"));
	WxChoice1->SetSelection(-1);

	CancelButton = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"), wxPoint(165,57), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));

	FindButton = new wxButton(this, ID_FINDBUTTON, wxT("Find"), wxPoint(86,57), wxSize(75,25), 0, wxDefaultValidator, wxT("FindButton"));
}

void FindDialog::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

void FindDialog::CancelButtonClick(wxCommandEvent& event)
{
	Destroy();
}

void FindDialog::FindButtonClick(wxCommandEvent& event)
{
	selectedModule = WxChoice1->GetString(WxChoice1->GetSelection());
	selectedModulePos = WxChoice1->GetSelection();
	Destroy();
}

void FindDialog::SetModuleList(std::vector< std::string > modules)
{
	for(int i = 0; i < (int)modules.size(); i++)
		WxChoice1->Insert(modules[i].c_str(), i);
}

const char * FindDialog::GetSelectedModule()
{
	return selectedModule.c_str();
}

int FindDialog::GetSelectedModulePos()
{
	return selectedModulePos;
}