#include "ParamsDlg.h"

BEGIN_EVENT_TABLE(ParamsDlg,wxDialog)
	EVT_CLOSE(ParamsDlg::OnClose)
	EVT_CHOICE(ID_WXCHOICE1,ParamsDlg::WxChoice1Selected)
END_EVENT_TABLE()

ParamsDlg::ParamsDlg(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

ParamsDlg::~ParamsDlg()
{
} 

void ParamsDlg::CreateGUIControls()
{
	WxPanel1 = new wxPanel(this, ID_WXPANEL1, wxPoint(0,0), wxSize(252,707));
	WxGrid1 = new wxGrid(WxPanel1, ID_WXGRID1, wxPoint(0,29), wxSize(250,605));
	WxGrid1->SetDefaultColSize(100);
	WxGrid1->SetDefaultRowSize(25);
	WxGrid1->SetRowLabelSize(20);
	WxGrid1->SetColLabelSize(0);
	WxGrid1->CreateGrid(23,2,wxGrid::wxGridSelectCells);
	wxArrayString arrayStringFor_WxChoice1;
	WxChoice1 = new wxChoice(WxPanel1, ID_WXCHOICE1, wxPoint(48,5), wxSize(145,21), arrayStringFor_WxChoice1, 0, wxDefaultValidator, wxT("WxChoice1"));
	WxChoice1->SetSelection(-1);
	SetTitle(wxT("Project2"));
	SetIcon(wxNullIcon);
	SetSize(8,8,236,749);
	Center();
}

void ParamsDlg::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}
void ParamsDlg::AddToList(const char * element)
{
	WxChoice1->Append(wxT(element));
}

void ParamsDlg::AddResults(std::string dataName, std::vector< std::string > paramName, std::vector< std::string > paramData)
{
	ParamNames[dataName] = paramName;
	ParamValues[dataName] = paramData;
}

void ParamsDlg::WxChoice1Selected(wxCommandEvent& event )
{
	WxGrid1->ClearGrid();
	std::string name(WxChoice1->GetString(WxChoice1->GetSelection()).c_str());
	for(int i = 0; i < (int) ParamNames[name].size(); i++)
	{
		WxGrid1->SetCellValue(i, 0, wxT(ParamNames[name][i].c_str()));
		WxGrid1->SetCellValue(i, 1, wxT(ParamValues[name][i].c_str()));
	}
}