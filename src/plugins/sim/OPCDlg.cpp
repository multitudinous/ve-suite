#include "OPCDlg.h"

using namespace ves::conductor;

BEGIN_EVENT_TABLE(OPCDlg,wxDialog)
	EVT_CLOSE(OPCDlg::OnClose)
	EVT_BUTTON(ID_WXBUTTON3, OPCDlg::OnSaveButton )
	EVT_BUTTON(ID_WXBUTTON4, OPCDlg::OnCancelButton )
	EVT_BUTTON(ID_WXBUTTON1, OPCDlg::OnAddButton )
	EVT_BUTTON(ID_WXBUTTON2, OPCDlg::OnRemoveButton )
END_EVENT_TABLE()

OPCDlg::OPCDlg(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

OPCDlg::~OPCDlg()
{
} 

void OPCDlg::CreateGUIControls()
{
	WxButton4 = new wxButton(this, ID_WXBUTTON4, wxT("Close"), wxPoint(306, 313), wxSize(75, 25), 0, wxDefaultValidator, wxT("WxButton4"));
	WxButton4->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	WxButton3 = new wxButton(this, ID_WXBUTTON3, wxT("Save"), wxPoint(221, 313), wxSize(75, 25), 0, wxDefaultValidator, wxT("WxButton3"));
	WxButton3->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	WxButton2 = new wxButton(this, ID_WXBUTTON2, wxT("<<"), wxPoint(179, 88), wxSize(32, 25), 0, wxDefaultValidator, wxT("WxButton2"));
	WxButton2->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	WxButton1 = new wxButton(this, ID_WXBUTTON1, wxT(">>"), wxPoint(179, 59), wxSize(32, 25), 0, wxDefaultValidator, wxT("WxButton1"));
	WxButton1->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	wxArrayString arrayStringFor_WxListBox3;
	WxListBox3 = new wxListBox(this, ID_WXLISTBOX3, wxPoint(214, 14), wxSize(165, 287), arrayStringFor_WxListBox3, wxLB_SINGLE);
	WxListBox3->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	wxArrayString arrayStringFor_WxListBox1;
	WxListBox1 = new wxListBox(this, ID_WXLISTBOX1, wxPoint(11, 14), wxSize(165, 287), arrayStringFor_WxListBox1, wxLB_SINGLE);
	WxListBox1->SetFont(wxFont(11, wxSWISS, wxNORMAL, wxNORMAL, false));

	SetTitle(wxT("OPCDialog"));
	SetIcon(wxNullIcon);
	SetSize(8,8,400,369);
	Center();
	
}

void OPCDlg::OnClose(wxCloseEvent& event)
{
	Destroy();
}

//void OPCDlg::PopulateLists( std::vector< std::string > list,
//						   std::vector< std::string > selected )
void OPCDlg::SetParentPlugin( SDPlugin * parent )
{
	m_parentPlugin = parent;
	
	std::vector< std::string > available = m_parentPlugin->GetAvailableVariables();
	std::vector< std::string > selected = m_parentPlugin->GetSelectVariables();

	for( int i = 0; i < available.size(); i++ )
	{
		m_availableVariables.Add( wxString(available[i].c_str(), wxConvUTF8) );
	}
	WxListBox1->Set( m_availableVariables );
	
	for( int i = 0; i < selected.size(); i++ )
	{
		m_selectedVariables.Add( wxString(selected[i].c_str(), wxConvUTF8) );
	}
	WxListBox3->Set( m_selectedVariables );
}

void OPCDlg::OnSaveButton( wxCommandEvent& event )
{
	std::vector< std::string > selectedVariables;
	for(int i = 0; i < m_selectedVariables.GetCount(); i++)
	{
		selectedVariables.push_back( std::string(m_selectedVariables[i].mb_str()) );
	}
	m_parentPlugin->SetSelectVariables( selectedVariables );
}

void OPCDlg::OnCancelButton( wxCommandEvent& event )
{
	Destroy();
}

void OPCDlg::OnAddButton( wxCommandEvent& event )
{
	wxArrayInt selections;
	WxListBox1->GetSelections( selections );

	for( int i = 0; i < selections.size(); i++ )
	{
		wxString selection = WxListBox1->GetString( selections[i] );
		if( !SearchArrayList( m_selectedVariables, selection ) )
		{
			m_selectedVariables.Add( selection );
		}
	}
	WxListBox3->Set( m_selectedVariables ); 
}

void OPCDlg::OnRemoveButton( wxCommandEvent& event )
{
	wxArrayInt selections;
	WxListBox3->GetSelections( selections );

	for( int i = 0; i < selections.size(); i++ )
	{
		m_selectedVariables.Remove( WxListBox3->GetString( selections[i] ) );
	}
	WxListBox3->Set( m_selectedVariables ); 
}

bool OPCDlg::SearchArrayList( wxArrayString arrayList, wxString entry )
{
	for( int i = 0; i < arrayList.GetCount(); i++)
	{
		if( entry.CompareTo( arrayList[i] ) == 0 )
		{
			return true;
		}
	}
	return false;
}