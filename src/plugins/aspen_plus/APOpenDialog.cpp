#include "APOpenDialog.h"
#include "../ConductorPluginEnums.h"
#include <vector>

BEGIN_EVENT_TABLE(APOpenDialog,wxDialog)
	EVT_CLOSE(APOpenDialog::OnClose)
	EVT_BUTTON(APOPENDIALOG_CANCELBUTTON, APOpenDialog::CancelButtonClick)
	EVT_BUTTON(APOPENDIALOG_OKBUTTON, APOpenDialog::OKButtonClick)
END_EVENT_TABLE()

APOpenDialog::APOpenDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

APOpenDialog::~APOpenDialog()
{
} 

void APOpenDialog::CreateGUIControls()
{
	SetTitle(wxT("BKP/APW File"));
	SetIcon(wxNullIcon);
	SetSize(8,8,370,138);
	Center();

	Label = new wxStaticText(this, -1, wxT("Aspen Plus Project"), wxPoint(8,16), wxDefaultSize, 0, wxT("Label"));
	Label->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));

	CancelButton = new wxButton(this, APOPENDIALOG_CANCELBUTTON, wxT("Cancel"), wxPoint(275,55), wxSize(75,25), 0, wxDefaultValidator, wxT("CancelButton"));
	CancelButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));

	OKButton = new wxButton(this, APOPENDIALOG_OKBUTTON, wxT("Ok"), wxPoint(195,55), wxSize(75,25), 0, wxDefaultValidator, wxT("OKButton"));
	OKButton->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
}

void APOpenDialog::OnClose( wxCloseEvent& )
{
	Destroy();
}

void APOpenDialog::OKButtonClick( wxCommandEvent& event )
{
    EndModal( wxID_OK );
}

void APOpenDialog::CancelButtonClick( wxCommandEvent& event )
{
    EndModal( wxID_CANCEL );
}

void APOpenDialog::SetPopulateFilenames( )
{
    wxDir pluginsDir( ::wxGetCwd() );
    wxString filename;

    //apw
    wxString ext = wxString( "*.apw", wxConvUTF8 );
    std::vector< wxString > apwList;
    bool cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 4 );
        apwList.push_back( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    //bkp
    ext = wxString( "*.bkp", wxConvUTF8 );
    std::vector< wxString > bkpList;
    cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 4 );
        bkpList.push_back( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    //find file that have both apw and bkp files
    for( int i = 0; i < apwList.size(); i++ )
    {
        for( int j = 0; j < bkpList.size(); j++ )
        {
            if( apwList[i].CmpNoCase( bkpList[j] ) == 0 )
            {
                arrayStringFor_ComboBox.Add( apwList[i] );
            }
        }
    }

    // construct combo box
	ComboBox = new wxComboBox(this, -1, wxT(""), wxPoint(149,13), wxSize(208,27), arrayStringFor_ComboBox, 0, wxDefaultValidator, wxT("ComboBox"));
	ComboBox->SetFont(wxFont(12, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));
}

wxString APOpenDialog::GetFilename( )
{
    return arrayStringFor_ComboBox[ComboBox->GetCurrentSelection()];
}