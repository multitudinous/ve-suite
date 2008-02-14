// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- My Includes --- //
#include "HyperLabUIDialog.h"

// --- wxWidgets Includes --- //
#include <wx/notebook.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/radiobox.h>
#include <wx/button.h>
#include <wx/statline.h>
//#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/listbox.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/panel.h>

BEGIN_EVENT_TABLE( HyperLabUIDialog, wxDialog )
EVT_RADIOBOX( ID_SHADER_EFFECTS, HyperLabUIDialog::OnShaderEffects )
EVT_SLIDER( ID_AMBIENT_SLIDER, HyperLabUIDialog::OnAmbientRGB )
EVT_SLIDER( ID_DIFFUSE_SLIDER, HyperLabUIDialog::OnDiffuseRGB )
EVT_SLIDER( ID_SPECULAR_SLIDER, HyperLabUIDialog::OnSpecularRGB )
EVT_BUTTON( ID_OK_BUTTON, HyperLabUIDialog::OnOK )
EVT_BUTTON( ID_CANCEL_BUTTON, HyperLabUIDialog::OnCancel )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
HyperLabUIDialog::HyperLabUIDialog( wxWindow* parent,
                                    int id,
                                    ves::conductor::util::CORBAServiceList* service,
                                    std::string* portNumber )
:
UIDialog( parent, id, _( "HyperLab" ) ),
p_portNumber( portNumber ),

m_portTextCtrl( 0 ),

m_shaderEffectsRadioBox( 0 ),

m_arSlider( 0 ),
m_agSlider( 0 ),
m_abSlider( 0 ),
m_drSlider( 0 ),
m_dgSlider( 0 ),
m_dbSlider( 0 ),
m_srSlider( 0 ),
m_sgSlider( 0 ),
m_sbSlider( 0 ),

m_instructions( 0 ),

m_serviceList( service )
{
    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
HyperLabUIDialog::~HyperLabUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool HyperLabUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool HyperLabUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::BuildGUI()
{
    wxDialog* itemDialog = this;

    SetForegroundColour( wxColour( 255, 255, 255 ) );
    SetBackgroundColour( wxColour( 0, 0, 0 ) );

    //Base
    wxBoxSizer* base_bs_1 = new wxBoxSizer( wxVERTICAL );
    itemDialog->SetSizer( base_bs_1 );

    wxBoxSizer* base_bs_2 = new wxBoxSizer( wxVERTICAL );
    base_bs_1->Add( base_bs_2, 1,wxGROW | wxALL, 5 );

    wxBoxSizer* base_bs_3 = new wxBoxSizer( wxHORIZONTAL );
    base_bs_2->Add( base_bs_3, 0, wxALIGN_LEFT | wxALL, 5 );

    wxBoxSizer* base_bs_4 = new wxBoxSizer( wxHORIZONTAL );
    base_bs_3->Add( base_bs_4, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxIcon bitmap( _( "./Icons/vesuite.ico" ), wxBITMAP_TYPE_ICO );
    wxStaticBitmap* base_sbm_1 = new wxStaticBitmap( itemDialog, wxID_STATIC, bitmap, wxDefaultPosition, wxSize( 32, 32 ), 0 );
    base_bs_4->Add( base_sbm_1, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5 );

    wxStaticText* base_st_1 = new wxStaticText( itemDialog, wxID_STATIC, _( "  Hyper Controls  " ), wxDefaultPosition, wxDefaultSize, wxDEFAULT );
    base_st_1->SetForegroundColour( wxColour( 255, 255, 255 ) );
    base_st_1->SetBackgroundColour( wxColour( 0, 0, 0 ) );
    base_st_1->SetFont( wxFont( 10, wxNORMAL, wxNORMAL, wxBOLD, false ) );
    base_bs_4->Add( base_st_1, 0, wxALIGN_CENTER_VERTICAL | wxALL | wxADJUST_MINSIZE, 5 );

    wxStaticLine* base_sl_1 = new wxStaticLine( itemDialog, wxID_STATIC, wxDefaultPosition, wxSize( 200, 2 ), wxLI_HORIZONTAL );
    base_bs_4->Add( base_sl_1, 0, wxALIGN_TOP | wxLEFT, 60 );

    //Notebook
    wxNotebook* notebook = new wxNotebook( itemDialog, ID_NOTEBOOK, wxDefaultPosition, wxDefaultSize, wxNB_TOP );
    notebook->SetBackgroundColour( wxColour( 0, 0, 0 ) );

    //Unit Panel
    wxPanel* unit_panel=new wxPanel(notebook,ID_UNIT_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    unit_panel->SetBackgroundColour(wxColour(255,144,30));
    wxBoxSizer* unit_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    unit_panel->SetSizer(unit_panel_bs_1);

    notebook->AddPage(unit_panel,_("Unit") );

    //Physics Panel
    wxPanel* physics_panel=new wxPanel(notebook,ID_PHYSICS_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    physics_panel->SetBackgroundColour(wxColour(255,144,30));
    wxBoxSizer* physics_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    physics_panel->SetSizer(physics_panel_bs_1);

    notebook->AddPage( physics_panel, _( "Physics" ) );

    //Material Panel
    wxPanel* material_panel=new wxPanel(notebook,ID_MATERIAL_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    material_panel->SetBackgroundColour(wxColour(255,144,30));
    wxBoxSizer* material_panel_bs_1=new wxBoxSizer(wxHORIZONTAL);
    material_panel->SetSizer(material_panel_bs_1);

    wxString material_panel_clb_strings[] = { _( "Default Effects" ), _( "Advanced Effects" ), _( "X-Ray Effects" ) };
    m_shaderEffectsRadioBox = new wxRadioBox( material_panel, ID_SHADER_EFFECTS, _("Shader Effects"), wxDefaultPosition, wxDefaultSize, 3, material_panel_clb_strings, 1, wxRA_SPECIFY_COLS );
  
    material_panel_bs_1->Add( m_shaderEffectsRadioBox, 0, wxALIGN_LEFT|wxLEFT|wxRIGHT|wxTOP|wxBOTTOM, 10 );

    notebook->AddPage(material_panel,_("Material") );

    //Light Panel
    wxPanel* light_panel=new wxPanel(notebook,ID_LIGHT_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    light_panel->SetBackgroundColour(wxColour(255,144,30));
    wxBoxSizer* light_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    light_panel->SetSizer(light_panel_bs_1);

    wxStaticText* light_panel_st_1=new wxStaticText(light_panel,wxID_STATIC, _("Ambient"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_1->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_1->SetFont(wxFont(9,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_1,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_2=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_2,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_17=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_2->Add(light_panel_bs_17,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_2=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_2->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_2->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_2,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_3=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_2->Add(light_panel_bs_3,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,32);

    wxStaticText* light_panel_st_3=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_3->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_3->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_3,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_4=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_2->Add(light_panel_bs_4,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,32);

    wxStaticText* light_panel_st_4=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_4->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_4->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_4,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_5=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_5,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    m_arSlider=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_arSlider->SetForegroundColour(wxColour(255,255,255));
    m_arSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(m_arSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_agSlider=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_agSlider->SetForegroundColour(wxColour(255,255,255));
    m_agSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(m_agSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_abSlider=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_abSlider->SetForegroundColour(wxColour(255,255,255));
    m_abSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(m_abSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    wxStaticLine* light_panel_line_1=new wxStaticLine(light_panel,wxID_STATIC,wxDefaultPosition,wxDefaultSize,wxLI_HORIZONTAL);
    light_panel_bs_1->Add(light_panel_line_1,0,wxGROW|wxALL,5);

    //wxBoxSizer* light_panel_bs_6=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_1->Add(light_panel_bs_6,0,wxALIGN_CENTER_HORIZONTAL|wxALL,5);

    wxStaticText* light_panel_st_5=new wxStaticText(light_panel,wxID_STATIC,_("Diffuse"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_5->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_5->SetFont(wxFont(9,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_5,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_7=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_7,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_18=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_7->Add(light_panel_bs_18,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_6=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_6->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_6->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_6,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_8=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_7->Add(light_panel_bs_8,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,42);

    wxStaticText* light_panel_st_7=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_7->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_7->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_7,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_9=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_7->Add(light_panel_bs_9,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,42);

    wxStaticText* light_panel_st_8=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_8->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_8->SetFont(wxFont(8,wxNORMAL, wxNORMAL, wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_8,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_10=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_10,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    m_drSlider=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,231,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_drSlider->SetForegroundColour(wxColour(255,255,255));
    m_drSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(m_drSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_dgSlider=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,231,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_dgSlider->SetForegroundColour(wxColour(255,255,255));
    m_dgSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(m_dgSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_dbSlider=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,115,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_dbSlider->SetForegroundColour(wxColour(255,255,255));
    m_dbSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(m_dbSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    wxStaticLine* light_panel_line_2=new wxStaticLine(light_panel,wxID_STATIC,wxDefaultPosition,wxDefaultSize,wxLI_HORIZONTAL);
    light_panel_bs_1->Add(light_panel_line_2,0,wxGROW|wxALL,5);

    //wxBoxSizer* light_panel_bs_11=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_1->Add(light_panel_bs_11,0,wxALIGN_CENTER_HORIZONTAL|wxALL,5);

    wxStaticText* light_panel_st_9=new wxStaticText(light_panel,wxID_STATIC,_("Specular"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_9->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_9->SetFont(wxFont(9,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_9,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_12=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_12,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_19=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_12->Add(light_panel_bs_19,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_10=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_10->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_10->SetFont(wxFont(8,wxNORMAL, wxNORMAL, wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_10,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_13=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_12->Add(light_panel_bs_13,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,52);

    wxStaticText* light_panel_st_11=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_11->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_11->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_11,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_14=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_12->Add(light_panel_bs_14,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,52);

    wxStaticText* light_panel_st_12=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_12->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_12->SetFont(wxFont(8,wxNORMAL, wxNORMAL, wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_12,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_15=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_15,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    m_srSlider=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_srSlider->SetForegroundColour(wxColour(255,255,255));
    m_srSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(m_srSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_sgSlider=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_sgSlider->SetForegroundColour(wxColour(255,255,255));
    m_sgSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(m_sgSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    m_sbSlider=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    m_sbSlider->SetForegroundColour(wxColour(255,255,255));
    m_sbSlider->SetFont(wxFont(8,wxNORMAL,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(m_sbSlider,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    wxStaticLine* light_panel_line_3=new wxStaticLine(light_panel,wxID_STATIC,wxDefaultPosition,wxDefaultSize,wxLI_HORIZONTAL);
    light_panel_bs_1->Add(light_panel_line_3,0,wxGROW|wxALL,5);

    wxBoxSizer* light_panel_bs_16=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_16,0,wxALIGN_CENTER_HORIZONTAL|wxALL,5);

    notebook->AddPage(light_panel,_("Lights"));

    //Base
    base_bs_2->Add(notebook,1,wxGROW|wxALL,5);

    wxBoxSizer* base_bs_5=new wxBoxSizer(wxHORIZONTAL);
    base_bs_2->Add(base_bs_5,0,wxGROW,5);

    wxStaticLine* base_sl_2=new wxStaticLine(itemDialog,wxID_STATIC,wxDefaultPosition,wxSize(200,2),wxLI_HORIZONTAL);
    base_bs_5->Add(base_sl_2,0,wxALIGN_BOTTOM|wxRIGHT|wxTOP,60);

    wxButton* base_b_1=new wxButton(itemDialog,ID_OK_BUTTON,_("OK"),wxDefaultPosition,wxDefaultSize,0);
    base_bs_5->Add(base_b_1,1,wxALIGN_CENTER_VERTICAL|wxALL,5);

    wxButton* base_b_2=new wxButton(itemDialog,ID_CANCEL_BUTTON,_("Cancel"),wxDefaultPosition,wxDefaultSize,0);
    base_bs_5->Add(base_b_2,1,wxALIGN_CENTER_VERTICAL|wxALL,5);

    SetBestFittingSize();
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::UpdateGUI()
{
    //m_portTextCtrl->SetValue( ( *p_portNumber ).c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnShaderEffects( wxCommandEvent& event )
{
    unsigned int data = m_shaderEffectsRadioBox->GetSelection();

    //Build the command
    m_commandName = "SHADER_EFFECTS_UPDATE";

    ves::open::xml::DataValuePairSharedPtr shaderEffectsDVP = new ves::open::xml::DataValuePair();
    shaderEffectsDVP->SetData( "shaderEffects", data );
    m_instructions.push_back( shaderEffectsDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnAmbientRGB( wxCommandEvent& event )
{
    double data[ 3 ];
    data[ 0 ] = m_arSlider->GetValue() / 256.0f;
    data[ 1 ] = m_agSlider->GetValue() / 256.0f;
    data[ 2 ] = m_abSlider->GetValue() / 256.0f;

    //Build the command
    m_commandName = "AMBIENT_UPDATE";

    ves::open::xml::DataValuePairSharedPtr arDVP = new ves::open::xml::DataValuePair();
    arDVP->SetData( "arColor", data[ 0 ] );
    m_instructions.push_back( arDVP );

    ves::open::xml::DataValuePairSharedPtr agDVP = new ves::open::xml::DataValuePair();
    agDVP->SetData( "agColor", data[ 1 ] );
    m_instructions.push_back( agDVP );

    ves::open::xml::DataValuePairSharedPtr abDVP = new ves::open::xml::DataValuePair();
    abDVP->SetData( "abColor", data[ 2 ] );
    m_instructions.push_back( abDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnDiffuseRGB( wxCommandEvent& event )
{
    double data[ 3 ];
    data[ 0 ] = m_drSlider->GetValue() / 256.0f;
    data[ 1 ] = m_dgSlider->GetValue() / 256.0f;
    data[ 2 ] = m_dbSlider->GetValue() / 256.0f;

    //Build the command
    m_commandName = "DIFFUSE_UPDATE";

    ves::open::xml::DataValuePairSharedPtr drDVP = new ves::open::xml::DataValuePair();
    drDVP->SetData( "drColor", data[ 0 ] );
    m_instructions.push_back( drDVP );

    ves::open::xml::DataValuePairSharedPtr dgDVP = new ves::open::xml::DataValuePair();
    dgDVP->SetData( "dgColor",data[ 1 ] );
    m_instructions.push_back( dgDVP );

    ves::open::xml::DataValuePairSharedPtr dbDVP = new ves::open::xml::DataValuePair();
    dbDVP->SetData( "dbColor", data[ 2 ] );
    m_instructions.push_back( dbDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnSpecularRGB( wxCommandEvent& event )
{
    double data[ 3 ];
    data[ 0 ] = m_srSlider->GetValue() / 256.0f;
    data[ 1 ] = m_sgSlider->GetValue() / 256.0f;
    data[ 2 ] = m_sbSlider->GetValue() / 256.0f;

    //Build the command
    m_commandName = "SPECULAR_UPDATE";

    ves::open::xml::DataValuePairSharedPtr srDVP = new ves::open::xml::DataValuePair();
    srDVP->SetData( "srColor", data[ 0 ] );
    m_instructions.push_back( srDVP );

    ves::open::xml::DataValuePairSharedPtr sgDVP = new ves::open::xml::DataValuePair();
    sgDVP->SetData( "sgColor", data[ 1 ] );
    m_instructions.push_back( sgDVP );

    ves::open::xml::DataValuePairSharedPtr sbDVP = new ves::open::xml::DataValuePair();
    sbDVP->SetData( "sbColor", data[ 2 ] );
    m_instructions.push_back( sbDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnOK( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnCancel( wxCommandEvent& event )
{
    Close( true );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::ClearInstructions()
{
    m_instructions.clear();
    m_commandName.clear() ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command = new ves::open::xml::Command();

    for( size_t i = 0; i < m_instructions.size(); ++i )
    {
        command->AddDataValuePair( m_instructions.at( i ) );
    }

    command->SetCommandName( m_commandName );

    m_serviceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
