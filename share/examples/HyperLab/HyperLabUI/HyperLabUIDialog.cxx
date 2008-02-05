// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "HyperLabUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/panel.h>
#include <wx/statline.h>
#include <wx/listbox.h>
#include <wx/checklst.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/notebook.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( HyperLabUIDialog, wxDialog )
EVT_CHECKLISTBOX( ID_SHADER_EFFECTS, HyperLabUIDialog::OnShaderEffects )
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
UIDialog( parent, id, _("HyperLab") ),
p_portNumber( portNumber )
{
    serviceList = service;
    portTextCtrl = 0;

    phong_check = false;
    texture_check = true;
    shadow_check = false;
    reflection_check = false;
    xray_check = false;

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
void HyperLabUIDialog::Lock(bool l)
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

    wxBoxSizer* base_bs_3=new wxBoxSizer(wxHORIZONTAL);
    base_bs_2->Add(base_bs_3,0,wxALIGN_LEFT|wxALL,5);

    wxBoxSizer* base_bs_4=new wxBoxSizer(wxHORIZONTAL);
    base_bs_3->Add(base_bs_4,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

    wxIcon bitmap( _("./Icons/vesuite.ico"), wxBITMAP_TYPE_ICO);
    wxStaticBitmap* base_sbm_1=new wxStaticBitmap(itemDialog,wxID_STATIC,bitmap,wxDefaultPosition,wxSize(32,32),0);
    base_bs_4->Add(base_sbm_1,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

    wxStaticText* base_st_1=new wxStaticText(itemDialog,wxID_STATIC,_("  Hyper Controls  "),wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER);
    base_st_1->SetForegroundColour(wxColour(255,255,255));
    base_st_1->SetBackgroundColour(wxColour(128,128,64));
    base_st_1->SetFont(wxFont(10,wxNORMAL,wxNORMAL,wxBOLD,false));
    base_bs_4->Add(base_st_1,0,wxALIGN_CENTER_VERTICAL|wxALL|wxADJUST_MINSIZE,5);

    wxStaticLine* base_sl_1=new wxStaticLine(itemDialog,wxID_STATIC,wxDefaultPosition,wxSize(200,2),wxLI_HORIZONTAL);
    base_bs_4->Add(base_sl_1,0,wxALIGN_TOP|wxLEFT,60);

    //Notebook
    wxNotebook* notebook=new wxNotebook(itemDialog,ID_NOTEBOOK,wxDefaultPosition,wxDefaultSize,wxNB_TOP);
    notebook->SetBackgroundColour(wxColour(0,0,0));

    //Unit Panel
    wxPanel* unit_panel=new wxPanel(notebook,ID_UNIT_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    unit_panel->SetBackgroundColour(wxColour(85,0,0));
    wxBoxSizer* unit_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    unit_panel->SetSizer(unit_panel_bs_1);

    notebook->AddPage(unit_panel,_("Unit") );

    //Physics Panel
    wxPanel* physics_panel=new wxPanel(notebook,ID_PHYSICS_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    physics_panel->SetBackgroundColour(wxColour(85,0,0));
    wxBoxSizer* physics_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    physics_panel->SetSizer(physics_panel_bs_1);

    notebook->AddPage(physics_panel,_("Physics") );

    //Material Panel
    wxPanel* material_panel=new wxPanel(notebook,ID_MATERIAL_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    material_panel->SetBackgroundColour(wxColour(85,0,0));
    wxBoxSizer* material_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    material_panel->SetSizer(material_panel_bs_1);

    wxBoxSizer* material_panel_bs_2=new wxBoxSizer(wxVERTICAL);
    material_panel->SetSizer(material_panel_bs_2);

    wxStaticText* material_panel_st_1=new wxStaticText(material_panel,wxID_STATIC,_("Shader Effects:"),wxDefaultPosition,wxDefaultSize,0);
    material_panel_st_1->SetForegroundColour(wxColour(255,255,255));
    material_panel_st_1->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,true));
    material_panel_bs_2->Add(material_panel_st_1,0,wxALIGN_LEFT|wxALL|wxADJUST_MINSIZE,5);

    wxString material_panel_clb_strings[]={_("Phong"),_("Texture"),_("Shadow"),_("Reflection"),_("X-Ray")};
    material_panel_clb_se=new wxCheckListBox(material_panel,ID_SHADER_EFFECTS,wxDefaultPosition,wxDefaultSize,5,material_panel_clb_strings,wxLB_SINGLE|wxSUNKEN_BORDER);
    material_panel_clb_se->Check(1,true);
    material_panel_bs_2->Add(material_panel_clb_se,0,wxALIGN_LEFT|wxALL,5);

    notebook->AddPage(material_panel,_("Material") );

    //Light Panel
    wxPanel* light_panel=new wxPanel(notebook,ID_LIGHT_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    light_panel->SetBackgroundColour(wxColour(85,0,0));
    wxBoxSizer* light_panel_bs_1=new wxBoxSizer(wxVERTICAL);
    light_panel->SetSizer(light_panel_bs_1);

    wxStaticText* light_panel_st_1=new wxStaticText(light_panel,wxID_STATIC, _("Ambient"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_1->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_1->SetFont(wxFont(9,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_1,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxTOP|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_2=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_2,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_17=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_2->Add(light_panel_bs_17,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_2=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_2->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_2->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_2,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_3=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_2->Add(light_panel_bs_3,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,32);

    wxStaticText* light_panel_st_3=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_3->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_3->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_3,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_4=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_2->Add(light_panel_bs_4,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,32);

    wxStaticText* light_panel_st_4=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_4->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_4->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_2->Add(light_panel_st_4,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_5=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_5,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_ar=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_ar->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_ar->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(light_panel_sl_ar,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_ag=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_ag->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_ag->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(light_panel_sl_ag,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_ab=new wxSlider(light_panel,ID_AMBIENT_SLIDER,103,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_ab->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_ab->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_5->Add(light_panel_sl_ab,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    wxStaticLine* light_panel_line_1=new wxStaticLine(light_panel,wxID_STATIC,wxDefaultPosition,wxDefaultSize,wxLI_HORIZONTAL);
    light_panel_bs_1->Add(light_panel_line_1,0,wxGROW|wxALL,5);

    //wxBoxSizer* light_panel_bs_6=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_1->Add(light_panel_bs_6,0,wxALIGN_CENTER_HORIZONTAL|wxALL,5);

    wxStaticText* light_panel_st_5=new wxStaticText(light_panel,wxID_STATIC,_("Diffuse"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_5->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_5->SetFont(wxFont(9,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_5,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_7=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_7,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_18=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_7->Add(light_panel_bs_18,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_6=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_6->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_6->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_6,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_8=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_7->Add(light_panel_bs_8,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,42);

    wxStaticText* light_panel_st_7=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_7->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_7->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_7,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_9=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_7->Add(light_panel_bs_9,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,42);

    wxStaticText* light_panel_st_8=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_8->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_8->SetFont(wxFont(8,wxDECORATIVE, wxNORMAL, wxBOLD,false));
    light_panel_bs_7->Add(light_panel_st_8,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_10=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_10,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_dr=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,231,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_dr->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_dr->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(light_panel_sl_dr,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_dg=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,231,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_dg->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_dg->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(light_panel_sl_dg,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_db=new wxSlider(light_panel,ID_DIFFUSE_SLIDER,115,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_db->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_db->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_10->Add(light_panel_sl_db,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    wxStaticLine* light_panel_line_2=new wxStaticLine(light_panel,wxID_STATIC,wxDefaultPosition,wxDefaultSize,wxLI_HORIZONTAL);
    light_panel_bs_1->Add(light_panel_line_2,0,wxGROW|wxALL,5);

    //wxBoxSizer* light_panel_bs_11=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_1->Add(light_panel_bs_11,0,wxALIGN_CENTER_HORIZONTAL|wxALL,5);

    wxStaticText* light_panel_st_9=new wxStaticText(light_panel,wxID_STATIC,_("Specular"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_9->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_9->SetFont(wxFont(9,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_1->Add(light_panel_st_9,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_12=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_12,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    //wxBoxSizer* light_panel_bs_19=new wxBoxSizer(wxHORIZONTAL);
    //light_panel_bs_12->Add(light_panel_bs_19,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    wxStaticText* light_panel_st_10=new wxStaticText(light_panel,wxID_STATIC,_("R:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_10->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_10->SetFont(wxFont(8,wxDECORATIVE, wxNORMAL, wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_10,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_13=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_12->Add(light_panel_bs_13,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,52);

    wxStaticText* light_panel_st_11=new wxStaticText(light_panel,wxID_STATIC,_("G:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_11->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_11->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_11,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_14=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_12->Add(light_panel_bs_14,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,52);

    wxStaticText* light_panel_st_12=new wxStaticText(light_panel,wxID_STATIC,_("B:"),wxDefaultPosition,wxDefaultSize,0);
    light_panel_st_12->SetForegroundColour(wxColour(255,255,255));
    light_panel_st_12->SetFont(wxFont(8,wxDECORATIVE, wxNORMAL, wxBOLD,false));
    light_panel_bs_12->Add(light_panel_st_12,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT|wxADJUST_MINSIZE,5);

    wxBoxSizer* light_panel_bs_15=new wxBoxSizer(wxHORIZONTAL);
    light_panel_bs_1->Add(light_panel_bs_15,0,wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_sr=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_sr->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_sr->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(light_panel_sl_sr,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_sg=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_sg->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_sg->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(light_panel_sl_sg,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

    light_panel_sl_sb=new wxSlider(light_panel,ID_SPECULAR_SLIDER,128,0,256,wxDefaultPosition,wxDefaultSize,wxSL_AUTOTICKS|wxSL_LABELS|wxSL_TOP);
    light_panel_sl_sb->SetForegroundColour(wxColour(255,255,255));
    light_panel_sl_sb->SetFont(wxFont(8,wxDECORATIVE,wxNORMAL,wxBOLD,false));
    light_panel_bs_15->Add(light_panel_sl_sb,0,wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT,5);

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
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::UpdateGUI()
{
    //portTextCtrl->SetValue( (*p_portNumber).c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnShaderEffects( wxCommandEvent& event )
{
    static bool xray_hold = false;

    phong_check=material_panel_clb_se->IsChecked(0);
    texture_check=material_panel_clb_se->IsChecked(1);
    shadow_check=material_panel_clb_se->IsChecked(2);
    reflection_check=material_panel_clb_se->IsChecked(3);
    xray_check=material_panel_clb_se->IsChecked(4);

    if((xray_hold==xray_check)&&(xray_check!=false)&&(phong_check==true||texture_check==true||shadow_check==true||reflection_check==true)){
    material_panel_clb_se->Check(4,false);

    xray_hold=material_panel_clb_se->IsChecked(4);
    }

    else if((xray_hold!=xray_check)&&(xray_check!=false)&&(phong_check==true||texture_check==true||shadow_check==true||reflection_check==true)){
    material_panel_clb_se->Check(0,false);
    material_panel_clb_se->Check(1,false);
    material_panel_clb_se->Check(2,false);
    material_panel_clb_se->Check(3,false);

    xray_hold=xray_check; 
    }

    else
    {
    xray_hold=xray_check;
    }

    phong_check=material_panel_clb_se->IsChecked(0);
    texture_check=material_panel_clb_se->IsChecked(1);
    shadow_check=material_panel_clb_se->IsChecked(2);
    reflection_check=material_panel_clb_se->IsChecked(3);
    xray_check=material_panel_clb_se->IsChecked(4);

    //Build the command
    command_name=std::string("SHADER_EFFECTS_UPDATE");

    ves::open::xml::DataValuePairSharedPtr phongID_DVP=new ves::open::xml::DataValuePair();
    phongID_DVP->SetData(std::string("phongID"),(unsigned int)(phong_check));
    instructions.push_back(phongID_DVP);

    ves::open::xml::DataValuePairSharedPtr textureID_DVP=new ves::open::xml::DataValuePair();
    textureID_DVP->SetData(std::string("textureID"),(unsigned int)(texture_check));
    instructions.push_back(textureID_DVP);

    ves::open::xml::DataValuePairSharedPtr shadowID_DVP=new ves::open::xml::DataValuePair();
    shadowID_DVP->SetData("shadowID",(unsigned int)(shadow_check));
    instructions.push_back(shadowID_DVP);

    ves::open::xml::DataValuePairSharedPtr reflectionID_DVP=new ves::open::xml::DataValuePair();
    reflectionID_DVP->SetData("reflectionID",(unsigned int)(reflection_check));
    instructions.push_back(reflectionID_DVP);

    ves::open::xml::DataValuePairSharedPtr xrayID_DVP=new ves::open::xml::DataValuePair();
    xrayID_DVP->SetData("xrayID",(unsigned int)(xray_check));
    instructions.push_back(xrayID_DVP);

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnAmbientRGB( wxCommandEvent& event )
{
    ambient_red=light_panel_sl_ar->GetValue()/256.0f;
    ambient_green=light_panel_sl_ag->GetValue()/256.0f;
    ambient_blue=light_panel_sl_ab->GetValue()/256.0f;

    //Build the command
    command_name=std::string("AMBIENT_UPDATE");

    ves::open::xml::DataValuePairSharedPtr ar_color_DVP=new ves::open::xml::DataValuePair();
    ar_color_DVP->SetData(std::string("ar_color"),ambient_red);
    instructions.push_back(ar_color_DVP);

    ves::open::xml::DataValuePairSharedPtr ag_color_DVP=new ves::open::xml::DataValuePair();
    ag_color_DVP->SetData(std::string("ag_color"),ambient_green);
    instructions.push_back(ag_color_DVP);

    ves::open::xml::DataValuePairSharedPtr ab_color_DVP=new ves::open::xml::DataValuePair();
    ab_color_DVP->SetData("ab_color",ambient_blue);
    instructions.push_back(ab_color_DVP);

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnDiffuseRGB( wxCommandEvent& event )
{
    diffuse_red=light_panel_sl_dr->GetValue()/256.0f;
    diffuse_green=light_panel_sl_dg->GetValue()/256.0f;
    diffuse_blue=light_panel_sl_db->GetValue()/256.0f;

    //Build the command
    command_name=std::string("DIFFUSE_UPDATE");

    ves::open::xml::DataValuePairSharedPtr dr_color_DVP=new ves::open::xml::DataValuePair();
    dr_color_DVP->SetData(std::string("dr_color"),diffuse_red);
    instructions.push_back(dr_color_DVP);

    ves::open::xml::DataValuePairSharedPtr dgID_DVP=new ves::open::xml::DataValuePair();
    dgID_DVP->SetData(std::string("dg_color"),diffuse_green);
    instructions.push_back(dgID_DVP);

    ves::open::xml::DataValuePairSharedPtr db_color_DVP=new ves::open::xml::DataValuePair();
    db_color_DVP->SetData("db_color",diffuse_blue);
    instructions.push_back(db_color_DVP);

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::OnSpecularRGB( wxCommandEvent& event )
{
    specular_red=light_panel_sl_sr->GetValue()/256.0f;
    specular_green=light_panel_sl_sg->GetValue()/256.0f;
    specular_blue=light_panel_sl_sb->GetValue()/256.0f;

    //Build the command
    command_name=std::string("SPECULAR_UPDATE");

    ves::open::xml::DataValuePairSharedPtr sr_color_DVP=new ves::open::xml::DataValuePair();
    sr_color_DVP->SetData(std::string("sr_color"),specular_red);
    instructions.push_back(sr_color_DVP);

    ves::open::xml::DataValuePairSharedPtr sg_color_DVP=new ves::open::xml::DataValuePair();
    sg_color_DVP->SetData(std::string("sg_color"),specular_green);
    instructions.push_back(sg_color_DVP);

    ves::open::xml::DataValuePairSharedPtr sb_color_DVP=new ves::open::xml::DataValuePair();
    sb_color_DVP->SetData("sb_color",specular_blue);
    instructions.push_back(sb_color_DVP);

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
    instructions.clear();
    command_name.clear() ;
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::Command* command = new ves::open::xml::Command();

    for( size_t i = 0; i < instructions.size(); ++i )
    {
        command->AddDataValuePair( instructions.at( i ) );
    }

    command->SetCommandName( command_name );

    serviceList->SendCommandStringToXplorer( command );

    //Clean up memory
    delete command;
}
////////////////////////////////////////////////////////////////////////////////
