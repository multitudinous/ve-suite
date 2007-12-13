#ifndef HYPER_LAB_UI_DIALOG_H
#define HYPER_LAB_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/open/xml/DataValuePairPtr.h>

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace ves
{
namespace conductor
{
namespace util
{
    class CORBAServiceList;
}
}
}

class wxTextCtrl;
class wxSlider;
class wxSpinCtrl;
class wxCheckListBox;
class wxSpinEvent;

enum HYPER_IDS
{
    ID_PORT_TEXTCTRL,
    ID_NOTEBOOK,
    ID_UNIT_PANEL,
    ID_PHYSICS_PANEL,
    ID_MATERIAL_PANEL,
    ID_LIGHT_PANEL,
    ID_SHADER_EFFECTS,
    ID_AMBIENT_SLIDER,
    ID_DIFFUSE_SLIDER,
    ID_SPECULAR_SLIDER,
    ID_OK_BUTTON,
    ID_CANCEL_BUTTON
};

class HyperLabUIDialog : public ves::conductor::UIDialog
{
public:
   HyperLabUIDialog( wxWindow* parent,
                     int id,
                     ves::conductor::util::CORBAServiceList* service,
                     std::string* portNumber );
  
    virtual ~HyperLabUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

    std::string* p_portNumber;

    bool phong_check;
    bool texture_check;
    bool shadow_check;
    bool reflection_check;
    bool xray_check;

    double ambient_red;
    double ambient_green;
    double ambient_blue;
    double diffuse_red;
    double diffuse_green;
    double diffuse_blue;
    double specular_red;
    double specular_green;
    double specular_blue;

private:
    void BuildGUI();
    void UpdateGUI();

    void OnShaderEffects( wxCommandEvent& event );
    void OnAmbientRGB( wxCommandEvent& event );
    void OnDiffuseRGB( wxCommandEvent& event );
    void OnSpecularRGB( wxCommandEvent& event );
    void OnOK( wxCommandEvent& event );
    void OnCancel( wxCommandEvent& event );

    wxTextCtrl* portTextCtrl;
    wxCheckListBox* material_panel_clb_se;
    wxSlider* light_panel_sl_ar;
    wxSlider* light_panel_sl_ag;
    wxSlider* light_panel_sl_ab;
    wxSlider* light_panel_sl_dr;
    wxSlider* light_panel_sl_dg;
    wxSlider* light_panel_sl_db;
    wxSlider* light_panel_sl_sr;
    wxSlider* light_panel_sl_sg;
    wxSlider* light_panel_sl_sb;

    std::vector< ves::open::xml::DataValuePairSharedPtr > instructions;
    std::string command_name;

    void SendCommandsToXplorer();
    void ClearInstructions();

    ves::conductor::util::CORBAServiceList* serviceList;

    DECLARE_EVENT_TABLE()

};

#endif //HYPER_LAB_UI_DIALOG_H
