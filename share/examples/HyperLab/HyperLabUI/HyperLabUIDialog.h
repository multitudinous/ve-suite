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
class wxRadioBox;
class wxSlider;

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

private:
    void BuildGUI();
    void UpdateGUI();

    void OnShaderEffects( wxCommandEvent& event );
    void OnAmbientRGB( wxCommandEvent& event );
    void OnDiffuseRGB( wxCommandEvent& event );
    void OnSpecularRGB( wxCommandEvent& event );
    void OnOK( wxCommandEvent& event );
    void OnCancel( wxCommandEvent& event );

    void SendCommandsToXplorer();
    void ClearInstructions();

    wxTextCtrl* m_portTextCtrl;

    wxRadioBox* m_shaderEffectsRadioBox;

    wxSlider* m_arSlider;
    wxSlider* m_agSlider;
    wxSlider* m_abSlider;
    wxSlider* m_drSlider;
    wxSlider* m_dgSlider;
    wxSlider* m_dbSlider;
    wxSlider* m_srSlider;
    wxSlider* m_sgSlider;
    wxSlider* m_sbSlider;

    std::vector< ves::open::xml::DataValuePairSharedPtr > m_instructions;
    std::string m_commandName;

    ves::conductor::util::CORBAServiceList* m_serviceList;

    DECLARE_EVENT_TABLE()
};

#endif //HYPER_LAB_UI_DIALOG_H
