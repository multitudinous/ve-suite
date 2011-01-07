///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Sep 12 2010)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#ifndef __DynamicVehicleSimToolBase__
#define __DynamicVehicleSimToolBase__
#include <ves/conductor/UIDialog.h>

#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/tglbtn.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/scrolwin.h>
#include <wx/dialog.h>

///////////////////////////////////////////////////////////////////////////

namespace dvst
{
///////////////////////////////////////////////////////////////////////////////
/// Class DynamicVehicleSimToolBase
///////////////////////////////////////////////////////////////////////////////
class DynamicVehicleSimToolBase : public ves::conductor::UIDialog 
{
private:

protected:
    wxTextCtrl* m_computerTextCtrl;
    wxTextCtrl* m_portTextCtrl;
    wxToggleButton* m_toggleBtn1;
    wxButton* m_resetButton;
    wxChoice* m_simScale;
    wxScrolledWindow* m_scrolledWindow1;
    wxBoxSizer* m_scrolledWindowSizer;
    wxStaticText* m_staticText1;
    wxChoice* m_choice1;
    wxStaticText* m_staticText11;
    wxChoice* m_choice11;
    wxButton* m_addButton;
    wxButton* m_removeButton;
    wxChoice* m_constrainedGeomChoice;
    wxChoice* m_frontBird;
    wxChoice* m_leftRearBird;
    wxChoice* m_rightRearBird;
    wxTextCtrl* m_xSIPOffset;
    wxTextCtrl* m_ySIPOffset;
    wxTextCtrl* m_zSIPOffset;
    wxButton* m_registrationButton;
    wxStdDialogButtonSizer* m_sdbSizer1;
    wxButton* m_sdbSizer1OK;
    wxButton* m_sdbSizer1Apply;
    
    // Virtual event handlers, overide them in your derived class
    virtual void OnComputerNameEnter( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnPortNumberEnter( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnStartStopButton( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnResetSimulation( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnGeometryDataMapping( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnAddGeometryGroupButton( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnRemoveGeometryGroupButton( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnConstrainedGeometrySelection( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnRegisterButton( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnApplyButton( wxCommandEvent& event ) { event.Skip(); }
    virtual void OnOKButton( wxCommandEvent& event ) { event.Skip(); }
    

public:
    
    DynamicVehicleSimToolBase( wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxT("Dynamic Vehicle Sim Tool"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxSize( 400,800 ), long style = wxDEFAULT_DIALOG_STYLE );
    ~DynamicVehicleSimToolBase();
};
} // namespace dvst

#endif //__DynamicVehicleSimToolBase__
