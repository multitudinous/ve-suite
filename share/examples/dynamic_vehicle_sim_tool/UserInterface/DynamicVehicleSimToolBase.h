///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#ifndef __DynamicVehicleSimToolBase__
#define __DynamicVehicleSimToolBase__

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
#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/scrolwin.h>
#include <wx/dialog.h>

#include <ves/conductor/UIDialog.h>

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
            wxTextCtrl* m_textCtrl1;
            wxTextCtrl* m_textCtrl2;
            wxToggleButton* m_toggleBtn1;
            wxButton* m_resetButton;
            wxScrolledWindow* m_scrolledWindow1;
            wxStaticText* m_staticText1;
            wxChoice* m_choice1;
            wxStaticText* m_staticText11;
            wxChoice* m_choice11;
            wxButton* m_button4;
            wxButton* m_button5;
            wxChoice* m_choice3;
            wxStdDialogButtonSizer* m_sdbSizer1;
            wxButton* m_sdbSizer1OK;
            wxButton* m_sdbSizer1Apply;
            wxBoxSizer* bSizer9;

            // Virtual event handlers, overide them in your derived class
            virtual void OnComputerNameEnter( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnPortNumberEnter( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnStartStopButton( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnResetSimulation( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnGeometryDataMapping( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnAddGeometryGroupButton( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnRemoveGeometryGroupButton( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnConstrainedGeometrySelection( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnApplyButton( wxCommandEvent& event ){ event.Skip(); }
            virtual void OnOKButton( wxCommandEvent& event ){ event.Skip(); }
            
        
        public:
            DynamicVehicleSimToolBase( wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxT("Dynamic Vehicle Sim Tool"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxDEFAULT_DIALOG_STYLE );
            ~DynamicVehicleSimToolBase();
        
    };
    
} // namespace dvst

#endif //__DynamicVehicleSimToolBase__
