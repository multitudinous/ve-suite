///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#ifndef __ScalarControlDialog__
#define __ScalarControlDialog__

#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/gdicmn.h>
#include <wx/font.h>
#include <wx/colour.h>
#include <wx/settings.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/statbox.h>
#include <wx/dialog.h>

///////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
/// Class ScalarControlDialog
///////////////////////////////////////////////////////////////////////////////
namespace ves
{
namespace conductor
{
class ScalarControlDialog : public wxDialog 
{
	private:
	
	protected:
		wxTextCtrl* m_minTextCtrl;
		wxSlider* m_minSlider;
		wxStaticLine* m_staticline1;
		wxTextCtrl* m_maxTextControl;
		wxSlider* m_maxSlider;
		
		// Virtual event handlers, overide them in your derived class
		virtual void OnMinTextInput( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnMinSlider( wxScrollEvent& event ){ event.Skip(); }
		virtual void OnMaxTextInput( wxCommandEvent& event ){ event.Skip(); }
		virtual void OnMaxSlider( wxScrollEvent& event ){ event.Skip(); }
		
	
	public:
		ScalarControlDialog( wxWindow* parent, wxWindowID id = wxID_ANY, const wxString& title = wxT("Scalar Control"), const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxCLOSE_BOX|wxDEFAULT_DIALOG_STYLE|wxMAXIMIZE_BOX|wxMINIMIZE_BOX );
		~ScalarControlDialog();
	
};
}
}
#endif //__ScalarControlDialog__
