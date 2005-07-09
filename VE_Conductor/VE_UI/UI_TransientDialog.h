#ifndef _VE_UI_TRANSIENT_DIALOG_H_
#define _VE_UI_TRANSIENT_DIALOG_H_


#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
#include <wx/spinctrl.h>
#include <wx/image.h>
#include <wx/bmpbuttn.h>
class UI_Tabs;
class wxSpinCtrlDbl;
//Transient control ids
enum TRANS_DIALOG_IDS
{
   PLAY_BUTTON,
   STOP_BUTTON,
   FORWARD_STEP_BUTTON,
   BACKWARD_STEP_BUTTON,
   DURATION_CNTL_BOX,
   CURRENT_FRAME
};

class UI_TransientDialog : public wxDialog {
public:
   UI_TransientDialog(int numTimeSteps,
                    wxWindow* parent, 
		               wxWindowID id = -1, 
                    const wxString& title = "Transient Controls", 
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxSize(200,100),
                    long style = wxDEFAULT_DIALOG_STYLE, 
                    const wxString& name = "Transient Controls");
   ~UI_TransientDialog(){};

   void SetTabControl(UI_Tabs* tab);
protected:
   void _buildDialog();

   unsigned int _nTimeSteps;

   UI_Tabs* _tab;
   wxImage* _playImage;
   wxImage* _stopImage;
   wxImage* _forwardImage;
   wxImage* _backwardImage;

   wxBitmapButton* _playButton;
   wxBitmapButton* _stopButton;

   wxBitmapButton* _nextButton;
   wxBitmapButton* _prevButton;

   wxSpinCtrl* _currentFrame;
   wxSpinCtrlDbl* _duration;
   void _onBackwardStep(wxCommandEvent& event);
   void _onForwardStep(wxCommandEvent& event);
   void _onPlay(wxCommandEvent& event);
   void _onStop(wxCommandEvent& event);
   void _onSelectFrame(wxSpinEvent& event);
   void _onSetDuration(wxSpinEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_TRANSIENT_DIALOG_H_

