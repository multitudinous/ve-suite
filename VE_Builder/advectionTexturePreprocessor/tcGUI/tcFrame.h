#ifndef _TC_FRAME_H_
#define _TC_FRAME_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "wx/spinctrl.h"
#include "tcFrame.h"
#include "textureCreator.h"

enum TCFrameIDs{
   TRANSLATE_BUTTON,
   INPUT_TEXT_BOX,
   OUTPUT_TEXT_BOX,
   INPUT_BROWSE,
   OUTPUT_BROWSE,
   XRES_BOX,
   YRES_BOX,
   ZRES_BOX,
   NUM_FILES,
   QUIT_BUTTON
};
class TCFrame: public wxFrame{
public:
   TCFrame (wxWindow* parent,
           wxWindowID id, 
           const wxString& title,
           const wxPoint& pos = wxDefaultPosition, 
           const wxSize& size = wxDefaultSize, 
           long style = wxDEFAULT_FRAME_STYLE);

   virtual ~TCFrame();

protected:
   void _buildGUI();
   int _numFiles;
   int _resolution[3];
   wxString _inputDir;
   wxString _outputDir;

   wxString _outputTextFile;

   wxDirDialog* _dirDialog;

   wxButton* _browseInputDir;
   wxButton* _browseOutputDir;
   wxButton* _goButton;
   wxButton* _quitButton;

   wxGauge* _transProgress;
   wxGauge* _fileProgress;

   wxTextCtrl* _inputDirBox;
   wxTextCtrl* _outputDirBox;
   wxSpinCtrl* _numFilesBox;

   wxSpinCtrl* _xResBox;
   wxSpinCtrl* _yResBox;
   wxSpinCtrl* _zResBox;

   VTKDataToTexture* _translator;

   
   //event callbacks
   void _onQuitCallback(wxCommandEvent& event);
   void _onTranslateCallback(wxCommandEvent& event);
   void _onBrowseCallback(wxCommandEvent& event);
   void _chooseDirectory(int style, int browseID);
   void _onResolutionCallback(wxSpinEvent& event);
   void _onNumFilesCallback(wxSpinEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif//_TC_FRAME_H_
