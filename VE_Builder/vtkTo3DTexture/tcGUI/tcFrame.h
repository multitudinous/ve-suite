#ifndef _TC_FRAME_H_
#define _TC_FRAME_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include "wx/wx.h"
#include "wx/spinctrl.h"
#include <wx/progdlg.h>
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
   GRID_RBOX,
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
   enum GridType{STRUCTURED,UNSTRUCTURED,RECTILINEAR};
   void UpdateStatus(const char* statusString);
   void SetGridType(GridType type){_type = type;}
   void UpdateProgressDialog(const char* msg);

   unsigned int GetNumberOfTimeSteps(){return _numFiles;}

protected:
   GridType _type;
   void _buildGUI();
   int _numFiles;
   int _resolution[3];
   int _currentFile;
   std::vector<const char*> _inputFiles;
   wxString _inputDir;
   wxString _outputDir;

   wxString _outputTextFile;

   wxDirDialog* _dirDialog;
   
   wxArrayString _gridFiles;

   wxButton* _browseInputDir;
   wxButton* _browseOutputDir;
   wxButton* _goButton;
   wxButton* _quitButton;

   wxGauge* _transProgress;
   wxProgressDialog* _fileProgress;

   wxTextCtrl* _inputDirBox;
   wxTextCtrl* _outputDirBox;
   wxSpinCtrl* _numFilesBox;

   wxComboBox* _xResBox;
   wxComboBox* _yResBox;
   wxComboBox* _zResBox;

   wxRadioBox* _gridTypeBox;

   VTKDataToTexture* _translator;

   
   //event callbacks
   void _onQuitCallback(wxCommandEvent& event);
   void _onTranslateCallback(wxCommandEvent& event);
   void _onBrowseCallback(wxCommandEvent& event);
   void _chooseDirectory(int style, int browseID);
   void _onResolutionCallback(wxCommandEvent& event);
   void _onNumFilesCallback(wxSpinEvent& event);
   void _onGridTypeCallback(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif//_TC_FRAME_H_
