
#include "tcFrame.h"
#include "wx/string.h"
#include <iostream>
#include <cmath>
BEGIN_EVENT_TABLE(TCFrame,wxFrame)
   EVT_BUTTON(TRANSLATE_BUTTON,TCFrame::_onTranslateCallback)
   EVT_BUTTON(INPUT_BROWSE,TCFrame::_onBrowseCallback)
   EVT_BUTTON(OUTPUT_BROWSE,TCFrame::_onBrowseCallback)
   EVT_BUTTON(QUIT_BUTTON,TCFrame::_onQuitCallback)
   EVT_SPINCTRL(XRES_BOX,TCFrame::_onResolutionCallback)
   EVT_SPINCTRL(YRES_BOX,TCFrame::_onResolutionCallback)
   EVT_SPINCTRL(ZRES_BOX,TCFrame::_onResolutionCallback)
   EVT_SPINCTRL(NUM_FILES,TCFrame::_onNumFilesCallback)
END_EVENT_TABLE()
////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
TCFrame::TCFrame(wxWindow* parent,
           wxWindowID id, 
           const wxString& title,
           const wxPoint& pos , 
           const wxSize& size, 
           long style)
:wxFrame(parent, -1, title,pos, size, style)
{
   _numFiles = 0;
   _inputDir = wxT("./");
   _outputDir = wxT("./");

   _outputTextFile = wxT("./out.txt");

   _dirDialog = 0;

   _browseInputDir = 0;
   _browseOutputDir = 0;
   _goButton = 0;
   _quitButton = 0;
   _translator = 0;

   _transProgress = 0;
   _fileProgress = 0;

   _inputDirBox = 0;
   _outputDirBox = 0;

   _xResBox = 0;
   _yResBox = 0;
   _zResBox = 0;

   _numFilesBox = 0;

   _resolution[0] = 2;
   _resolution[1] = 2;
   _resolution[2] = 2;
   _buildGUI();

}
///////////////////
TCFrame::~TCFrame()
{
   if(_translator){
      delete _translator;
      _translator = 0;
   }

   if(_dirDialog){
      delete _dirDialog;
      _dirDialog = 0;
   }
   if(_browseInputDir){
      delete _browseInputDir;
      _browseInputDir = 0;
   }
   if(_browseOutputDir){
      delete _browseOutputDir;
      _browseOutputDir = 0;
   }
   if(_goButton){
      delete _goButton;
      _goButton = 0;
   }

   if(_transProgress){
      delete _transProgress;
      _transProgress = 0;
   }
   if(_fileProgress){
      delete _fileProgress;
      _fileProgress = 0;
    
   }
   if(_inputDirBox){
      delete _inputDirBox;
      _inputDirBox = 0;
   }
   if(_outputDirBox){
      delete _outputDirBox;
      _outputDirBox = 0; 
   }
   if(_quitButton){
      delete _quitButton;
      _quitButton = 0;
   }

   if(_xResBox){
      delete _xResBox;
      _xResBox = 0;
   }
   if(_yResBox){
      delete _yResBox;
      _yResBox = 0;
   }
   if(_zResBox){
      delete _zResBox;
      _zResBox = 0;
   }
   if(_numFilesBox){
      delete _numFilesBox;
      _numFilesBox = 0;
   }
   if(_inputFiles.size()){
      _inputFiles.clear();
   }
}
/////////////////////////
void TCFrame::_buildGUI()
{
   //create the panel for this
   wxPanel* panel = new wxPanel(this);
   wxBoxSizer* panelSizer = new wxBoxSizer(wxHORIZONTAL);
   
   //sizers for our gui's main sections
   wxBoxSizer* frameSizer = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* leftSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* rightSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* progressSizer = new wxBoxSizer(wxHORIZONTAL);
   

   //file progress
   _fileProgress = new wxGauge(panel,
                            wxID_ANY, 200, 
                            wxDefaultPosition, 
                            wxDefaultSize,
                            wxGA_VERTICAL|wxNO_BORDER );
   //translation progress
   _transProgress = new wxGauge(panel,
                            wxID_ANY, 200, 
                            wxDefaultPosition, 
                            wxDefaultSize,
                            wxGA_VERTICAL|wxNO_BORDER );
   //add the progress bars
   progressSizer->Add(_fileProgress,1,wxALIGN_CENTER|wxEXPAND);
   progressSizer->Add(_transProgress,1,wxALIGN_CENTER|wxEXPAND);

   
   //add the progress sizer
   leftSizer->Add(progressSizer,1,wxALIGN_CENTER);
   
   //the update button
   _goButton = new wxButton(panel,TRANSLATE_BUTTON,wxT("Translate"));
   leftSizer->Add(_goButton,0,wxALIGN_CENTER);

   _numFilesBox = new wxSpinCtrl(panel,NUM_FILES);
   leftSizer->Add(_numFilesBox,0,wxALIGN_CENTER);

   //the right section
   wxBoxSizer* row0 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row1 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row2 = new wxBoxSizer(wxHORIZONTAL);

   //the resolution input boxes
   _xResBox = new wxSpinCtrl(panel, 
                       XRES_BOX, 
                       wxEmptyString, 
                       wxDefaultPosition, 
                       wxDefaultSize, 
                       wxSP_ARROW_KEYS, 
                       1, 9, 
                       1, 
                       wxT("X Res"));
   
   _yResBox = new wxSpinCtrl(panel, 
                       YRES_BOX, 
                       wxEmptyString, 
                       wxDefaultPosition, 
                       wxDefaultSize, 
                       wxSP_ARROW_KEYS, 
                       1, 9, 
                       1, 
                       wxT("Y Res"));
   
   _zResBox = new wxSpinCtrl(panel, 
                       ZRES_BOX, 
                       wxEmptyString, 
                       wxDefaultPosition, 
                       wxDefaultSize, 
                       wxSP_ARROW_KEYS, 
                       1, 9, 
                       1, 
                       wxT("Z Res"));

   
   //resolution row
   row0->Add(_xResBox,1,wxEXPAND);
   row0->Add(_yResBox,1,wxEXPAND);
   row0->Add(_zResBox,1,wxEXPAND);

   //add the input text box and browse button to the two row
   //row 1
   //wxStaticText inDirName(panel,-1,"Input Directory");  
   _inputDirBox =  new wxTextCtrl(panel,INPUT_TEXT_BOX);
   _browseInputDir = new wxButton(panel,INPUT_BROWSE,wxT("Input dir"));

   
   row1->Add(_inputDirBox,3,wxEXPAND);
   row1->Add(_browseInputDir,1,wxEXPAND);

   //row 2
   //wxStaticText outDirName(panel,-1,"Output Directory");
   _outputDirBox =  new wxTextCtrl(panel,OUTPUT_TEXT_BOX);
   _browseOutputDir = new wxButton(panel,OUTPUT_BROWSE,wxT("Output dir"));
 
   row2->Add(_outputDirBox,3,wxEXPAND);
   row2->Add(_browseOutputDir,1,wxEXPAND);

   //quit button
   _quitButton = new wxButton(panel,QUIT_BUTTON,wxT("Quit"));

   rightSizer->Add(row0,0,wxALIGN_CENTER);
   rightSizer->Add(row1,0,wxALIGN_CENTER);
   rightSizer->Add(row2,0,wxALIGN_CENTER);
   rightSizer->Add(_quitButton,0,wxALIGN_CENTER);
  
   //add the sizers to the panel
   panelSizer->Add(leftSizer,0,wxEXPAND);
   panelSizer->Add(rightSizer,1,wxEXPAND|wxALIGN_CENTER);
   
   panel->SetSizer(panelSizer);
   panelSizer->Layout();

   panel->SetAutoLayout(true); 
   panelSizer->Fit(panel);

   frameSizer->Add(panel,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   frameSizer->Layout();
   SetSizer(frameSizer);

   SetAutoLayout(true); 
   frameSizer->Fit(this);
}
/////////////////////////////////////////////////////////
//event callbacks                                      //
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
void TCFrame::_onTranslateCallback(wxCommandEvent& event)
{
   if(!_translator){
      _translator = new VTKDataToTexture();
      ////biv--FIXME:need to fix this to be selectable from the GUI
      //_translator->setRectilinearGrid();
   }
   //vtkUnstructuredGridReader* usgrid = vtkUnstructuredGridReader::New();
   char iname[1024];
   char oname[256];
   char fileName[256];
   _fileProgress->SetRange(_numFiles);

   //read in the file names,breaking out for parallel junk -- still need to fix this to 
   //find files in the dir but w/ the new format may be
   //much trickerier than before
   for(unsigned int i = 0; i < _numFiles; i++){
      strcpy(iname,_inputDir);
      strcpy(fileName,"/picker.vtk");
      //sprintf(fileName,"/picker.vtk",i);
      //sprintf(fileName,"/zhto0%dad.vtk",i);
      //sprintf(fileName,"/flowdata_%d.vtk",i);
      strcat(iname,fileName);
      _inputFiles.push_back(iname);
   }

   //process the files
   for(int i = 0; i < _numFiles; i++){
      _fileProgress->SetValue(i);
      _translator->reset();
      _translator->setOutputDirectory((char*)_outputDir.c_str());
      //biv--FIXME:this filename should be selectable from the gui as well!!!
      
      /*strcpy(fileName,"/picker.vtk");
      //sprintf(fileName,"/picker.vtk",i);
      //sprintf(fileName,"/zhto0%dad.vtk",i);
      //sprintf(fileName,"/flowdata_%d.vtk",i);
      strcat(iname,fileName);*/

      sprintf(oname,"_%d",i);

      _translator->createDataSetFromFile(_inputFiles.at(i));
      _translator->setOutputResolution(_resolution[0],
                                   _resolution[1],
                                   _resolution[2]);
      _translator->setVelocityFileName(oname);
     

      _translator->createTextures();
   }
   //biv--FIXME:this isn't working how it should be. . .
   _fileProgress->SetValue(_numFiles);
}
//////////////////////////////////////////////////////////
void TCFrame::_onResolutionCallback(wxSpinEvent& event)
{
   float factorOfTwo = 2;
   int value = event.GetPosition();
   int id = event.GetId();
   std::cout<<"The value: "<<value<<std::endl;

   switch (id){
      case XRES_BOX:
         _resolution[0] = pow(2,value);
         std::cout<<"The X resolution: "<<_resolution[0]<<std::endl;
         break;
      case YRES_BOX:
         _resolution[1] = pow(2,value);
         std::cout<<"The Y resolution: "<<_resolution[1]<<std::endl;
         break;
      case ZRES_BOX:
         _resolution[2] = pow(2,value);
         std::cout<<"The Z resolution: "<<_resolution[1]<<std::endl;
         
         break;
      default:
         break;
   };
}
////////////////////////////////////////////////////
void TCFrame::_onQuitCallback(wxCommandEvent& event)
{
   exit(0);
}
//////////////////////////////////////////////////////
void TCFrame::_onNumFilesCallback(wxSpinEvent& event)
{
   _numFiles = event.GetPosition();
}
////////////////////////////////////////////////////////
void TCFrame::_chooseDirectory(int style, int browseID)
{
   if(!_dirDialog){
      _dirDialog = new wxDirDialog(this,
          _T("Choose a directory"), wxT("./"), style);
   }else{
      //set the path to the current stored path
      if(browseID == INPUT_BROWSE){
         _dirDialog->SetPath(_inputDir);
      }else{
         _dirDialog->SetPath(_outputDir);
      }
   }

   //get the input from the user 
   if (_dirDialog->ShowModal() == wxID_OK){
      if(browseID == INPUT_BROWSE){
         _inputDir = wxT(_dirDialog->GetPath().c_str());
         
         //set the text in the text box
         _inputDirBox->SetValue(_inputDir);
      }else{
         _outputDir = wxT(_dirDialog->GetPath().c_str());
         _outputDirBox->SetValue(_outputDir);
      }
   }
}
//////////////////////////////////////////////////////
void TCFrame::_onBrowseCallback(wxCommandEvent& event )
{
    _chooseDirectory(wxDD_DEFAULT_STYLE & ~wxDD_NEW_DIR_BUTTON,
                   event.GetId());
}
   
