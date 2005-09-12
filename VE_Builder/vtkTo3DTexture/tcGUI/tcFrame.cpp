
#include "tcFrame.h"
#include "wx/string.h"
#include <wx/dir.h>
#include <wx/filename.h>
#include <iostream>
#include <sstream>
#include <cmath>
BEGIN_EVENT_TABLE(TCFrame,wxFrame)
   EVT_BUTTON(TRANSLATE_BUTTON,TCFrame::_onTranslateCallback)
   EVT_BUTTON(INPUT_BROWSE,TCFrame::_onBrowseCallback)
   EVT_BUTTON(OUTPUT_BROWSE,TCFrame::_onBrowseCallback)
   EVT_BUTTON(QUIT_BUTTON,TCFrame::_onQuitCallback)
   EVT_COMBOBOX(XRES_BOX,TCFrame::_onResolutionCallback)
   EVT_COMBOBOX(YRES_BOX,TCFrame::_onResolutionCallback)
   EVT_COMBOBOX(ZRES_BOX,TCFrame::_onResolutionCallback)
   EVT_SPINCTRL(NUM_FILES,TCFrame::_onNumFilesCallback)
   EVT_RADIOBOX(GRID_RBOX,TCFrame::_onGridTypeCallback)
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
   _type = UNSTRUCTURED;
   _translator = new VTKDataToTexture();
   _translator->setParentGUI(this);
   _currentFile = 0;
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
   if(_gridTypeBox){
      delete _gridTypeBox;
      _gridTypeBox = 0;
   }
   _gridFiles.Clear();
     
   if(!_inputFiles.empty()){
      for ( unsigned int i = 0; i < _inputFiles.size(); ++i )
         delete [] _inputFiles.at( i );
      _inputFiles.clear();
   }
}
/////////////////////////
void TCFrame::_buildGUI()
{
   //sizers for our gui's main sections
   wxBoxSizer* frameSizer = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* rightSizer = new wxBoxSizer(wxVERTICAL);
   
   wxStaticBox* dimensionsGroup = new wxStaticBox(this, -1,"Texture Dimensions");
   wxStaticBoxSizer* textureDimensionsSizer = new wxStaticBoxSizer(dimensionsGroup,
                                                            wxHORIZONTAL);

   //the right section
   wxBoxSizer* buttonRow = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row1 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row2 = new wxBoxSizer(wxHORIZONTAL);

   wxArrayString factorsOfTwo;

   factorsOfTwo.Add("2");
   factorsOfTwo.Add("4");
   factorsOfTwo.Add("8");
   factorsOfTwo.Add("16");
   factorsOfTwo.Add("32");
   factorsOfTwo.Add("64");
   factorsOfTwo.Add("128");
   factorsOfTwo.Add("256");
   factorsOfTwo.Add("512");

   wxString choices[] = {"Rectilinear","Structured","Unstructured"};
   wxBoxSizer* rBoxSizer = new wxBoxSizer(wxHORIZONTAL);
   _gridTypeBox = new wxRadioBox(this, 
                              GRID_RBOX, 
                              wxString("Grid Type"), 
                              wxDefaultPosition, 
                              wxDefaultSize, 
                              3, choices,wxRA_SPECIFY_COLS);
   rBoxSizer->Add(_gridTypeBox,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //the resolution input boxes
   _xResBox = new wxComboBox(this, XRES_BOX, "", 
                                wxDefaultPosition,
                                wxDefaultSize,
                                factorsOfTwo,
                                wxCB_READONLY,
                                wxDefaultValidator,
                                  "X");
   _xResBox->SetSelection(0);
   
   _yResBox = new wxComboBox(this, YRES_BOX, "", 
                                wxDefaultPosition,
                                wxDefaultSize,
                                factorsOfTwo,
                                wxCB_READONLY,
                                wxDefaultValidator,
                                  "Y");
   _yResBox->SetSelection(0);
   
   _zResBox = new wxComboBox(this, ZRES_BOX, "", 
                                wxDefaultPosition,
                                wxDefaultSize,
                                factorsOfTwo,
                                wxCB_READONLY,
                                wxDefaultValidator,
                                  "Z");
   _zResBox->SetSelection(0);
   
   //resolution row
   textureDimensionsSizer->Add(_xResBox,1,wxALIGN_CENTER_HORIZONTAL);
   textureDimensionsSizer->Add(_yResBox,1,wxALIGN_CENTER_HORIZONTAL);
   textureDimensionsSizer->Add(_zResBox,1,wxALIGN_CENTER_HORIZONTAL);

   //add the input text box and browse button to the two row
   //row 1 
   _inputDirBox =  new wxTextCtrl(this,INPUT_TEXT_BOX);
   _browseInputDir = new wxButton(this,INPUT_BROWSE,wxT("Input dir"));

   row1->Add(_inputDirBox,3,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);
   row1->Add(_browseInputDir,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);

   //row 2
   _outputDirBox =  new wxTextCtrl(this,OUTPUT_TEXT_BOX);
   _browseOutputDir = new wxButton(this,OUTPUT_BROWSE,wxT("Output dir"));
 
   row2->Add(_outputDirBox,3,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);
   row2->Add(_browseOutputDir,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);

   //quit button
   _quitButton = new wxButton(this,QUIT_BUTTON,wxT("Quit"));
   _goButton = new wxButton(this,TRANSLATE_BUTTON,wxT("Translate"));

   buttonRow->Add(_goButton,0,wxALIGN_CENTER);
   buttonRow->Add(_quitButton,0,wxALIGN_CENTER);

   rightSizer->Add(textureDimensionsSizer,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   rightSizer->Add(rBoxSizer,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   rightSizer->Add(row1,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);
   rightSizer->Add(row2,1,wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER_HORIZONTAL);
   rightSizer->Add(buttonRow,0,wxALIGN_CENTER);
  
   frameSizer->Add(rightSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   frameSizer->Layout();
   SetSizer(frameSizer);

   SetAutoLayout(true); 
   frameSizer->Fit(this);
   SetStatusBar(new wxStatusBar(this,-1)); 
   GetStatusBar()->SetStatusText(wxString("..."));
}
///////////////////////////////////////////////////
void TCFrame::UpdateProgressDialog(const char* msg)
{
   if(_fileProgress){
      _fileProgress->Update(_currentFile,msg);
   }
}
/////////////////////////////////////////////////////
void TCFrame::UpdateStatus(const char* statusString)
{
   GetStatusBar()->SetStatusText(wxString(statusString));
}
/////////////////////////////////////////////////////////
//event callbacks                                      //
/////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////
void TCFrame::_onGridTypeCallback(wxCommandEvent& event)
{
   UpdateStatus("Switching grid type. . .");
   int type = _gridTypeBox->GetSelection();
   switch (type){
      case 0:
         _type = RECTILINEAR;
         _translator->setRectilinearGrid();
         UpdateStatus("Grid type: RECTILINEAR");   
         break;
      case 1:
         _type = STRUCTURED;
         _translator->setStructuredGrid();
         UpdateStatus("Grid type: STRUCTURED");
         break;
      case 2:
         _type = UNSTRUCTURED;
         _translator->setUnstructuredGrid();
         UpdateStatus("Grid type: UNSTRUCTURED");
      default:
         break;
   };
}
/////////////////////////////////////////////////////////
void TCFrame::_onTranslateCallback(wxCommandEvent& event)
{
   _translator->setBatchOff();
   UpdateStatus("Translating to 3D texture files. . .");
   //char* oname;
   wxString statusMsg = "";
   _fileProgress = new wxProgressDialog(wxString("Translation Progress"),
                  " ", 
                  _numFiles,this,
                  wxPD_AUTO_HIDE|wxPD_SMOOTH|wxPD_ELAPSED_TIME|wxPD_ESTIMATED_TIME);
   //process the files
   for(int i = 0; i < _numFiles; i++){
      _currentFile = i;
      statusMsg = wxString("Translating ") + _gridFiles[i];
      UpdateStatus(statusMsg);
      _fileProgress->Update(i, statusMsg);
      _translator->reset();
      _translator->setOutputDirectory((char*)_outputDir.c_str());
      //sprintf(oname,"_%d",i);
	  std::ostringstream dirStringStream;
	  dirStringStream << "_" << i;
	  std::string dirString = dirStringStream.str();
	  //oname = (char*)dirString.c_str();
      _translator->createDataSetFromFile(_gridFiles[i].c_str());
      _translator->setOutputResolution(_resolution[0],
                                   _resolution[1],
                                   _resolution[2]);
      _translator->setVelocityFileName((char*)dirString.c_str());
      _translator->createTextures();
   }
   statusMsg = wxString("Files written to: ") + _outputDir;
   UpdateStatus(statusMsg);
   if(_fileProgress){
      delete _fileProgress;
      _fileProgress = 0;
   }
   _translator->reInit();
}
//////////////////////////////////////////////////////////
void TCFrame::_onResolutionCallback(wxCommandEvent& event)
{
   int value = event.GetSelection()+1;
   int id = event.GetId();

   switch (id){
      case XRES_BOX:
         _resolution[0] = (int)std::pow(2.,value);
         break;
      case YRES_BOX:
         _resolution[1] = (int)std::pow(2.,value);
         break;
      case ZRES_BOX:
         _resolution[2] = (int)std::pow(2.,value);
         break;
      default:
         break;
   };
}
////////////////////////////////////////////////////
void TCFrame::_onQuitCallback(wxCommandEvent& event)
{
   Destroy();
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
   wxDir inputFileDirectory;
   //get the input from the user 
   if (_dirDialog->ShowModal() == wxID_OK){
      if(browseID == INPUT_BROWSE){
         _gridFiles.Clear();
         _numFiles = 0;
         _inputDir = wxT(_dirDialog->GetPath().c_str());
         
         //make the output dir == to the current input directory
         _outputDir = wxT(_inputDir.c_str());

         inputFileDirectory.Open(wxString(_inputDir));
         if(inputFileDirectory.IsOpened()){
            wxString path;
            wxFileName filename;
            wxString fullPath;
            bool cont = inputFileDirectory.GetFirst(&path);
            while ( cont ){
               fullPath = wxString(_inputDir) + wxString("/") +path;
               filename.Assign(fullPath);
               if(filename.HasExt()){
                  if(filename.GetExt()==wxString("vtk")||
                    filename.GetExt()==wxString("vtu")){
                     _gridFiles.Add(fullPath);
                  }
               }
               cont = inputFileDirectory.GetNext(&path);
            }
            _numFiles = _gridFiles.GetCount();
         }
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
////////////////////////////////////////////////////////
//Batch translation interfaces                        //
////////////////////////////////////////////////////////
////////////////////////////////////////////////////////
void TCFrame::SetInputDirectory(const char* inDirectory)
{
   if(inDirectory){
      if(!_gridFiles.IsEmpty()){
         _gridFiles.Clear();
      }
     _numFiles = 0;

     wxDir inputFileDirectory;
     inputFileDirectory.Open(wxString(inDirectory));

     if(inputFileDirectory.IsOpened()){
        _inputDir = wxT(inDirectory);
        wxString path;
        wxFileName filename;
        wxString fullPath;
        bool cont = inputFileDirectory.GetFirst(&path);
        while ( cont ){
           fullPath = wxString(_inputDir) + wxString("/") +path;
           filename.Assign(fullPath);
           if(filename.HasExt()){
              if(filename.GetExt()==wxString("vtk")||
                 filename.GetExt()==wxString("vtu")){
                 _gridFiles.Add(fullPath);
              }
           }
           cont = inputFileDirectory.GetNext(&path);
        }
      }else{
        std::cout<<"Couldn't open directory: "<<inDirectory<<std::endl;
      }
      _numFiles = _gridFiles.GetCount();
   }
}
//////////////////////////////////////////////////////////
void TCFrame::SetOutputDirectory(const char* outDirectory)
{
   _outputDir = wxString(outDirectory);
}
//////////////////////////////////////////////////////
void TCFrame::SetTextureResolution(int x,int y, int z)
{
   _resolution[0] = x;
   _resolution[1] = y;
   _resolution[2] = z;
}
/////////////////////////////////////////////////
void TCFrame::SetGridType(GridType type)
{
   switch (type){
      case RECTILINEAR:
         _translator->setRectilinearGrid();
         break;
      case STRUCTURED:
         _translator->setStructuredGrid();
         break;
      case UNSTRUCTURED:
         _translator->setUnstructuredGrid();
      default:
         break;
   };
   _type = type;
}
////////////////////////////////
void TCFrame::BatchTranslation()
{
   _translator->setBatchOn();
   //process the files
   //char* oname;
   for(int i = 0; i < _numFiles; i++){
      _translator->reset();
      _translator->setOutputDirectory((char*)_outputDir.c_str());
      //sprintf(oname,"_%d",i);
	  std::ostringstream dirStringStream;
	  dirStringStream << "_" << i;
	  std::string dirString = dirStringStream.str();
	  //oname = (char*)dirString.c_str();
      _translator->createDataSetFromFile(_gridFiles[i].c_str());
      _translator->setOutputResolution(_resolution[0],
                                   _resolution[1],
                                   _resolution[2]);
      _translator->setVelocityFileName((char*)dirString.c_str());
      _translator->createTextures();
   }
}
